# Environment -------------------------------------------------------------

rm(list = ls())

# Packages ----------------------------------------------------------------

library(ordinal)
library(tidyverse)
library(ggeffects)
library(lme4)
library(gghalves)

# custom functions

make_block <- function(x, k){
  l <- length(x)
  res <- rep(1:k, each = floor(l / k))
  if(length(res) < l){
    res <- c(res, rep(tail(res, 1), l - length(res)))
  }
  return(res)
}

gg_theme <- function(base_size = 20){
    theme_minimal(base_size = base_size)
}

# Importing Data ----------------------------------------------------------

dat <- read_rds("data/clean/behavioral/dat_clean.rds")

# Adding blocks ------------------------------------------------------------------

dat <- dat |> 
  group_by(subject) |> 
  mutate(trial = 1:n(),
         block = make_block(trial, 3)) |>  # make 3 blocks
  ungroup()

dat$block <- factor(dat$block)
dat$trial_type <- factor(dat$trial_type)
contrasts(dat$trial_type) <- contr.sum(2)/2 # sum to 0 contrasts for traditional criterion
dat$trial_type_c <- contrasts(dat$trial_type)[dat$trial_type]
dat$pas1_234_rev <- ifelse(dat$pas1_234 == 1, 0, 1) # revert for the correct sign from glm

# checking number of trials

# dat |> 
#   group_by(subject, block, trial_type) |> 
#   count() |> 
#   data.frame()

# PAS stability plot

# in this plot we show the pattern of PAS responses for valid/catch trials
# during the experiment divided into three blocks
# for each subject we count the number of responses in each condition and the
# boxplots represent the variability across subjects

pas_block_plot <- dat |> 
  count(subject, pas, block, trial_type)  |> 
  mutate(block = paste("Block", block),
         trial_type = str_to_title(trial_type),
         isaware = ifelse(pas == 1, "Unaware", "Aware")) |> 
  ggplot(aes(x = factor(pas), y = n, fill = isaware)) +
  geom_boxplot(show.legend = FALSE) +
  facet_grid(trial_type~block) +
  scale_fill_manual(values = c("#E74C3C", "#3498DB")) +
  gg_theme() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

# PAS stability model

# then we can fit a binomial probit model estimating changes in criterion
# and dprime during the experiment (3 blocks)
# we parametrized the trial_type with -0.5, 0.5 thus the intercept is the
# usual criterion (the midpoint between signal and noise)
# we parametrized the block as repeated contrasts coding thus the two coefficients are the
# differences between criterion in block 2 vs 1 and block 3 vs 2
# we included random intercept for subjects and the random interaction to estimate
# variability in differences in dprime and criterion across blocks

# singular fit, maybe for correlations but variances are fine

# note that pas1 and pas234 is reversed, for the sign of the criterion
dat_agg <- dat |> 
  group_by(subject, block, trial_type) |> 
  summarise(npas1 = sum(pas1_234_rev),
            n = n(),
            npas234 = n - npas1) |> 
  ungroup()

contrasts(dat_agg$block) <- codingMatrices::contr.diff

# we are using the rev version thus the sign of the criterion match
# the usual interpretation
# c < 0 --> tendency to say yes, liberal
# c > 0 --> tendency to say no, conservative

dat_agg <- arrange(dat_agg, subject)
fit <- glmer(cbind(npas1, npas234) ~ trial_type * block + (trial_type * block|subject), 
             data = dat_agg, 
             family = binomial(link = "probit"))

ss <- broom.mixed::tidy(fit, conf.int = TRUE)
ss <- data.frame(ss)
mutate(ss, across(where(is.numeric), round, 3)) |> 
  filter(effect == "fixed")  |> 
  select(-effect, -group)

summary(fit)
car::Anova(fit)

coefs <- data.frame(coef(fit)$subject)
names(coefs)[1] <- "intercept"
coefs$subject <- unique(dat$subject)

# here we calculate the critarion for each block for each subject
# based on model predictions

coefs$crit_block1 <- coefs$intercept
coefs$crit_block2 <- coefs$intercept + coefs$block2.1
coefs$crit_block3 <- coefs$crit_block2 + coefs$block3.2

# this plot represents the criterion for each subject in each block
# the trend is stable for most of the participants

crit_block_plot <- coefs |> 
  dplyr::select(starts_with("crit")) |> 
  mutate(id = 1:n()) |> 
  pivot_longer(starts_with("crit")) |> 
  mutate(name = paste("Block", parse_number(name))) |> 
  ggplot(aes(x = name, y = value)) +
  geom_boxplot(width = 0.5, alpha = 0.8, fill = "dodgerblue") +
  geom_point(position = position_jitter(width = 0.1, seed = 2025), size = 3) +
  geom_line(aes(group = id), position = position_jitter(width = 0.1, seed = 2025)) +
  gg_theme() +
  theme(axis.title.x = element_blank()) +
  ylab("Criterion") +
  geom_hline(yintercept = 0, col = "firebrick", lwd = 1) +
  ylim(c(-1.2, 1.2)) +
  annotate("text", x = 3.5, y = 1, label = "Conservative", angle = 90, size = 8, hjust = 1) +
  annotate("text", x = 3.5, y = -1, label = "Liberal", angle = 90, size = 8, hjust = -1)

# here we check the direction and the consistency with a subject-based computation
# not within a model

sdt <- function(say_signal, is_signal){
  hit <- sum(say_signal == 1 & is_signal == 1)
  fa <- sum(say_signal == 1 & is_signal == 0)
  miss <- sum(say_signal == 0 & is_signal == 1)
  cr <- sum(say_signal == 0 & is_signal == 0)
  data.frame(hit, fa, miss, cr)
}

dat |> 
  mutate(say_signal = pas1_234,
         is_signal = ifelse(trial_type == "valid", 1, 0)) |> 
  group_by(subject, block)  |> 
  nest()  |> 
  mutate(sdt_data = map(data, ~sdt(.x$say_signal, .x$is_signal)),
         sdt_psycho = map(sdt_data, ~data.frame(psycho::dprime(.x$hit, .x$fa, .x$miss, .x$cr)))) |> 
  unnest(sdt_psycho) |> 
  ggplot(aes(x = block, y = c)) +
  geom_point() +
  geom_line(aes(group = subject)) +
  geom_boxplot()

# Saving

ggsave("figures/figure3.png", pas_block_plot, width = 8, height = 5)
ggsave("figures/figure3.pdf", pas_block_plot, width = 8, height = 5)

ggsave("figures/figure4.png", crit_block_plot, width = 10, height = 7)
ggsave("figures/figure4.pdf", crit_block_plot, width = 10, height = 7)

# RDS full results

res_pas_stability <- list(
  criterion_coefs = coefs,
  fit_pas_sdt_block = fit
)

saveRDS(res_pas_stability, "objects/pas_stability_res.rds")