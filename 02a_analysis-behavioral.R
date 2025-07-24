# Environment -------------------------------------------------------------

rm(list = ls())

# Packages ----------------------------------------------------------------

library(tidyverse)
library(lme4)
library(lmerTest)
library(ggeffects)
library(effects)
library(codingMatrices)
library(broom.mixed)
library(flextable)
library(ftExtra)
library(ggeffects)
library(gghalves)

select <- dplyr::select

# Functions ---------------------------------------------------------------

gg_theme <- function(base_size = 20){
    theme_minimal(base_size = base_size)
}

custom_tab <- function(data){
    data |> 
        flextable() |> 
        autofit() |> 
        theme_vanilla() |> 
        colformat_double(digits = 3)
}

exclude_ran <- function(data){
    cols <- c("effect", "term", "estimate")
    data[data$effect == "ran_pars", !names(data) %in% cols] <- NA
    data
}

# Importing Data ----------------------------------------------------------

dat <- read_rds("data/clean/behavioral/dat_clean.rds")

# Subjects of the CDA analysis --------------------------------------------

cda <- readxl::read_xlsx("data/clean/eeg/all relevant conditions_300-1200.xlsx")
cda_subj <- cda$File
cda_subj <- parse_number(cda_subj)

# MODELS ------------------------------------------------------------------

## acc ~ pas --------------------------------------------------------------

datx <- dat |> 
  filter(trial_type == "valid") |> 
  group_by(subject) |> 
  mutate(trial = 1:n()) |> 
  ungroup()

datx_agg <- datx |> 
    group_by(subject, pasf_sdiff) |> 
    summarise(nc = sum(test_acc),
              nt = n(),
              nf = nt - nc) |> 
    ungroup()

contrasts(datx_agg$pasf_sdiff) <- codingMatrices::contr.diff

fit_acc_pas <- glmer(cbind(nc, nf) ~ pasf_sdiff + (1|subject),
                     data = datx_agg,
                     family = binomial(link = "logit"))

fit_acc_pas_cda <- glmer(cbind(nc, nf) ~ pasf_sdiff + (1|subject),
                     data = filter(datx_agg, subject %in% cda_subj),
                     family = binomial(link = "logit"))

## sdt model pas ----------------------------------------------------------

# using the glm to compute the dprime the intepretation of the criterion
# we are flipping the 0-1 response thus the criterion has the same sign of
# the classical formulation.
# negative --> tendency to say YES
# positive --> tendency to say NO

dat$pas1_234_rev <- ifelse(dat$pas1_234 == 1, 0, 1) # revert for the correct sign from glm

dat_agg <- dat |> 
  group_by(subject, trial_type) |> 
  summarise(n1 = sum(pas1_234_rev),
            n = n(),
            n0 = n - n1) |> 
  ungroup()

# convert to integer
dat_agg$is_signal <- factor(ifelse(dat_agg$trial_type == "valid", 1, 0))

fit_sdt_pas <- glmer(cbind(n1, n0) ~ is_signal + (is_signal|subject),
                     data = dat_agg,
                     family = binomial(link = "probit"),
                     contrasts = list(is_signal = contr.sum(2)/2))


# # check the sign, no multilevel but rough estimation
#sdt <- table(dat$sdt_pas)
#psycho::dprime(sdt["hit"], sdt["fa"], sdt["miss"], sdt["cr"])

# TABLES ------------------------------------------------------------------

table_fit_acc_pas <- fit_acc_pas |> 
    tidy(conf.int = TRUE, exponentiate = TRUE) |> 
    select(effect, term, starts_with("estimate"), everything(), -group) |> 
    mutate(
        term = case_when(
            term == "(Intercept)" ~ "PAS 1 vs 50%",
            term == "pasf_sdiff2-1" ~ "PAS 2 vs PAS 1",
            term == "pasf_sdiff3-2" ~ "PAS 3 vs PAS 2",
            term == "pasf_sdiff4-3" ~ "PAS 4 vs PAS 3",
            term == "sd__(Intercept)" ~ "$\\sigma_{id}$"
        ),
        p.value = ifelse(p.value < 0.001, "$p < 0.001$", sprintf("$p = %s$", round(p.value, 3))),
        ci = sprintf("[%s, %s]", round(conf.low, 3), round(conf.high, 3)),
        estimate = ifelse(term == "PAS 1 vs 50%", sprintf("%.3f ($\\hat p_{correct} = %.3f$)", estimate, plogis(log(estimate))), sprintf("%.3f", estimate))) |> 
    exclude_ran() |> 
    select(term, estimate, std.error, ci, statistic, p.value) |> 
    custom_tab() |> 
    set_header_labels(values = list(
        term = "Parameter",
        estimate = "$\\beta$",
        std.error = "SE",
        statistic = "z",
        p.value = "*p*",
        ci = "95% CI"
    )) |> 
    align(part = "all", align = "center") |> 
    colformat_md(part = "all")

table_fit_acc_pas_cda <- fit_acc_pas_cda |> 
    tidy(conf.int = TRUE, exponentiate = TRUE) |> 
    select(effect, term, starts_with("estimate"), everything(), -group) |> 
    mutate(
        term = case_when(
            term == "(Intercept)" ~ "PAS 1 vs 50%",
            term == "pasf_sdiff2-1" ~ "PAS 2 vs PAS 1",
            term == "pasf_sdiff3-2" ~ "PAS 3 vs PAS 2",
            term == "pasf_sdiff4-3" ~ "PAS 4 vs PAS 3",
            term == "sd__(Intercept)" ~ "$\\sigma_{id}$"
        ),
        p.value = ifelse(p.value < 0.001, "$p < 0.001$", sprintf("$p = %s$", round(p.value, 3))),
        ci = sprintf("[%s, %s]", round(conf.low, 3), round(conf.high, 3)),
        estimate = ifelse(term == "PAS 1 vs 50%", sprintf("%.3f ($\\hat p_{correct} = %.3f$)", estimate, plogis(log(estimate))), sprintf("%.3f", estimate))) |> 
    exclude_ran() |> 
    select(term, estimate, std.error, ci, statistic, p.value) |> 
    custom_tab() |> 
    set_header_labels(values = list(
        term = "Parameter",
        estimate = "$\\text{exp}(\\beta)$",
        std.error = "SE",
        statistic = "z",
        p.value = "*p*",
        ci = "95% CI"
    )) |> 
    align(part = "all", align = "center") |> 
    colformat_md(part = "all")

table_n_trials <- dat |> 
    mutate(pas = factor(pas),
           trial_type = factor(trial_type, levels = c("valid", "catch"))) |> 
    count(subject, trial_type, pas) |> 
    group_by(trial_type, pas) |> 
    summarise(mean = mean(n),
              sd = sd(n)) |> 
    custom_tab() |> 
    merge_v(1) |> 
    set_header_labels(values = c("Trial type", "PAS", "Mean", "SD")) 

table_p_trials <- dat |> 
    select(subject, pas, trial_type) |> 
    group_by(trial_type, pas) |> 
    count() |> 
    group_by(trial_type) |> 
    mutate(tot = sum(n)) |> 
    ungroup() |> 
    mutate(p = n / tot) |> 
    select(trial_type, pas, p) |> 
    mutate(p = round(p * 100, 2),
           pas = as.integer(pas)) |> 
    custom_tab() |> 
    set_header_labels(values = c("Trial type", "PAS", "%")) |> 
    merge_v(1)


# PLOTS -------------------------------------------------------------------

## pas ~ trial_type -------------------------------------------------------

plot_pas_trial_type <- dat |> 
    mutate(trial_type = str_to_title(trial_type)) |> 
    count(subject, trial_type, pas) |> 
    mutate(isaware = ifelse(pas == 1, "Unaware", "Aware")) |> 
    ggplot(aes(x = factor(pas), y = n, fill = isaware)) +
    geom_boxplot() +
    facet_grid(~trial_type) +
    gg_theme() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank()) +
    scale_fill_manual(values = c("#E74C3C", "#3498DB"))

## acc ~ pas --------------------------------------------------------------

dat_fit_acc_pas_agg <- ggeffect(fit_acc_pas, bias_correction = TRUE)[[1]] |> 
    data.frame() |> 
    mutate(x = paste("PAS", x))

dat_fit_acc_plot <- fit_acc_pas@frame
dat_fit_acc_plot$nc <- dat_fit_acc_plot$`cbind(nc, nf)`[, 1]
dat_fit_acc_plot$nf <- dat_fit_acc_plot$`cbind(nc, nf)`[, 2]
dat_fit_acc_plot$nt <- with(dat_fit_acc_plot, nc + nf)
dat_fit_acc_plot$acc <- with(dat_fit_acc_plot, nc / nt)

dat_fit_acc_plot <- dat_fit_acc_plot |> 
    mutate(pasf = paste("PAS", pasf_sdiff))

plot_acc_pas <- ggplot() +
    geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.7) +
    geom_half_point(data = dat_fit_acc_plot,
                    aes(x = pasf, y = acc, fill = pasf),
                    size = 4,
                    position = position_jitter(width = 0.01, height = 0, seed = 2022),
                    colour="black",
                    pch=21) +
    ylim(c(0.3, 1)) +
    geom_pointrange(data = dat_fit_acc_pas_agg,
                    aes(x = x, 
                        y = predicted, 
                        ymin = conf.low, 
                        ymax = conf.high),
                    size = 0.8) +
    cowplot::theme_minimal_grid(font_size = 18) +
    theme(axis.title.x = element_blank(),
          legend.position = "none") +
    ylab("Accuracy")

## Saving ---------------------------------------------------------------

# FIGURE 2

ggsave("figures/figure-2.pdf", plot_pas_trial_type, width = 10, height = 5)
ggsave("figures/figure-2.png", plot_pas_trial_type, width = 8, height = 5, dpi = 300)

# FIGURE 5

ggsave("figures/figure-5.pdf", plot_acc_pas, width = 8, height = 5)
ggsave("figures/figure-5.png", plot_acc_pas, width = 8, height = 5, dpi = 300)

# TABLE n_trials
save_as_docx(table_n_trials, path = "tables/n_trials.docx")

# TABLE p_trials
save_as_docx(table_p_trials, path = "tables/p_trials.docx")

# TABLE fit_acc_pas
save_as_docx(table_fit_acc_pas, path = "tables/fit_acc_pas.docx")

# TABLE fit_acc_pas_cda
save_as_docx(table_fit_acc_pas_cda, path = "tables/fit_acc_pas_cda.docx")

# RDS full results

behav_res <- list(
    fit_sdt_pas = fit_sdt_pas,
    table_fit_acc_pas = table_fit_acc_pas,
    table_fit_acc_pas_cda = table_fit_acc_pas
)

saveRDS(behav_res, "objects/behav_res.rds")