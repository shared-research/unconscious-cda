# Environment -------------------------------------------------------------

rm(list = ls())

# Packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(BayesFactor)
library(latex2exp)
library(effectsize)

# main function to describe and analize
analyze <- function(y1, y2, data, alternative = "two.sided"){

  data <- data[, c(y1, y2)]
  data <- data[complete.cases(data), ]

  # t test, contrast
  tt <- t.test(data[[y1]], data[[y2]], paired = TRUE, alternative = alternative)

  # bayes factor
  if(alternative == "less"){
    nullInterval = c(-Inf, 0)
  } else if(alternative == "greater"){
    nullInterval = c(0, Inf)
  } else{
    nullInterval = c(-Inf, Inf)
  }

  bf <- BayesFactor::ttestBF(data[[y1]], data[[y2]], paired = TRUE, nullInterval=nullInterval)
  
  # descriptive, always cda less than 0, alternative = less
  se <- function(x) sd(x) / sqrt(length(x))
  
  t_test <- function(x) {
    rr <- t.test(x, mu = 0, alternative = "less")
    rrd <- broom::tidy(rr)
    rrd$se <- rr$stderr
    rrd
  }
  
  bf0 <- function(x){
    bf <- BayesFactor::ttestBF(x, mu = 0, nullInterval = c(-Inf, 0))
    exp(bf@bayesFactor$bf[1])
  }
  
  get_d <- function(x) cohens_d(x, mu = 0)$Cohens_d
  funs <- c(mean = mean, sd = sd, se = se, n = length, t_test = t_test, d = get_d, bf10 = bf0)
  desc_y1 <- data.frame(lapply(funs, function(f) f(data[[y1]])))
  desc_y2 <- data.frame(lapply(funs, function(f) f(data[[y2]])))

  desc <- rbind(desc_y1, desc_y2)
  desc$cond <- c(y1, y2)

  # res

  ttd <- broom::tidy(tt)
  ttd$se <- tt$stderr
  ttd$contrast <- paste(y1, "-", y2)
  ttd$bf10 <- exp(bf@bayesFactor$bf[1])
  ttd$bf01 <- 1/ttd$bf10
  ttd <- select(ttd, contrast, everything())
  ttd$dz <- effectsize::cohens_d(data[[y1]], data[[y2]], paired = TRUE)$Cohens_d
  ttd$drm <- effectsize::repeated_measures_d(data[[y1]], data[[y2]])$d_rm

  # plot
 
  dd <- data.frame(y = c(data[[y1]], data[[y2]]), x = rep(c(y1, y2), c(length(data[[y1]]), length(data[[y2]]))))
  plt <- ggplot(dd, aes(x = x, y = y)) +
    geom_point(position = position_jitter(width = 0.1)) +
    geom_boxplot(aes(fill = x), show.legend = FALSE, alpha = 0.5) +
    theme(axis.title.x = element_blank()) +
    ylab(latex2exp::TeX("$\\mu$ V")) +
    ggtitle(paste(y1, "-", y2))

  list(
    descriptive = desc,
    test = ttd,
    plot = plt
  )

}

gg_theme <- function(base_size = 20){
    theme_minimal(base_size = base_size)
}

theme_set(gg_theme())

round_df <- function(x){
  mutate(x, across(where(is.numeric), function(x) round(x, 3)))
}

# Data --------------------------------------------------------------------

cda <- readxl::read_xlsx("data/clean/eeg/all relevant conditions_300-1200.xlsx")
cda <- cda[, c(1, 14, 15, 16, 17)]

names(cda) <- c("id", "pas1_catch", "pas1_correct", "pas1_wrong", "pas2_correct")

# comparing pas 1 correct vs pas 1 wrong

pas1_correct_vs_pas1_wrong <- analyze("pas1_correct", "pas1_wrong", cda, alternative = "less")

# comparing pas1 correct vs pas2 correct

pas1_correct_vs_pas2_correct <- analyze("pas1_correct", "pas2_correct", cda, alternative = "two.sided")

# comparing pas1 correct vs pas1 catch

pas1_correct_vs_pas1_catch <- analyze("pas1_correct", "pas1_catch", cda, alternative = "two.sided")

# combine

cda_res <- list(
  pas1_correct_vs_pas1_wrong = pas1_correct_vs_pas1_wrong,
  pas1_correct_vs_pas2_correct = pas1_correct_vs_pas2_correct,
  pas1_correct_vs_pas1_catch = pas1_correct_vs_pas1_catch
)

# waveforms

# plotting -----------------------------------------------------------

read_wv <- function(x){
  dd <- read.table(x, dec = ",")
  y <- unname(t(dd[, -1])[, 1])
  c(y, NA) # last time point
}

wv_pas1_catch <- read_wv("data/clean/eeg/waveforms/cda-catch.dat")
wv_pas1_correct <- read_wv("data/clean/eeg/waveforms/cda-pas1-correct.dat")
wv_pas1_wrong <- read_wv("data/clean/eeg/waveforms/cda-pas1-wrong.dat")
wv_pas2_correct <- read_wv("data/clean/eeg/waveforms/cda-pas2-correct.dat")

wv_data <- data.frame(
  pas1_catch = wv_pas1_catch,
  pas1_correct = wv_pas1_correct,
  pas1_wrong = wv_pas1_wrong,
  pas2_correct = wv_pas2_correct
)

wv_data$time <- seq(-200, 1200, length.out = nrow(wv_data))

# pas 1 correct vs pas 1 wrong

wv_plot_pas1_correct_vs_pas1_wrong <- wv_data |> 
  select(time, pas1_correct, pas1_wrong) |> 
  pivot_longer(2:3) |> 
  mutate(name = ifelse(name == "pas1_correct", "PAS 1 Correct", "PAS 1 Wrong")) |> 
  ggplot(aes(x = time, y = value, color = name)) +
  geom_hline(yintercept = 0, lwd = 1) +
  geom_vline(xintercept = 0, lwd = 1) +
  geom_line(lwd = 1.5) +
  scale_x_continuous(breaks = seq(-200, 1200, 200)) +
  scale_y_continuous(breaks = seq(-1.5, 1.5, 0.5), limits = c(-1.5, 1.5)) +
  gg_theme() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  ylab(latex2exp::TeX("$\\mu$ V")) +
  scale_color_manual(values = c("firebrick", "dodgerblue"))

# pas1 correct vs pas2 correct

wv_pas1_correct_vs_pas2_correct <- wv_data |> 
  select(time, pas1_correct, pas2_correct) |> 
  pivot_longer(2:3) |> 
  mutate(name = ifelse(name == "pas1_correct", "PAS 1 Correct", "PAS 2 Correct")) |> 
  ggplot(aes(x = time, y = value, color = name)) +
  geom_hline(yintercept = 0, lwd = 1) +
  geom_vline(xintercept = 0, lwd = 1) +
  geom_line(lwd = 1.5) +
  scale_x_continuous(breaks = seq(-200, 1200, 200)) +
  scale_y_continuous(breaks = seq(-1.5, 1.5, 0.5), limits = c(-1.5, 1.5)) +
  gg_theme() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  ylab(latex2exp::TeX("$\\mu$ V")) +
  scale_color_manual(values = c("firebrick", "dodgerblue"))

# pas1 correct vs catch

wv_pas1_correct_vs_catch <- wv_data |> 
  select(time, pas1_correct, pas1_catch) |> 
  pivot_longer(2:3) |> 
  mutate(name = ifelse(name == "pas1_correct", "PAS 1 Correct", "PAS 1 Catch")) |> 
  ggplot(aes(x = time, y = value, color = name)) +
  geom_hline(yintercept = 0, lwd = 1) +
  geom_vline(xintercept = 0, lwd = 1) +
  geom_line(lwd = 1.5) +
  scale_x_continuous(breaks = seq(-200, 1200, 200)) +
  scale_y_continuous(breaks = seq(-1.5, 1.5, 0.5), limits = c(-1.5, 1.5)) +
  gg_theme() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank()) +
  ylab(latex2exp::TeX("$\\mu$ V")) +
  scale_color_manual(values = c("dodgerblue", "firebrick"))

# saving

ggsave("figures/figure-6.pdf", wv_plot_pas1_correct_vs_pas1_wrong, dpi = 300, width = 10)
ggsave("figures/figure-6.png", wv_plot_pas1_correct_vs_pas1_wrong, dpi = 300, width = 10)

ggsave("figures/figure-7.pdf", wv_pas1_correct_vs_catch, dpi = 300, width = 10)
ggsave("figures/figure-7.png", wv_pas1_correct_vs_catch, dpi = 300, width = 10)

ggsave("figures/figure-8.pdf", wv_pas1_correct_vs_pas2_correct, dpi = 300, width = 10)
ggsave("figures/figure-8.png", wv_pas1_correct_vs_pas2_correct, dpi = 300, width = 10)

# RDS full results

saveRDS(cda_res, "objects/cda_res.rds")