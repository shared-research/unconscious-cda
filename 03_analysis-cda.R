# Environment -------------------------------------------------------------

rm(list = ls())

# Packages ----------------------------------------------------------------

library(readxl)
library(BayesFactor)

# Data --------------------------------------------------------------------

cda <- readxl::read_xlsx("data/clean/eeg/cda-data.xlsx")

# paired t-test -----------------------------------------------------------

# correct expected to be lower (more negative) than wrong 
t.test(cda$Correct, cda$Wrong, paired = TRUE, alternative = "less")

# bayes factor testing delta < 0
BayesFactor::ttestBF(cda$Correct, cda$Wrong, paired = TRUE, nullInterval=c(-Inf,0))
