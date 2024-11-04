# Environment -------------------------------------------------------------

rm(list = ls())

# Packages ----------------------------------------------------------------

library(tidyverse)

# Importing Data ----------------------------------------------------------

all_files <- list.files("data/raw/behavioral/csv/", pattern = "csv", full.names = TRUE)

dat_list <- map(all_files, read_csv)

# Fixing Missing Subject 1 Catch ------------------------------------------

# the subject 1 has some missing markers, thus we recovered the marker from
# the vmrk file. To run the code the data/raw/eeg folder need to have the
# corresponding files but they are too large for github thus they have been
# uploaded on OSF.

subj1_markers <- readLines("data/raw/eeg/sj_0001.vmrk")
subj1_markers <- subj1_markers[-c(1:13)] # removing header
subj1_markers <- str_remove_all(subj1_markers, " ") # removing spaces
subj1_markers <- str_split(subj1_markers, ",") # splitting into trials
subj1_markers <- sapply(subj1_markers, function(x) x[[2]]) # extracting markers

# removing bad markers
subj1_markers <- subj1_markers[!str_detect(subj1_markers, 
                                           "ControlBoxisnotconnectedviaUSB|actiCAPUSBPowerOn")]

subj1_markers <- split(subj1_markers, ceiling(seq_along(subj1_markers)/8)) # by trial

id_catch <- which(grepl("S130", subj1_markers)) # trial number for catch trials
id_valid <- which(!grepl("S130", subj1_markers)) # trial number for valid trials

subj1_markers_catch <- subj1_markers[id_catch]

# assigning the PAS response. See the triggers.py file for the markers table

pas <- case_when(grepl("S71", subj1_markers_catch) ~ 1,
                 grepl("S72", subj1_markers_catch) ~ 2,
                 grepl("S73", subj1_markers_catch) ~ 3,
                 grepl("S74", subj1_markers_catch) ~ 4)

# creating the minimal dataset for catch trials

dat1_recover <- tibble(subject = 1, pas, trial_type = "catch",
                       id_trial = id_catch, contrast = 0, 
                       age = dat_list[[1]]$age[1], 
                       gender = dat_list[[1]]$gender[1])

# combining and arranging for the real experiment

dat_list[[1]]$id_trial <- id_valid # the trial number

dat_list[[1]] <- bind_rows(dat_list[[1]], dat1_recover) |> 
  arrange(id_trial) |> 
  dplyr::select(-id_trial)

# combining the full dataset

dat <- bind_rows(dat_list)

# Cleaning ----------------------------------------------------------------

# here when the QUEST goes to low, the proposed contrast is 0 thus no stimulus
# we fixed these trials as catch trials (beyond what is already a catch trial)

dat_clean <- dat |> 
  mutate(trial_type = ifelse(trial_type == "valid" & contrast == 0, # bad QUEST trials
                             "catch",
                             trial_type),
         # SDT Parameters
         
         is_signal = ifelse(trial_type == "valid", TRUE, FALSE),
         say_signal = ifelse(pas > 1, TRUE, FALSE),
         is_change = ifelse(type == "change", TRUE, FALSE),
         say_change = ifelse(test == "change", TRUE, FALSE),
         
         # SDT Measures
         
         sdt_pas = case_when(
           is_signal & say_signal ~ "hit",
           is_signal & !say_signal ~ "miss",
           !is_signal & say_signal ~ "fa",
           !is_signal & !say_signal ~ "cr"
         ),
         sdt_cdt = case_when(
           is_change & say_change ~ "hit",
           is_change & !say_change ~ "miss",
           !is_change & say_change ~ "fa",
           !is_change & !say_change ~ "cr"
         ),
         
         # PAS alternative
         
         pasf_o = factor(pas, ordered = TRUE),
         pasf = factor(pas),
         pasf_sdiff = pasf,
         pas1_234 = ifelse(pas == 1, 0, 1),
         pas12_34 = ifelse(pas <= 2, 0, 1)
         )

# Saving ------------------------------------------------------------------

saveRDS(dat_clean, "data/clean/behavioral/dat_clean.rds")
