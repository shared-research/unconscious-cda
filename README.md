
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Tracking the Unconscious: Neural evidence for the retention of unaware information in visual working memory

[<img alt="alt_text" src="https://img.shields.io/badge/OSF-https://osf.io/gkmsy/-337AB7"/>](https://osf.io/gkmsy/)

This repository contains the code to reproduce the analysis, figures and
tables of the paper *Tracking the Unconscious: Neural evidence for the
retention of unaware information in visual working memory* by Gambarota
et al. The repository contains also the code to run the experiment using
Psychopy along with raw EEG and behavioral data.

## Structure

- The `data` folder contains the raw and cleaned EEG and behavioral
  data:

<!-- -->

    data
    ├── clean
    │   ├── behavioral
    │   └── eeg
    └── raw
        ├── behavioral
        │   ├── csv
        │   └── pkl
        └── eeg

- The `experiment/` folder contains python scripts to run the
  experiment.

<!-- -->

    experiment
    ├── experiment.py
    ├── triggers.py
    └── utils.py

- `tables/` and `figures/` contains the tables and figures from the
  paper created with the `02_analysis-behavioral.R`.
- the `01_pre-processing-behavioral.R` script cleans the raw behavioral
  data
- the `02_analysis-behavioral.R` script runs the multilevel models for
  the CDT accuracy and SDT analysis creating also tables and figures
- the `03_analysis-cda.R` script runs the analysis on ERP data
- the `brain-vision-analyzer-script.ehtp` is the EEG/ERP pre-processing
  script

Given the storage limits, the raw EEG data are stored on OSF within the
`eeg-raw-data` folder. The content of this folder need be downloaded and
placed within the `data/raw/eeg` folder before running the scripts.

All the packages are managed using `renv`. Before running the code open
the `cda-subliminal.Rproj` file and run `renv::restore()` to create the
custom library with all packages. If the `renv` package is not installed
run `install.packages("renv")`.

## Session

     setting  value
     version  R version 4.4.1 (2024-06-14)
     os       Pop!_OS 22.04 LTS
     system   x86_64, linux-gnu
     ui       X11
     language (EN)
     collate  en_US.UTF-8
     ctype    en_US.UTF-8
     tz       Europe/Rome
     date     2024-07-18
     pandoc   3.1.11 @ /usr/lib/rstudio/resources/app/bin/quarto/bin/tools/x86_64/ (via rmarkdown)
