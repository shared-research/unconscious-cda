# calling scripts

callr::rscript("01_pre-processing-behavioral.R")
callr::rscript("02a_analysis-behavioral.R")
callr::rscript("02b_pas-stability.R")
callr::rscript("03_analysis-cda.R")

# compile report

quarto::quarto_render("docs/paper-analysis.qmd")
quarto::quarto_render("docs/sdt.qmd")