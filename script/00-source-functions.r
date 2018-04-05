htmltools::tagList(rmarkdown::html_dependency_font_awesome())
pkgs <- c("tidyverse", "pls", "effects")
for (pkg in pkgs) require(pkg, quietly = TRUE, character.only = TRUE)
source("scripts/00-function.r")
source("scripts/00-getFunction.r")
source("scripts/00-plotFunction.r")
load("scripts/output/unsync/coef.rdata")
load("scripts/output/unsync/design.rdata")

