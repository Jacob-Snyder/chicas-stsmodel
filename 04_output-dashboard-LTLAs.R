# LOAD REQUIRED PACKAGES AND FUNCTIONS -----------------------------------------
if (!require("pacman")) install.packages("pacman")
pkgs = c("surveillance", "sf", "ggplot2", "dplyr", "hhh4addon") # package names
pacman::p_load(pkgs, character.only = T)

source("R/functions.R")

source("R/04.R")

analysis04(outputdir = "output/")
