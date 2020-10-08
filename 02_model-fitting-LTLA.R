# LOAD REQUIRED PACKAGES AND FUNCTIONS -----------------------------------------
if (!require("pacman")) install.packages("pacman")
pkgs = c("surveillance", "dplyr", "sp", "sf", "timeDate", "readxl",
         "hhh4addon") # package names
pacman::p_load(pkgs, character.only = T)

source("R/functions.R")
source("R/02.R")
analysis02(outputdir = "output/")
