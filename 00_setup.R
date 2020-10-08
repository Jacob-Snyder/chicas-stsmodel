setwd("C:/Users/BCIcomputer01/Box Sync/Hershey_2020/COVID Modeling/HHH4/stsmodel-africa")



library(devtools)
devtools::install_gitlab("chicas-covid19/stsmodel")
library(stsmodel)
load_all("stsmodel")
