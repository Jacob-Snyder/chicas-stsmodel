# LOAD REQUIRED PACKAGES AND FUNCTIONS -----------------------------------------
if (!require("pacman")) install.packages("pacman")
pkgs = c("dplyr", "sf","sp") # package names
pacman::p_load(pkgs, character.only = T)


source("R/functions.R")
source("R/01.R")
source("R/inputs.R")
source("R/model_and_map.R")


cases_loc = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
africa_loc = "data/original/geodata/africa.gpkg"


alldata = read_all(cases=cases_loc, areas=africa_loc)



countries_to_remove <-  c("Madagascar", "Western Sahara", "Guinea-Bissau", "Equatorial Guinea")
alldata$areas <- alldata$areas[!(alldata$areas$name %in% countries_to_remove), ] 
alldata$cases <- alldata$cases %>% 
  select(-`Province/State`, -Lat, -Long) %>% 
  rename(country = `Country/Region`) %>% 
  mutate(country = case_when(
    country == "Congo (Kinshasa)" ~ "Democratic Republic of the Congo",
    country == "Gambia" ~ "The Gambia",
    country == "Eswatini" ~ "Swaziland",
    country == "Congo (Brazzaville)" ~ "Republic of Congo",
    country == "Gambia" ~ "The Gambia",
    TRUE ~ country
  )) %>% 
  filter(country %in% alldata$areas$name)

# Check that the order of cases and countries in the shapefile are the same
alldata$cases <- alldata$cases[order(alldata$cases$country), ]
all(alldata$cases$country == alldata$areas$name)




analysis01(outputdir = "output/", alldata)
