data_proc_africa <- function(outputdir){
  # LOAD REQUIRED PACKAGES AND FUNCTIONS -----------------------------------------
if (!require("pacman")) install.packages("pacman")
pkgs = c("dplyr", "sf","sp") # package names
pacman::p_load(pkgs, character.only = T)

# LOAD DATA --------------------------------------------------------------------

# Cases at country level 

# ECDC
# cases <- readr::read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/ecdc/new_cases.csv")

# JHU
cases <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

# Shapefile for Africa
africa <- st_read("data/original/geodata/africa.gpkg") 

# Policy index
policy <- readr::read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv")

# DATA PREPARATION -------------------------------------------------------------



caseloc = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
africaloc = "data/original/geodata/africa.gpkg"
policyloc = "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"

alldata = read_all(cases=caseloc, areas=africaloc, policy=policyloc)



# Remove Countries with missing policy data
countries_to_remove <-  c("Madagascar", "Western Sahara", "Guinea-Bissau", "Equatorial Guinea")
alldata$areas <- alldata$areas[!(alldata$areas$name %in% countries_to_remove), ] 

# # THIS IS THE CLEANING FOR ECDC DATA
# # Change names of countries according to the shapefils
# names(cases)[grep("Congo", names(cases))] <- c("Republic of Congo", "Democratic Republic of the Congo")
# names(cases)[grep("Gambia", names(cases))] <- "The Gambia"
# 
# 
# # Subset the cases only to the 46 African countries we work with
# cases <- cases[, c(1, match(africa$name, names(cases)))]
# cases[is.na(cases)] <- 0

# Clean JHU data :
#   - rename countries according to shapefile
#   - keep only countries that we are going to analyse
#   - calculate daily new cases
#   - replace the 4 negatives with 0

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

# Reshape cases with times in the row and geographical units in the column
# so each column is the cases time series for the county
# the results should be of dimension T x N
# where T is the number of time points and N is the number of countries
# remember that this structure needs to be kept also for the 
# predictors

# First change the names of the columns to proper dates
names(alldata$cases)[-1] <- paste0(names(alldata$cases)[-1], 20) %>% 
  as.Date(format = "%m/%d/%Y") %>% 
  as.character()
alldata$counts <- t(alldata$cases[, -1]) 
colnames(alldata$counts) <- alldata$cases$country
alldata$counts <- apply(alldata$counts, 2, diff)
alldata$counts[alldata$counts < 0] <- 0

# Clean policy data
alldata$policy_clean <- alldata$policy %>% 
  select(country = CountryName, date = Date, 
         testing = `H2_Testing policy`, sindex = StringencyIndex) %>% 
  mutate(date = as.Date(as.character(date), format = "%Y%m%d"),
         country = case_when(
           country == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
           country == "Gambia" ~ "The Gambia",
           country == "Eswatini" ~ "Swaziland",
           country == "Congo" ~ "Republic of Congo",
           TRUE ~ country
         )) %>% 
  filter(country %in% alldata$areas$name)
  
alldata$testing <- alldata$policy_clean %>% 
  select(-sindex) %>% 
  tidyr::spread(key = country, value = testing) %>% 
  select(-date) %>% 
  as.matrix()

alldata$testing[is.na(alldata$testing)] <- 0

rownames(alldata$testing) <- unique(as.character(alldata$policy_clean$date))

alldata$sindex <- alldata$policy_clean %>% 
  select(-testing) %>% 
  tidyr::spread(key = country, value = sindex) %>% 
  select(-date) %>% 
  as.matrix()

alldata$sindex[is.na(alldata$sindex)] <- 0

rownames(alldata$sindex) <- unique(as.character(alldata$policy_clean$date))


# Subset to the cases dates
alldata$testing <- alldata$testing[rownames(alldata$testing) %in% rownames(alldata$counts), ]
alldata$sindex <- alldata$sindex[rownames(alldata$sindex) %in% rownames(alldata$counts), ]


return(alldata)
}
