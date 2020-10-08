analysis01 <- function(outputdir, alldata){
  
  # LOAD REQUIRED PACKAGES AND FUNCTIONS -----------------------------------------
  if (!require("pacman")) install.packages("pacman")
  pkgs = c("dplyr", "sf","surveillance") # package names
  pacman::p_load(pkgs, character.only = T)
  
  

  source("R/inputs.R")

  
# LOAD DATA --------------------------------------------------------------------
    outd = opf(outputdir)
    ind = ipf(outputdir)

### Africa cases
    cases = alldata$cases

### Africa shapefile
    africa<- alldata$areas

## Commute matrix
#wMat <- alldata$flows

# DATA PREPARATION -------------------------------------------------------------

### case merging for combined areas in the spatial data

#     mergefrom <- function(cases, colname, codes){
#         matches = cases[[colname]] %in% codes
#         if(any(matches)){
#             cases[[colname]][matches] = paste0(codes,collapse=",")
#         }
#         return(cases)
#     }
#     
# # Merge together some of the LTLAS
# cases = mergefrom(cases, "LTLA_code", c("E06000052", "E06000053"))
# cases = mergefrom(cases, "LTLA_code",  c("E09000001", "E09000033"))
    
# Keep only useful columns and aggregate everything at LTLA level
cases_clean <- cases %>%
  rename(name=country) %>%
  inner_join(st_drop_geometry(africa[, c("name", "Pop2020")]))


cases_clean %>% 
  select(name, pop = Pop2020) %>% 
  readr::write_csv(outd("data/processed/pop_africa.csv"))

cases_clean$Pop2020 <- NULL

# Replace missing days with NAs columns 
dates_obs <- as.numeric(as.Date(names(cases_clean)[-1],format="%m/%d/%y"))
dates_full <- min(dates_obs):max(dates_obs) 

new_cols <- as.Date(setdiff(dates_full, dates_obs), origin = "1970/01/01")
new_dates <- matrix(NA, nrow = nrow(cases_clean), ncol = length(new_cols))
colnames(new_dates) <- as.character(new_cols)

dates_clean <- cbind(cases_clean[,-1], new_dates)

# Replace all NAs with zeros
dates_clean[is.na(dates_clean)] <- 0
#This line doesnt work because it puts october (10) in front
#dates_clean <- dates_clean[order(names(dates_clean))]

# Reorder the columns based on the date
cases_clean <- cbind(cases_clean[, 1], dates_clean) %>%
  rename(country=name)

# Subset ltla shapefile and save
africa <- africa %>% 
  filter(name %in% cases_clean$country) #%>% 
  #select(lad19cd, lad19nm)

## Subset the commute matrix and transform it to a n x n matrix
#id_keep <- wMat$From %in% cases_clean$lad19cd & wMat$To %in% cases_clean$lad19cd
#wMat <- wMat[id_keep, ] 
#wMat = reshape2::acast(wMat, From ~ To, value.var = 'Flow', fun.aggregate=sum)
#diag(wMat) = 0



map <- as(africa, "Spatial")
row.names(map) <- as.character(africa$name)
# Create adj mat and neighbours order
africa_adjmat <- poly2adjmat(map)
africa_nbOrder <- nbOrder(africa_adjmat, maxlag = Inf)






# Save
readr::write_csv(cases_clean, outd("data/processed/cases_africa.csv"))
st_write(africa, outd("data/processed/geodata/africa.gpkg"), delete_dsn = T)
saveRDS(africa_nbOrder, outd("data/processed/weight_matrix_africa.rds"))
}
