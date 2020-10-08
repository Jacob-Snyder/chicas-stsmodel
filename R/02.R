
analysis02 <- function(outputdir, alldata){

    op = opf(outputdir)
    ip = ipf(outputdir)
    
# LOAD DATA --------------------------------------------------------------------

# Cases at UTLA level
cases <- readr::read_csv(ip("data/processed/cases_africa.csv"))

# Population
pop_data <- readr::read_csv(ip("data/processed/pop_africa.csv"))

# Shapefile for LTLAs
africa <- st_read(ip("data/processed/geodata/africa.gpkg") )

# Movement data as weight matrix written by stage 01
wMat <- readRDS(ip("data/processed/weight_matrix_africa.rds"))

#ommute_vol <- alldata$traffic

# DATA PREPARATION -------------------------------------------------------------

# Check that the order of cases and utlas is the same
all(cases$country == africa$name)

# Reshape cases with times in the row and geographical units in the column
# so each column is the cases time series for the county
# the results should be of dimension T x I 

counts <- t(cases[, -1]) 
colnames(counts) <- cases$country

# Replace the last `days_to_remove` days with NA and add
# `days_to_predict` extra days for prediction
days_to_remove <- 4 + 7
days_to_predict <- 7
pred_start <- as.Date(rownames(counts)[nrow(counts) - days_to_remove + 1],format="%m/%d/%y")
pred_end <- pred_start + days_to_predict - 1
counts_to_predict <- matrix(NA, nrow = length(pred_start:pred_end), 
                            ncol = ncol(counts), 
                            dimnames = list())

rownames(counts_to_predict) <- as.character(as.Date(pred_start:pred_end, origin = "1970/01/01"))

counts <- rbind(counts[1:(nrow(counts) - days_to_remove), ], counts_to_predict)


map <- as(africa, "Spatial")
row.names(map) <- as.character(africa$name)

epi_sts <- sts(observed = counts,
               start = c(2020, 30),
               frequency = 365,
               population = pop_data$pop / sum(pop_data$pop),
               map = map)

# Create covariates 

# Lockdown binary indicator 

#loclock_date <-  "2020-03-31"
id_lock_date <- which(rownames(counts) == lock_date)
lockdown <- matrix(c(rep(0, id_lock_date), rep(1, nrow(counts) - id_lock_date)),
                   ncol = ncol(epi_sts), nrow = nrow(epi_sts))

message("Weekend effect")
weekend <- as.numeric(isWeekend(as.Date(rownames(counts),format="%m/%d/%y"), wday = 1:5))
weekend <- matrix(weekend, ncol = ncol(epi_sts), nrow = nrow(epi_sts))

message(" Population")
pop <- population(epi_sts)

# Commute matrix

# wMat is the not symmetric matrix
wMat_no_sym <- as.matrix(wMat) 
wMat_sym <- wMat_no_sym + t(wMat_no_sym) 

## Commute volume covariate
#commute <- commute_covariate(v = commute_vol$Cars / 100, 
#                             v_init_date = as.character(commute_vol$Date[1]), 
#                             nUnits = ncol(epi_sts),
#                             date_low = as.character(as.Date(rownames(counts)[1]) - 1),
#                             date_high = as.character(as.Date(rownames(counts)[nrow(counts)]) - 1))

message("MODEL ------------------------------------------------------------------------")

# Days used to conduct inference
days_for_inferece <- 60
fit_start <- nrow(epi_sts) - days_to_predict - days_for_inferece
fit_end <- nrow(epi_sts) - days_to_predict

# Formulas for the 3 components of the model
f_end <- ~ 1 + t + weekend
f_ar <- ~ 1 + log(pop) 
f_ne <- ~ 1 +  log(pop)


# Model with Poisson distributed lags

# Number of lags that optimise the model (lowest AIC)
lags <- 7 

model_lag <- list(
  end = list(f = f_end, offset = population(epi_sts)),
  ar = list(f = f_ar),
  ne = list(f = f_ne, weights = wMat_no_sym, normalize = T),
  optimizer = list(stop = list(iter.max = 50)),
  family = "NegBin1",
  data = list(wMat_no_sym = wMat_no_sym, pop = pop,
              weekend = weekend),
  subset = fit_start:fit_end,
  funct_lag = poisson_lag, 
  max_lag = 7)

fit_lag <- profile_par_lag(epi_sts, model_lag)

saveRDS(fit_lag, op("fitted_model_LTLA.rds"))


}
