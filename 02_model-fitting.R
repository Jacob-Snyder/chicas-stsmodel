model_africa <- function(outputdir){
  
  # LOAD REQUIRED PACKAGES AND FUNCTIONS -----------------------------------------
  if (!require("pacman")) install.packages("pacman")
  pkgs = c("surveillance", "dplyr", "sp", "sf", "ggplot2") # package names
  pkgs_git = "jbracher/hhh4addon"
  pacman::p_load(pkgs, character.only = T)
  pacman::p_load_current_gh(pkgs_git)

# LOAD DATA --------------------------------------------------------------------

# Cases at country level
counts <- readRDS(file.path(outputdir, "daily_cases.rds"))

# Shapefile for Africa
africa <- st_read(file.path(outputdir, "africa.gpkg"))

# Policy variables
sindex <- readRDS(file.path(outputdir, "stringency.rds"))
testing <- readRDS(file.path(outputdir, "testing.rds"))

# DATA PREPARATION -------------------------------------------------------------

# Check that the order of cases and countries in the shapefile are the same
all(colnames(counts) == africa$name)

# Replace the last `days_to_remove` days with NA and add
# `days_to_predict` extra days for prediction
#days_to_remove <- 0
#days_to_predict <- 7
days_to_remove <- 7 # 4 + 7
days_to_predict <- 7
pred_start <- as.Date(rownames(counts)[nrow(counts) - days_to_remove]) + 1
pred_end <- pred_start + days_to_predict - 1
counts_to_predict <- matrix(NA, nrow = length(pred_start:pred_end), 
                            ncol = ncol(counts), 
                            dimnames = list())

rownames(counts_to_predict) <- as.character(as.Date(pred_start:pred_end, 
                                                    origin = "1970/01/01"))

counts <- rbind(counts[1:(nrow(counts) - days_to_remove), ], counts_to_predict)

#save the observed + predicted cases matrix


# Extend also the policy variables time window
# we are assuming that there won't be any change 
# in policy next week. If you want to generate scenarios
# you can substitute the *_expected matrix
# with a new days_to_predict x N matrix (N is the number of countries)
sindex <- sindex[as.Date(rownames(sindex)) < pred_start,]
sindex_expected <- matrix(sindex[nrow(sindex), ], nrow = days_to_predict, 
                          ncol = ncol(sindex), byrow = T)
rownames(sindex_expected) <- as.character(as.Date(pred_start:pred_end, 
                                                  origin = "1970/01/01"))
sindex <- rbind(sindex, sindex_expected)

testing <- testing[as.Date(rownames(testing)) < pred_start,]
testing_expected  <- matrix(testing[nrow(testing), ], nrow = days_to_predict, 
                            ncol = ncol(sindex), byrow = T)
rownames(testing_expected) <- as.character(as.Date(pred_start:pred_end, 
                                                   origin = "1970/01/01"))
testing <- rbind(testing, testing_expected)


# Lag sindex and testing policy
lag <- 14
sindex_lag <- xts::lag.xts(sindex, lag)
testing_lag <- xts::lag.xts(testing, lag)


map <- as(africa, "Spatial")
row.names(map) <- as.character(africa$name)

# Create adj mat and neighbours order
africa_adjmat <- poly2adjmat(map)
africa_nbOrder <- nbOrder(africa_adjmat, maxlag = Inf)

epi_sts <- sts(observed = counts,
               start = c(2020, 23),
               frequency = 365,
               population = africa$Pop2020 / sum(africa$Pop2020),
               neighbourhood = africa_nbOrder,
               map = map)

# Create covariates 

pop <- population(epi_sts)


# HDI by category

HDI_cat <- as.numeric(africa$HDI_Level)
HDImedium <- ifelse(HDI_cat == 1, 1, 0) 
HDIhigh <- ifelse(HDI_cat == 2, 1, 0) 
HDI_cat <- matrix(HDI_cat, ncol = ncol(epi_sts), nrow = nrow(epi_sts),
                  byrow = T)
HDI_medium <- matrix(HDImedium, ncol = ncol(epi_sts), nrow = nrow(epi_sts),
                     byrow = T)

HDI_high <- matrix(HDIhigh, ncol = ncol(epi_sts), nrow = nrow(epi_sts),
                   byrow = T)

# Median age
mage <- matrix(africa$Median_age, ncol = ncol(epi_sts), nrow = nrow(epi_sts),
               byrow = T)

# Sub-saharan africa vs North Africa
SSA <- matrix(africa$SSA, 
              ncol = ncol(epi_sts), nrow = nrow(epi_sts),
              byrow = T)
# Land locked
LL <- matrix(africa$landlock, 
             ncol = ncol(epi_sts), nrow = nrow(epi_sts),
             byrow = T)
# MODEL ------------------------------------------------------------------------

# Set up the days use to fit the model
start_day <- "2020-03-28"
end_day <- rownames(counts)[nrow(epi_sts) - days_to_predict] # last observed day by default

fit_start <- which(rownames(counts) == start_day) 
fit_end <- which(rownames(counts) == end_day) 

# Set up formulas for the 3 different parts of the model:
#   end = endemic
#   ar  = autoregressive
#   ne  = neighbour
f_end <- ~ 1 
f_ar <- ~ 1 + log(pop) + HDI_cat + LL + sindex_lag 
f_ne <- ~ 1 + log(pop) + HDI_cat + LL

# Model with Poisson distributed lags and no random effects

# Number of lags to include in the model
mlag <- 7

model_lag <- list(
  end = list(f = f_end, offset = population(epi_sts)),
  ar = list(f = f_ar),
  ne = list(f = f_ne, weights = W_powerlaw(maxlag = 9)),
  optimizer = list(stop = list(iter.max = 50)),
  family = "NegBin1",
  # Always include all the variables that are present
  # in the model formulas in the list below
  data = list(pop = pop, HDI_cat = HDI_cat, 
              LL = LL, sindex_lag = sindex_lag),
  subset = fit_start:fit_end,
  funct_lag = poisson_lag, 
  max_lag = mlag) 

# Fit the model
fit_lag <- profile_par_lag(epi_sts, model_lag)

# Save the results 
saveRDS(fit_lag, file.path(outputdir, "fitted_model_LAG.rds"))

# Add random effects
# Note that when you add REs you need to remove the intercept from 
# the model formula
f_end <- ~ 1 
f_ar <- ~ -1 + log(pop) + HDI_cat + LL + sindex_lag + ri()
f_ne <- ~ -1 + log(pop) + HDI_cat + LL + ri()
  
model_lag <- list(
  end = list(f = f_end, offset = population(epi_sts)),
  ar = list(f = f_ar),
  ne = list(f = f_ne, weights = W_powerlaw(maxlag = 9)),
  optimizer = list(stop = list(iter.max = 50)),
  family = "NegBin1",
  data = list(pop = pop, HDI_cat = HDI_cat, 
              LL = LL, sindex_lag = sindex_lag),
  subset = fit_start:fit_end,
  funct_lag = poisson_lag, 
  par_lag = fit_lag$par_lag,
  max_lag = mlag) 
  
fit_lag_RE <- hhh4_lag(epi_sts, model_lag)
saveRDS(fit_lag_RE, file.path(outputdir, "fitted_model_LAG_RE.rds"))
  
# Generate table with results
fit <- fit_lag_RE
nterms <- terms(fit)$nGroups
coefs <- exp(coef(fit)[1:nterms])
CIs <- exp(confint(fit)[1:nterms, ])
id_log <- c(grep("log", names(coefs)), grep("over", names(coefs)))
coefs[id_log] <- log(coefs[id_log])
CIs[id_log, ] <- log(CIs[id_log, ])
tab <- round(cbind(coefs, CIs), 3)
write.csv(tab, file.path(outputdir, "tab_params_LAG_RE.csv"))
}