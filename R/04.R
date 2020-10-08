analysis04 <- function(outputdir){

    ofld = opf(outputdir)
    ifld = ipf(outputdir)
    
##source("R/mean_simulate.R")
    

# LOAD DATA --------------------------------------------------------------------

cases <- readRDS(ofld("daily_cases.rds"))

# Final model 
fit <- readRDS(ofld("fitted_model_LAG_RE.rds"))

# Shapefile for ltlas
africa <- st_read(ofld("africa.gpkg")) %>%
  rename(country=name)

# RESULTS ----------------------------------------------------------------------

# Reshape cases to then merge with predictions
cases_dates = data.frame(cbind(rownames(cases), cases),check.names=FALSE)


cases_to_plot <- cases_dates %>%
  tidyr::gather(key = "country", value = "observed", -V1) %>% 
  mutate(V1 = as.Date(V1)) %>%
  rename(time = V1)


# Predictions ------------------------------------------------------------------

# Calculate the days used to fit the model
inference_days <- fit$control$subset 
inference_dates <- as.Date(rownames(cases)[inference_days])
start_day <- inference_days[1]
end_day <- inference_days[length(inference_days)]

# Predicted mean
fit$terms <- terms(fit)
pred_mean <- meanHHH(theta = unname(fit$coefficients), model = fit$terms, 
                     subset = start_day:end_day, total.only = TRUE)

# Calculte the overdispersion for the neg binomial (parametrised acocording to
# dnbinom in R)

size <- exp(fit$coefficients[["-log(overdisp)"]]) 

preds <- pred_mean %>% 
  as_tibble() %>% 
  bind_cols(time = inference_dates) %>% 
  tidyr::gather(key = "country", value = "mean", -time) %>% 
  mutate(low95 = qnbinom(p = 0.025, size = size, mu = mean), 
         low90 = qnbinom(p = 0.05, size = size, mu = mean), 
         low50 = qnbinom(p = 0.25, size = size, mu = mean), 
         median = qnbinom(p = 0.50, size = size, mu = mean), 
         up50 = qnbinom(p = 0.75, size = size, mu = mean),
         up90 = qnbinom(p = 0.95, size = size, mu = mean),
         up95 = qnbinom(p = 0.975, size = size, mu = mean)) %>% 
  inner_join(cases_to_plot, by = c("country", "time"))
  

# Forcasts ---------------------------------------------------------------------
  
nsims <- 10000
y.start <- as.matrix(observed(fit$stsObj)[(end_day - fit$max_lag + 1):end_day, ])
sims <- simulate(fit, y.start = y.start, 
                 subset = (end_day + 1):nrow(fit$stsObj), 
                 nsim = nsims, simplify = F)

sims <- do.call(rbind, lapply(sims, observed))
sims <- as_tibble(sims)


pred_start <- as.Date(rownames(cases[,-1])[end_day]) + 1
pred_end <- pred_start + nrow(fit$stsObj) - end_day - 1
sims$time <- rep(as.Date(pred_start:pred_end, origin = "1970/01/01"), 
                 time = nsims)

forecast <- sims %>% 
  tidyr::gather(key = "country", value = "value", -time) %>%
  group_by(country, time) %>% 
  summarise(mean = mean(value), 
            low95 = quantile(value, p = 0.025),
            low90 = quantile(value, p = 0.05),
            low50 = quantile(value, p = 0.25),
            median = quantile(value, p = 0.5),
            up50 = quantile(value, p = 0.75),
            up90 = quantile(value, p = 0.95),
            up95 = quantile(value, p = 0.975)) %>% 
  ungroup() %>% 
  inner_join(cases_to_plot, by = c("country", "time"))


# Add NA for observed data
# forecast$observed <- NA

# Rbind with predictions and save
preds %>% 
  bind_rows(forecast) %>%
  inner_join(st_drop_geometry(africa), by = "country") %>% 
  readr::write_csv(ofld("pred_forecast.csv"))


# Anomaly detection ------------------------------------------------------------

# Calculate the distribution of the total number of predicted cases
# last week in each LTLA
daily_samples <- split(sims[,-ncol(sims)], f = sims$time) 
tot_week_pred <- Reduce("+", daily_samples)

# clean up memory
rm(sims)
gc()

    
# Calculate the total number of observed cases last week in each LTLA
tot_week_cases <- forecast  %>%
  select(time, country, observed) %>%
  tidyr::spread(key = country, value = observed) %>%
  select(-time)

tot_week_cases <-  colSums(data.matrix(tot_week_cases)) %>% 
  matrix(nrow = nrow(tot_week_pred), ncol = ncol(tot_week_pred), byrow = T)

colnames(tot_week_cases) = colnames(tot_week_pred)

# Calculate the probability that the total number of observed cases last week 
# are greater than predicted cases
saveRDS(tot_week_pred, ofld("tot_week_pred.rds"))
saveRDS(tot_week_cases, ofld("tot_week_cases.rds"))

ex_prob <- tibble(country = names(tot_week_pred),
                  ex_prob = colMeans(tot_week_cases  > tot_week_pred))

readr::write_csv(ex_prob, ofld("ex_prob_gr.csv"))

}
