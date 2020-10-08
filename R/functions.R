#' Creates a time-varying matrix
#'
#' This function multiplies a vector by a matrix, broadcasting
#' along an extra dimension.  That is, for vector v and matrix M,
#' the product A = v*M has shape [nrow(M), ncol(M), length(v)].
#'
#' Additionally, the vector may be extended beyond its limits by
#' replicating the terminal values at either end.
#'
#' @import abind
#' 
time_var_commute = function(v, M, v_init_date, date_low=v_init_date, date_high=NA) {
  
  # Sanitise dates
  v_init_date = as.Date(v_init_date)
  date_low = as.Date(date_low)
  if(is.na(date_high))
    date_high = v_init_date + length(v)
  date_high = as.Date(date_high)
  
  # Compute size of lead in and lead out
  n_leadin = as.integer(v_init_date - date_low)
  n_leadout = length(date_low:date_high) - (n_leadin + length(v)) 
  
  # Assemble vector
  v_series = c(rep(v[1], n_leadin),
               v,
               rep(v[length(v)], n_leadout))
  
  # Broadcast multipy
  a = lapply(v_series, function(x) x*M)
  abind::abind(a, along=3)
}

commute_covariate = function(v, nUnits, v_init_date, date_low=v_init_date, date_high=NA) {
  
  # Sanitise dates
  v_init_date = as.Date(v_init_date)
  date_low = as.Date(date_low)
  if(is.na(date_high))
    date_high = v_init_date + length(v)
  date_high = as.Date(date_high)
  
  # Compute size of lead in and lead out
  n_leadin = as.integer(v_init_date - date_low)
  n_leadout = length(date_low:date_high) - (n_leadin + length(v)) 
  
  # Assemble vector
  v_series = c(rep(v[1], n_leadin),
               v,
               rep(v[length(v)], n_leadout))
  
  # Create a nTimes X nUnits matrix
  commute <- matrix(rep(v_series, nUnits), ncol = nUnits, nrow = length(v_series))
  return(commute)
}


create_labels <- function(x, greater = F, smaller = F) {
  n <- length(x)
  x <- gsub(" ", "", format(x))
  labs <- paste(x[1:(n - 1)], x[2:(n)], sep = " - ")
  if (greater) {
    labs[length(labs)] <- paste("\u2265", x[n - 1])
  }
  if (smaller) {
    labs[1] <- paste("<", x[2])
  }
  
  return(labs)
}

floor_dec <- function(x, digits=1) round(x - 5*10^(-digits-1), digits)
ceiling_dec <- function(x, digits=1) round(x + 5*10^(-digits-1), digits)

map_ltlas <- function(x, shape, palette, breaks = NULL, n, digits = 0, 
                      legend_title, panel_title = NULL, labg = F, ...) {
  
  # Generate breaks for color legend
  if (is.null(breaks)) {
    breaks <- as.numeric(quantile(shape[[x]], probs = seq(0, 1, l = n + 1)))
    breaks[2:n] <- round(breaks[2:n], digits)
    breaks[1] <- floor_dec(breaks[1], digits)
    breaks[n + 1] <- ceiling_dec(breaks[n + 1], digits) 
  }
  
  # Generate labels for color legend
  labs <- create_labels(breaks, greater = labg)
  
  # Generate color palette
  if (is.character(palette)) {
    pal <- tmaptools::get_brewer_pal(palette, n = length(labs), contrast = c(0, 1), plot = F)
  } else {
    pal <- palette(length(labs))
  }
  
  # Produce map
  tm_shape(shape) +
    tm_polygons(col = x, palette = pal, breaks = breaks, style = "fixed", 
                legend.show = T, labels = labs, title = legend_title,
                border.col = "black") +
    tm_compass(position = c("right", "top")) +
    tm_scale_bar(position = c("right", "bottom"), text.size = .8) +
    tm_layout(outer.margins = 0, asp = 0, 
              # =legend.bg.color = "white", legend.frame = "black",
              legend.text.size = 0.95, legend.title.size = 1.35,
              panel.show = T, panel.labels = panel_title, 
              panel.label.size = 1.4, panel.label.fontfamily = "bold", 
              ...)
}

pred_week <- function(x, nsims, size) {
  samples <- matrix(nrow = length(x), ncol = nsims)
  for (i in 1:length(x)) {
    samples[i, ] <- rnbinom(n = nsims, size = size, mu = x[i])
  }
  return(colSums(samples))
}

growth_summary <- function(forecast, cases, size, nsims) {
  
  # Distribution of total expected number of reported cases for next week
  distr_nextw <- apply(forecast, 2, pred_week, nsims = nsims, size = size)
  
  # Total number of cases observed next week
  tot_cases <- matrix(colSums(cases), 
                      nrow = nrow(distr_nextw), ncol = ncol(distr_nextw),
                      byrow = T)
  
  # Distribution of "growth rate"
  distr_gr <- tot_cases / distr_nextw
  
  # Prob that growth rate is > 1  per LTLA 
  ex_prob <- apply(distr_gr, 2, function(x) mean(x > 1, na.rm = T))
  
  # Quantiles
  quantiles <- t(apply(distr_gr, 2, quantile, 
                       prob = c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = T))
  quantiles <- as_tibble(quantiles)
  names(quantiles) <- c("low95", "low50", "median", "up50", "up95")
  
  mean <- apply(distr_gr, 2, mean, na.rm = T)
  
  out <- tibble(lad19cd = names(ex_prob), ex_prob, mean) %>% 
    bind_cols(quantiles)
  return(out)
}


op <- function(path,root="./output"){
    full = file.path(root, path)
    dir.create(dirname(full),showWarnings=FALSE, recursive=TRUE)
    full
}

opf <- function(root){
    function(path){
        op(path, root=root)
    }
}

ip <- function(path, root="./output"){
    file.path(root, path)
}

ipf <- function(root){
    function(path){
        ip(path, root=root)
    }
}
