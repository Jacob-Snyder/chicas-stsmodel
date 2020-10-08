save_plots <- function(fandp, ddir){
    ddir = normalizePath(ddir)
    for(code in unique(fandp$fits$country)){
        fig = make_plot_for(fandp, code)
        save_html(fig,file.path(ddir,paste0(code,".html")))
    }
}

make_plot_for <- function(fandp, code, plot_opts=NULL) {
    fits = fandp$fits
    fits$time = as.Date(fits$time)
    fits = fits[fits$country == code,]
    preds = fandp$predicts
    preds$time = as.Date(preds$time)
    preds = preds[preds$country == code,]
    make_plotly(fits, preds, plot_opts)
}

make_ggplot <- function(ltla_pred, ltla_forecast){
    ltla_pred$time = as.Date(ltla_pred$time)
    ltla_forecast = rbind(ltla_pred[nrow(ltla_pred),], ltla_forecast)
###  ltla_forecast <- forecast[forecast$lad19cd == i, ] %>% 
#    bind_rows(ltla_pred[ltla_pred$time == pred_start - 1, ])
#  ltla_forecast$ltla_name <- name
  ggplot(ltla_pred, aes(x = time)) +
    geom_ribbon(aes(ymin = low50, ymax = up50), fill = "dodgerblue3", alpha = .3) +
    geom_ribbon(aes(ymin = low95, ymax = up95), fill = "dodgerblue3", alpha = .3) +
    geom_ribbon(data = ltla_forecast, aes(ymin = low50, ymax = up50), 
                fill = "orange", alpha = .3) +
    geom_ribbon(data = ltla_forecast, aes(ymin = low95, ymax = up95), 
                fill = "orange", alpha = .3) +
    geom_line(aes(y = median), col = "dodgerblue3", size = .8) +
    # geom_line(aes(y = mean), col = "dodgerblue3", size = .8, linetype = 2) +
    geom_line(data = ltla_forecast, aes(y = median), col = "orange", size = .8) +
    # geom_line(data = ltla_forecast, aes(y = mean), col = "orange", size = .8, linetype = 2) +
    geom_line(aes(y = observed), linetype = 2) +   
    #geom_point(aes(y = observed), shape = 21) + 
#    facet_wrap(~ ltla_name, scales = "free_y") +
    labs(x = "", y = "Reported cases") +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    coord_cartesian(expand = 0)
}

make_plotly <- function(fits, preds, plot_opts=NULL){

    if(is.null(plot_opts))
      plot_opts = list()
  
    xaxis = list(title="Date")
    yaxis = list(title="Count")

    ## make the first prediction join up with the last data:
    preds = rbind(fits[nrow(fits),], preds)
    
    fig = plot_ly(fits, x=~time,
                  height=plot_opts$height, 
                  width=plot_opts$width)
  
    if(isFALSE(plot_opts$displayModeBar))
      fig = config(fig, displayModeBar=FALSE)
    
    if(is.null(plot_opts$showlegend))
      plot_opts$showlegend=TRUE

    fig <- fig %>% add_ribbons(ymin=~low95, ymax=~up95,
                               legendgroup="Model",
                               line=list(color="transparent"),
                               fillcolor='rgb(208,227,245)',
                               showlegend=plot_opts$showlegend, name="95% CI") # name doesn't show
    fig <- fig %>% add_ribbons(ymin=~low50, ymax=~up50,
                               legendgroup="Model",
                               line=list(color="transparent"),
                               fillcolor='rgb(171,205,237)',
                               showlegend=plot_opts$showlegend, name="50% CI") # name doesn't show
    fig  <- fig %>% add_trace(y=~mean, type="scatter",mode="lines",
                              legendgroup="Model",
                              line=list(color="black"),
                              showlegend=plot_opts$showlegend,
                               fillcolor='rgba(100,100,80,.2)', name="Mean")


    fig <- fig %>% add_ribbons(data=preds,
                               ymin=~low95, ymax=~up95,
                               legendgroup="Forecast",
                               line=list(color="transparent"),fillcolor='rgb(255,237,204)',
                               showlegend=plot_opts$showlegend, 
                               name="95% CI") # name doesn't show
    fig <- fig %>% add_ribbons(ymin=~low50, ymax=~up50,
                               legendgroup="Forecast",
                               line=list(color="transparent"),fillcolor='rgb(255,201,102)',
                               showlegend=plot_opts$showlegend, 
                               name="50% CI") # name doesn't show

    fig <- fig %>% add_trace(x=~time, y=~mean, data=preds,
                             legendgroup="Forecast",
                              line=list(color="red"),
                              type="scatter", mode="lines",
                             showlegend=plot_opts$showlegend, name="Forecast")

    fig <- fig %>% add_markers(x=~time, y=~observed, data=preds,
                               legendgroup="New Data",
                               marker = list(color="red", symbol="cross",
                                             line=list(color="white", width=1)),
                               showlegend=plot_opts$showlegend, 
                               name="New Data")
    
    fig <- fig %>% add_markers(data=fits, y=~observed, x=~time,
                               marker=list(color="grey",  symbol="circle",
                                           line=list(width=1, color="white")
                                           ),
                               showlegend=plot_opts$showlegend,
                               name="Cases")

    fig <- fig %>% layout(xaxis=xaxis, 
                          yaxis=yaxis,
                          title=preds$country[1])
    return(fig)
    
}
