fits_and_predicts <- function(outputdir, daysoff=7){
    #od = ipf(normalizePath(outputdir))
    fits = NA
    predicts = NA
    if(file.exists(paste0(outputdir,"/pred_forecast.csv"))){
        ## single file needs splitting
        fp = read.csv(paste0(outputdir,"/pred_forecast.csv"))
        last_predict <- max(as.Date(fp$time))
        is_fit = as.Date(fp$time) <= (last_predict - daysoff)
        fits = fp[is_fit,]
        predicts = fp[!is_fit,]
    }
    if(all(file.exists(paste0(outputdir,"/model_fitted.csv"), paste0(outputdir,"/model_predicted.csv")))){
        fits = read.csv(paste0(outputdir,"/model_fitted.csv"))
        predicts = read.csv(paste0(outputdir,"/model_predicted.csv"))
    }
    return(list(fits=fits, predicts=predicts))
    
}
