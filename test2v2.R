### Performs test2 (borrowing scripts from plots.R and predictors.R).
### Search for "C:\" to change directories in the script.

if(!exists("divisions", mode="function")) source('test1v2.R')
require(xlsx)

SalesData2 = read_excel("C:\\Users\\Nikos\\Desktop\\itim\\sales_data.xlsx", sheet=3)

rows2 = nrow(SalesData2)
cols2 = ncol(SalesData2)
dummy_rows2 = c(-1,-2)
dummy_cols2 = c(-1:-5,-110,-111)

select_class = function(row2){
  current_data = as.integer(data.matrix(SalesData2[row2, dummy_cols2]))
}

ts_class = function(row2){
  current_data = select_class(row2)
  ts_data = ts(current_data, start=2014, frequency=52)
}

multireg_class = function(row2, cout=FALSE){
  # Forecasts a class for the next ten weeks by multiple regression with predictors the 52 weeks of a year.
  # Returns a list with the summary of the fit, the fitted data and the forecast data.
  data = ts_class(row2)
  fit = tslm(data ~ trend + season)
  sum_up = summary(fit)
  fitted_data = fitted(fit)
  resids = residuals(fit)
  fcast = forecast(fit, h=10)
  return_list = list("sum_up"=sum_up, "fitted_data"=fitted_data,
                     "fcast_data"=data.frame(fcast), "residuals"=data.frame(resids))
  
  if (cout==FALSE){
    par(mfrow=c(1,1))
    plot(fcast, main=SalesData2[row2,4], xlab='Year (weeks)', ylab='Total Sales')
    lines(data)
    lines(fitted_data, col=2)
    legend("topleft", lty=1, col=c(1,2), legend = c("Data", "Multi-reg"))
    
    par(mfrow=c(1,2))
    plot(resids, xlab='Year (weeks)', ylab='Residuals')
    Acf(resids, main="")
  }
  else{
    png(filename=paste(gsub("\\.","_", SalesData2[row2,4]), "_forecast.png",sep=""))
    par(mfrow=c(1,1))
    plot(fcast, main=SalesData2[row2,4], xlab='Year (weeks)', ylab='Total Sales')
    lines(data)
    lines(fitted_data, col=2)
    legend("topleft", lty=1, col=c(1,2), legend = c("Data", "Multi-reg"))
    dev.off()
    png(filename=paste(gsub("\\.","_", SalesData2[row2,4]), "_residuals.png",sep=""), type="cairo")
    par(mfrow=c(1,2))
    plot(resids, xlab='Year (weeks)', ylab='Residuals')
    Acf(resids, main="")
    dev.off()
  }
  return(return_list)
}

multireg_class_all = function(cout=FALSE, data_cout=FALSE){
  if (data_cout==FALSE){
    for (row2 in seq(28, rows2)){
      multireg_class(row2, cout=cout)
    }
  }
  else{
    for (row2 in seq(28, rows2)){
      setwd('C:\\Users\\Nikos\\Desktop')
      x = data.frame(multireg_class(row2, cout=cout)$fcast_data)[1]
      write(x, file=row2)
      setwd('C:\\Users\\Nikos\\Desktop\\itim')
    }
  }
}

cout_plots = function(data_cout=FALSE){
  setwd('C:\\Users\\Nikos\\Desktop\\itim\\test2_graphs')
  multireg_class_all(cout=TRUE, data_cout=data_cout)
  setwd('C:\\Users\\Nikos\\Desktop\\itim')
}
















