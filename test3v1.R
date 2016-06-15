### Performs test2 (borrowing scripts from plots.R and predictors.R).
### Search for "C:\" to change directories in the script.

if(!exists("divisions", mode="function")) source('test1v2.R')
require(xlsx)

SalesData3 = read_excel("C:\\Users\\Nikos\\Desktop\\itim\\sales_data.xlsx", sheet=4)

rows3 = nrow(SalesData3)
cols3 = ncol(SalesData3)
dummy_rows3 = c(-1,-2)
dummy_cols3 = c(-1:-5,-93,-94)

select_class = function(row3){
  current_data = as.integer(data.matrix(SalesData3[row3, dummy_cols3]))
}

ts_class = function(row3){
  current_data = select_class(row3)
  ts_data = ts(current_data, start=2014, frequency=52)
}

multireg_div = function(row3){
  data = create_timeseries_div(SalesData3[row3,1])
  fit = tslm(data ~ trend + season)
  sum_up = summary(fit)
  fitted_data = fitted(fit)
  resids = residuals(fit)
  return_list = list("sum_up"=sum_up, "fitted_data"=fitted_data,
                     "residuals"=data.frame(resids))
  return(return_list)
}

forecast_class = function(row3, cout=FALSE){
  data_div = multireg_div(row3)$fitted_data
  avg_div = mean(data_div); std_div = sd(data_div)
  norm_data_div = (data_div - avg_div)/std_div
  
  data_class = ts_class(row3)
  avg_class = mean(data_class); std_class = sd(data_class)
  data_class_append = norm_data_div[88:104]*std_class + avg_class
  data_class = c(data_class, data_class_append)
  data_class = ts(data_class, start=2014, frequency=52)
  
  if (cout==FALSE){
    plot(data_class, main=SalesData3[row3,4], xlab='Year (weeks)', ylab='Total Sales')
    abline(v=2015+35/52, col=2)
  }
  else{
    png(filename=paste(gsub("\\.","_", SalesData3[row3,4]), "_forecast.png",sep=""))
    plot(data_class, main=SalesData3[row3,4], xlab='Year (weeks)', ylab='Total Sales')
    abline(v=2015+35/52, col=2)
    dev.off()
  }
  return(data_class_append)
}

forecast_class_all = function(cout=FALSE){
  if (cout==FALSE){
    for (row3 in seq(3, rows3)){
      forecast_class(row3, cout=FALSE)
    }
  }
  else{
    fcast_data_set = SalesData3
    
    for (col2 in seq(1,17)){
      fcast_data_set[1,cols3+col2] = 2015
      fcast_data_set[2,cols3+col2] = fcast_data_set[2,40+col2]
    }
    setwd('C:\\Users\\Nikos\\Desktop\\itim\\test3_graphs')
    for (row3 in seq(3, rows3)){
      current_data = forecast_class(row3, cout=TRUE)
      for (col3 in seq(1,17)){
        fcast_data_set[row3,cols3+col3] = current_data[col3] 
      }
      write.xlsx(fcast_data_set, "fitted_data_set.xlsx", sheetName="fitted_data")
    }
    setwd('C:\\Users\\Nikos\\Desktop\\itim')
  }
  
}









