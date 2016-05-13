### Performs test2 (borrowing scripts from plots.R and predictors.R).

library(forecast)

# DEFINITIONS

test = test2
dummy_rows = c(-1:-4,-109:-110)
dummy_cols = c(-1:-3)

rows = nrow(test)
cols = ncol(test)
classes = colnames(test[dummy_cols])

# FUNCTIONS

# Data acquisition
select_department = function(department){
  current_data = as.integer(data.matrix(test[dummy_rows, department]))
}

create_timeseries = function(department, is_stl=TRUE){ #Modified to allow stl forecasting
  if (is_stl==TRUE){
    init = 2014-1/52
    temp = select_department(department)
    current_data = temp[1]
    for (i in seq(1,length(temp))){
      current_data[i+1]=temp[i]
    }
    ts_data = ts(current_data, start=2014, frequency=52)
  }
  else{
    current_data = select_department(department)
    ts_data = ts(current_data, start=2014, frequency=52)
  }
}

# Stl forecasting
stl_class = function(class, trend=15, season="periodic", lambda=0.35, cout=FALSE){ #Forecasts a class for ten weeks
  data = create_timeseries(class)
  fit = stl(data, t.window=trend, s.window=season)
  sum_up = summary(fit)
  fcast = stlf(data, t.window=trend, s.window=season, robust=TRUE, h=10, lambda=lambda)
  return_list = list("sum_up"=sum_up, "fcast"=data.frame(fcast))
  if (cout==FALSE){
    plot(fcast, main=test[3, class], xlab='Year (weeks)', ylab='Sales')
    lines(data)
    legend("topleft", lty=1, col=c(1,2), legend = c("Data"))
  }
  else{
    png(filename=paste(gsub("\\.","_", test[3,class]), ".png",sep=""))
    plot(fcast, main=test[3, class], xlab='Year (weeks)', ylab='Sales')
    lines(data)
    legend("topleft", lty=1, col=c(1,2), legend = c("Data"))
    dev.off
  }
  return(return_list)
}

stl_classes = function(){
  setwd('C:\\Users\\Nikos\\Desktop\\itim\\test2_v1')
  for (class in classes){
    fcast = stl_class(class, cout=TRUE)
    write.csv(fcast$fcast, file=paste(gsub("\\.","_", test[3,class]), ".txt",sep=""))
  }
  setwd('C:\\Users\\Nikos\\Desktop\\itim')
}












