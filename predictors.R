# Forecasts divisions
setwd('C:\\Users\\Nikos\\Desktop\\itim\\stlm_forecasts')
require(grDevices)
library(forecast)

test = test1

rows = nrow(test)
cols = ncol(test)
divisions = colnames(test)

# Define the divisions
division_titles=c()
k=0
states=c()
nums=c("1", "2", "3", "4", "5", "6", "7", "8", "9")
for (i in divisions[-1:-2]){
  for (j in nums){
    states[as.integer(j)] = grepl(j, i)
  }
  if (TRUE %in% states){next}
  division_titles[k]=i
  k=k+1
}
lt = length(division_titles)

select_dep = function(department){
# Selects a department data set ignoring irrelevant info e.g. department titles
  current_data = as.integer(data.matrix(test[c(-1,-2,-107,-108), department]))
}

multireg_division = function(division, res=TRUE){
# Forecasts a division for the next year by multiple regression with predictors the 52 weeks of a year.
# Returns a list with the summary of the fit, the Durbin-Watson test, the fitted data and the forecasted data.
# It is not very good according to the Durbin-Watson test
  deps=c()
  j=1
  for (i in seq(1,cols)){
    if (grepl(division,colnames(test)[i])){
      deps[j] = colnames(test)[i]
      j=j+1
    }
  }
  data=0
  for (dep in deps)
  {
    data = data + select_dep(dep)
  }
  data = ts(data.frame(data), start=2014, frequency=52)
  fit = tslm(data ~ trend + season)
  sum_up = summary(fit)
  fitted_data = fitted(fit)
  dw = dwtest(fit, alt="two.sided")
  fcast = forecast(fit, h=52)
  return_list = list(sum_up, dw, fitted_data, data.frame(fcast))
  
  #png(filename=paste(gsub("\\.","_", division), "_forecast.png",sep=""))
  par(mfrow=c(1,1))
  plot(fcast, main=division, xlab='Year (weeks)', ylab='Total Sales')
  lines(data)
  lines(fitted_data, col=2)
  legend("topleft", lty=1, col=c(1,2), legend = c("Data", "Multi-reg"))
  #dev.off()
  
  if (res==TRUE){
    #png(filename=paste(gsub("\\.","_", division), "_residuals.png",sep=""), type="cairo")
    resids = residuals(fit)
    par(mfrow=c(1,2))
    plot(resids, xlab='Year (weeks)', ylab='Residuals')
    Acf(resids, main='Autocorrelation')
    #dev.off()
  }
  
  return(return_list)
}

stl_division = function(division, trend=15, season="periodic", lambda=0.35){
  # Forecasts a division for the next year by decomposition into seasonal and trend components.
  # Returns a list with the fitted data and the forecasted data.
  deps=c()
  j=1
  for (i in seq(1,cols)){
    if (grepl(division,colnames(test)[i])){
      deps[j] = colnames(test)[i]
      j=j+1
    }
  }
  data=0
  for (dep in deps)
  {
    data = data + select_dep(dep)
  }
  data_temp = data[1]
  for (i in seq(1,length(data))){
         data_temp[i+1]=data[i]
  }
  init = 2014-1/52
  data = ts(data_temp, start=init, frequency=52)
  fit = stl(data, t.window=trend, s.window=season)
  fcast = stlf(data, t.window=trend, s.window=season, robust=TRUE, h=52, lambda=lambda)
  sum_up = summary(fit)
  return_list = list(fit, data.frame(fcast))
  png(filename=paste(gsub("\\.","_", division), "_forecast.png",sep=""))
  par(mfrow=c(1,1))
  plot(fcast, main=division, xlab='Year (weeks)', ylab='Total Sales')
  lines(data)
  legend("topleft", lty=1, col=c(1,2), legend = c("Data"))
  dev.off()
  return(return_list)
}

stl_test = function(division){
  for (i in seq(0,1,0.05)) {
    stl_division(division, trend=15, season="periodic",lambda=i)
  }
}

multireg_all_divisions = function(do_res=TRUE){
# Calls multireg_division for all divisions
# Returns a list with four elements-lists which correspond to the divisions:
# 1. Fit summaries 2. Durbin-Watson tests 3. Fitted data 4. Forecasted data
# Example to acccess the desired element, if values=multireg_all_divisions():
# values$fcasts$GIRLS gets the forecasted values for the "GIRLS" division.
  sum_ups=list(); dws=list(); fits=list(); fcasts=list()
  
  for (division in division_titles){
    temp = multireg_division(division, res=do_res)
    div = gsub("\\.","_", division)
    sum_ups[div]=temp[1]; dws[div]=temp[2]; fits[div]=temp[3]; fcasts[div]=temp[4]
  }
  return_list=list("sumup"=sum_ups, "dw"=dws, "fits"=fits, "fcasts"=fcasts)
  return(return_list)
}

stl_all_divisions = function(trend=15, season=3, lambda=0.35){
# Calls multireg_division for all divisions
# Returns a list with two elements-lists which correspond to the divisions:
# 1. Fitted data 2. Forecasted data
# Example to acccess the desired element, if values=stl_all_divisions():
# values$fits$GIRLS gets the fitted values for the "GIRLS" division.
  fits=list(); fcasts=list()
  
  for (division in division_titles){
    temp = stl_division(division, trend=trend, season=season, lambda=0.3)
    div = gsub("\\.","_", division)
    fits[div]=temp[1]; fcasts[div]=temp[2]
  }
  return_list=list("fits"=fits, "fcasts"=fcasts)
}





