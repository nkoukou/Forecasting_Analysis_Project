# Forecasts divisions
setwd('C:\\Users\\Nikos\\Desktop\\itim\\initial_forecasts')
require(grDevices)

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

forecast_division = function(division, res=TRUE){
# Forecasts a division for the next year by multiple regression with predictors the 52 weeks of a year.
# Returns a list with the division title, the summary of the fit, the Durbin-Watson test and the fitted data.
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
  
  png(filename=paste(gsub("\\.","_", division), "_forecast.png",sep=""))
  par(mfrow=c(1,1))
  plot(fcast, main=division, xlab='Year (weeks)', ylab='Total Sales')
  lines(data)
  lines(fitted_data, col=2)
  legend("topleft", lty=1, col=c(1,2), legend = c("Data", "Multi-reg"))
  dev.off()
  
  if (res==TRUE){
    png(filename=paste(gsub("\\.","_", division), "_residuals.png",sep=""), type="cairo")
    resids = residuals(fit)
    par(mfrow=c(1,2))
    plot(resids, xlab='Year (weeks)', ylab='Residuals')
    Acf(resids, main='Autocorrelation')
    dev.off()
  }
  
  return(return_list)
}

forecast_all_divisions = function(do_res=TRUE){
# Calls forecast_division for all divisions
# Returns a list with four elements-lists which correspond to the divisions alphabetically:
# 1. Fit summaries 2. Durbin-Watson tests 3. Fitted data 4. Forecasted data
  #sum_ups=list(); dws = list(); fits = list(); fcasts = list()
  #length(sum_ups) = lt; length(dws) = lt; length(fits) = lt; length(fcasts) = lt
  #i=0
  for (division in division_titles){
    temp = forecast_division(division, res=do_res)
    #sum_ups[i] = temp[1]; dws[i] = temp[2]; fits[i] = temp[3]; fcasts[i] = temp[4]
    #i=i+1
  }
  #return_list=list(sum_ups, dws, fits, fcast)
}







