### Performs test1 (borrowing scripts from plots.R and predictors.R).
### Search for "C:\" to change directories in the script.

require(forecast)
require(tseries)
require(readxl)

# DEFINITIONS

SalesData = read_excel("C:\\Users\\Nikos\\Desktop\\itim\\sales_data.xlsx", sheet=2)

rows = nrow(SalesData)
cols = ncol(SalesData)
dummy_rows = c(-1:-3)
dummy_cols = c(-1:-3,-108,-109)

departments = SalesData[dummy_rows,2]
divisions=c() #Defines (below) all the different divisions by their title
divisions[1] = SalesData[4,1]
j=2
for (i in seq(12, rows)){
  j_test = divisions[j-1]
  if (identical(SalesData[i,1],j_test)){
    next
  }
  divisions[j] = SalesData[i,1]
  j=j+1
}

# FUNCTIONS

# Department-Division matching
div_to_deps = function(div){
  deps=c()
  j=1
  for (i in seq(1,rows)){
    if (identical(SalesData[i,1],div)){
      deps[j] = SalesData[i,2]
      j=j+1
    }
  }
  return(deps)
}

# Data acquisition
select_department = function(department){
  for (i in seq(1,rows)){
    if (identical(SalesData[i,2],department)){
      dep = i
      break
    }
  }
  current_data = as.integer(data.matrix(SalesData[dep, dummy_cols]))
}

create_timeseries = function(department){
  current_data = select_department(department)
  ts_data = ts(current_data, start=2014, frequency=52)
}

create_timeseries_div = function(division){
  deps = div_to_deps(division)
  current_data=0
  for (dep in deps)
  {
    current_data = current_data + select_department(dep)
  }
  ts_data = ts(current_data, start=2014, frequency=52)
}

# Division profile determination
# !!! profile_divs and stationary_test have length 160, while departments have length 161

stationarity_test = function(){ #Tests for significance of differencing
                                #A few departments need differencing
  pvalues = list()
  for (dep in departments){
    data = create_timeseries(dep)
    if (try(adf.test(data, "stationary"), TRUE) == geterrmessage()){
      pvalue = "0.01"
    }
    else{
      pvalue = adf.test(data, "stationary")$p.value
    }
    pvalues[dep] = pvalue
  }
  return(pvalues)
}

stats_dep = function(dep){ #Returns ts data, mean, std for department
  current_data = create_timeseries(dep)
  avg = mean(current_data); std = sd(current_data); sumup=summary(current_data)
  znormal = (current_data - avg)/std
  return(list("data"=current_data, "avg"=avg, "std"=std, "sumup"=sumup, "znormal"=znormal))
}

stats_div = function(div){ #Returns ts data, mean, std for division
  current_data = create_timeseries_div(div)
  avg = mean(current_data); std = sd(current_data); sumup=summary(current_data)
  znormal = (current_data - avg)/std
  return(list("data"=current_data, "avg"=avg, "std"=std, "sumup"=sumup, "znormal"=znormal))
}

profile_div = function(div){ #Calculates Euclidean metric between division and its departments 
  metrics=list()
  deps = div_to_deps(div)
  div_stats = stats_div(div)
  for (dep in deps){
    dep_stats = stats_dep(dep)
    euclidean = sqrt(sum((div_stats$znormal+dep_stats$znormal)^2))
    metrics[dep] = euclidean
  }
  return(metrics)
}

profile_divs = function(){#Calls profile_div for all divisions
  metrics=list()
  for (div in divisions){
    metric = profile_div(div)
    i=0
    for (dep in metric){
      metrics[paste(div,"$",i,sep="")] = dep
      i=i+1
    }
  }
  return(metrics)
}

# Time series plots
ts_plot_dep = function(dep, cout=FALSE){ #Plots a department
  current_data=create_timeseries(dep)
  if (cout==FALSE){
    plot(current_data, main=dep, xlab='Year (weeks)', ylab='Sales') 
  }
  else{
    png(filename=paste(gsub("\\.","_", dep), "_ts.png",sep=""))
    plot(current_data, main=dep, xlab='Year (weeks)', ylab='Sales')
    dev.off
  }
}

ts_plot_deps = function(div, cout=FALSE){ #Plots the departments of a division
  deps = div_to_deps(div)
  for (dep in deps){
    ts_plot_dep(dep, cout=cout)
  }
}

ts_plot_div = function(div, cout=FALSE){ #Plots a division
  current_data = create_timeseries_div(div)
  if (cout==FALSE){
    plot(current_data, main=div, xlab='Year (weeks)', ylab='Total Sales')
  }
  else{
    png(filename=paste(gsub("\\.","_", div), "_ts.png",sep=""))
    plot(current_data, main=div, xlab='Year (weeks)', ylab='Total Sales')
    dev.off
  }
}

# Seasonal plots
season_plot_dep = function(dep, cout=FALSE){ #Plots a department
  if (cout==FALSE){
    current_data = create_timeseries(dep)
    seasonplot(current_data, main=dep, xlab='Weeks', col=c("black","red"),
               ylab='Sales', year.labels=TRUE, year.labels.left=TRUE)
  }
  else{
    png(filename=paste(gsub("\\.","_", dep), ".png",sep=""))
    seasonplot(current_data, main=dep, xlab='Weeks', col=c("black","red"),
               ylab='Sales', year.labels=TRUE, year.labels.left=TRUE)
    dev.off
  }
}

season_plot_deps = function(div, cout=FALSE){ #Plots the departments of a division
  deps = div_to_deps(div)
  for (dep in deps){
    season_plot_dep(dep,cout=cout)
  }
}

season_plot_div = function(div, cout=FALSE){ #Plots a division
  current_data = create_timeseries_div(div)
  if (cout==FALSE){
    seasonplot(current_data, main=div, xlab='Weeks', ylab='Total Sales', col=c("black","red"),
               year.labels=TRUE, year.labels.left=TRUE)
  }
  else{
    png(filename=paste(gsub("\\.","_", div), ".png",sep=""))
    seasonplot(current_data, main=div, xlab='Weeks', ylab='Total Sales', col=c("black","red"),
               year.labels=TRUE, year.labels.left=TRUE)
    dev.off
  }
}

# Normalised graphs
norm_div_dep = function(dep, cout=FALSE){ #Plots normalised department and division on same graph
  for (i in seq(1,rows)){
    if (identical(SalesData[i,2],dep)){
      div = SalesData[i,1]
      break
    }
  }
  norm_dep = stats_dep(dep)
  norm_div = stats_div(div)
  if (cout==FALSE){
    plot(norm_dep$znormal, main=paste("Normalised",dep,"of division",div),
         xlab='Year (weeks)', ylab='Total Sales', col=1)
    lines(norm_div$znormal, col=2)
    legend("topleft", lty=1, col=c(1,2), legend = c(dep, div))
  }
  else{
    png(filename=paste(gsub("\\.","_", dep), "_ts.png",sep=""))
    plot(norm_dep$znormal, main=paste("Normalised",dep,"of division",div),
         xlab='Year (weeks)', ylab='Total Sales', col=1)
    lines(norm_div$znormal, col=2)
    legend("topleft", lty=1, col=c(1,2), legend = c(dep, div))
    dev.off
  }
}

norm_div_dep_all = function(div, cout=FALSE){ #Calls norm_div_dep for all departments of a division
  deps = div_to_deps(div)
  for (dep in deps){
    norm_div_dep(dep, cout=cout)
  }
}

cout_plots = function(div){
  setwd('C:\\Users\\Nikos\\Desktop\\itim\\norm_graphs_action_concepts')
  norm_div_dep_all(div,cout=TRUE)
  setwd('C:\\Users\\Nikos\\Desktop\\itim')
}




