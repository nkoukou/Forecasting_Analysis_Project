### Performs test1 (borrowing scripts from plots.R and predictors.R).

library(forecast)

# DEFINITIONS

test = test1
dummy_rows = c(-1,-2,-107,-108)

rows = nrow(test)
cols = ncol(test)
departments = colnames(test) #Includes the first three dummy columns of test1 ('X', 'x.1', 'Division')
divisions=c() #Defines (below) all the different divisions by their title

k=0
states=c()
nums=c("1", "2", "3", "4", "5", "6", "7", "8", "9")
for (i in departments[-1:-2]){
  for (j in nums){
    states[as.integer(j)] = grepl(j, i)
  }
  if (TRUE %in% states){next}
  divisions[k]=i
  k=k+1
}

# FUNCTIONS

# Data acquisition
select_department = function(department){
  current_data = as.integer(data.matrix(test[dummy_rows, department]))
}

create_timeseries = function(department){
  current_data = select_department(department)
  ts_data = ts(current_data, start=2014, frequency=52)
}

# Time series plots
ts_plot_dep = function(dep, cout=FALSE){ #Plots a department
  current_data=create_timeseries(dep)
  if (cout==FALSE){
    plot(current_data, main=test[1,dep], xlab='Year (weeks)', ylab='Sales') 
  }
  else{
    png(filename=paste(gsub("\\.","_", test[1,dep]), " ts.png",sep=""))
    plot(current_data, main=test[1,dep], xlab='Year (weeks)', ylab='Sales')
    dev.off
  }
}

ts_plot_deps = function(div, cout=FALSE){ #Plots the departments of a division
  deps=c()
  j=1
  for (i in seq(1,cols)){
    if (grepl(div,departments[i])){
      deps[j] = departments[i]
      j=j+1
    }
  }
  for (dep in deps){
    ts_plot_dep(dep, cout=cout)
  }
}

ts_plot_div = function(div){ #Plots a division
  deps=c()
  j=1
  for (i in seq(1,cols)){
    if (grepl(div,colnames(test)[i])){
      deps[j] = colnames(test)[i]
      j=j+1
    }
  }
  current_data=0
  for (dep in deps)
  {
    current_data = current_data + select_department(dep)
  }
  current_data = ts(current_data, start=2014, frequency=52)
  plot(current_data, main=div, xlab='Year (weeks)', ylab='Total Sales')
}

# Seasonal plots
season_plot_dep = function(dep, cout=FALSE){ #Plots a department
  current_data = create_timeseries(dep)
  if (cout==FALSE){
    seasonplot(current_data, main=test[1,dep], xlab='Weeks', col=c("black","red"),
               ylab='Sales', year.labels=TRUE, year.labels.left=TRUE)
  }
  else{
    png(filename=paste(gsub("\\.","_", test[1,dep]), ".png",sep=""))
    seasonplot(current_data, main=test[1,dep], xlab='Weeks', col=c("black","red"),
               ylab='Sales', year.labels=TRUE, year.labels.left=TRUE)
    dev.off
  }
}

season_plot_deps = function(div, cout=FALSE){ #Plots the departments of a division
  deps=c()
  j=1
  for (i in seq(1,cols)){
    if (grepl(div,departments[i])){
      deps[j] = departments[i]
      j=j+1
    }
  }
  for (dep in deps){
    season_plot_dep(dep,cout=cout)
  }
}

season_plot_div = function(div){ #Plots a division
  deps=c()
  j=1
  for (i in seq(1,cols)){
    if (grepl(div,colnames(test)[i])){
      deps[j] = colnames(test)[i]
      j=j+1
    }
  }
  current_data=0
  for (dep in deps)
  {
    current_data = current_data + select_department(dep)
  }
  current_data = ts(current_data, start=2014, frequency=52)
  seasonplot(current_data, main=div, xlab='Weeks', ylab='Total Sales', col=c("black","red"),
       year.labels=TRUE, year.labels.left=TRUE)
}

# Division profile determination (subject to improvement)

profile_div = function(div, season=TRUE, ts=FALSE){
  setwd('C:\\Users\\Nikos\\Desktop\\itim\\action_concepts_profile_v1')
  if (season==TRUE){
    png(filename=paste(gsub("\\.","_", div), "_division.png",sep=""))
    season_plot_div(div)
    dev.off
    season_plot_deps(div, cout=TRUE)
  }
  if (ts==TRUE){
    png(filename=paste(gsub("\\.","_", div), "_division_ts.png",sep=""))
    ts_plot_div(div)
    dev.off
    ts_plot_deps(div, cout=TRUE)
  }
  setwd('C:\\Users\\Nikos\\Desktop\\itim')
}













