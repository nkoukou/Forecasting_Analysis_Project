# Makes various plots for all data (104 weeks) of all divisions

test = test1

rows = nrow(test)
cols = ncol(test)
divisions = colnames(test)

# Define the division titles
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

select_department = function(department){
  current_data = as.integer(data.matrix(test[c(-1,-2,-107,-108), department]))
}

create_timeseries = function(department){
  current_data = select_department(department)
  ts_data = ts(current_data, start=2014, frequency=52)
}

plot_timeseries = function(start, end=start){
  for (dep in seq(start,end,by=1))
  {
    current_data=create_timeseries(dep)
    plot(current_data, main=test[1,dep], xlab='Year (weeks)', ylab='Sales')
  }
}

plot_division_timeseries = function(division){
  deps=c()
  j=1
  for (i in seq(1,cols)){
    if (grepl(division,colnames(test)[i])){
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
  plot(current_data, main=division, xlab='Year (weeks)', ylab='Total Sales')
}

plot_all_divisions_timeseries = function(){
  for (division in division_titles){
    plot_division_timeseries(division)
  }
}

season_plot = function(department){
  current_data = create_timeseries(department)
  seasonplot(current_data, main=test[1,department], xlab='Weeks', 
             ylab='Sales', year.labels=TRUE, year.labels.left=TRUE)
}

scatter_plot = function(dep1, dep2){
  x_data = select_department(dep1)
  y_data = select_department(dep2)
  plot(x_data, y_data, main='Scatterplot', xlab=test[1,dep1], ylab=test[1,dep2])
}

scatter_division_plot = function(division){
  deps=list()
  titles=c() # !!!
  j=1
  for (i in seq(1,cols)){
    if (grepl(division,divisions[i])){
      length(deps)=length(deps)+1
      deps[j] = list(select_department(i))
      titles[j]=i
      j=j+1
    }
  }
  pairs(deps, main=division, text.panel="hello") # !!!
}