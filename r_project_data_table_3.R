#Load packages
library(data.table)
library(ggplot2)


setwd("~/ddf/precip_data/out")
#create function to find information for depth duratio curve
ddf = function(r){

  
#transfor precipitation from hours to seconds  
  r[, p := p * 60 * 60]
 
#calculates totol sum of precipiration over a n(1,2,3,6,12,24) year period
  r = r[, .(D1 = p, D2 = frollsum(p, 2), D3 = frollsum(p, 3), D6 = frollsum(p, 6), D12 = frollsum(p, 12), D24 = frollsum(p, 24)), by = year(date)]
  
#reform data table
  r = melt.data.table(r, id.vars = 'year')
  
#selects max value for each variable and year
  mx = r[, .(Depth = max(value, na.rm = TRUE)), by = .(year, variable)]
#probibiliity of event occuring 
  mx = mx[, .(Depth = quantile(Depth, p = 1 - 1/c(2, 5, 10, 20)), Freq = c(2, 5, 10, 20)), by = .(Duration = variable)]
# creating a numeric to easily work with data
  mx[, hour := as.numeric(gsub('D', '', Duration))]
}
#Use function created apply to list created
DDF = lapply(dta, ddf)
#add names of list - helping identify regional and global climate moodels
names(DDF) = dir()[1:31]
names(DDF)
#create one list
?rbindlist()
DDF = rbindlist(DDF, idcol = 'SID')
DDF
# add exta colounms in data table, grouping RCP model, regional climae model and global climate model
DDF[, RCP:=sapply(strsplit(SID, '_'), function(x)x[5])]
DDF[, type_of_model:=sapply(strsplit(SID, '_'), function(x)x[4])]
DDF[, RCM:=sapply(strsplit(SID, '_'), function(x)x[7])]
#since we are interested in predicited scenarios, historic entriies are not relevent
pDDF <- DDF[DDF[, RCP != ("historical")],]
pDDF
#Plotting graphs - since some values have multiple models, an average is taken
ggplot(pDDF) + geom_smooth(aes(x = hour, y = Depth, col = factor(Freq), group = Freq), se = F) + facet_grid(RCP~type_of_model) + labs(title = "Global Climate Model Depth Duration curve")
ggplot(pDDF) + geom_smooth(aes(x = hour, y = Depth, col = factor(Freq), group = Freq), se = F) + facet_grid(RCP~RCM) + labs(title = "Regional Climate Model Depth Duration curve")

# negative gradients in plots shoud be further investigated, i.e what are the intial conditions for each model, what assumptions havve been made ect 

