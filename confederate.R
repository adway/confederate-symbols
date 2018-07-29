# Load Packages
library(dplyr)
library(ggplot2)
library(mapdata)
library(ggmap)

setwd('~/machine_learning/confederate')

data <- read.csv(file = "confederate_symbols.csv", as.is = TRUE)
data <- data[-c(1100), ]

groupedByState <- group_by(data, data$state)

summarizedState <- summarize(groupedByState, count = n())
summarizedState <- summarizedState[!(summarizedState$`data$state`=="1913"),]
# Sort
summarizedState <- summarizedState[order(summarizedState$count, decreasing = TRUE),]
# Bar Plot
barplot(summarizedState$count, names.arg = summarizedState$`data$state`, las = 2)

# GROUP BY TYPE
groupedByType <- group_by(data, data$category)

summarizedType <- summarize(groupedByType, count = n())

#Sort 
summarizedType <- summarizedType[order(summarizedType$count, decreasing = TRUE),]

#Plot
barplot(summarizedType$count, names.arg = summarizedType$`data$category`, las = 2)

## VIS ON MAP
states <- map_data("state")
map <- ggplot()
map <- map + geom_polygon(data=states, aes(x=long, y=lat, group = group),colour="#a50104")
map <- map + geom_point(data=data, aes(x=longitude, y=latitude), color="white")
map

# Census API -- Population of State
pop <- read.csv(file = "PEP_2017_PEPANNRES_with_ann.csv", as.is = TRUE)
groupedPopByState <- group_by(pop, pop$GEO.display.label)
groupedPopByState <- groupedPopByState[7:57,]
groupedPopByState <- groupedPopByState[!(groupedPopByState$GEO.display.label=='District of Columbia'),]


stateCount <- data.frame(groupedPopByState$GEO.display.label, numeric(50))
statePop <- data.frame(groupedPopByState$GEO.display.label, groupedPopByState$respop72017)
percapita <- data.frame(groupedPopByState$GEO.display.label, numeric(50))

for(i in 1:1480) {
  for(j in 1:50) {
    if(toString(groupedByState$state[i]) == toString(stateCount$groupedPopByState.GEO.display.label[j])){
      stateCount$numeric.50.[j] <- stateCount$numeric.50.[j] + 1
    }   
  }
 
}

colnames(percapita) <- c("State","Per Capita")
colnames(stateCount) <- c("State","Number of Monuments")
colnames(statePop) <- c("State","Population")
percapita$`Per Capita` <- as.numeric(stateCount$`Number of Monuments`)/as.numeric(as.character(statePop$Population))
per100K <- data.frame(groupedPopByState$GEO.display.label, numeric(50))
colnames(per100K) <- c("State", "Per 100k")
per100K$`Per 100k` <- percapita$`Per Capita`*100000

per100K <- per100K[order(per100K$`Per 100k`, decreasing = TRUE),]
per100K <- per100K[!(per100K$`Per 100k`== 0),]
barplot(per100K$`Per 100k`, names.arg = per100K$State, las=2)