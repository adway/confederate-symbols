# Load Packages
library(dplyr)
library(ggplot2)
library(mapdata)
library(ggmap)


data <- read.csv('confederate_symbols.csv')
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
map <- map + geom_polygon(data=states, aes(x=long, y=lat, group = group),colour="black")
map <- map + geom_point(data=data, aes(x=longitude, y=latitude), color="PINK")
map
