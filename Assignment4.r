data <- read.csv(file.choose(), header = T)
ot <- data.frame(data)

library(ggplot2)
library(ggExtra)
library(highcharter)
library(igraph)

library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(ggmap)
library(htmltools)
library(mapproj)
library(mapdata)

#Choose one quantitative variable to focus on - DepDelay
# Chosen quantitative variable is Departure Delay

#----------------------------------------------------------------------------------------------------------------

#Viz 1
# a univariate plot 

ot %>% ggplot(aes(x = DepDelay)) +
  geom_histogram(color = 'red') +
  ggtitle("Distribution of Departure Delay") + 
  scale_x_continuous("Departure Delay(mins)",limits = c(-100,200)) 


# The reason for choosing this plot is because I wanted to know what the distribution of dep delay was. Needed to know what was the usual delay time of flights. From the plot we can see that most flights are delayed for less than 100 mins. Very few flight are delayed for more than 250 mins and even fewer for more than 500 mins.Also can see that some flights leave early but the majority of them leave less than 20 mins early.Chose histogram because it helps us see the variation clearly

#----------------------------------------------------------------------------------------------------------------------

#Viz 2
# a bivariate plot - scatter plot

ot$DistanceGroup <- as.factor(ot$DistanceGroup)
b <- ot %>% ggplot(aes(x = DepDelay, y = ArrDelay, colour =DistanceGroup)) +
  geom_point() +
  scale_colour_brewer(palette = "Set3") +
  ggtitle("Arrival Delay vs Departure Delay") +
  scale_x_continuous("Departure Delay(min)") +
  scale_y_continuous("Arrival Delay(min)") +
  facet_wrap(~DistanceGroup)

plot(b)

# The reason for choosing this scatter plot between DepDelay and ArrDelay is to study the relationship b/w them. Colouring the flights with their distance group, we can see that the relationship remains almost similar across all distance groups. Question to answer was whether flights over longer distances experience more delay on arrival as compared to departure. It seems that on average, longer and shorter flights experience the same performance mid air. Choose a scatter plot for a bi variate viz because it allows us to see any outliers easily. For eg. we can see that for distance group 3, one flight experienced a major delay of over 1000 mins.

#---------------------------------------------------------------------------------------------------------------------------

#Viz 3
# Another bi-variate plot

#Grouping Data
c1 <- ot %>% group_by(DayofMonth) %>% 
  summarise(Count = sum(CarrierDelay, na.rm = TRUE))
c2 <- ot %>% group_by(DayofMonth) %>% 
  summarise(Count = sum(WeatherDelay, na.rm = TRUE))
c3 <- ot %>% group_by(DayofMonth) %>% 
  summarise(Count = sum(NASDelay, na.rm = TRUE))
c4 <- ot %>% group_by(DayofMonth) %>% 
  summarise(Count = sum(SecurityDelay, na.rm = TRUE))
c5 <- ot %>% group_by(DayofMonth) %>% 
  summarise(Count = sum(LateAircraftDelay, na.rm = TRUE))

#Plotting multiple lines
ggplot(c1,aes(DayofMonth)) +
  geom_line(aes(y=c1$Count,colour = "Carrier Delay"))+
  geom_line(aes(y=c2$Count,colour='Weather Delay'))+
  geom_line(aes(y=c3$Count,colour='NAS Delay'))+
  geom_line(aes(y=c4$Count,colour='Security Delay'))+
  geom_line(aes(y=c5$Count,colour='Late Aircraft Delay')) +
  scale_color_manual("",
                     breaks = c("Carrier Delay","Weather Delay","NAS Delay","Security Delay","Late Aircraft Delay"),
                     values = c("purple","blue","green","yellow","red")) +
  ggtitle("Variation of Causes of Delay per day of month")+
  scale_x_continuous("Day of Month") +
  scale_y_continuous("Total delay in mins")  +
  theme_bw()

#Choose this particular viz to understand the various causes for delay and their day to day impact. We can see that for most days Late aircraft delay was the  main cause of flight delays. This plot is helpful for someone who wants to concentrate on one of the causes for delays and try to reduce their impact. Question to answer was whether any specific cause was more influential in delaying flights than others.
# 
  
#-------------------------------------------------------------------------------------------------------------------------

#Viz 4
#Interactive Plot
d1 <- ot %>% group_by(DestStateName) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count))
d <- ot %>% filter(DestStateName=="California"|DestStateName=="Texas"|DestStateName=="Illinois"|DestStateName=="Georgia"|DestStateName=="Florida")
d <- d %>% filter(ArrDelay<100, na.rm=TRUE)

#Transforming data to box plot data
d1 <-data_to_boxplot(d,ArrDelay, DestStateName, group_var = DestStateName)

#Plotting
highchart() %>% 
  hc_xAxis(type="category") %>% 
  hc_add_series_list(d1) %>% 
  hc_title(text = "Arrival Delay in 5 Busiest Airports") %>% 
  hc_yAxis(title = list(text="Arrival Delay(mins)"))
  

#Chose this boxplot viz to see the variation of Arrival Delay Time in the top 5 states with the most incoming flights. We can notice that Illinois has the maximum variation. The largest delay is of 66 mins while at the same time flight has arrived 47 minutes earlier as well. Also approximately spread is over arrival delay is over the similar range for the top 5 busiest states. Need this information to understand if the traffic in airports have an effect on arrival delay

#------------------------------------------------------------------------------------------------------------------

#Viz 5
# Another interactive plot

ot %>% group_by(UniqueCarrier) %>% 
  summarise(Mean = mean(DepDelayMinutes,na.rm=TRUE)) %>% 
  hchart(type = "column", hcaes(x=UniqueCarrier,y=Mean)) %>% 
  hc_title(text = 'Mean Departure Delay per Carrier') %>% 
  hc_yAxis(title=list(text='Average Departure Delay(mins)'))

#To study the performance of various unique carriers, we needed some information on the delay per carrier. So, created this bar chart showing the average departure delay per carrier and we can see that the carrier MQ has the highest average delay and the carrier HA has the lowest average delay. The variation we see now gives rise to the question about what the good performing carriers are doing differently as compared to the bad performing ones.

#------------------------------------------------------------------------------------------------------------------

#Viz 6
#Geospatial Viz

#import states data
s <- map_data('state')
states <- ot %>% group_by(OriginStateName) %>% 
  summarise(Mean = mean(DepDelay,na.rm = TRUE))

library(openintro)
states$OriginStateName <- tolower(states$OriginStateName)

#merge our data with states data
data3 <- merge(s,states,
               by.x = 'region',
               by.y = 'OriginStateName')

ggplot(data3,aes(x = long, y = lat, group = group, fill = Mean)) +
  geom_polygon(color = 'white') +
  coord_map('polyconic') +
  scale_fill_gradient2(low = 'white', high = 'blue') +
  ggtitle("Average Departure Delay in different states") 

#Choose this geo plot to see the variation in departure delay in different states. We can see that Illinois and Vermont have the largest mean departure delay in their airports and at the same time states like Idaho and Montana have the lowest average departure delay. Would be helpful for someone who wants to improve performance by concentrating in the bad performing states

#-----------------------------------------------------------------------------------------------------------------

#Viz 7
#Network Viz


g <- ot %>% group_by(OriginStateName) %>% 
  summarise(Count=n()) %>% 
  arrange(desc(Count))
g <- ot %>% filter(OriginStateName==c("California","Texas","Illinois","Georgia","Florida"))
g1 <- g %>% filter(DestStateName == c("California","Texas","Illinois","Georgia","Florida"))
g2 <- data.frame(g1$OriginStateName,g1$DestStateName)
g3 <- g1 %>% group_by(OriginStateName) %>% 
  summarise(Mean = mean(DepDelay,na.rm=T)) %>% 
  arrange(desc(Mean))
#net <- graph.data.frame(g2,directed = F)

set.seed(222)
#plot(net,
    # vertex.size = 4,
    # edge.arrow.size = 0.1,
    # vertex.label.cex = 0.8,
    # layout = layout.kamada.kawai())

net<- graph.data.frame(g2,directed = F)
cnet <- cluster_edge_betweenness(net)
V(net)$degree <- g3$Mean
plot(cnet,net,
     vertex.size = V(net)$degree*5,
     vertex.label.cex = .8,
     layout = layout.kamada.kawai)

title(main = "Network of busiest states")

#Choose this particular network show the connections between the 5 busiest origin and destination states. The size of the vertices is proportional to the Average Departure Delay in those states. We can see that Florida has the largest mean departure delay and California has the least among the busiest airports also we saw that California is the busiest of all the states in term of incoming and outgoing flights, so their performance might be observed and monitored to see how they achieve a low average departure delay in mins

#-------------------------------------------------------------------------------------------------------------------------------------

#Viz-8
#Improvement in Geospatial plot

states <- ot %>% group_by(OriginStateName) %>% 
  summarise(Mean = mean(DepDelay,na.rm = TRUE))

highchart() %>% 
  hc_add_series_map(usgeojson, states,
                    name = "OriginStateName",
                    value = "Mean",
                    joinBy = c('woename', 'OriginStateName')) %>% #woename is where on earth name and joins
 hc_mapNavigation(enabled = T) %>% 
  hc_title(text = "US map with average departure delay in minutes")



