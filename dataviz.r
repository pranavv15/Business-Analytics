#Read data
data <- read.csv(file.choose(), header = TRUE)
library(dplyr)
data$State <- as.factor(data$State)
summary(data)

#Filter this data
car <- data %>% filter(State == "TX" | State == "CA")

#Arrange this new data in descending order

data %>% group_by(State) %>% mutate(CPH = lc/lh) %>% 
  summarise(Avg = mean(CPH)) %>% 
  arrange(desc(Avg))

#visualizations
library(ggplot2)      #grammar of graphics - gg

#Histogram - only for numeric data/ not for State
data %>% filter(State == "TX" | State == "CA") %>% 
  ggplot(aes(x = lc, fill = State)) + #connect two layers in ggplot with a '+' sign
  geom_histogram()  +     #we must add layers to ggplot. 1. was aesthetic, then is the histogram
  ggtitle("Labor Cost in CA and TX") +
  facet_wrap(~State)    #Have the histograms side by side instead of stacked

#Histogram is right skewed - most prices will be on the lower side but some will be expensive
# tells us where most of the observations are located, skewness of the variable

#Interactive Plot
library(highcharter)

# Box Plot
hcboxplot(data$Mileage, data$State, color = "red")

#Geo Plot
data1 <- data %>% group_by(State) %>% 
  summarise(count = n())
library(openintro)    #helps deal with the problem of acronym in state names
data1$State <- abbr2state(data1$State)

highchart() %>% 
  hc_add_series_map(usgeojson, data1,
                    name = "State",
                    value = "count",
                    joinBy = c('woename', 'State')) %>% #woename is where on earth name and joins
  hc_mapNavigation(enabled = T)

#Network Graph
library(igraph)
g <- graph(c(1,2,2,3,3,4,4,1),directed = F,
           n=8)
set.seed(555)
plot(g,
     vertex.color = "green",
     vertex.size = 30,
     edge.color = "red")

###########################################################################################

#Visualization with ggplot

library(ggExtra)

veh <- data %>% filter(State == "CA" | State == "TX" | State == "FL")
veh %>% ggplot(aes(x = lh, y =lc)) +
        geom_point() +
        facet_wrap(~State) +
        ggtitle("Labor Hours vs Labour Costs") +
        geom_smooth(method = 'lm',
                    col = 'red')  +
          scale_x_continuous("Labor Hours",
                             limits = c(0,40)) +
          scale_y_continuous("Labor Costs",
                              limits = c(0,3000)) 

# Bar Plot
data %>% ggplot(aes(x = State)) +
          geom_bar() +
          coord_flip()

veh %>% filter(fm ==1 | fm ==2) %>% 
        ggplot(aes(x = State, fill = State)) +
        geom_bar() +
        facet_grid(.~fm) +
        ggtitle("Failure in top 3 States in months 1 and 2")

#Pie Chart
veh %>% ggplot(aes(x = State, fill = State)) +
        geom_bar(width = 1) +
        coord_polar(theta = 'x')

#Histogram
veh %>% ggplot(aes(x = Mileage, fill = State)) +
        geom_histogram(binwidth = 5000,
                       color = "blue",
                       aplha = 0.8,
                       aes(y = ..density..)) +   #changes y scale to a density scale
        facet_wrap(~State)  +  #Creates different graphs, groups by states
        scale_fill_brewer(palette = "Set1") +   #creates a new set of colors
        geom_density(alpha = 0.2) # gives a density plot line

# Density plot

veh %>% ggplot(aes(x = Mileage, fill = State)) +
  geom_density(binwidth = 5000,
                 color = "blue",
                 aplha = 0.8,
                 aes(y = ..density..)) +   #changes y scale to a density scale
  facet_wrap(~State)  +  #Creates different graphs, groups by states
  scale_fill_brewer(palette = "Set1")   #creates a new set of colors

# Bi-variate plots

#Box Plot
veh %>% ggplot(aes(x = State, y = Mileage, fill = State)) +
        geom_boxplot() +
        scale_fill_manual(values = c('grey','grey','red'),
                            guide = F)

veh %>% filter(fm==1|fm==2) %>% 
        ggplot(aes(x = interaction(State,fm), y = Mileage, fill = State)) +
        geom_boxplot() 

#Violin plot - to be used when we want to compare many distributions

veh %>% ggplot(aes(x = State, y = Mileage, fill = State)) +
        geom_violin(adjust = 2) #Lower value of adjust introduces more variability 
# we can see that for CA failures occur much earlier in the mileage range
#whereas for other states, failures are more evenly spread out

#Dot Plot

veh_avg <- data %>% 
            group_by(State) %>% 
            summarise(avg = mean(Mileage),
                      CPH = sum(lc)/sum(lh))

veh_avg %>% ggplot(aes(x = avg, y = State)) +
            geom_point()

veh_avg %>% ggplot(aes(x = CPH, y = reorder(State,CPH))) +
              geom_point(color = "red") +
              scale_y_discrete('State')
#Scatter Plot
veh %>% ggplot(aes(x = fm, y = Mileage, color = State)) +
            geom_point() +
            stat_smooth(se = 0) + # Gives trendlines
            facet_wrap(~State)

# 2-D Density Plot 


# Polar Plot
veh %>% ggplot(aes(x = Mileage, fill = State)) +
        geom_histogram(binwidth = 5000,
                       color = "blue",
                       aplha = 0.8) +
        coord_polar() +
        facet_wrap(~State)

# Marginal Plot
p <- veh %>% ggplot(aes(x = lh, y =lc)) +
    geom_point() +
    ggtitle("Labor Hours vs Labour Costs") +
    geom_smooth(method = 'lm',
              col = 'red')  +
    scale_x_continuous("Labor Hours",
                     limits = c(0,40)) +
    scale_y_continuous("Labor Costs",
                     limits = c(0,3000)) 
ggMarginal(p, type = "histogram")  # p is the scatter plot which is to be plotted
                                    # type is the kind of plot that we want on the top and side

####################################################################################################

# Geospatial Visualization

library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(ggmap)
library(htmltools)
library(mapproj)
library(mapdata)

#World map with different countries
w <- map_data('world')
isf <- map_data('world', region = c("Italy","Spain","France"))
ggplot(isf,aes(x = long, y = lat, group = group, fill = region)) +
  geom_polygon(color = 'black') +
  coord_map('polyconic')

#Map of the US
s <- map_data('state')
ggplot(s,aes(x = long, y = lat, group = group, fill = region)) +
  geom_polygon(color = 'black') +
  coord_map('polyconic') +
  guides(fill = F)

#Map of the US with covid data
c <- read.csv(file.choose(), header = TRUE)
usa <- c %>% filter(country == "United States")
usa <- usa %>% group_by(province) %>% 
        summarise(count = n()) %>% 
        arrange(desc(count))


usa$province <- tolower(usa$province)  #make everything lower case
data <- merge(s, usa,
              by.x = "region",
              by.y = "province")     #merge data with data in s

ggplot(data,aes(x = long, y = lat,group = group, fill = count)) +
  geom_polygon(color = 'white') +
  coord_map('polyconic') +
  scale_fill_gradient2(low = 'yellow', high = 'red') +
  theme_void() +
  ggtitle("Covid cases in different states")

#US map with each county

usa <- c %>% filter(country == 'United States')
usa <- usa %>% group_by(city, province, longitude, latitude) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

mycolor <- colorNumeric('RdBu',
                        domain = c(1:1000),
                        reverse = TRUE)


usa %>% leaflet() %>% addProviderTiles("CartoDB") %>%
          addCircleMarkers(radius = ~0.01*count,
                           color = ~mycolor(count),
                           popup = ~paste0(city,
                                           "<br/>",
                                           count)) %>% 
          addLegend(pal = mycolor,
                    values = c(1:1000),
                    opacity = 0.75,
                    title = "Count",
                    position = "topleft")

############################################################################################################

#Network Visualization

library(igraph)
library(maps)
h <- map_data("us.cities")
g <- graph(c(1,2,2,1),
           directed = FALSE)
plot(g,
     vertex.color = "green",
     edge.color = "red")
g[]

g1<- graph(c("Amy","Ram","Ram","Li","Li","Amy","Amy","Li","Kate","Li"))
plot(g1,
     vertex.color = "green",
     vertex.size = 40,
     edge.color = "red")
g1

#Network Measures

degree(g1, mode = all) # number of connections/for incoming connections mode = "in"
diameter(g1, directed=F, weights = NA)
edge_density(g1, loops = F) #number of edges/number of all possible edges

ecount(g1) #edge count
vcount(g1) #vertex count

reciprocity(g1)
closeness(g1, mode = "all", weights = NA)
betweenness(g1, directed = TRUE, weights = NA)
edge_betweenness(g1,directed = T,weights = NA)

#Read data file

data <- read.csv(file.choose(), header = T)
y<- data.frame(data$first,data$second)

#Create Network

net <- graph.data.frame(y,directed = T)
V(net)
E(net)

V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

#Histogram of node degree
hist(V(net)$degree)

#Network Diagram
set.seed(222)
plot(net,
     vertex.color = "green",
     vertex.size = 4,
     edge.arrow.size = 0.1,
      vertex.label.cex = 0.8)

#Highlight Degrees and Layouts
plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)

#Hubs and Authorities
hs <- hub.score(net)$vector
as <- authority.score(net)$vector

par(mfrow = c(1,2))
set.seed(123)
plot(net,
     vertex.size = 30*hs,
     main = "Hubs",
     vertex.color = rainbow(52),
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)

plot(net,
     vertex.size = 30*as,
     main = "Authorities",
     vertex.color = rainbow(52),
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)
par(mfrow = c(1,1))

#Community Detection
net<- graph.data.frame(y,directed = F)
cnet <- cluster_edge_betweenness(net)
plot(cnet,net,
     vertex.size = 10,
     vertex.label.cex = .8)
