data <- read.csv(file.choose(), header = T)

#Max sd of "ArrDelay" for Origin city name
maxsd <- data %>% group_by(OriginCityName) %>% 
  summarise(sd = sd(ArrDelay)) %>% 
  arrange(desc(sd))

#no of flights from Origin ND
nume <- data %>% filter(OriginCityName == "Dickinson, ND") %>% 
  summarise(count = n())

#highest mean ArrDelay
maxmean <- data %>% group_by(OriginCityName) %>% 
  summarise(mean = mean(ArrDelay)) %>% 
  arrange(desc(mean))

#Hawaii as destination
destH <- data %>% filter(DestStateName == "Hawaii") %>% 
  summarise(count = n())

#Dest Rhode Island max ArrDelay
destRH <- data %>% filter(DestStateName == "Rhode Island") %>% 
  summarise(max = max(ArrDelay))

#Max mean ArrDelay by dest state
maxmeanD <- data %>% group_by(DestStateName) %>% 
  summarise(mean = mean(ArrDelay)) %>% 
  arrange(desc(mean))

#try plot
library(ggplot2)
x <- data$Distance
y <- data$ArrDelay
data %>% ggplot(aes(x = (Distance), y = ArrDelay)) +
  geom_point()

#sd ArrDelay for montana
sdM <- data %>% filter(DestStateName == "Montana") %>% 
  summarise(sd = sd(ArrDelay))
