data <- read.csv(file.choose(), header = T)

#Minimum number of games
mini <- data %>% group_by(day_of_week) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

#Rockies as opponents
rock <- data %>% group_by(opponent) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

#Average attendence for Rockies
att <- data %>% filter(opponent == "Rockies") %>% 
  summarise(Average = mean(attend))

#Average attendance for games
attend <- data %>% group_by(opponent) %>% 
  summarise(Avg = mean(attend)) %>% 
  arrange(desc(Avg))

#SD of attendance
attendsd <- sd(data$attend)

#SD of attend when opponents are Pirates
pir_sd <- data %>% filter(opponent=="Pirates")
sdx <- sd(pir_sd$attend)

#Total average attendance
a <- mean(data$attend)

#Cloudy skies
cloud <- data %>% group_by(skies) %>% 
  summarise(count = n())
  
#dat/night
d_n <- data %>% group_by(day_night) %>% 
  summarise(count = n())
