library(pROC)
library(glmnet)
library(ggplot2)
library(ggExtra)

# Read A6 Data into csv
data <- read.csv(file.choose(), header = F)
summary(data)
data$V5 <- as.factor(data$V5)
summary(data)
sd(data$V1)

data %>% ggplot(aes(x = V1, y = V5)) +
  geom_boxplot() +
  scale_fill_manual(values = c('grey','grey','red'),
                    guide = F)

data %>% ggplot(aes(x = V5, y = V3)) +
  geom_boxplot() +
  scale_fill_manual(values = c('grey','grey','red'),
                    guide = F)
hist(data$V3)
cor(data$V2, y=data$V3)
