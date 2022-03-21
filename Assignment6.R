library(dplyr)
library(pROC)
library(ggplot2)
library(ggExtra)

# Read the data
data <- read.csv(file.choose(), header = F)
data$V5 <- as.factor(data$V5)

summary(data)         # 762 forged banknotes and 610 genuine banknotes

set.seed(222)

# Partitioning the data
indices <- sample(1:nrow(data), size = nrow(data)*0.5)
train <- data[indices,]
test <- data[-indices,]

# Data description
summary(train)      # 397 forged banknotes and 289 genuine banknotes
summary(test)       # 365 forged banknotes and 321 genuine banknotes

# Create model for 50:50 split
m1 <- glm(formula = V5 ~., data = train, family = 'binomial')
summary(m1)

# we can see that V4 is not a significant variable for predictions, so we drop it
m1.best <- glm(formula = V5 ~.-V4, data = train, family = 'binomial')
summary(m1.best)

# Final equation :  p = 7.935 - 7.791*V1 - 4.040*V2 - 5.278*V3

# Confusion matrices 
p1 <- predict(m1.best, train, type = 'response')
pred1 <- ifelse(p1>0.5,1,0)
cm1 <- table(pred1, train$V5)    # for training data
cm2 <- table(pred1, test$V5)     # for testing data

# Misclassification Error/Accuracy
errorTrain <- (395+288)/(395+288+2+1)         # 1%
errorTest <- (288+153)/(288+168+137+153)      # 41%

# Specificity, sensitivity and accuracy
Spec1 <- cm1[1]/(cm1[1]+cm1[3])
sens1 <- cm1[4]/(cm1[2]+cm1[4])

Spec2 <- cm2[1]/(cm2[1]+cm2[3])
sens2 <- cm2[4]/(cm2[2]+cm2[4])

# ROC Curve
r <- multiclass.roc(train$V5, p1, percent = TRUE)
roc <- r[['rocs']]
r1 <- roc[[1]]
plot.roc(r1)

r <- multiclass.roc(test$V5, p1, percent = TRUE)
roc <- r[['rocs']]
r1 <- roc[[1]]
plot.roc(r1)

##########################################################################################################
