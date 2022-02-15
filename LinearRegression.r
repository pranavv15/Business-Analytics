data2 <- read.csv("/Users/pranavvinod/Downloads/zbp00totals.csv", header = TRUE)
data2
summary(data2[4:7])
pairs(data2[4:7])

#finding out rows to ignore
data <- data2[!(data2$EMPFLAG=="A"|data2$EMPFLAG=="B"|data2$EMPFLAG=="C"|data2$EMPFLAG=="D"|data2$EMPFLAG=="E"|data2$EMPFLAG=="F"|data2$EMPFLAG=="G"|data2$EMPFLAG=="H"|data2$EMPFLAG=="I"|data2$EMPFLAG=="J"|data2$EMPFLAG=="K"|data2$EMPFLAG=="L"|data2$EMPFLAG=="M"),]


#partition data into training and testing
set.seed(10)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.6,0.4))
training_data <- data[ind==1,]
testing_data <- data[ind==2,]
head(training_data,5)
head(testing_data)

#Summary of data
summary(training_data[4:7])
colMeans(training_data[4:7])
sd(training_data$EMP)
sd(training_data$QP1)
sd(training_data$AP)
sd(training_data$EST)

#quantile for Total Annual Payroll
quantile(training_data$AP, probs = 0.95)

#correleations among variables
cor(training_data[4:7])
pairs(training_data[4:7])

#linear regression on training
model <- lm(AP~QP1,data = training_data)
model

summary(model)

#linear regression on testing
model2 <- lm(AP~QP1,data = testing_data)
model2
summary(model2)x

#plotting 
plot(AP~QP1, training_data)
abline(model)
plot(AP~QP1, testing_data)
abline(model2)
