library(dplyr)
library(corrplot)
library(ggplot2)
library(Amelia)
library(randomForest)
library(pROC)

library(mlbench)
library(caret)
library(e1071)
library(lime)
library(corrplot)
library(neuralnet)
par(mfrow = c(1,1))
# Reading the datax
data <- read.csv(file.choose(), header = T)
data <- data[,-c(1)]
summary(data)

str(data)
# Missing map of data
missmap(data)         # So not missing any data

# Histogram of scores
hist(data$score)

# Box plot of variables
data %>% ggplot(aes(x=V16, y=score,))+
  geom_boxplot()


# Cor plot
cor_matrix <- cor(data)
corrplot(cor_matrix, method = 'color')

# There is a strong correlation between variables V3-V15
# Let me try droppping all but one of the strongle correlated variables
# So i will keep V3 and V8 and drop the rest until V15

new <- data
#[,-c(3,4,5,6,7,8,9,10,11,12,13,14)]
# New cor plot
cor_mat2 <- cor(new)
corrplot(cor_mat2,method = 'color')

par(mfrow = c(3,3))
hist(data$V3)
hist(data$V4)
hist(data$V5)
hist(data$V6)
hist(data$V7)
hist(data$V8)
hist(data$V9)
hist(data$V10)
hist(data$V11)

################################################################################################

# Let me try to run linear regression on this data
mydata <- as.data.frame((new))
set.seed(1234) 
ind <- sample(2, nrow(mydata), replace = T, prob = c(0.8, 0.2))
train <- mydata[ind == 1,]
mean(train$score)
test <- (mydata[ind == 2,])
mean(test$score)
# Scale training data
train2 <- as.data.frame((train))

# Simple Linear Regression
# Training
mod <- lm(score~., data = train2)
summary(mod)

# Testing
test_reg <- predict(mod, test)
# *sd(train$score) + mean(train$score)
plot(test_reg ~ test$score, main = 'Predicted Vs Actual Score - Test data')
sqrt(mean((test$score - test_reg)^2))
cor(test$score, test_reg) ^2

###############################################################################################

# Boosting
set.seed(1234)
train2 <- scale(train)
cvcontrol <- trainControl(method="repeatedcv", 
                          number = 5,
                          repeats = 2,
                          allowParallel=TRUE)
boo <- train(score ~ ., 
             data=train2,
             method="xgbTree", 
             trControl=cvcontrol,
             tuneGrid = expand.grid(nrounds = 5000,
                                    max_depth = 7,
                                    eta = 0.15,
                                    gamma = 2.1,
                                    colsample_bytree = 1,
                                    min_child_weight = 1,
                                    subsample = 1))
plot(varImp(boo))

# Plot, RMSE, R-square
bo <-  predict(boo,  test)*sd(test$score) + mean((test$score))
plot(bo ~ test$score, main = 'Predicted Vs Actual MEDV - Test data')
sqrt(mean((test$score - bo)^2))
cor(test$score, bo) ^2

##################################################################################################

# Random Forest
rf.reg <- randomForest(score~V12+V14+V15+V17+V18, data=train2, mtry=round(sqrt(ncol(train2) - 1)), importance=TRUE, xtest=test[,c(12,14,15,17,18)], ytest=test$score)
importance(rf.reg)

varImpPlot(rf.reg, cex=0.5)
plot(rf.reg)

# Test different out of bag error
p <- ncol(train) - 1
oob.error <- double(p) #initialize empty vector
test.error <- double(p)

for(m in 1:p) {
  fit <- randomForest(score ~ ., data=train, mtry=m, ntree=225)
  oob.error[m] <- fit$mse[225]
  test.error[m] <- mean((test$score - predict(fit, newdata=test))^2)
}	

matplot(1:p, cbind(oob.error, test.error), pch=19, col=c("red", "blue"), type="b", ylab="Mean Squared Error" , xlab = "Number of Features")
legend("bottomright", c('OOB', 'Test') ,col=seq_len(2),cex=0.8,fill=c("red",  "blue"))

rf.reg <- randomForest(score~., data=train, mtry=8, importance=TRUE, xtest=test[,-c(1)], ytest=test$score)

rf.train.mse <- min(rf.reg$mse)
rf.train.mse

rf.test.mse <- rf.reg$test$mse[which.min(rf.reg$mse)]
sqrt(rf.test.mse)

# # Plot, RMSE, R-square
# rf <-  predict(rf.reg,  test)
# plot(rf ~ test$score, main = 'Predicted Vs Actual MEDV - Test data')
# sqrt(mean((test$score - rf)^2))
# cor(test$score, rf) ^2

# Plot, RMSE, R-square
rf <-  predict(forest,  test)
plot(rf ~ test$score, main = 'Predicted Vs Actual MEDV - Test data')
sqrt(mean((test$score - rf)^2))
cor(test$score, rf) ^2

# Explain predictions
explainer <- lime(test[1:3,], forest, n_bins = 5)
explanation <- explain( x = test[1:3,], 
                        explainer = explainer, 
                        n_features = 5)
plot_features(explanation)

###############################################################################################
# Proper Boosting
library(gbm)
cor <- cor(train)
corrplot(cor)
reg.boost = gbm(score ~ V12+V14+V15+V17+V18, data = train, n.trees = 50000, distribution = "gaussian", shrinkage = 0.001)
summary(reg.boost)

train.mse <- min(reg.boost$train.error)
train.mse

test.mse <- mean((test$score - predict(reg.boost, test[,-1]))^2)
test.mse

################################################################################################
# Neural Network

maxs <- apply(train, 2, max) 
mins <- apply(train, 2, min)
train <- as.data.frame(scale(train, center = mins, 
                              scale = maxs - mins))

maxs <- apply(test, 2, max) 
mins <- apply(test, 2, min)
test <- as.data.frame(scale(test, center = mins, 
                              scale = maxs - mins))

nn <- neuralnet(score~V12+V14+V15+V17+V18, data=train[,c(2,12,14,15,17,18)], hidden = c(5,3),
                linear.output = TRUE,
                lifesign = "minimal",
                rep = 3)
plot(nn)

output <- compute(nn, test[,c(12,14,15,17,18)])

# Compute mean squared error
pr.nn_ <- output$net.result * (max(mydata$score) - min(mydata$score)) + min(mydata$score)
test.r <- (test$score) * (max(mydata$score) - min(mydata$score)) + min(mydata$score)
MSE.nn <- sum((test.r - pr.nn_)^2) / nrow(test)

plot(test$score, pr.nn_, col = "red", main = 'Real vs Predicted')
abline(0, 1, lwd = 2)
###############################################################################################
# Test data from website
test_final <- read.csv(file.choose(), header = T)
corr <- cor(test_final)
corrplot(corr, method = 'color')
#test_final <- test_final[,-c(3,4,5,6,8,9,10,11,12)]
# Predictions from boosted tree
# Plot, RMSE, R-square
# bo <-  predict(boo,  test_final[,-c(1)])
rf.reg <- randomForest(score~., data=train, mtry=19, importance=TRUE, xtest=test_final[,-c(1)])
rf.reg$predicted
submission <- data.frame(test_final$Id, rf.reg[["test"]][["predicted"]])
colnames(submission) <- c('Id', 'Expected')

setwd("~/Desktop")
write.csv(submission, "submission.csv",row.names = F)
