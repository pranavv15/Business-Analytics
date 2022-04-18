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
par(mfrow = c(1,1))
# Reading the data
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

new <- data[,-c(3,4,5,6,8,9,10,11,12)]
# New cor plot
cor_mat2 <- cor(new)
corrplot(cor_mat2,method = 'color')

# Let me try to run linear regression on this data
library(mlbench)
library(caret)
library(e1071)
library(lime)

mydata <- as.data.frame((new))
set.seed(1234) 
ind <- sample(2, nrow(mydata), replace = T, prob = c(0.8, 0.2))
train <- mydata[ind == 1,]
test <- (mydata[ind == 2,])

# Scale training data
train2 <- as.data.frame(scale(train))

# Simple Linear Regression

# Training
mod <- lm(score~., data = train2)
summary(mod)

# Testing
test_reg <- predict(mod, test)*sd(train2$score) + mean(train2$score)
plot(test_reg ~ test$score, main = 'Predicted Vs Actual Score - Test data')
sqrt(mean((test$score - test_reg)^2))
cor(test$score, test_reg) ^2

###############################################################################################

# Boosting
set.seed(1234)
cvcontrol <- trainControl(method="repeatedcv", 
                          number = 5,
                          repeats = 2,
                          allowParallel=TRUE)
boo <- train(score ~ ., 
             data=train,
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
bo <-  predict(boo,  test)
plot(bo ~ test$score, main = 'Predicted Vs Actual MEDV - Test data')
sqrt(mean((test$score - bo)^2))
cor(test$score, bo) ^2

##################################################################################################

# Random Forest
rf.reg <- randomForest(score~., data=train, mtry=round(sqrt(ncol(train) - 1)), importance=TRUE, xtest=test[,-c(1)], ytest=test$score)
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

rf.reg <- randomForest(score~., data=train, mtry=19, importance=TRUE, xtest=test[,-c(1)], ytest=test$score)

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


# Test data from website
test_final <- read.csv(file.choose(), header = T)
test_final <- test_final[,-c(3,4,5,6,8,9,10,11,12)]
# Predictions from boosted tree
# Plot, RMSE, R-square
bo <-  predict(boo,  test_final[,-c(1)])

submission <- data.frame(test_final$Id, bo)
colnames(submission) <- c('Id', 'Expected')

setwd("~/Desktop")
write.csv(submission, "submission.csv",row.names = F)
