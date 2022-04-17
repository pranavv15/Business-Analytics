library(dplyr)
library(pROC)

# Read data
data <- read.csv(file.choose(), header = T)
data <- data[,-c(1)]


# Regression

library(mlbench)
library(caret)
library(e1071)
library(lime)
library(corrplot)

# Descriptive data analysis

summary(data)
plot(data)

# Pair Plot of data with score
pairs(~score+V3+V4+V5+V6+V7, data=data)
pairs(~score+V8+V9+V10+V11+V12, data=data)
pairs(~score+V13+V14+V15+V16+V17, data=data)
pairs(~score+V18+V19+V20+V21+V22, data=data)
pairs(~score+V23+V24+V25+V26+V27+V28+V29, data=data)

# Correlation Plot of Data
cor_mat <- cor(data)
corrplot(cor_mat, method='color')

# It seems from the correleation plot that V3-V& are highly correleated and same for V8-V13
# This has the potential to skew our analysis

# 

# Medical Condition Data

# prepoc <- preProcess(data, method = c("center","scale"))

mydata <- as.data.frame((data))
mydata <- mydata[,c(1,2,3,7,13,14,15,16,17,18,21)]
set.seed(1234) 
ind <- sample(2, nrow(mydata), replace = T, prob = c(0.5, 0.5))
train <- mydata[ind == 1,]
test <- mydata[ind == 2,]

# y_train <- train$score
# x_train <- train[,-c(1)]
# x_train <- scale(x_train)

###############################################################################################

# Regression

# Simple Linear Regression

# Training
mod <- lm(score~., data = train)
summary(mod)

# It seems like only V3,V4, V8, V14,V15,V16,V17,V18,V19 and V22 are significant
# So keeping only those variable and running again


# Testing
test_reg <- predict(mod, test)
plot(test_reg ~ test$score, main = 'Predicted Vs Actual Score - Test data')
sqrt(mean((test$score - test_reg)^2))
cor(test$score, test_reg) ^2




# Bagging
set.seed(1234)
cvcontrol <- trainControl(method="repeatedcv", 
                          number = 5,
                          repeats = 2,
                          allowParallel=TRUE)
set.seed(1234)
bag <- train(score ~ ., 
             data=train,
             method="treebag",
             trControl=cvcontrol,
             importance=TRUE)
plot(varImp(bag))

# Plot, RMSE, R-square
ba <- predict(bag,  test)
plot(ba ~ test$score, main = 'Predicted Vs Actual Score - Test data')
sqrt(mean((test$score - ba)^2))
cor(test$score, ba) ^2

# RF
set.seed(1234)
forest <- train(score ~ ., 
                data=train,
                method="rf",
                trControl=cvcontrol,
                importance=TRUE)
plot(varImp(forest))

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

# Boosting
set.seed(1234)
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


# Test data from website
test_final <- read.csv(file.choose(), header = T)
scaled_new <- predict(prepoc,newdata = test_final)

# Predictions from boosted tree
# Plot, RMSE, R-square
bo <-  predict(boo,  test_final[,-c(1)])

submission <- data.frame(test_final$Id, bo)
colnames(submission) <- c('Id', 'Expected')

setwd("~/Desktop")
write.csv(submission, "submission.csv",row.names = F)
