library(caret)
library(e1071)
library(lime)
library(DAAG)
library(party)
library(rpart)
library(rpart.plot)
library(mlbench)
library(pROC)
library(tree)
library(nnet)
library(dplyr)

# Classification 
mydata <- read.csv(file.choose(), header = T)
str(mydata)
table(mydata$MatchO)

d <- read.csv(file.choose(), header = T)
# FGS has been read as numerical instead of Factor which it is
mydata$FGS0 <- as.factor(mydata$FGS0)
mydata$FGS1 <- as.factor(mydata$FGS1)
mydata$MatchO <- as.factor(mydata$MatchO)

str(mydata)
table(mydata$MatchO)

set.seed(464) 
ind <- sample(2, nrow(mydata), replace = T, prob = c(0.5, 0.5))
train <- mydata[ind == 1,]
test <- mydata[ind == 2,]

# No of each category in the train and test set
table(train$MatchO)
table(test$MatchO)

#-------------------------------------------------------------------------------------------------

# Multinomial  

# Model
m <- multinom(MatchO~ . , data = train)
summary(m)

#2-tailed z test
z <- summary(m)$coefficients/summary(m)$standard.errors
(1 - pnorm(abs(z), 0, 1)) * 2

# Predict
p <- predict(m, train, type = 'class')
confusionMatrix(p, train$MatchO)

p <- predict(m, test, type = 'class')
confusionMatrix(p, test$MatchO)

#-------------------------------------------------------------------------------------------------

#  Classification Tree 

# Tree
tree <- rpart(MatchO ~., data = train)
rpart.plot(tree)
printcp(tree)
plotcp(tree)

# Confusion matrix -train
p <- predict(tree, train, type = 'class')
confusionMatrix(p, train$MatchO)

# Confusion matrix -test
p <- predict(tree, test, type = 'class')
confusionMatrix(p, test$MatchO)


# ROC
p1 <- predict(tree, test, type = 'prob')
p1 <- p1[,2]
r <- multiclass.roc(test$MatchO, p1, percent = TRUE)
roc <- r[['rocs']]
r1 <- roc[[1]]
plot.roc(r1,
         print.auc=TRUE, 
         auc.polygon=TRUE, 
         grid=c(0.1, 0.2),
         grid.col=c("green", "red"), 
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue", 
         print.thres=TRUE, 
         main= 'ROC Curve')

#-------------------------------------------------------------------------------------------------

# RF
set.seed(464) 
cvcontrol <- trainControl(method="repeatedcv", 
                          number = 5,
                          repeats = 1,
                          allowParallel=TRUE)
set.seed(464)
forest <- train(MatchO ~ . , 
                data=train,
                method="rf",
                trControl=cvcontrol,
                importance=TRUE)
plot(varImp(forest),
     main = "Variable Importance Plot for RF")

# Testing confusion matrix
p <- predict(forest, test, type = 'raw')
confusionMatrix(p, test$MatchO)

# Training confusion matrix
p <- predict(forest, train, type = 'raw')
confusionMatrix(p, train$MatchO)

#-------------------------------------------------------------------------------------------------

# Boosting
set.seed(464)
boo <- train(MatchO ~ ., 
             data=train,
             method="xgbTree",   
             trControl=cvcontrol,
             tuneGrid = expand.grid(nrounds = 1500,
                                    max_depth = 5,
                                    eta = 0.20,
                                    gamma = 1.85,
                                    colsample_bytree = 1,
                                    min_child_weight = 1,
                                    subsample = 1))
plot(varImp(boo),
     main = "Variable Importance Plot for XGB")

p <- predict(boo, train, type = 'raw')
confusionMatrix(p, train$MatchO)

p <- predict(boo, test, type = 'raw')
confusionMatrix(p, test$MatchO)


