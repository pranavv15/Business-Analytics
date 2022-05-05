library(dplyr)
library(tidyverse)
library(DataExplorer)
library(purrr)
library(cluster)    # clustering algorithms
library(factoextra)
library(tibble)

data <- read.csv(file.choose(), header = T)
scores <- data$score
data <- data[,-c(2)]
data_scaled <- scale(data)

##########################################################################################

# PCA

# Run PCA, which performs singular-value decomposition
pc <- prcomp(data_scaled)
pc$rotation <- pc$rotation * -1 # Reflect loadings matrix for positive values
pc

# View variances
pc$sdev^2

# Plot the biplot
biplot(pc, cex=0.6, xlab="First Principal Component", ylab="Second Principal Component", panel.first = c(abline(h = 0, v = 0, col="lightgray", lty="dotted")))

# Scree plots
layout(matrix(1:2, ncol=2)) # Place next to each other
screeplot(pc, main="", col.lab="white") # Plot as bars
title(xlab="Principal Components", ylab="Variance")
axis(1, at=c(0,0.7,1.9,3.1,4.3), labels=c(0,1,2,3,4), pos=c(0,0))

screeplot(pc, type="lines", main="", col.lab="white") # Plot as lines
title(xlab="Principal Components", ylab="Variance")

# Calculate PVE
pve <- (pc$sdev^2) / sum(pc$sdev^2)
pve

# Plot PVE
plot(pve, type="b", pch=19, xlab="Principal Components", ylab="Proportion of Variance Explained (PVE)", xaxt="n", ylim=c(0,1))
axis(1, at=c(1,2,3,4), labels=c(1,2,3,4), pos=c(-0.05,1))


cum_pve <- numeric(length(pve))
cum_pve[1] <- pve[1]
for (i in 2:length(cum_pve)){
  cum_pve[i] <- pve[i] + cum_pve[i-1]
}
cum_pve

plot(cum_pve, type="b", pch=19, xlab="Combined Principal Components", ylab="Cumulative PVE", xaxt="n", ylim=c(0,1), main = "Proportion of Variance Explained")
axis(1, at=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19), labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19), pos=c(-0.05,1))

#######################################################################################3

# Random Forest using pc

# Using the new clusters are our new data
new_data <- pc[["x"]]
new_data <- new_data[,c(1,2,3,4,8,9,10,11,12,13,14,15,16,18,19,20,21)]
new_data <- new_data*100

# Let me try to run linear regression on this data
mydata <- as.data.frame((new_data))
set.seed(1234) 
ind <- sample(2, nrow(mydata), replace = T, prob = c(0.8, 0.2))
train <- mydata[ind == 1,]
scores_train <- scores[ind==1]
test <- (mydata[ind == 2,])
scores_test <- scores[ind==2]
# Scale training data
train2 <- as.data.frame((train))

# Simple Linear Regression
# Training
mod <- lm(scores_train~., data = train2)
summary(mod)

# Testing
test_reg <- predict(mod, test)
# *sd(train$score) + mean(train$score)
plot(test_reg ~ scores_test, main = 'Predicted Vs Actual Score - Test data')
sqrt(mean((scores_test - test_reg)^2))
cor(scores_test, test_reg) ^2

par(mfrow=c(1,1))

# Random Forest
rf.reg <- randomForest(scores_train~., data=train2, mtry=round(sqrt(ncol(train2) - 1)), importance=TRUE, xtest=test, ytest=scores_test)
importance(rf.reg)

rf.reg <- randomForest(scores_train~., data=train2, mtry=8, importance=TRUE, xtest=test, ytest=scores_test)

rf.train.mse <- min(rf.reg$mse)
rf.train.mse

rf.test.mse <- rf.reg$test$mse[which.min(rf.reg$mse)]
sqrt(rf.test.mse)

#########################################################################################

# Test data from website
test_final <- read.csv(file.choose(), header = T)
# Run PCA, which performs singular-value decomposition
data_scaled <- scale(test_final)
pc <- prcomp(data_scaled)
pc$rotation <- pc$rotation * -1 # Reflect loadings matrix for positive values
pc

test_2 <- pc[["x"]]
test_2 <- test_2[,c(1,2,3,4,8,9,10,11,12,13,14,15,16,18,19,20,21)]
test_2 <- as.data.frame(test_2*100)

rf.reg <- randomForest(scores_train~., data=train2, mtry=19, importance=TRUE, xtest=test_2)
rf.reg$predicted
submission <- data.frame(test_final$Id, rf.reg[["test"]][["predicted"]])
colnames(submission) <- c('Id', 'Expected')

setwd("~/Desktop")
write.csv(submission, "submission.csv",row.names = F)
