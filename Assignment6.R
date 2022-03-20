library(dplyr)
library(pROC)
library(ggplot2)
library(ggExtra)

# Read the data
data <- read.csv(file.choose(), header = F)

set.seed(222)

# Partitioning the data
