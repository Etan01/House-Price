
#Import library
library(ggplot2)
library(dplyr)
library(lattice)
library(reshape2)
library(tidyverse)
library(corrplot)
library(igraph)
library(igraphdata)

#Read files
training = read.csv("train.csv")
test = read.csv("test.csv")
sum(is.na(training))
sum(is.na(test))

#Check NA values
colnames(is.na(training)==TRUE)
#Cant omit NA as every row has NA values
#training = na.omit(training)

#Observation
head(training)
training %>% count() #1460 rows
summary(training$SalePrice)
  
#Pre-processing: Clean data
training %>% group_by(Electrical) %>% count()
training$Electrical[is.na(training$Electrical)] <-'SBrkr'

str(training$TotalBsmtSF[is.na(training$TotalBsmtSF) == TRUE])
training %>% group_by(Alley) %>% count()
training %>% group_by(PoolQC) %>% count()
training %>% group_by(Fence) %>% count()
training %>% group_by(MiscFeature) %>% count()

#Drop columns which NA values more than 50%
training$Alley <- NULL
training$Id <- NULL
training$'PoolQC'<- NULL
training$'Fence'<- NULL
training$'MiscFeature'<- NULL

summary(training)
str(training)
training[, c(64)]

numeric <- training[, c(1, 3:4, 16:17, 25, 33, 35:37, 42:51, 53, 55, 60:61, 65:72, 76)]
summary(numeric)

numeric %>% group_by(LotFrontage) %>% count() %>% arrange(desc(n))
numeric$LotFrontage[is.na(numeric$LotFrontage)] <- 0

numeric$TotalBsmtSF[is.na(numeric$TotalBsmtSF)] <- 0

numeric$GrLivArea[is.na(numeric$GrLivArea)] <- 0
numeric$LotFrontage[is.na(numeric$GrLivArea)] <- 0
numeric$MasVnrArea[is.na(numeric$MasVnrArea)] <- 0
numeric$GarageYrBlt[is.na(numeric$GarageYrBlt)] <- 0

summary(numeric)

#Perform normalisation
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

numeric <- as.data.frame(lapply(numeric, min_max_norm))

boxplot(numeric)

M=cor(numeric)
corrplot(M, method = 'color')

str(numeric)
ggplot(numeric, aes(X1stFlrSF, TotalBsmtSF)) + geom_point()

#Build linear regression
regression <- lm(SalePrice~., data=numeric)
summary(regression)
par(mfrow=c(2,2))
plot(regression)

#DElete NA
regression <- lm(SalePrice~.-TotalBsmtSF-GrLivArea, data=numeric)
summary(regression)
par(mfrow=c(2,2))
plot(regression)


#Build linear regression using important variables
regression1 <- lm(SalePrice~MSSubClass+LotArea+OverallQual+OverallCond+MasVnrArea
                  +X1stFlrSF+X2ndFlrSF+BsmtFullBath+BedroomAbvGr+KitchenAbvGr+
                    TotRmsAbvGrd+WoodDeckSF+GarageCars, data=numeric)
summary(regression1)
par(mfrow=c(2,2))
plot(regression1)

#Predict saleprice using testing data with the regression1
predicted <- test[1]
predicted$SalePrice<- predict(regression1, test)
predicted$SalePrice <- predicted$SalePrice*100


predicted$SalePrice[is.na(predicted$SalePrice)] <- 0
summary(predicted$SalePrice)

res <- residuals(regression1)
res<-as.data.frame(res)
res
summary(res)

write.csv(predicted, "/Users/tanengteck/Downloads/House-Price-main/Predicted.csv", row.names=FALSE)


