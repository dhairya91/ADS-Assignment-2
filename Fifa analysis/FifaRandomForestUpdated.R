
#### LIBRARIES #################
##library(readr)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)

#### LOAD FILES ################
d1 <- read.csv("C:\\Users\\dj_ch\\Desktop\\data science engg methods\\mid-term\\Cleaned data\\FIFA18_19WithMaxMinNormalization.csv", 1)
d1
str(d1)
### Splitting the data###
set.seed(50)
#
sample <- sample.int(n = nrow(d1), size =10000, replace = F)
sample
#training set
train <- d1[sample,]
train
#validation set
test <- d1[-sample,]
test
###Making a NORMAL DECISION TREE ###

tree <- rpart(y ~., method ="anova",
             data = train, control = rpart.control(minsplit = 4, cp = 0.001))
printcp(tree) # display the results 
plotcp(tree) # visualize cross-validation results 
summary(tree) # detailed summary of splits

### Testing Our Prediction###
predtree <- predict(tree, newdata = test)
predtree
summary(predtree)
###rmse.rf <- sqrt(sum(((pred2) - test2$y)^2)/
###length(test2$y))
###c(RMSE = rmse.rf, R2 = mean(model2$rsq))

rmse.tree <- sqrt(sum(((predtree) - test$y)^2)/length(test$y))
rmse.tree
###1791.488###
#Model accuracy - 94.49% Accurate 




####Fitting random forest###
####Case 1######

### Fitting Model1 
model1 <- randomForest(y ~ .- Club - Nationality ,data = train)
model1
summary(model1)
importance(model1)
varImpPlot(model1)
plot(model)
pred <- predict(model1, newdata = test)
pred

rmse.rf <- sqrt(sum(((pred) - test$y)^2)/
                  length(test$y))
c(RMSE = rmse.rf, R2 = mean(model1$rsq))
#    RMSE           R2 
##1555.2954002    0.9435494 

####CASE 2#####
###Fitting second model#####
d2 <- read.csv("C:\\Users\\dj_ch\\Desktop\\data science engg methods\\mid-term\\Cleaned data\\FIFA18_19WithMaxMinNormalization.csv", 1)
d2
##Dropping columns club and nationality as they have factors with more than 53 lvs which is a limitation for our model.
d2$Nationality <- NULL
d2$Club <- NULL
str(d2)
### Splitting the data###
sample2 <- sample.int(n = nrow(d2), size =10000, replace = F)
sample2
train2 <- d2[sample2,]
train2
#validation set
test2 <- d2[-sample2,]
test2
### Fitting our model withe best mtry###
model2 <- randomForest(y ~ .,data = train2, mtry = 9, ntree = 400 )
model2
summary(model2)
importance(model2)
varImpPlot(model2)
plot(model2)
### Testing Our Prediction###
pred2 <- predict(model2, newdata = test2)
pred2

rmse.rf <- sqrt(sum(((pred2) - test2$y)^2)/
                  length(test2$y))
c(RMSE = rmse.rf, R2 = mean(model2$rsq))
##RMSE           R2 
##1807.5452553    0.9305833  

### BEST M-TRY
bestmtry <- tuneRF(train2, train2$y, ntreeTry = 400, stepFactor = 1.5, improve = 0.1, trace = T, plot = T )
bestmtry
#####   mtry OOBError
####    9     9 942300.7
####   13   13 553678.7
####   19   19 324627.6
####   28   28 222462.1
####   39   39 213251.5

## CASE 3

model3 <- randomForest(y ~ .,data = train2, mtry = 28, ntree = 500 )
model3
summary(model3)
importance(model3)
varImpPlot(model3)
plot(model3)

pred3 <- predict(model3, newdata = test2)
pred3

rmse.rf1 <- sqrt(sum(((pred3) - test2$y)^2)/
                  length(test2$y))
c(RMSE = rmse.rf, R2 = mean(model3$rsq))

###     RMSE           R2 
### 1385.1694216    0.9722564 
### This is a very good and accurate result but it has the least oob rate (22%) so we should select this model or try tuning it further.


##### Considering all the results i would say that case 3 is the best fit for our model as it has the least rmse and oob error and 97% accurate.