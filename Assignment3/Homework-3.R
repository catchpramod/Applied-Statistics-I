library(boot)
# Q1

# a. With the whole data compare the two models (Use AIC and/or error rate)

library(ISLR)
data("Default",package="ISLR")
logistic.model1 <- glm(default ~ student + balance, data=Default, family = binomial())
logistic.model2 <- glm(default ~ balance, data=Default, family = binomial())
levs <- c("Yes","No")

predict.model1 <- predict(logistic.model1,type="response")
predicted1 <- factor(ifelse(predict.model1>=0.5,"Yes","No"), levels = levs)
table1 <- table(predicted1,True=Default$default)
err.rate1 <- sum(table1[1,1]+table1[2,2])/sum(table1) 

predict.model2 <- predict(logistic.model2,type="response")
predicted2 <- factor(ifelse(predict.model2>=0.5,"Yes","No"), levels = levs)
table2 <- table(predicted2,True=Default$default)
err.rate2 <- sum(table2[1,1]+table2[2,2])/sum(table2) 

#printing summary and error rates of the two models
summary(logistic.model1)
err.rate1

summary(logistic.model2)
err.rate2

# Comparing the AIC and error rates, model1 is better


# b. Use validation set approach and choose the best model. 
# Be aware that we have few people who defaulted in the data.

index <- sample(dim(Default)[1], size = dim(Default)[1]*0.5)
data.train <- Default[index,]
data.test <- Default[-index,]

logistic.model1 <- glm(default ~ student + balance, data=data.train, family = binomial())
logistic.model2 <- glm(default ~ balance, data=data.train, family = binomial())


predict.model1 <- predict(logistic.model1,data.test,type="response")
predicted1 <- factor(ifelse(predict.model1>=0.5,"Yes","No"), levels = levs)
table1 <- table(predicted1,True=data.test$default)
table1
err.rate1 <- sum(table1[1,1]+table1[2,2])/sum(table1) 
err.rate1

predict.model2 <- predict(logistic.model2, data.test,type="response")
predicted2 <- factor(ifelse(predict.model2>=0.5,"Yes","No"), levels = levs)
table2 <- table(predicted2,True=data.test$default)
err.rate2 <- sum(table2[1,1]+table2[2,2])/sum(table2) 
table2
err.rate2

summary(logistic.model1)
err.rate1

summary(logistic.model2)
err.rate2

# comparing with validation set approach, we see same error rate, but the AIC is less for model 1



# c. Use LOOCV approach and choose the best model

logistic.model1 <- glm(default ~ student + balance, data=Default, family = binomial())
logistic.model2 <- glm(default ~ balance, data=Default, family = binomial())

# LOOCV
LOOCV1 <- cv.glm(Default, logistic.model1)$delta[1]
LOOCV1

LOOCV2 <- cv.glm(Default, logistic.model2)$delta[1]
LOOCV2

# d. Use 10-fold cross-validation approach and choose the best model 
# K-fold CV
CV10_1 <- cv.glm(Default, logistic.model1, K= 10)$delta[1]
CV10_1

CV10_2 <- cv.glm(Default, logistic.model2, K= 10)$delta[1]
CV10_2



# Q2.
data("Smarket",package="ISLR")

index <- sample(dim(Default)[1], size = dim(Default)[1]*0.5)
data.train <- Smarket[Smarket$Year!='2005',]
data.test <- Smarket[Smarket$Year=='2005',]
logistic.model <- glm(Direction ~  Lag1 + Lag2 + Lag3 + Lag4 + Volume ,
                      data=data.train, family = binomial())
ulevs= c("Up","Down")
predict.model <- predict(logistic.model,data.test,type="response")
predicted <- factor(ifelse(predict.model>=0.5,"Up","Down"), levels = ulevs)
length(predicted)
length(data.test$Direction)
ttable <- table(predicted,True=data.test$Direction)
ttable
err.rate <- sum(ttable[1,1]+ttable[2,2])/sum(ttable) 

summary(logistic.model)
err.rate