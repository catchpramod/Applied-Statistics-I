library(ggplot2)
library("gee")
library(lme4)
library("multcomp")
library(geepack)
library(plyr)
library("MESS")
library(gamlss.data)
library(lattice)
library("quantreg")
library("TH.data")
library(rpart)
library("HSAUR3")


# Q 1. Investigate the use of other correlational structures than the independence and exchangeable structures 
# for the epilepsy data. Compare the results using QIC - which model is better?

data("epilepsy", package = "HSAUR3")
dim(epilepsy)
head(epilepsy, n = 15)

per <- rep(log(2),nrow(epilepsy))
epilepsy$period <- as.numeric(epilepsy$period)
names(epilepsy)[names(epilepsy) == "treatment"] <- "trt"
fm <- seizure.rate ~ base + age + trt + offset(per)

epl.gee.unst <- geeglm(fm, data = epilepsy, id = subject,family = "poisson", corstr = "unstructured")
epl.gee.ar1 <- geeglm(fm, data = epilepsy, id = subject,family = "poisson", corstr = "ar1")

summary(epl.gee.unst)
summary(epl.gee.ar1)

QIC(epl.gee.unst)["QIC"]
QIC(epl.gee.ar1)["QIC"]


# Q 2. Consider the clouds data from the HSAUR3 package
data(clouds)
head(clouds)

# a. Review the linear model fitted to this data in Chapter 6 of the text book and report the model and findings

clouds.lm <- lm(rainfall ~ seeding + seeding:(sne + cloudcover + prewetness + echomotion) + time, 
                data = clouds)
clouds.lm.pred <- predict(clouds.lm, newdata = clouds)
clouds.lm.MSE <- mean((clouds$rainfall - clouds.lm.pred)^2)

summary(clouds.lm)

xlim <- range(clouds$rainfall)
plot(clouds.lm.pred ~ clouds$rainfall, data = clouds, xlab = "Observed",
     ylab = "Predicted", ylim = xlim, xlim = xlim, main = "Linear model")
abline(a = 0, b = 1)

# b. Fit a median regression model
cloud.qn.median <- rq(rainfall ~ seeding + seeding:(sne + cloudcover + prewetness + echomotion) + time, 
                      data = clouds, tau = 0.5)
cloud.qn.pred <- predict(cloud.qn.median, newdata = clouds)
cloud.qn.MSE <- mean((clouds$rainfall - cloud.qn.pred)^2)

summary(cloud.qn.median)

xlim <- range(clouds$rainfall)
plot(cloud.qn.pred ~ clouds$rainfall, data = clouds, xlab = "Observed",
     ylab = "Predicted", ylim = xlim, xlim = xlim, main  = "Quantile median regression")
abline(a = 0, b = 1)

# c. Compare the two results.

clouds.lm.MSE

cloud.qn.MSE


# Q 3. Reanalyze the bodyfat data from the TH.data package. Compare the regression tree approach from chapter 9 of the 
# textbook to median regression and summarize the different findings.



data("bodyfat")

head(bodyfat)
bodyfat.rpart <- rpart(DEXfat ~ age + waistcirc + hipcirc +elbowbreadth + kneebreadth, data = bodyfat,
                       control = rpart.control(minsplit = 10))

opt <- which.min(bodyfat.rpart$cptable[,"xerror"])

cp <- bodyfat.rpart$cptable[opt, "CP"]
bodyfat.prune <- prune(bodyfat.rpart, cp = cp)

DEXfat.pred <- predict(bodyfat.prune, newdata = bodyfat)
MSE.reg.tree <- mean((bodyfat$DEXfat - DEXfat.pred)^2)

xlim <- range(bodyfat$DEXfat)
plot(DEXfat.pred ~ DEXfat, data = bodyfat, xlab = "Observed",ylab = "Predicted", ylim = xlim, xlim = xlim)
abline(a = 0, b = 1)

DEXfat.median <- rq(DEXfat ~ age + waistcirc + hipcirc +elbowbreadth + kneebreadth,data = bodyfat,tau = 0.5)
DExFat.median.pred <- predict(DEXfat.median, newdata = bodyfat)

xlim <- range(bodyfat$DEXfat)
plot(DExFat.median.pred ~ DEXfat, data = bodyfat, xlab = "Observed",ylab = "Predicted", ylim = xlim, xlim = xlim)
abline(a = 0, b = 1)
MSE.median <- mean((bodyfat$DEXfat - DExFat.median.pred)^2)

summary(bodyfat.prune)
summary(DEXfat.median)

MSE.reg.tree
MSE.median


# Q 4. Consider db data from the lecture notes (package gamlss.data). 
# Refit the additive quantile regression models presented (rqssmod) with varying values of λ in qss. 
# How do the estimated quantile curves change?

data(db)
head(db)
dim(db)
plot(db$head ~ db$age, xlab = "Age(months)", ylab = "Head circumference",
     pch = 16, cex = 0.5, col = "gray")

db$cut <- cut(db$age, breaks = c(2, 9, 23),
              labels = c("2-9 yrs", "9-23 yrs"))
tau <- c(.01, .1, .25, .5, .75, .9, .99)
gage <- c(2:9, 9:23)
i <- 1:8



rqssmod <- vector(mode = "list", length = length(tau))
db$lage <- with(db, age^(1/3))


pfun <- function(x, y, ...) {
  panel.xyplot(x = x, y = y, ...)
  apply(p, 2, function(x) panel.lines(gage, x))
  panel.text(rep(max(db$age), length(tau)),
             p[nrow(p),], label = tau, cex = 0.9)
  panel.text(rep(min(db$age), length(tau)),
             p[1,], label = tau, cex = 0.9)
}



#lamda =  0.01
lmd <- 0.01

for (i in 1:length(tau))
  rqssmod[[i]] <- rqss(head ~ qss(lage, lambda = lmd),
                       data = db, tau = tau[i])

gage <- seq(from = min(db$age), to = max(db$age), length = 50)
p <- sapply(1:length(tau), function(i) { predict(rqssmod[[i]],
                                                 newdata = data.frame(lage = gage^(1/3)))
})



xyplot(head ~ age | cut, data = db, xlab = "Age (years)",
       ylab = "Head circumference (cm)", pch = 19,
       scales = list(x = list(relation = "free")),
       layout = c(2, 1), col = rgb(.1, .1, .1, .1),
       panel = pfun)

  

#lamda =  0.5
lmd <- 0.5

for (i in 1:length(tau))
  rqssmod[[i]] <- rqss(head ~ qss(lage, lambda = lmd),
                       data = db, tau = tau[i])

gage <- seq(from = min(db$age), to = max(db$age), length = 50)
p <- sapply(1:length(tau), function(i) { predict(rqssmod[[i]],
                                                 newdata = data.frame(lage = gage^(1/3)))
})



xyplot(head ~ age | cut, data = db, xlab = "Age (years)",
       ylab = "Head circumference (cm)", pch = 19,
       scales = list(x = list(relation = "free")),
       layout = c(2, 1), col = rgb(.1, .1, .1, .1),
       panel = pfun)


#lamda =  1

lmd <- 1

for (i in 1:length(tau))
  rqssmod[[i]] <- rqss(head ~ qss(lage, lambda = lmd),
                       data = db, tau = tau[i])

gage <- seq(from = min(db$age), to = max(db$age), length = 50)
p <- sapply(1:length(tau), function(i) { predict(rqssmod[[i]],
                                                 newdata = data.frame(lage = gage^(1/3)))
})


xyplot(head ~ age | cut, data = db, xlab = "Age (years)",
       ylab = "Head circumference (cm)", pch = 19,
       scales = list(x = list(relation = "free")),
       layout = c(2, 1), col = rgb(.1, .1, .1, .1),
       panel = pfun)

