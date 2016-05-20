# Q1

# Handbook 2.1

library(ggplot2)
library(gridExtra)
data("household",package="HSAUR2")
hh.data <- household
hh.data$total <- hh.data$housing + hh.data$food + hh.data$goods + hh.data$service
attach(hh.data)
# par(mfrow=c(2,2))
plot.1 <- ggplot(hh.data, aes(x=total, y=housing, colour=gender)) + geom_point()
plot.2 <- ggplot(hh.data, aes(x=total, y=food, colour=gender)) + geom_point()
plot.3 <- ggplot(hh.data, aes(x=total, y=goods, colour=gender)) + geom_point()
plot.4 <- ggplot(hh.data, aes(x=total, y=service, colour=gender)) + geom_point()
grid.arrange(plot.1, plot.2, plot.3, plot.4)


# Handbook 2.3

# 1)
library(ggplot2)
library(gridExtra)
data("USstates",package="HSAUR2")


pairs(USstates[,1:7], 
      panel =  function(x,y) {
        points(x,y,cex=0.7, col='Red')
        text(x,y, labels=state.abb[match(row.names(USstates),state.name)], cex = 0.8)
      })


# 2)
ggplot(USstates, aes(x=Life.Expectancy, y = Homicide, color = Income)) + geom_point(size = 5)



# Q2

data("plasma",package="HSAUR2")
attach(plasma)
plasma
summary(plasma)

ggplot(plasma, aes(x=fibrinogen, y=globulin, colour=ESR)) + geom_point()




# Q3

data("bladdercancer",package="HSAUR3")
attach(bladdercancer)

bc.poisson.model <-glm(formula = number ~ time + tumorsize, family = poisson(),data = bladdercancer)
summary(bc.poisson.model)
exp(coef(bc.poisson.model)["tumorsize>3cm"])


# Q4

# a

library(ggplot2)
library("vcd")
library("ISLR")
data("Default",package="ISLR")
attach(Default)
summary(Default)

ggplot(Default, aes(balance, income)) +
  geom_point(size = 2, aes(color = default)) +
  facet_grid(. ~ student) + 
  ggtitle("Students")

ggplot(Default, aes(balance)) +
  geom_density(size = 1, aes(color = default)) +
  ggtitle("Density Plot of balance")


# b

regression.model <- glm(default ~ student + income + balance, data=Default, family = binomial())
summary(regression.model)


# c

regression.model.interaction <- glm(default ~ student + income + balance + student*balance + student*income + balance*income , data=Default, family=binomial())
summary(regression.model.interaction)


# d

predicted.default <- as.numeric(predict(regression.model, type="response"))
head(predicted.default)

data.default <- Default
data.default$pred <- ifelse(predicted.default < 0.5, "No", "Yes")
head(data.default)

obs.res <- Default[ , 1]
pred.res <- as.factor(ifelse(predicted.default < 0.5, "No", "Yes"))
obs.pred <- data.frame(obs.res, pred.res)
tab.pred <- table(obs.pred)
pred.err.rate <- sum(tab.pred[1,2]+tab.pred[2,1])/sum(tab.pred)
pred.err.rate * 100

res <- residuals(regression.model,type="deviance")
max(abs(res)) * c(-1,1)

