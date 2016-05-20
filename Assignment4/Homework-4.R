#Q1

data("galaxies", package="MASS")

data.galaxies <- galaxies
layout(matrix(1:3, ncol = 3))
hist(data.galaxies, xlab = "Velocity in km/sec", probability = TRUE, main = "Gaussian kernel", border = "gray")
lines(density(data.galaxies), lwd = 2)
rug(data.galaxies)
hist(data.galaxies, xlab = "Velocity in km/sec", probability = TRUE, main = "Rectangular kernel", border = "gray")
lines(density(data.galaxies, window = "rectangular"), lwd = 2)
rug(data.galaxies)
hist(data.galaxies, xlab = "Velocity in km/sec", probability = TRUE, main = "Triangular kernel", border = "gray")
lines(density(data.galaxies, window = "triangular"), lwd = 2)
rug(data.galaxies)



#a

library(ggplot2)
require(gridExtra)
require(gridBase)
library(grid)
library(MASS)

hist(galaxies)
truehist(galaxies)
qplot(galaxies)

#b

log.galaxies = log(galaxies)
par(mfrow = c(1,3))
hist(log.galaxies)
truehist(log.galaxies)
p <- qplot(log.galaxies)


plot.new()
vps <- baseViewports()
pushViewport(vps$figure)
vp1 <-plotViewport(c(0,0,0,0))
print(p, vp = vp1)

#c

x <- galaxies
layout(matrix(1:2, ncol = 2))
hist(x, xlab = "Velocity in km/sec", ylab = "Frequency", probability = TRUE, main = "Gaussian kernel", border = "gray")
lines(density(x, width = 20000), lwd = 2)
rug(x)
hist(x, xlab = "Velocity in km/sec", ylab = "Frequency", probability = TRUE, main = "Rectangular kernel", border = "gray")
lines(density(x, width = 20000, window = "rectangular"), lwd = 2)
rug(x)


x <- galaxies
layout(matrix(1:2, ncol = 2))
hist(x, xlab = "Velocity in km/sec", ylab = "Frequency", probability = TRUE, main = "Gaussian kernel", border = "gray")
lines(density(x, width = 1000), lwd = 2)
rug(x)
hist(x, xlab = "Velocity in km/sec", ylab = "Frequency", probability = TRUE, main = "Rectangular kernel", border = "gray")
lines(density(x, width = 1000, window = "rectangular"), lwd = 2)
rug(x)

x <- galaxies
layout(matrix(1:2, ncol = 2))
hist(x, xlab = "Velocity in km/sec", ylab = "Frequency", probability = TRUE, main = "Gaussian kernel", border = "gray")
lines(density(x, width = 5000), lwd = 2)
rug(x)
hist(x, xlab = "Velocity in km/sec", ylab = "Frequency", probability = TRUE, main = "Rectangular kernel", border = "gray")
lines(density(x, width = 5000, window = "rectangular"), lwd = 2)
rug(x)


#e

library(mclust)
mc <- Mclust(x)
summary(mc)
mc$parameters
par(mfrow = c(2,2))
plot(mc)



#Q2 

data("birthdeathrates",package="HSAUR3")
bd.data <- birthdeathrates

library("KernSmooth")
data("CYGOB1", package = "HSAUR2")
bd.bkde <- bkde2D(bd.data, bandwidth = sapply(bd.data, dpik))
contour(x = bd.bkde$x1, y = bd.bkde$x2, z = bd.bkde$fhat, xlab="Birth rate", ylab="Death rate")
points(bd.data, pch=20, col="red")

persp(x = bd.bkde$x1, y = bd.bkde$x2, z = bd.bkde$fhat,
      xlab="Birth rate", ylab="Death rate", zlab = "estimated density",
      theta = -35, axes = TRUE, box = TRUE)

library(mclust)
mc <- Mclust(bd.data)
summary(mc)
par(mfrow = c(2,2))
plot(mc)


#Q3.

data("schizophrenia", package="HSAUR3")
sc.data <- schizophrenia
male.data <-sc.data[sc.data$gender == 'male',]
head(male.data)
female.data <-sc.data[sc.data$gender != 'male',]
head(female.data)
par(mfrow = c(2,1))

hist(male.data$age, xlab = "Age", probability = TRUE, main = "Male", border = "gray")
lines(density(male.data$age), lwd = 2)

hist(female.data$age, xlab = "Age",  probability = TRUE, main = "Female", border = "gray")
lines(density(female.data$age), lwd = 2)


logL <- function(param, x) {
  d1 <- dnorm(x, mean = param[2], sd = param[3])
  d2 <- dnorm(x, mean = param[4], sd = param[5])
  -sum(log(param[1] * d1 + (1 - param[1]) * d2)) 
  }

male.startparam <- c(p = 0.9, mu1 = 20, sd1 = 5, mu2 = 45, sd2 = 2)
male.optim <- optim(male.startparam, logL, x = male.data$age)
male.optim$par

female.startparam <- c(p = 0.9, mu1 = 25, sd1 = 5, mu2 = 45, sd2 = 2)
female.optim <- optim(female.startparam, logL, x = female.data$age)
female.optim$par

