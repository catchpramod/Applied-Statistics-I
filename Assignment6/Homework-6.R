# Q 1. Consider the body fat data introduced in Chapter 9 (bf bodyfat data from TH.data package).
# a. Explore the data graphically. What variables do you think need to be included for predicting bodyfat?
library("TH.data")
head(bodyfat)
pairs(bodyfat)
library(GGally)
ggpairs(bodyfat, colour = 2, alpha = 0.4)
# b. Fit a generalised additive model assuming normal errors using function gam. 
# Include as many variables as the data allows. Are all potential covariates informative? 
# Make variable selection (objectively or subjectively).
library(mgcv)
bf.gam <- gam(DEXfat ~ s(age, k =3)+ s(waistcirc, k =3)+s(hipcirc, k =3)+s(elbowbreadth, k =3)+s(kneebreadth, k =3)+
                s(anthro3a, k =3)+ s(anthro3b, k =3)+s(anthro3c, k =3)+s(anthro4, k =3), data = bodyfat)
bf.gam
layout(matrix(1:9, ncol = 3))
plot(bf.gam, ask = FALSE)

# c. Fit generalised additive model that underwent AIC-based variable selection (fitted using function gamboost).

library("mboost")
bf.boost <- gamboost(DEXfat ~ ., data = bodyfat)
bf.aic <- AIC(bf.boost)
bf.aic
bf.boost.gam <- bf.boost[mstop(bf.aic)]
layout(matrix(1:9, ncol = 3))
plot(bf.boost.gam, ask = FALSE, ylim = c(-8,8))

bf.boost.gam <- gam(DEXfat ~ waistcirc + s(hipcirc, k =3) +  s(kneebreadth, k =3)+
                s(anthro3a, k =3)+ s(anthro3b, k =3)+s(anthro3c, k =3)+s(anthro4, k =3), data = bodyfat)
bf.boost.gam

# d. Fit an additive model this time with a log-transformed response.

bf.log.boost <- gamboost(log(DEXfat) ~ ., data = bodyfat)
bf.log.aic <- AIC(bf.log.boost)
bf.log.aic
bf.log.gam <- bf.log.boost[mstop(bf.log.aic)]
layout(matrix(1:9, ncol = 3))
plot(bf.log.gam, ask = FALSE, ylim = c(-0.3,0.3))

bf.log.gam <- gam(log(DEXfat) ~ anthro3b + anthro4 + s(hipcirc, k =3) +  s(kneebreadth, k =3)+
                s(waistcirc, k =3)+ s(anthro3c, k =3), data = bodyfat)
bf.log.gam


# e. Compare the above models. Which one is more appropriate?
#creating residual plots for each of the models
layout(matrix(1:3, ncol=3))

bf.hat <- predict(bf.gam)
plot(bf.hat, bodyfat$DEXfat - bf.hat, pch=16, col='dark green', ylim=c(-10,10))
abline(h = 0, lty = 2, col = "grey")

bf.boost.hat <- predict(bf.boost.gam)
plot(bf.boost.hat, bodyfat$DEXfat - bf.boost.hat, pch=16, col='dark red', ylim=c(-10,10))
abline(h = 0, lty = 2, col = "grey")

bf.log.hat <- predict(bf.log.gam)
plot(bf.log.hat, bodyfat$DEXfat - exp(bf.log.hat), pch=16, col='dark blue',ylim=c(-10,10))
abline(h = 0, lty = 2, col = "grey")



#Q 2. Fit a logistic additive model to the glaucoma data. (Here use family = "binomial") Which covariates
# should enter the model and how is their influence on the probability of suffering from glaucoma? Compare
# your results with the results we found in class and previous homework.

head(GlaucomaM)

gl.boost <- gamboost(Class ~ ., data = GlaucomaM, family=Binomial())

gl.boost.gam <- gl.boost[mstop(gl.aic)]
layout(matrix(1:18, ncol = 6))
plot(gl.boost.gam, ask = FALSE, pch='.', ylim=c(-0.7,0.7))

gl.gam <- gam(Class ~ s(as, bs = "cr", k = 3) +
                s(as, bs = "cr", k = 3) +
                s(mhcg, bs = "cr", k = 3) +
                s(phcg, bs = "cr", k = 3) +
                s(hvc, bs = "cr", k = 3) +
                s(vari, bs = "cr", k = 3) +
                s(abrs, bs = "cr", k = 3) +
                s(mhcn, bs = "cr", k = 3) +
                s(phcn, bs = "cr", k = 3) +
                s(vass, bs = "cr", k = 3) +
                s(tmi, bs = "cr", k = 3) +
                s(hic, bs = "cr", k = 3) +
                s(mhci, bs = "cr", k = 3) +
                s(phci, bs = "cr", k = 3) +
                s(vars, bs = "cr", k = 3) +
                s(mv, bs = "cr", k = 3) ,
                family = binomial, data = GlaucomaM)
gl.gam

AIC(gl.gam)

#Q 3. Investigate the use of different types of scatterplot smoothers on the Hubble data from Chapter 6.
# (Hint: follow the example on men1500m data scattersmoothers page 199 Ed 3)
layout(matrix(1:1))
library('gamair')
data('hubble')
head(hubble)

# plot the points
plot(y ~ x, data = hubble, pch=16, col='dark green')


# fit a linear model 
hub.lm <- lm(y ~ x, data = hubble)
plot(y ~ x, data = hubble, pch=16, col='dark red')
# plot linear model line
abline(hub.lm, col='gray')

layout(matrix(1:4, ncol = 2))
plot(hub.lm, ask=FALSE)

#lowess 
layout(matrix(1:1))
hub.lws <- lowess(hubble$x,hubble$y)
plot(y ~ x, data = hubble, pch=16, col='dark green')
lines(hub.lws)

#cubic 
hub.cub <- gam(y ~ s(x, bs="cr"), data=hubble)
plot(y ~ x, data = hubble, pch=16, col='dark orange')
lines(hubble$x, predict(hub.cub),col='blue', lty=1)
#using quadratic model
hub.lm2 <- lm(y ~ x + I(x^2), data = hubble)
plot(y ~ x, data = hubble, pch=16, col='dark green')
lines(hubble$x, predict(hub.lm2), col='red')
