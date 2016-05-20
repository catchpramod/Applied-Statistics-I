library("multcomp")

# Q 1. Consider alpha dataset from the coin package. 
# Compare the results when using glht and TukeyHSD (Refer to Chap 5 review for TukeyHSD).

data("alpha", package="coin")
head(alpha)
amod <- aov(elevel ~ alength, data = alpha)

amod_hsd <- TukeyHSD(amod, "alength")
amod_hsd
plot(amod_hsd)

k <- matrix(c(0,1,0,0,0,1,0,-1,1), ncol=3, byrow = T)
amod_glht <- glht(amod, linfct = k)
# amod_glht$linfct
summary(amod_glht)




# Q 2. Consider clouds data from HSAUR3 package 

# a)	Read and write a report on the clouds data given in Chapter 15 section 15.3.3 
# from Handbook Ed 3 ( or Chapter 14 section 14.3.3 from Handbook Ed 2). 



data("clouds", package = "HSAUR3")
head(clouds)
dim(clouds)
# layout(matrix(1:1, nrow = 1))
boxplot(rainfall ~ seeding, data = clouds, ylab = "Rainfall", xlab= "Seeding")


confband <- function(subset, main) {
  
  mod <- lm(rainfall ~ sne, data = clouds, subset = subset)
  sne_grid <- seq(from = 1.5, to = 4.5, by = 0.25)
  K <- cbind(1, sne_grid)
  sne_ci <- confint(glht(mod, linfct = K))
  plot(rainfall ~ sne, data = clouds, subset = subset,
      xlab = "S-Ne criterion", main = main,
      xlim = range(clouds$sne),
      ylim = range(clouds$rainfall))
      abline(mod)
      lines(sne_grid, sne_ci$confint[,2], lty = 2)
      lines(sne_grid, sne_ci$confint[,3], lty = 2)
}

layout(matrix(1:2, ncol = 2))
confband(clouds$seeding == "no", main = "No seeding")
confband(clouds$seeding == "yes", main = "Seeding")

# b. Consider the linear model fitted to the clouds data as summarised in Chapter 6 (Figure 6.5). 
# Set up a matrix K corresponding to the global null hypothesis that all interaction terms present in the model are zero. 
# Test both the global hypothesis and all hypotheses corresponding to each of the interaction terms.


lm.clouds <- lm(rainfall ~ seeding + seeding:(sne + cloudcover + prewetness + echomotion) + time, data = clouds)
lm.k <- diag(length(coef(lm.clouds)))
rownames(lm.k) <- names(coef(lm.clouds))
glht.lm.clouds <- glht(lm.clouds, linfct = lm.k)
summary(glht.lm.clouds)

aov.clouds <- aov(rainfall ~ seeding + sne + cloudcover + prewetness + echomotion + time, data = clouds)
aov.k <- diag(length(coef(aov.clouds)))
rownames(aov.k) <- names(coef(aov.clouds))
glht.aov.clouds <- glht(aov.clouds, linfct = aov.k)
summary(glht.aov.clouds)


# Q 3. For the logistic regression model presented in Chapter 7 (Figure 7.7) perform a multiplicity 
# adjusted test on all regression coefficients (except for the intercept) being zero. 
# Do the conclusions drawn in Chapter 7 remain valid?


data("womensrole", package = "HSAUR2")
head(womensrole)

glm.womensrole <- glm(cbind(agree,disagree) ~ gender * education, data = womensrole, family = binomial())
summary(glm.womensrole)

k <- diag(length(coef(glm.womensrole)))[-1,]
rownames(k) <- names(coef(glm.womensrole))[-1]
glht.wrole <- glht(glm.womensrole, linfct = k)
summary(glht.wrole)
