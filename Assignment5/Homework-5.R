library("vcd")
library("lattice")
library("randomForest")
library("party")
library("partykit")
library("mboost")
library("TH.data")
library("rpart")
library("mlbench")
library("boot")
library("adabag")


#Q 1

data(BostonHousing)
head(BostonHousing)
# a
bh.rpart  <- rpart(medv ~ ., data = BostonHousing, control = rpart.control(minsplit = 10))
printcp(bh.rpart)
plotcp(bh.rpart)
print(bh.rpart)
plot(bh.rpart)
text(bh.rpart)

plot(as.party(bh.rpart), tp_args = list(id = FALSE))

opt <- which.min(bh.rpart$cptable[,"xerror"])

cp <- bh.rpart$cptable[opt, "CP"]
bh.rpart <- prune(bh.rpart, cp = cp)
plot(as.party(bh.rpart), tp_args = list(id = FALSE))
print(bh.rpart$cptable)

bh.pred <- predict(bh.rpart, newdata = BostonHousing)
xlim <- range(BostonHousing$medv)
plot(bh.pred ~ medv, data = BostonHousing, xlab = "Observed",
     ylab = "Predicted", ylim = xlim, xlim = xlim)
abline(a = 0, b = 1)

mean((BostonHousing$medv - bh.pred)^2)

# b
trees <- vector(mode = "list", length = 50)
n <- nrow(BostonHousing)
bootsamples <- rmultinom(length(trees), n, rep(1, n)/n)
mod <- rpart(medv ~ ., data = BostonHousing, control = rpart.control(xval = 0))
for (i in 1:length(trees))
  trees[[i]] <- update(mod, weights = bootsamples[,i])

table(sapply(trees, function(x) as.character(x$frame$var[1])))

classprob <- matrix(0, nrow = n, ncol = length(trees))
for (i in 1:length(trees)) {
  classprob[,i] <- predict(trees[[i]], newdata = BostonHousing)
  classprob[bootsamples[,i] > 0,i] <- NA
}

avg.prediction <- rowMeans(classprob, na.rm = TRUE)

mean((BostonHousing$medv - avg.prediction)^2)

xlim <- range(BostonHousing$medv)
plot(avg.prediction ~ medv, data = BostonHousing, xlab = "Observed", ylab = "Predicted", ylim = xlim, xlim = xlim
     ,col = "red", lwd = 1)
abline(a = 0, b = 1)

# c
rf <- randomForest(medv ~ ., data = BostonHousing, maxnodes=50)
rf.pred <- predict(rf)

mean((BostonHousing$medv - RF.pred.medv)^2)

xlim <- range(BostonHousing$medv)
plot(rf.pred ~ medv, data = BostonHousing, xlab = "Observed", ylab = "Predicted", ylim = xlim, xlim = xlim
     ,col = "darkgreen", lwd = 1)
abline(a = 0, b = 1)


#Q 2

# a
data("GlaucomaM")
head(GlaucomaM)
gl.logistic <- glm(Class ~ . , data = GlaucomaM, family = binomial())
summary(gl.logistic)
predicted <- predict(gl.logistic, type = "response")
predicted.class <- ifelse(predicted < 0.5, "glaucoma", "normal")
co.matrix <- table(predicted.class,GlaucomaM$Class)
co.matrix

error.rate <- sum(co.matrix[1,2]+co.matrix[2,1])/sum(co.matrix)
error.rate


# b

gl.cv <-  cv.glm(GlaucomaM, gl.logistic, K=10)
gl.cv$delta[1]


# c

adaboost<-boosting(Class ~ ., data=GlaucomaM, boos=TRUE, mfinal=20,coeflearn='Breiman')
adaboost$trees
adaboost$weights
sort(adaboost$importance, decreasing=TRUE)
errorevol(adaboost,GlaucomaM)
pred <- predict(adaboost,GlaucomaM)
pred$error

t1<-adaboost$trees[[1]]
plot(t1)
text(t1,pretty=0)
