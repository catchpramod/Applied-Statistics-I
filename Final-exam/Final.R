# Question 1

library(ggplot2)
library("gee")
library(plyr)



data("schizophrenia2", package = "HSAUR3")
sch <- schizophrenia2
head(schizophrenia2)

# generating data summary 
sch.summary <- ddply(sch, c("onset","month"), summarise,
                     N= length(subject),
                     Np= sum(disorder=='present', na.rm = TRUE),
                     Na= sum(disorder=='absent', na.rm = TRUE),
                     Present_ratio= Np/N,
                     Absent_ratio = Na/N
                     
)

sch.summary

# plot the disorder ratio by month for two onset values
ggplot(sch, aes(x=month, fill=disorder)) +
  geom_bar(position="fill", colour="grey20") +
  facet_grid(onset ~ .) +
  scale_fill_brewer(palette="Set1") +
  ylab("Disorder ratio") +
  xlab("Month") +
  ggtitle("Disorder ratio by month for two onset values")+
  coord_flip()

# manipulate data to fit GEE
sch1 <- subset(sch, month > "0")
sch1$baseline <- rep(subset(sch, month == "0")$disorder,rep(4, 44))
sch1$nstat <- as.numeric(sch1$disorder == "absent")
head(sch1, n = 3)

# fit GEE model
sch_gee2 <- gee(nstat ~ onset + month + baseline, 
                data = na.omit(sch1), family = "binomial", id = subject, 
                corstr = "exchangeable", scale.fix = TRUE, 
                scale.value = 1)

summary(sch_gee2)

# Generating coefficient p-values
round(2*pnorm(abs(summary(sch_gee2)$coef[,5]), lower.tail = FALSE),3)

# calculate the odds range 
se <- summary(sch_gee2)$coefficients["onset> 20 yrs","Robust S.E."]

coef(sch_gee2)["onset> 20 yrs"] +  c(-1, 1) * se * qnorm(0.975)

exp(coef(sch_gee2)["onset> 20 yrs"] + c(-1, 1) * se * qnorm(0.975))





# Question 2
library(ggplot2)
library(class)
set.seed(345)

# read data from the source files
obs <- read.table("/Users/pramod/Downloads/y.dat", col.names=c('v1','v2','v3','v4'))
labels <- read.table("/Users/pramod/Downloads/idy.dat",col.names='group_label')

# split into test and train data
group_records <- 135
obs.train <- obs[1:group_records,]
obs.test <- obs[(group_records+1):150,]

# merge the given labels to the train data
train <- merge(obs.train, labels, by="row.names", all.x=TRUE)[,2:6]


# modified pairs plot using ggplot
# referenced from https://gastonsanchez.wordpress.com/2012/08/27/scatterplot-matrices-with-ggplot/
makePairs <- function(data) 
{
  grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
  grid <- subset(grid, x != y)
  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]
    data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol], 
               x = data[, xcol], y = data[, ycol], data)
  }))
  all$xvar <- factor(all$xvar, levels = names(data))
  all$yvar <- factor(all$yvar, levels = names(data))
  densities <- do.call("rbind", lapply(1:ncol(data), function(i) {
    data.frame(xvar = names(data)[i], yvar = names(data)[i], x = data[, i])
  }))
  list(all=all, densities=densities)
}

gg1 = makePairs(train[,-5])

mega_train = data.frame(gg1$all, group_label=rep(factor(train$group_label), length=nrow(gg1$all)))

# plot the data as like pairs
ggplot(mega_train, aes_string(x = "x", y = "y")) + 
  facet_grid(xvar ~ yvar, scales = "free") + 
  geom_point(aes(colour=group_label), na.rm = TRUE, alpha=0.6) + 
  stat_density(aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)), 
               data = gg1$densities, position = "identity", 
               colour = "grey20", geom = "line")


# model fitting and testing

# knn cross validation 
y.pred.cv <- knn.cv(train[,1:4],cl = train$group_label, k = 5)
table(y.pred.cv, train$group_label)
mcl.rate = (3+2)/135
mcl.rate
accruacy = 1-mcl.rate
accruacy

# predicting the group label for the test set
y.pred <- knn(train[,1:4],test= obs.test, cl = train$group_label, k = 5)
y.pred
summary(knn.pred)





# Question 3

library("Flury")
library(GGally)
library(bestglm)
library(boot)
library(pROC)

set.seed(345)

data("microtus")

# plot to see the variable correlation and scatterplot
ggpairs(microtus[,2:9], alpha = 0.7, colour=1)

# split into train and test 
train <- microtus[microtus$Group!="unknown",]
test <- microtus[microtus$Group=="unknown",]

# order columns for bestglm 
train.best <-train[, c("M1Left", "M2Left", "M3Left", "Foramen", "Pbone", "Length", "Height", "Rostrum", "Group")]
# bestglm needs the output column name to be 'y'
colnames(train.best)[9] <- "y"

# find the best model
best.logistic <- bestglm(Xy = train.best, family = binomial, IC = "AIC", method = "exhaustive")
best.logistic$BestModels
summary(best.logistic$BestModel)

# fit glm with the variables from best model
logistic <- glm(Group ~ M1Left + M3Left + Foramen + Length + Height, data=train, family = binomial())
summary(logistic)

# perform cross validation
cv.10fold <- cv.glm(train, logistic, K= 10)
cv.10fold$delta[1]
cv.accruacy <- 1 - cv.10fold$delta[1]
cv.accruacy

# predict the group for the unknown records
predicted <- predict(logistic, newdata = test, type="response")
test$Group_pred <- factor(ifelse(predicted<=0.5,"multiplex","subterraneus"))
summary((test$Group_pred))

# model stats
train.pred <- predict(logistic, type="response")
train$Group_pred <- factor(ifelse(train.pred<=0.5,"multiplex","subterraneus"))
table(train$Group, train$Group_pred)
mcl = (3)/nrow(train)
mcl
glm.accruacy = 1-mcl
glm.accruacy

# plot ROC curve 
plot(roc(train$Group, train.pred))

