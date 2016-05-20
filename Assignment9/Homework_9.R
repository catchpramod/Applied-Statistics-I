library("HSAUR3")
library("gee")
library("lme4")
library("Matrix")
library("multcomp")
library("ggplot2")

# Q 1. Following up with the Beat the Blues data from class (package HSAUR3) do the following

data("BtheB", package = "HSAUR3")
head(BtheB)

# a. Construct boxplots to compare the factor variable drug similar to how we constructed boxplots in for treatment variable. Discuss the results.
# drug boxplot
layout(matrix(1:2, nrow = 1))
ylim <- range(BtheB[,grep("bdi", names(BtheB))], 
              na.rm = TRUE)
drug.No <- subset(BtheB, drug == "No")[,
                                       grep("bdi", names(BtheB))]
boxplot(drug.No, main = "Drug not taken (drug='No')", ylab = "BDI", 
        xlab = "Time (in months)", names = c(0, 2, 3, 5, 8), 
        ylim = ylim)

drug.Yes <- subset(BtheB, drug == "Yes")[,
                                         grep("bdi", names(BtheB))]

boxplot(drug.Yes, main = "Taken drug (drug='Yes')", ylab = "BDI", 
        xlab = "Time (in months)", names = c(0, 2, 3, 5, 8), 
        ylim = ylim)


# b. Repeat (a) for the length variable. Discuss the result in the statement of the problem.
# length boxplot
layout(matrix(1:2, nrow = 1))
ylim <- range(BtheB[,grep("bdi", names(BtheB))], 
              na.rm = TRUE)
length.gt <- subset(BtheB, length == ">6m")[,
                                       grep("bdi", names(BtheB))]
boxplot(length.gt, main = "Length > 6 months (length >6m)", ylab = "BDI", 
        xlab = "Time (in months)", names = c(0, 2, 3, 5, 8), 
        ylim = ylim)

length.lt <- subset(BtheB, length == "<6m")[,
                                         grep("bdi", names(BtheB))]

boxplot(length.lt, main = "Length < 6 months (length <6m)", ylab = "BDI", 
        xlab = "Time (in months)", names = c(0, 2, 3, 5, 8), 
        ylim = ylim)


# c. Use the lm function to fit a model to the Beat the Blues data that assumes that the repeated measurements are independent. Compare the results to those from fitting the random intercept model BtheB lmer1 from class.

BtheB$subject <- factor(rownames(BtheB))
nobs <- nrow(BtheB)
BtheB_long <- reshape(BtheB, idvar = "subject", 
                      varying = c("bdi.2m", "bdi.3m", "bdi.5m", "bdi.8m"),
                      direction = "long") 
BtheB_long$time <- rep(c(2, 3, 5, 8), rep(nobs, 4)) 


BtheB_lmer1 <- lmer(bdi ~ bdi.pre + time + treatment + drug + 
                      length + (1 | subject), data = BtheB_long, 
                    REML = FALSE, na.action = na.omit)

linearModel <- lm(bdi ~ bdi.pre + time + treatment + drug + length, data = BtheB_long)
linearModel
summary(linearModel)
summary(BtheB_lmer1)
anova(BtheB_lmer1, linearModel)



# d. Investigate whether there is any evidence of an interaction between treatment and time for the Beat the Blues data.

BtheB_lmer1.Interaction <- lmer(bdi ~ bdi.pre + time + treatment+ time * treatment + drug + 
                      length + (1 | subject), data = BtheB_long, 
                    REML = FALSE, na.action = na.omit)

summary(BtheB_lmer1.Interaction)
anova(BtheB_lmer1, BtheB_lmer1.Interaction)


# e. Construct a plot of the mean profiles of both groups in the Beat the Blues data, showing also standard deviation bars at each time point. (Attempt to use ggplot2 library to do this).


library(plyr)
cdata <- ddply(BtheB_long[,c(3,6,7)], c("treatment","time"), summarise,
               N    = length(bdi),
               mean = mean(bdi, na.rm=TRUE),
               sd   = sd(bdi, na.rm=TRUE)
)

cdata

pd<-position_dodge(0.1)
ggplot(cdata, aes(x=time, y=mean, colour=treatment, group=treatment)) + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)+
  ylab("bdi") +
  xlab("Time (in months)") +
  ggtitle("Mean profile plots between two groups, with stanndard deviation bars")+
  theme_bw()
  

# Q 2. Consider the phosphate data from the package HSAUR3. This data shows the plasma inorganic phosphate levels for 33 subjects, 20 of whom are controls and 13 of whom have been classified as obese (Davis, 2002). Perform the following on this dataset
# a. Construct boxplots by group and discuss.
data("phosphate", package ="HSAUR3")
head(phosphate)

layout(matrix(1:2, nrow = 1))
ylim <- range(phosphate[,grep("t", names(phosphate))], 
              na.rm = TRUE)
phosphate.Control <- subset(phosphate, group == "control")[,
                                       grep("t", names(phosphate))]
boxplot(phosphate.Control, main = "Control group (group='control')", ylab = "Phosphate Level", 
        xlab = "Time (hour)", names = c(0,0.5,1,1.5,2,3,4,5), 
        ylim = ylim)

phosphate.Obese <- subset(phosphate, group == "obese")[,
                                                           grep("t", names(phosphate))]

boxplot(phosphate.Obese, main = "Obese group (group='obese')", ylab = "Phosphate Level", 
        xlab = "Time (hour)", names = c(0,0.5,1,1.5,2,3,4,5), 
        ylim = ylim)


# b. Produce separate plots of the profiles of the individuals in each group.

colnames(phosphate)
phosphate$obs <- factor(rownames(phosphate))
nobs <- nrow(phosphate)
phos.long <- reshape(phosphate, idvar = "obs", 
                     varying = c("t0","t0.5","t1","t1.5", "t2", "t3","t4","t5"),
                     sep="",
                     direction = "long") 
phos.long


ggplot(phos.long, aes(x=time, y=t, colour=obs, group=obs)) + 
  geom_line(position=pd ) +
  geom_point(position=pd)+
  guides(colour=FALSE)+
  ylab("Phosphate Level") +
  xlab("Time (in hour)") +
  ggtitle("Profile plot for individuals in each group")+
  theme_bw()+
  facet_grid(. ~ group)



# d. Convert the data to long version and fit the model of your choice and discuss the results.

colnames(phosphate)
phosphate$obs <- factor(rownames(phosphate))
nobs <- nrow(phosphate)
phos.long <- reshape(phosphate, idvar = "obs", 
                     varying = c("t0.5","t1","t1.5", "t2", "t3","t4","t5"),
                     sep="",
                     direction = "long") 
head(phos.long)

lmer1.phos <- lmer(t ~ t0+ group + time  + (1 | obs), data = phos.long, 
                   REML = FALSE, na.action = na.omit)   
lmer1.phos


lmer2.phos <- lmer(t ~ t0+ group + time  + (time | obs), data = phos.long, 
                    REML = FALSE, na.action = na.omit)   
summary(lmer2.phos)
cftest(lmer2.phos)
anova(lmer1.phos, lmer2.phos)
