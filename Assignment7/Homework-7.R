# Q 1. An investigator collected data on survival of patients with lung cancer at Mayo Clinic. The investigator would like you, the statistician, to answer the following questions and provide some graphs. The data is located in the survival package under the name:cancer
# a. What is the probability that someone will survive past 300 days?

library("survival")
head(cancer)
csurv <- with(cancer, Surv(time, status == 2))
csurv
cfit <- survfit(Surv(time, 1*(status == 2))~1, data = cancer)
cfit
summary(cfit)


# b. Provide a graph, including 95% confidence limits, of the Kaplan-Meier estimator the entire study.
plot(cfit, conf.int=TRUE, col="red")

# c. Is there a difference in the survival rates between males and females? 
# Provide formal statistical test with p-value and visual evidence.

cfit.gender <- survfit(Surv(time, status == 2) ~ sex, data = cancer)
cfit.gender
plot(cfit.gender, conf.int=TRUE, col=c("red", "blue"),lty=c(4,5)) 
legend("bottomleft", c("Male", "Female"), col = c("red", "blue"), lty = c(4,5))
cfit.gender.diff <- survdiff(Surv(time, status == 1) ~ sex, data = cancer)
cfit.gender.diff
# d. Is there a difference in the survival rates for the older half of the group versus the younger half? 
# Provide a formal statistical test with p-value and visual evidence

median(cancer$age)
cancer$agegrp = cancer$age > 63
head(cancer)
cfit.age <- survfit(Surv(time, status == 2) ~ agegrp, data = cancer)
cfit.age
plot(cfit.age, conf.int=TRUE, col=c("red", "blue"),lty=c(4,5)) 
legend("bottomleft", c("Young", "Old"), col = c("red", "blue"), lty = c(4,5))
cfit.age.diff <- survdiff(Surv(time, status == 1) ~ agegrp, data = cancer)
cfit.age.diff


# Q 2. The mastectomy data from the HSAUR3 package are the survival times (in months) after mastec- tomy of women with breast cancer. 
# The cancers are classified as having metastised or not based on a histochemical marker.
# a. Plot the survivor functions of each group, estimated using the Kaplan-Meier estimate, 
# on the same graph and comment on the differences.
library(HSAUR3)
head(mastectomy)

cfit.met <- survfit(Surv(time, event) ~ metastasized, data = mastectomy)
cfit.met
plot(cfit.met, conf.int=TRUE, col=c("dark green", "dark red"),lty=c(4,5)) 
legend("bottomleft", c("Not-Metastized", "Metastized"), col = c("dark green", "dark red"), lty = c(4,5))

# b. Use a log-rank test tocompare the survival experience of each group more formally.
library(coin)

cfit.met.logrank <- logrank_test(Surv(time, event) ~ metastasized, data = mastectomy)
cfit.met.logrank
