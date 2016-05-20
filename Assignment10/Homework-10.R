library(ggplot2)
library("gee")
library(lme4)
library("multcomp")
library(geepack)
library(plyr)
library("MESS")


#1
data("respiratory", package = "HSAUR3")

resp <- subset(respiratory, month > "0")
resp$baseline <- rep(subset(respiratory, month == "0")$status,rep(4, 111))
resp$nstat <- as.numeric(resp$status == "good")
head(resp, n = 3)

names(resp)[names(resp) == "treatment"] <- "trt"
levels(resp$trt)[2] <- "trt"

resp.gee.unst <- geeglm(nstat ~ centre + trt + gender + baseline + age, 
                    data = resp, id = subject, family = "binomial", corstr = "unstructured")
summary(resp.gee.unst)

resp.gee.ar1 <- geeglm(nstat ~ centre + trt + gender + baseline + age, 
                        data = resp, id = subject, family = "binomial", corstr = "ar1")
summary(resp.gee.ar1)

#2

resp.gee.indep <- geeglm(nstat ~ centre + trt + gender + baseline + age,
                         data = resp, id = subject, family = "binomial",corstr = "independence")
resp.gee.exc <- geeglm(nstat ~ centre + trt + gender + baseline + age,
                       data = resp, id = subject, family = "binomial",corstr = "exchangeable")

summary(resp.gee.indep)
summary(resp.gee.exc)


QIC(resp.gee.ar1)
QIC(resp.gee.unst)
QIC(resp.gee.indep)
QIC(resp.gee.exc)


misclassification <- function(model){
  level <- c("Good","Poor")
  predict.resp.gee <- predict(model, type="response")
  predicted <- factor(ifelse(predict.resp.gee>0.5,"Good","Poor"), levels = level)
  confusion.matrix <- table(predicted,True=resp$status)
  error.rate <- (confusion.matrix[1,1] + confusion.matrix[2,2]) / sum(confusion.matrix)
  return (error.rate*100)
}

misclassification(resp.gee.unst)
misclassification(resp.gee.ar1)

misclassification(resp.gee.indep)
misclassification(resp.gee.exc)



#3

#a

data("schizophrenia2", package = "HSAUR3")
sch <- schizophrenia2


sch.summary <- ddply(sch, c("onset","month"), summarise,
                             N= length(subject),
                            Np= sum(disorder=='present', na.rm = TRUE),
                            Na= sum(disorder=='absent', na.rm = TRUE),
                            Present_ratio= Np/N,
                            Absent_ratio = Na/N
  
)
 
sch.summary


ggplot(sch, aes(x=month, fill=disorder)) +
  geom_bar(position="fill", colour="grey20") +
  facet_grid(onset ~ .) +
  scale_fill_brewer(palette="Set1") +
  ylab("Disorder ratio") +
  xlab("Month") +
  ggtitle("Disorder ratio by month for two onset values")+
  coord_flip()
  


#b
sch1 <- subset(sch, month > "0")
sch1$baseline <- rep(subset(sch, month == "0")$disorder,rep(4, 44))
sch1$nstat <- as.numeric(sch1$disorder == "absent")
head(sch1, n = 3)

sch_gee2 <- gee(nstat ~ onset + month + baseline, 
                data = na.omit(sch1), family = "binomial", id = subject, 
                corstr = "exchangeable", scale.fix = TRUE, 
                scale.value = 1)

summary(sch_gee2)

round(2*pnorm(abs(summary(sch_gee2)$coef[,5]), lower.tail = FALSE),3)

se <- summary(sch_gee2)$coefficients["onset> 20 yrs",
                                      "Robust S.E."]

coef(sch_gee2)["onset> 20 yrs"] +  
  c(-1, 1) * se * qnorm(0.975)

exp(coef(sch_gee2)["onset> 20 yrs"] + 
      c(-1, 1) * se * qnorm(0.975))


#c

sch_lmer2 <- lmer(nstat ~ onset + month + baseline + (month | subject), 
                  data = sch1, 
                  REML = FALSE, na.action = na.omit)   

summary(sch_lmer2)
cftest(sch_lmer2)

