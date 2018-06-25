

#APPENDIX R code applied in the paper

              #ANOVA by participant analysis

anova.f1<-read.table ("analiza po ispitanicima.txt", sep="\t", header=T)

aovf1 = aov (Procenat ~ Modalitet * Zadatak + Error(Ispitanik/Modalitet,anova.f1))

summary (aovf1)

with (aovf1, pairwise.t.test(procenat, modalitet, p.adjust.method="bonferroni")) #post hoc test

              #ANOVA by stimuli analysis

anova.f2<-read.table ("analiza po stimulusima.txt", sep="\t", header=T)

aovf2 = aov (Proporcija ~ Zadatak * Modalitet + Error(rec1/Zadatak,anova.f2))

summary (aovf2)

with (aovf2, pairwise.t.test (procenat, modalitet, p.adjust.method="bonferroni")) #post hoc test

                 #Logistic Regression
# First model


log.reg<-read.table ("long data format eip 2010.txt", sep="\t",header= T)

logit0<-glm (odgovor ~ zadatak, family = binomial,data = log.reg)

summary (logit0) 

#Second model

logit1<-glm (odgovor ~ zadatak + modalitet, family = binomial,data = log.reg)

summary (logit1) 

#Third model - interaction

logit2<-glm (odgovor ~ zadatak * modalitet, family = binomial, data = log.reg)

summary (logit2)

#Nested model comparison

anova (logit0,logit1,logit2, test = "Chisq")

#Pseudo R2

library(rms)

N <- nobs(glmFit)

mod0<-glm(odgovor~1,family=binomial,log.reg)

lr.stat <- lrtest(mod0, logit2)

(1-exp(-as.numeric(lr.stat$stats[1])/N))/(1-exp(2*as.numeric(logLik(mod0)/N)))

#Mixed logit model analysis

library(lme4) 

# Random models

log.reg<-read.table ("long data format eip 2010.txt", sep="\t",header= T)

lmer0<-glmer(odgovor~1+(1|ispitanik)+(1|meta),data=log.reg,family=binomial)

ranefItem<-glmer(odgovor~ 1+(1|meta),data=log.reg,family=binomial)

ranefSubj<-glmer(odgovor~ 1+(1|ispitanik),data=log.reg,family=binomial)

anova(ranefItem,ranefSubj,lmer0) #Model comparison

# Fixed effects

Lmer1<-glmer(odgovor~zadatak+(1|ispitanik)+(1|meta),data=log.reg,family="binomial")

summary(lmer1) #ispis rezultata analize

Lmer2<-glmer(odgovor~zadatak+modalitet+(1|ispitanik)+(1|modalitet),data=log.reg,family="binomial")

summary(lmer2)

Lmer3<-glmer(odgovor~zadatak*modalitet+(1|ispitanik)+(1|modalitet),data=log.reg,family="binomial")

summary(lmer3)

anova(lmer1,lmer2,lmer3) #Nested model comparison

              #Bootstrap analysis

# A)	By participant analysis - F1 test

library (boot)

data1<-read.table ("analiza po ispitanicima.txt", sep="\t", header=T) 

F_values <- function(formula, data1, indices) {
 
  data2=subset(data1, data1$modalitet=="nula")
  data3=subset(data1, data1$modalitet=="malo")
  data4=subset(data1,data1$modalitet=="mnogo")
  data5 <- data4[indices,] # allows boot to select sample
  ispitanik=na.omit(data5$ispitanik)
  data6=rbind(data4[ispitanik,], data3[ispitanik,],data2[ispitanik,]) 
  data6$ispitanik=factor(rep(1:length(ispitanik),3))
  fit=aov(procenat~zadatak*modalitet+Error(ispitanik/modalitet),data=data6)  
 return(c(summary(fit)[1][[1]][[1]]$`F value`, summary(fit)[2][[1]][[1]]$`F value`))
 }

set.seed(123)

rezultati_isp<- boot(data=data1, statistic=F_values,
 R=2000, formula=procenat~zadatak*modalitet+Error(ispitanik/modalitet)) 

rezultati_isp # result summary

plot(rezultati_isp, index=1) # Distribution of bootstrap estimates
plot(rezultati_isp, index=3) 
plot(rezultati_isp, index=4) 
boot.ci(rezultati_stim, index=1) # CI of estimates
boot.ci(rezultati_stim, index=3)
boot.ci(rezultati_stim, index=4)

# B)	By stimuli analysis

data2<- read.table("analiza po stimulusima.txt", sep="\t", header=T) 

F_values <- function(formula, data2, indices) {
      data3=subset(data2, data2$zadatak=="CUED")
      data4=subset(data2, data2$zadatak=="FREE")
      data5 <- data4[indices,] 
      znak=na.omit(data5$znak)
      data6=rbind(data4[znak,], data3[znak,]) 
 data6$znak=factor(rep(1:length(znak),2))
 fit=aov(procenat~modalitet*zadatak+Error(znak/zadatak),data=data6)  
 return(c(summary(fit)[1][[1]][[1]]$`F value`, summary(fit)[2][[1]][[1]]$`F value`))
 }

rezultati_stim<- boot(data=data2, statistic=F_values,
 R=2000, formula=procenat~modalitet*zadatak+Error(znak/zadatak)) 

rezultati_stim 

plot(rezultati_stim,index=1) 
plot(rezultati_stim,index=3) 
plot(rezultati_stim,index=4) 

boot.ci(rezultati_stim,index=1)
boot.ci(rezultati_stim,index=3)
boot.ci(rezultati_stim,index=4)

#Bootstrap of Logistic regression analysis

boot.Logit<-function(log.reg, indices) {
         data <- log.reg[indices, ]
         mod <- glm(formula = odgovor ~ zadatak+ modalitet,family=binomial, data =data)
     summary(mod)$ coefficients  
} 

set.seed(123)

rezultati_log<-boot (data = data, statistic = boot.Log, R = 2000)

rezultati_log  

plot(rezultati_log, index=10) 
plot(rezultati_log, index=11) 
plot(rezultati_log, index=12) 

rezultati_log_ci<-boot (data = data, statistic = boot.Log, R = 10000) 

boot.ci (rezultati_log_ci, index=10) 
boot.ci (rezultati_log_ci, index=11)
boot.ci (rezultati_log_ci, index=12)

# Bootstrap of mixed logit models

boot.glmer<-function(log.reg, indices){
   data1<-log.reg[indices,]
	 data1$zadatak<-relevel(data1$zadatak,ref="free")
	 data1$modalitet<-relevel(data1$modalitet,ref="nula")
	 mod<-glmer(odgovor~zadatak+modalitet+(1|ispitanik)+(1|meta),data=data1,family="binomial")
	 summary(mod)$coefficients
	 }

# Calculation of SE	

set.seed(123)
 
 boot.SE<-boot (data=data1, statistic=boot.glmer, R=2000)
 boot.SE 
 
 plot(boot.SE,index=10)
 plot(boot.SE,index=11)
 plot(boot.SE,index=12)

# Bootstrap CI
 
 set.seed(123)

 boot.INT<-boot(data=data1,statistic=boot.glmer,R=10000)
 
 boot.ci(boot.INT,index=10)
 boot.ci(boot.INT,index=11)
 boot.ci(boot.INT,index=12)

