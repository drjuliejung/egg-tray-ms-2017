###
# title: "BoxPlayback.R"
# assignment: "recreate results for the egg-tray-ms-2017 paper"
# author: "Julie Jung"
# date: "October 4, 2017"
###

rm(list=ls()) #clear environment
setwd('/Users/juliejung/Documents/GitHub/egg-tray-ms-2017') #set working directory       

###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###############                    f u n c t i o n s                     ##########################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################

# gives mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###############                    start c o d e                     ##############################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
library(xlsx)
library(sciplot)
library(ggplot2)
library(MASS)
library(multcomp)
library(AICcmodavg)
library(car)

###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###############                                                        #######################################################################################################################################################################################################
###############                              #######################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################

data<-read.xlsx(file="WarkentinJungRuedaMcDaniel-DataForDeposit.xlsx", sheetName="TrayPlaybackExperimentData")

#follows order of presented results in tray playback m.s. 

min(data$TestEggs, na.rm=T) #check there are at least 8 eggs per tray

str(data)
data$GutCoilStage<-as.numeric(as.character(data$GutCoilStage))
data$T3length<-as.numeric(as.character(data$T3length))
data$ProportionHatched<-as.numeric(as.character(data$ProportionHatched))
data$STIMULUS<-as.factor(data$STIMULUS)
data$FirstHatch.s. <-as.numeric(as.character(data$FirstHatch.s. ))
data$FirstHatch.cycles. <-as.numeric(as.character(data$FirstHatch.cycles. ))

younger<-subset(data, Age.d.==5.2, na.rm=T)
older<-subset(data, Age.d.==5.7, na.rm=T)

Mode(younger$GutCoilStage)
Mode(older$GutCoilStage)

younger_errorstats_g8 <- summarySE(younger, measurevar="ProportionHatched", groupvars="STIMULUS")
younger_errorstats_g8$AgeGroup<-"younger"

older_errorstats_g8 <- summarySE(older, measurevar="ProportionHatched", groupvars="STIMULUS")
older_errorstats_g8$AgeGroup<-"older"

#############

hist(data$GutCoilStage) #non parametric
hist(younger$GutCoilStage)
hist(older$GutCoilStage)

#use mann-whitney-wilcoxon test because our 2 data samples (younger and older) are independent/come from distinct populations, so the samples do not affect each other. 
#also bc non-parametric data
wtest<-wilcox.test(data$GutCoilStage~data$Age.d., mu = 0, alt="two.sided", paired = F, conf.int=T, conf.level=0.95)
qnorm(wtest$p.value) #z-value to report
wtest
#how to report
#https://stats.stackexchange.com/questions/87403/how-do-you-report-a-mann-whitney-test
#confused because S is not normally reported and I have no idea what that is. 

################
#Hatchling total length

mean(younger$MeanLength)
mean(older$MeanLength)

se(younger$MeanLength)
se(older$MeanLength)

hist(data$MeanLength)#normal
var.test(younger$MeanLength, older$MeanLength) # equal variances
t.test(younger$MeanLength, older$MeanLength, alternative=c("two.sided"), paired=FALSE, var.equal=TRUE, conf.level=0.95)

################
#spontaneous hatching before the test period. 
mean(younger$AlreadyHatchedorRuptured)
se(younger$AlreadyHatchedorRuptured)
mean(older$AlreadyHatchedorRuptured)
se(older$AlreadyHatchedorRuptured)

hist(data$AlreadyHatchedorRuptured)#non-parametric, mann-whitney
wtest<-wilcox.test(data$AlreadyHatchedorRuptured~data$Age.d., mu = 0, alt="two.sided", paired = F, conf.int=T, conf.level=0.95)
qnorm(wtest$p.value) #z-value to report
wtest

# hatching from set up
mean(younger$HatchedinSetup)
se(younger$HatchedinSetup)
mean(older$HatchedinSetup)
se(older$HatchedinSetup)

hist(data$HatchedinSetup)#non-parametric, mann-whitney
wtest<-wilcox.test(data$HatchedinSetup~data$Age.d., mu = 0, alt="two.sided", paired = F, conf.int=T, conf.level=0.95)
qnorm(wtest$p.value) #z-value to report
wtest

#smaller number of test eggs per tray in older than younger
mean(younger$TestEggs)
se(younger$TestEggs)
mean(older$TestEggs)
se(older$TestEggs)

min(younger$TestEggs)
max(younger$TestEggs)
min(older$TestEggs)
max(older$TestEggs)

hist(data$TestEggs)#non-parametric, mann-whitney
wtest<-wilcox.test(data$TestEggs~data$Age.d., mu = 0, alt="two.sided", paired = F, conf.int=T, conf.level=0.95)
qnorm(wtest$p.value) #z-value to report
wtest

wtest<-wilcox.test(as.numeric(youngerG8$EP5), as.numeric(olderG8$EP5), mu = 0, alt="two.sided", paired = F, conf.int=T, conf.level=0.99)
qnorm(wtest$p.value) #z-value to report


#Age, stimulus, and their interaction affected the hatching response of embryos in playbacks 

#binomial glm
glm1<-glm(cbind(X10minHatch,TestEggs)~Age.d., family=binomial(logit), data=data)
glm2<-glm(cbind(X10minHatch,TestEggs)~STIMULUS, family=binomial(logit), data=data)
glm3<-glm(cbind(X10minHatch,TestEggs)~Age.d.+STIMULUS, family=binomial(logit), data=data)
glm4<-glm(cbind(X10minHatch,TestEggs)~Age.d.*STIMULUS, family=binomial(logit), data=data)

glms<-list(glm1, glm2, glm3, glm4)
aictab(glms)

Anova(glm4)

#############
# Figure 6

interaction.plot(data$Age.d., data$STIMULUS, data$ProportionHatched)


#############
# "separate binomial GLMs at each age"
#re: contrasts shaded in green - better performed with tukey tests???
# prob will get same results? why use GLMs? 

#binomial glm for YOUNGER
glm2<-glm(cbind(X10minHatch,TestEggs)~STIMULUS, family=binomial(logit), data=younger)
Anova(glm2)

#binomial glm for OLDER
glm2<-glm(cbind(X10minHatch,TestEggs)~STIMULUS, family=binomial(logit), data=older)
Anova(glm2)

####################################################
####################################################
#############     Latency analyses

#Because risk in predator attacks probably accrues as a function of time, 
#but information from temporal properties accrues as a function of cycles 
#we conducted analyses of latency measured both in time (seconds) 
#and in cycles (dividing time by the cycle length of the stimulus). 

min(data$FirstHatch.s., na.rm=T)
max(data$FirstHatch.s., na.rm=T)

data$FirstHatch.cycles.

#used ANOVAs of log-transformed data to test for 
#effects of age, stimulus and age-by-stimulus interaction 
#on the latency to hatch. 

hist(data$FirstHatch.s.) #nonparametric
hist(data$FirstHatch.cycles.) #nonparametric
hist(log(data$FirstHatch.s.)) #parametric!
hist(log(data$FirstHatch.cycles.)) #parametric!

# IN SECONDS, Anova
aov1 <- aov(FirstHatch.s. ~ Age.d.*STIMULUS, data=data)
summary(aov1)
# IN CYCLES, Anova
aov2 <- aov(FirstHatch.cycles. ~ Age.d.*STIMULUS, data=data)
summary(aov2)


# IN SECONDS, GLM METHOD
glm1<-glm(FirstHatch.s.~Age.d., family=gaussian(link = "identity"), data=data)
glm2<-glm(FirstHatch.s.~STIMULUS, family=gaussian(link = "identity"), data=data)
glm3<-glm(FirstHatch.s.~Age.d.+STIMULUS, family=gaussian(link = "identity"), data=data)
glm4<-glm(FirstHatch.s.~Age.d.*STIMULUS, family=gaussian(link = "identity"), data=data)

glms<-list(glm1, glm2, glm3, glm4)
aictab(glms)

Anova(glm4)

## IN CYCLES, GLM METHOD

glm1<-glm(FirstHatch.cycles.~Age.d., family=gaussian(link = "identity"), data=data)
glm2<-glm(FirstHatch.cycles.~STIMULUS, family=gaussian(link = "identity"), data=data)
glm3<-glm(FirstHatch.cycles.~Age.d.+STIMULUS, family=gaussian(link = "identity"), data=data)
glm4<-glm(FirstHatch.cycles.~Age.d.*STIMULUS, family=gaussian(link = "identity"), data=data)

glms<-list(glm1, glm2, glm3, glm4)
aictab(glms)

Anova(glm4)


#The shortest latency times were for the LF stimulus (younger: 39.5 ± 10.1 s; older 18.6 ± 1.9 s), 
# while latencies for the LS stimulus included the fewest cycles of the vibration pattern 
#(younger: 6.5 ± 1.6; older: 1.9 ± 0.3 cycles). 

HF<-subset(data, STIMULUS=="HF", na.rm=T)
LF<-subset(data, STIMULUS=="LF", na.rm=T)
LS<-subset(data, STIMULUS=="LS", na.rm=T)

HFyounger<-subset(HF, Age.d.==5.2, na.rm=T)
HFolder<-subset(HF, Age.d.==5.7, na.rm=T)
LFyounger<-subset(LF, Age.d.==5.2, na.rm=T)
LFolder<-subset(LF, Age.d.==5.7, na.rm=T)
LSyounger<-subset(LS, Age.d.==5.2, na.rm=T)
LSolder<-subset(LS, Age.d.==5.7, na.rm=T)

mean(LFyounger$FirstHatch.s., na.rm=T)
se(LFyounger$FirstHatch.s., na.rm=T)
mean(LFolder$FirstHatch.s., na.rm=T)
se(LFolder$FirstHatch.s., na.rm=T)

mean(LSyounger$FirstHatch.cycles., na.rm=T)
se(LSyounger$FirstHatch.cycles., na.rm=T)
mean(LSolder$FirstHatch.cycles., na.rm=T)
se(LSolder$FirstHatch.cycles., na.rm=T)


#We conducted analyses of latency both on the subset of trays from which at least one individual hatched 
#and also on the full dataset, assigning a latency of 600 s 
#(i.e., the full playback plus post-playback observation period) to trays in which no embryos hatched.

# IN SECONDS, Anova
aov1 <- aov(FirstHatch.s.600 ~ Age.d.*STIMULUS, data=data)
summary(aov1)

# IN SECONDS, GLM METHOD
glm1<-glm(FirstHatch.s.600~Age.d., family=gaussian(link = "identity"), data=data)
glm2<-glm(FirstHatch.s.600~STIMULUS, family=gaussian(link = "identity"), data=data)
glm3<-glm(FirstHatch.s.600~Age.d.+STIMULUS, family=gaussian(link = "identity"), data=data)
glm4<-glm(FirstHatch.s.600~Age.d.*STIMULUS, family=gaussian(link = "identity"), data=data)

glms<-list(glm1, glm2, glm3, glm4)
aictab(glms)

Anova(glm4)

