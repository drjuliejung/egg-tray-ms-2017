# Data analysis from ambiguity experiment (conducted summer 2017)
# July 2017
# Julie Jung

ls()
rm(list=ls())
ls()
setwd('/Users/juliejung/Desktop/ambiguity') 
getwd()         


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


###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###############        Part I                                    #######################################################################################################################################################################################################
###############                          #######################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################

# follows order of results section in Egg-TrayPlaybacks-ms1

ambiguity.df<-read.csv(file="ambiguity.csv")

# All hatchlings tested were at least stage 30 following Warkentin (2017) (cite SICB poster?)

min(ambiguity.df$Stage1)
min(ambiguity.df$Stage2)
min(ambiguity.df$Stage3)

younger<-subset(ambiguity.df, AgeGroup==5)
older<-subset(ambiguity.df, AgeGroup==6)

# find N when at least 8 eggs
enougheggs<-subset(ambiguity.df, ambiguity.df$EP5>7)
min(enougheggs$EP5)

youngerG8<-subset(enougheggs, AgeGroup==5)
olderG8<-subset(enougheggs, AgeGroup==6)

younger_errorstats_g8 <- summarySE(youngerG8, measurevar="PropH", groupvars="Treatment")
younger_errorstats_g8$AgeGroup<-5

older_errorstats_g8 <- summarySE(olderG8, measurevar="PropH", groupvars="Treatment")
older_errorstats_g8$AgeGroup<-6


min(youngerG8$Stage1)
min(youngerG8$Stage2)
min(youngerG8$Stage3)
max(youngerG8$Stage1)
max(youngerG8$Stage2)
max(youngerG8$Stage3)
Mode(youngerG8$AvgStage)

min(olderG8$Stage1)
min(olderG8$Stage2)
min(olderG8$Stage3)
max(olderG8$Stage1)
max(olderG8$Stage2)
max(olderG8$Stage3)
Mode(olderG8$AvgStage)

wtest<-wilcox.test(as.numeric(younger$AvgStage), as.numeric(older$AvgStage), mu = 0, alt="two.sided", paired = F, conf.int=T, conf.level=0.99)
qnorm(wtest$p.value) #z-value to report
wtest

#spontaneous hatching before the test period. 
youngerG8$spontaneous<-15 - youngerG8$ESU
olderG8$spontaneous<-15 - olderG8$ESU
mean(youngerG8$spontaneous)
sd(youngerG8$spontaneous)
mean(olderG8$spontaneous)
sd(olderG8$spontaneous)

wtest<-wilcox.test(as.numeric(youngerG8$spontaneous), as.numeric(olderG8$spontaneous), mu = 0, alt="two.sided", paired = F, conf.int=T, conf.level=0.99)
qnorm(wtest$p.value) #z-value to report

# hatching from set up
youngerG8$setup<-youngerG8$ESU - youngerG8$EP5
olderG8$setup<-olderG8$ESU - olderG8$EP5
mean(youngerG8$setup)
sd(youngerG8$setup)
mean(olderG8$setup)
sd(olderG8$setup)

wtest<-wilcox.test(as.numeric(youngerG8$setup), as.numeric(olderG8$setup), mu = 0, alt="two.sided", paired = F, conf.int=T, conf.level=0.99)
qnorm(wtest$p.value) #z-value to report

#smaller number of test eggs per tray in older than younger
wtest<-wilcox.test(as.numeric(youngerG8$EP5), as.numeric(olderG8$EP5), mu = 0, alt="two.sided", paired = F, conf.int=T, conf.level=0.99)
qnorm(wtest$p.value) #z-value to report

mean(olderG8$EP5)
sd(olderG8$EP5)
min(olderG8$EP5)
max(olderG8$EP5)

mean(youngerG8$EP5)
sd(youngerG8$EP5)
min(youngerG8$EP5)
max(youngerG8$EP5)



#Age, stimulus, and their interaction affected the hatching response of embryos in playbacks 

#binomial glm
glm1<-glm(cbind(NumHat,EP5-NumHat)~AgeGroup, family=binomial(logit), data=enougheggs)
glm2<-glm(cbind(NumHat,EP5-NumHat)~Treatment, family=binomial(logit), data=enougheggs)
glm3<-glm(cbind(NumHat,EP5-NumHat)~AgeGroup+Treatment, family=binomial(logit), data=enougheggs)
glm4<-glm(cbind(NumHat,EP5-NumHat)~AgeGroup*Treatment, family=binomial(logit), data=enougheggs)

library("AICcmodavg")
glms<-list(glm1, glm2, glm3, glm4)
aictab(glms)

library(car)
Anova(glm3)
Anova(glm4)



# 
# 
# mean(ambiguity.df$AvgStage)
# 
# library(ggplot2)
# 
# ggplot(younger, aes(x=as.factor(Treatment), y=PropH)) + 
#   geom_boxplot(data=younger, size=2) +
#   ylab("Proportion of tray hatched\n")+
#   theme_bw(20) +
#   xlab("\n Treatment")+
#   theme(legend.position="none")
# 
# ggplot(older, aes(x=as.factor(Treatment), y=PropH)) + 
#   geom_boxplot(data=older, size=2) +
#   ylab("Proportion of tray hatched\n")+
#   theme_bw(20) +
#   xlab("\n Treatment")+
#   theme(legend.position="none")
# 
# high<-subset(ambiguity.df, Treatment=="High")
# med<-subset(ambiguity.df, Treatment=="Med")
# low<-subset(ambiguity.df, Treatment=="Low")
# 
# younger_errorstats <- summarySE(younger, measurevar="PropH", groupvars="Treatment")
# younger_errorstats$AgeGroup<-5
# 
# older_errorstats <- summarySE(older, measurevar="PropH", groupvars="Treatment")
# older_errorstats$AgeGroup<-6
# 
# colour <- c("red", "orange", "yellow")
# str(ambiguity.df)
# library(ggplot2)
# ggplot(ambiguity.df, aes(x=as.factor(AgeGroup), y=PropH, colour=Treatment)) + 
#   geom_point(data=younger_errorstats, size=3) +
#   geom_line(data=younger_errorstats)+
#   geom_errorbar(data=younger_errorstats, aes(ymin=PropH-se, ymax=PropH+se), width=0.05)+ 
#   geom_point(data=older_errorstats, size=3) +
#   geom_line(data=older_errorstats)+
#   geom_errorbar(data=older_errorstats, aes(ymin=PropH-se, ymax=PropH+se), width=0.05)+ 
#   theme_bw(20)+
#   ylab("Proportion of tray hatched\n")+
#   xlab("\n Age Group")
# 
# library(MASS)
# library(multcomp)
# younger$Treatment<-as.factor(younger$Treatment)
# lm1<-lm(PropH~Treatment, data=younger)
# lm2<-glht(lm1, linfct=mcp(Treatment="Tukey"))
# summary(lm2)
# cld(lm2) 
# 
# older$Treatment<-as.factor(older$Treatment)
# lm1<-lm(PropH~Treatment, data=older)
# lm2<-glht(lm1, linfct=mcp(Treatment="Tukey"))
# summary(lm2)
# cld(lm2) 
# 
# 
# 
# ###### important plot ######
# ggplot(enougheggs, aes(x=as.factor(AgeGroup), y=PropH, colour=Treatment)) + 
#   geom_point(data=younger_errorstats_g8, size=3) +
#   geom_line(data=younger_errorstats_g8)+
#   geom_errorbar(data=younger_errorstats_g8, aes(ymin=PropH-se, ymax=PropH+se), width=0.05)+ 
#   geom_point(data=older_errorstats_g8, size=3) +
#   geom_line(data=older_errorstats_g8)+
#   geom_errorbar(data=older_errorstats_g8, aes(ymin=PropH-se, ymax=PropH+se), width=0.05)+ 
#   theme_bw(20)+
#   ylab("Proportion of tray hatched\n")+
#   xlab("\n Age Group")
# 
# 
# youngerG8$Treatment<-as.factor(youngerG8$Treatment)
# lm1<-lm(PropH~Treatment, data=youngerG8)
# lm2<-glht(lm1, linfct=mcp(Treatment="Tukey"))
# summary(lm2)
# cld(lm2) 
# 
# olderG8$Treatment<-as.factor(olderG8$Treatment)
# lm1<-lm(PropH~Treatment, data=olderG8)
# lm2<-glht(lm1, linfct=mcp(Treatment="Tukey"))
# summary(lm2)
# cld(lm2) 
# 
# 
# ######################################### plot main results figure ########################################
# 
# library(ggplot2)
# 
# # ggplot(youngerG8, aes(x=as.factor(Treatment), y=PropH)) + 
# #   geom_boxplot(data=youngerG8, size=2) +
# #   ylab("Proportion of tray hatched\n")+
# #   theme_bw(20) +
# #   xlab("\n Treatment")+
# #   theme(legend.position="none")
# # 
# # ggplot(olderG8, aes(x=as.factor(Treatment), y=PropH)) + 
# #   geom_boxplot(data=olderG8, size=2) +
# #   ylab("Proportion of tray hatched\n")+
# #   theme_bw(20) +
# #   xlab("\n Treatment")+
# #   theme(legend.position="none")
# 
# high<-subset(enougheggs, Treatment=="High")
# med<-subset(enougheggs, Treatment=="Med")
# low<-subset(enougheggs, Treatment=="Low")
# 
# younger_errorstats <- summarySE(youngerG8, measurevar="PropH", groupvars="Treatment")
# younger_errorstats$AgeGroup<-5
# 
# older_errorstats <- summarySE(olderG8, measurevar="PropH", groupvars="Treatment")
# older_errorstats$AgeGroup<-6
# 
# colour <- c("red", "orange", "yellow")
# # str(ambiguity.df)
# # library(ggplot2)
# ggplot(enougheggs, aes(x=as.factor(AgeGroup), y=PropH, colour=Treatment)) + 
#   geom_point(data=younger_errorstats, size=3) +
#   geom_line(data=younger_errorstats)+
#   geom_errorbar(data=younger_errorstats, aes(ymin=PropH-se, ymax=PropH+se), width=0.05)+ 
#   geom_point(data=older_errorstats, size=3) +
#   geom_line(data=older_errorstats)+
#   geom_errorbar(data=older_errorstats, aes(ymin=PropH-se, ymax=PropH+se), width=0.05)+ 
#   theme_bw(20)+
#   ylab("Proportion of tray hatched\n")+
#   xlab("\n Age Group")
# 
# ############################# end plotting figure #############################
# #Age, stimulus, and their interaction affected the hatching response of embryos in playbacks 
# 
# #binomial glm
# glm1<-glm(cbind(NumHat,EP5-NumHat)~AgeGroup, family=binomial(logit), data=enougheggs)
# glm2<-glm(cbind(NumHat,EP5-NumHat)~Treatment, family=binomial(logit), data=enougheggs)
# glm3<-glm(cbind(NumHat,EP5-NumHat)~AgeGroup+Treatment, family=binomial(logit), data=enougheggs)
# glm4<-glm(cbind(NumHat,EP5-NumHat)~AgeGroup*Treatment, family=binomial(logit), data=enougheggs)
# 
# library("AICcmodavg")
# glms<-list(glm1, glm2, glm3, glm4)
# aictab(glms)
# 
# library(car)
# Anova(glm3)
# Anova(glm4)
# 
# # Younger embryos showed equally little response to both the HF and LS stimuli but a substantial hatching response to the LF stimulus 
# 
# glm1<-glm(cbind(NumHat,EP5-NumHat)~AgeGroup, family=binomial(logit), data=youngerG8)
# glm2<-glm(cbind(NumHat,EP5-NumHat)~Treatment, family=binomial(logit), data=youngerG8)
# glm3<-glm(cbind(NumHat,EP5-NumHat)~AgeGroup+Treatment, family=binomial(logit), data=youngerG8)
# glm4<-glm(cbind(NumHat,EP5-NumHat)~AgeGroup*Treatment, family=binomial(logit), data=youngerG8)
# 
# glms<-list(glm1, glm2, glm3, glm4)
# aictab(glms)
# Anova(glm2)
# 
# 
# #### Tukey test 
# library(MASS)
# library(multcomp)
# #youngerG8$Treatment<-as.factor(youngerG8$Treatment)
# lm1<-lm(PropH~Treatment, data=youngerG8)
# lm2<-glht(lm1, linfct=mcp(Treatment="Tukey"))
# summary(lm2)
# cld(lm2) 
# 
# lm1<-lm(PropH~Treatment, data=olderG8)
# lm2<-glht(lm1, linfct=mcp(Treatment="Tukey"))
# summary(lm2)
# cld(lm2) 
# 
# # older embryos showed similarly strong hatching responses to both “high” and “med” stimuli and a weaker response to the “low” stimulus (stimulus effect:
# glm1<-glm(cbind(NumHat,EP5-NumHat)~AgeGroup, family=binomial(logit), data=olderG8)
# glm2<-glm(cbind(NumHat,EP5-NumHat)~Treatment, family=binomial(logit), data=olderG8)
# glm3<-glm(cbind(NumHat,EP5-NumHat)~AgeGroup+Treatment, family=binomial(logit), data=olderG8)
# glm4<-glm(cbind(NumHat,EP5-NumHat)~AgeGroup*Treatment, family=binomial(logit), data=olderG8)
# 
# glms<-list(glm1, glm2, glm3, glm4)
# aictab(glms)
# Anova(glm2)
# 
# 
# ### in younger embryos: 
# ### LOW–MED contrast χ2 = 1.48, P = 0.22; (LOW+MED)–HIGH contrast χ2 = 56.75, P < 0.0001).
# low_med_younger<-subset(youngerG8, Treatment=="Low" | Treatment =="Med")
# 
# glm1<-glm(cbind(NumHat,EP5-NumHat)~AgeGroup, family=binomial(logit), data=low_med_younger)
# glm2<-glm(cbind(NumHat,EP5-NumHat)~Treatment, family=binomial(logit), data=low_med_younger)
# glm3<-glm(cbind(NumHat,EP5-NumHat)~AgeGroup+Treatment, family=binomial(logit), data=low_med_younger)
# glm4<-glm(cbind(NumHat,EP5-NumHat)~AgeGroup*Treatment, family=binomial(logit), data=low_med_younger)
# 
# glms<-list(glm1, glm2, glm3, glm4)
# aictab(glms)
# Anova(glm2)
# 
# 
# 
# 
# 
# youngerG8$vsHigh<- 
#   
#   if youngerG8$Treatment=="Low" | youngerG8$Treatment =="Med"
# 
# high<-subset(enougheggs, Treatment=="High")
# med<-subset(enougheggs, Treatment=="Med")
# low<-subset(enougheggs, Treatment=="Low")
# 
# library(MASS)
# library(multcomp)
# younger$Treatment<-as.factor(younger$Treatment)
# lm1<-lm(PropH~Treatment, data=younger)
# lm2<-glht(lm1, linfct=mcp(Treatment="Tukey"))
# summary(lm2)
# cld(lm2) 
# 
# older$Treatment<-as.factor(older$Treatment)
# lm1<-lm(PropH~Treatment, data=older)
# lm2<-glht(lm1, linfct=mcp(Treatment="Tukey"))
# summary(lm2)
# cld(lm2) 
# 
# 
# 
# ###### important plot ######
# ggplot(enougheggs, aes(x=as.factor(AgeGroup), y=PropH, colour=Treatment)) + 
#   geom_point(data=younger_errorstats_g8, size=3) +
#   geom_line(data=younger_errorstats_g8)+
#   geom_errorbar(data=younger_errorstats_g8, aes(ymin=PropH-se, ymax=PropH+se), width=0.05)+ 
#   geom_point(data=older_errorstats_g8, size=3) +
#   geom_line(data=older_errorstats_g8)+
#   geom_errorbar(data=older_errorstats_g8, aes(ymin=PropH-se, ymax=PropH+se), width=0.05)+ 
#   theme_bw(20)+
#   ylab("Proportion of tray hatched\n")+
#   xlab("\n Age Group")
# 
# 
# youngerG8$Treatment<-as.factor(youngerG8$Treatment)
# lm1<-lm(PropH~Treatment, data=youngerG8)
# lm2<-glht(lm1, linfct=mcp(Treatment="Tukey"))
# summary(lm2)
# cld(lm2) 
# 
# olderG8$Treatment<-as.factor(olderG8$Treatment)
# lm1<-lm(PropH~Treatment, data=olderG8)
# lm2<-glht(lm1, linfct=mcp(Treatment="Tukey"))
# summary(lm2)
# cld(lm2) 
# 



