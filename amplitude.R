#ls()
rm(list=ls())
#ls()
setwd('/Users/juliejung/Desktop/amplitude') 
#getwd()         

###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###############                    f u n c t i o n s                     ##########################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################


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
library(ggplot2)

###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###############        Part III                                           #######################################################################################################################################################################################################
###############     using     a l l      a m p s                          #######################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################


all_amplitudes.df<-read.csv(file="all_amplitudes.csv")

###### important plot ######

four.df<- subset(all_amplitudes.df, Age == 4)
five.df<- subset(all_amplitudes.df, Age == 5)
six.df<- subset(all_amplitudes.df, Age == 6)

ggplot(five.df, aes(x=as.factor(RoundAmp), y=PropH)) + 
  geom_boxplot(data=five.df, size=2) +
  ylab("Proportion of tray hatched\n")+
  theme_bw(20) +
  xlab(bquote('Amplitude' ~ '(m/' ~ s^{2} ~ ')')) +
  theme(legend.position="none")

library(MASS)
library(multcomp)
# differences between ALL amp treatments of 5 d tested embryos
five.df$RoundAmp<-as.factor(five.df$RoundAmp)
lm1<-lm(PropH~RoundAmp, data=five.df)
lm2<-glht(lm1, linfct=mcp(RoundAmp="Tukey"))
summary(lm2)
cld(lm2) 

###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###############        Part I                                    #######################################################################################################################################################################################################
###############    h i g h e r    a m p s                        #######################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################

high.amps<- subset(all_amplitudes.df, Set == "High")
med.amps<- subset(all_amplitudes.df, Set == "Medium")
low.amps<- subset(all_amplitudes.df, Set == "Low")

propherrorstats <- summarySE(low.amps, measurevar="PropH", groupvars=c("RoundAmp", "Age"), na.rm=T)


##### ALL (4, 5, 6 d) AGES in higher amps (4 treatments)#####
high.propherrorstats <- summarySE(high.amps, measurevar="PropH", groupvars=c("RoundAmp", "Age"), na.rm=T)
#high.ltoherrorstats <- summarySE(high.amps, measurevar="LtoH", groupvars=c("RoundAmp", "Age"), na.rm=T)

#high.propherrorstats$Age<-as.numeric(high.propherrorstats$Age)
#high.propherrorstats$RoundAmp<-as.numeric(high.propherrorstats$RoundAmp)
#str(high.propherrorstats)

cols=c("red", "orange", "blue")
ggplot(high.propherrorstats, aes(x=as.numeric(RoundAmp), y=as.numeric(PropH), colour = Age)) + 
  geom_errorbar(aes(ymin=PropH-se, ymax=PropH+se), colour = "black", width=.1) +
  theme_bw(20)+
  theme(legend.position = c(0.1,0.8))+
  scale_x_continuous(breaks= c(1,2.1,3.9,7.4), labels = c("1.0","2.1","3.9","7.4"))+
  geom_line(size = 3) +
  geom_point(size=5, cex= 4, shape=21, fill="white")+
  scale_color_manual(values=cols[1:3], name=NULL, labels=c("4d", "5d", "6d"))+
  ylab("Proportion of tray hatched\n") +  
  xlab(bquote('Amplitude' ~ '(m/' ~ s^{2} ~ ')')) 



##Tukey test of PropH, separated by ages
library(multcomp)

four.df$RoundAmp<-as.factor(four.df$RoundAmp)
lm4<-lm(PropH~RoundAmp, data=four.df)
fourph1<-glht(lm4, linfct=mcp(RoundAmp="Tukey"))
cld(fourph1) 

five.high.df<-subset(five.df, Set == "High")
five.high.df$RoundAmp<-as.factor(five.high.df$RoundAmp)
lm5<-lm(PropH~RoundAmp, data=five.high.df)
fiveph1<-glht(lm5, linfct=mcp(RoundAmp="Tukey"))
cld(fiveph1) 

six.df$RoundAmp<-as.factor(six.df$RoundAmp)
lm6<-lm(PropH~RoundAmp, data=six.df)
sixph1<-glht(lm6, linfct=mcp(RoundAmp="Tukey"))
cld(sixph1) 


## determine N's for higher amps, per age. 




propherrorstats <- summarySE(all_amplitudes.df, measurevar="PropH", groupvars=c("RoundAmp", "Age"), na.rm=T)
ltoherrorstats <- summarySE(all_amplitudes.df, measurevar="LtoH", groupvars=c("RoundAmp", "Age"), na.rm=T)

propherrorstats$Age<-as.factor(propherrorstats$Age)
propherrorstats$RoundAmp<-as.numeric(propherrorstats$RoundAmp)

cols=c("red", "orange", "blue")

ggplot(propherrorstats, aes(x=RoundAmp, y=PropH, colour = Age)) + 
  geom_errorbar(aes(ymin=PropH-se, ymax=PropH+se), colour = "black", width=.1) +
  theme_bw(20)+
  theme(axis.line.x = element_line(),
        axis.line.y = element_line(), legend.position = c(.75,.35))+
  scale_x_continuous(breaks= c(1,2,4,7), labels = c("1","2","4","7"))+
  geom_line(size = 3) +
  geom_point(size=5, cex= 4, shape=21, fill="white")+
  scale_color_manual(values=cols[1:3], name=NULL, labels=c("4d", "5d", "6d"))+
  ylab("Proportion Hatched\n") +  
  xlab(bquote('Amplitude' ~ '(m/' ~ s^{2} ~ ')')) 


str(high.amps)
high.amps$RoundAmp<-as.factor(high.amps$RoundAmp)
ggplot(high.amps, aes(x=RoundAmp, y=PropH)) + 
  geom_boxplot(data=high.amps, size=3) +
  ylab("Proportion of tray hatched\n")+
  theme_bw(20) +
  xlab(bquote('Amplitude' ~ '(m/' ~ s^{2} ~ ')')) +
  theme(legend.position="none")

# differences between ALL 4 diff amp treatments of all (4, 5, and 6 d tested) embryos
high.amps$RoundAmp<-as.factor(high.amps$RoundAmp)
lm3<-lm(PropH~RoundAmp, data=high.amps)
lm4<-glht(lm3, linfct=mcp(RoundAmp="Tukey"))
summary(lm4)
cld(lm4) 

##### Subset higher amps by age (4 vs. 5 vs. 6 d)  #####
four.high.amps<- subset(high.amps, Age == 4)
five.high.amps<- subset(high.amps, Age == 5)
six.high.amps<- subset(high.amps, Age == 6)

## sep by age
ggplot(high.amps, aes(x=RoundAmp, y=PropH, color=Age)) + 
  geom_point(data=four.high.amps, color="red", size=3) +
  geom_point(data=five.high.amps, color="orange", size=3) +
  geom_point(data=six.high.amps, color="blue", size=3) +
  ylab("Proportion of tray hatched\n")+
  theme_bw(20) +
  xlab(bquote('Amplitude' ~ '(m/' ~ s^{2} ~ ')')) +
  theme(legend.position="none")




amp0 <- subset(amp_threshold_2017.df, AudacityAmp == 0)
amp001 <- subset(amp_threshold_2017.df, AudacityAmp == 0.001)
amp005 <- subset(amp_threshold_2017.df, AudacityAmp == 0.005)
amp01 <- subset(amp_threshold_2017.df, AudacityAmp == 0.01)
amp1 <- subset(amp_threshold_2017.df, AudacityAmp == 0.1)
amp220 <- subset(amp_threshold_2017.df, AudacityAmp == 0.22)

mean(amp0$PropH)
mean(amp001$PropH)
mean(amp005$PropH)
mean(amp01$PropH)
mean(amp1$PropH)
mean(amp220$PropH)

###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###############        Part I                                    #######################################################################################################################################################################################################
###############    h i g h e r    a m p s                        #######################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################


amplitude.df<-read.csv(file="amplitude.csv")

propherrorstats <- summarySE(amplitude.df, measurevar="PropH", groupvars=c("Amp", "Age"), na.rm=T)
ltoherrorstats <- summarySE(amplitude.df, measurevar="LtoH", groupvars=c("Amp", "Age"), na.rm=T)

NaRV.omit(ltoherrorstats) #omit the NaNs

four <- subset(amplitude.df, Age == "4") #want them as numbers? put in quotes if want as factor. 
five <- subset(amplitude.df, Age == "5")
six <- subset(amplitude.df, Age == "6")

amplitude.df$hatched<-amplitude.df$EP5-amplitude.df$EP3m
amplitude.df$nothatched<-amplitude.df$EP3m

str(amplitude.df) #both propH and LtoH (response variables) are numbers and should be treated as continous variables
                  # age and amp (predictors) are also numbers, but should be treated as factors in this case. 

##########################################
##                                      ##
##        PropH Analysis                ##
##                                      ##
########################################## 

#hist(amplitude.df$PropH) #non-normal
#hist(log(amplitude.df$PropH)) #still not normal
#hist(log(amplitude.df$PropH/(1-amplitude.df$PropH))) #slightly normal? is this good enough? is this the logit transformation? 
amplitude.df$Age<-as.factor(amplitude.df$Age)
amplitude.df$Amp<-as.factor(amplitude.df$Amp)
#str(amplitude.df)

# #### TEST BY EGG NOT BY TRAY
# i<-2
# 
# for(i in 1:nrow(amplitude.df)) {
#   tempdf<-amplitude.df[i,]
#   a<-data.frame(rep(tempdf,i), nrow=amplitude.df$EP5, byrow=T)
#   cbind (a, c(rep('H', amplitude.df$EP5[i]-amplitude.df$EP3m[i]), rep('NH',amplitude.df$EP3m[i])))
# }

library(MASS)
glm1 <- glm(cbind(hatched, nothatched) ~ Age, family=binomial(logit), data=amplitude.df)
glm2 <- glm(cbind(hatched, nothatched) ~ Amp, family=binomial(logit), data=amplitude.df)
glm3 <- glm(cbind(hatched, nothatched) ~ AvgTadLength, family=binomial(logit), data=amplitude.df)

glm4 <- glm(cbind(hatched, nothatched) ~ Age+Amp, family=binomial(logit), data=amplitude.df)
glm5 <- glm(cbind(hatched, nothatched) ~ Age*Amp, family=binomial(logit), data=amplitude.df)

glm6 <- glm(cbind(hatched, nothatched) ~ Amp+AvgTadLength, family=binomial(logit), data=amplitude.df)
glm7 <- glm(cbind(hatched, nothatched) ~ Amp*AvgTadLength, family=binomial(logit), data=amplitude.df)

library("AICcmodavg")
PropHmods<-list(glm1, glm2, glm3, glm4, glm5, glm6, glm7)
aictab(PropHmods)
library(car)
Anova(glm7) #*yes interaction effect (Amp * AvgTadLength) ### *** better model ***
Anova(glm5) #*no interaction effect (Amp * Age)
Anova(glm4)


library(multcomp)
##Separate by ages

four$Amp<-as.factor(four$Amp)
four<-lm(PropH~Amp, data=four)
fourph1<-glht(fourAncovaModel, linfct=mcp(Amp="Tukey"))
cld(fourph1) 

five$Amp<-as.factor(five$Amp)
five<-lm(PropH~Amp, data=five)
fiveph1<-glht(fiveAncovaModel, linfct=mcp(Amp="Tukey"))
cld(fiveph1) 

six$Amp<-as.factor(six$Amp)
sixAncovaModel<-lm(PropH~Amp, data=six)
sixph1<-glht(sixAncovaModel, linfct=mcp(Amp="Tukey"))
cld(sixph1) 

############################################################
###################   PropH PLOT   #########################
############################################################

# Standard error of the mean
library(ggplot2)

# ggplot(propherrorstats, aes(x=Age, y=PropH, colour = Amp)) + 
#   geom_errorbar(aes(ymin=PropH-se, ymax=PropH+se), width=.1) +
#   geom_point()+
#   theme_bw(20)+
#   ylab("Proportion Hatched\n") +  
#   xlab("\nAge(d))")

  
#install.packages("wesanderson")
library(wesanderson)
#names(wes_palettes)
cols=wes_palette("FantasticFox")
#cols
#str(propherrorstats$Age)
propherrorstats$Age<-as.factor(propherrorstats$Age)
propherrorstats$Amp<-as.numeric(propherrorstats$Amp)

ggplot(propherrorstats, aes(x=Amp, y=PropH, colour = Age)) + 
  geom_errorbar(aes(ymin=PropH-se, ymax=PropH+se), colour = "black", width=.1) +
  theme_bw(20)+
  theme(axis.line.x = element_line(),
        axis.line.y = element_line(), legend.position = c(.75,.35))+
  scale_x_continuous(breaks= c(1,2,4,7), labels = c("1","2","4","7"))+
  geom_line(size = 3) +
  geom_point(size=5, cex= 4, shape=21, fill="white")+
  scale_color_manual(values=cols[1:3], name=NULL, labels=c("4d", "5d", "6d"))+
  ylab("Proportion Hatched\n") +  
  xlab(bquote('Amplitude' ~ '(m/' ~ s^{2} ~ ')')) 

##########################################
##                                      ##
##        LtoH Analysis                 ##
##                                      ##
########################################## 

hist(amplitude.df$LtoH) #not quite normal
hist(log(amplitude.df$LtoH)) #normal-ish --> use a linear model

############################################################
#################   linear model   #########################
############################################################


#library(car) #so can do Anova not anova

lm1<-lm(LtoH~Age, data=amplitude.df) #1-way anova
lm2<-lm(LtoH~Amp, data=amplitude.df) #1-way anova
lm3<-lm(LtoH~Age*Amp, data=amplitude.df) #2-way anova # * = a shortcut in the code = Pred + Res + Pred:Res (interaction)

#which is the best model?
library(AICcmodavg)
mods<-list(lm1,lm2,lm3)
aictab(mods) #makes a nice table for comparing models
#delta AIC is the change in AIC
#AIC weights is the probability that that is the best model (if = 1, there's a 100% chance that that's the best model)
#response~1 (a model with no predictors at all: only variance of your response)

summary(lm1); Anova(lm1)# NO age effect
summary(lm2); Anova(lm2) # YES amplitude effect ******* this is the best model according to AIC
summary(lm3); Anova(lm3) #age effect, amp effect, and no interaction effect

############################################################
###########   mixed linear model   #########################
############################################################
#should I include random effects? --> mixed models
#fixed: influence mean of the response; things you control
#random: influence variance of the response; things you can't control
library(lme4)
mod1<-lmer(LtoH~Amp+(1|Date)+(1|Set)+(1|Clutch)+(1|EP5/EP3m), data=amplitude.df)
mod2<-lmer(LtoH~Amp+(1|Date)+(1|Set)+(1|Clutch)+(1|EP5)+(1|EP3m), data=amplitude.df)
mod3<-lmer(LtoH~Amp+(1|Date)+(1|Set)+(1|Clutch), data=amplitude.df)

mod4<-lmer(LtoH~Amp+(1|Date)+(1|Set), data=amplitude.df) #
mod5<-lmer(LtoH~Amp+(1|Date)+(1|Clutch), data=amplitude.df)
mod6<-lmer(LtoH~Amp+(1|Set)+(1|Clutch), data=amplitude.df)

mod7<-lmer(LtoH~Amp+(1|Date), data=amplitude.df) #
mod8<-lmer(LtoH~Amp+(1|Clutch), data=amplitude.df)
mod9<-lmer(LtoH~Amp+(1|Set), data=amplitude.df) #

mixedmods<-list(mod4, mod7, mod9)
aictab(mixedmods)


## should i exclude some of the rounds where we have only a few eggs left in the box?
plot(PropH~EP5, data=amplitude.df)
plot(LtoH~EP5, data=amplitude.df)

############################################################
####################   LtoH PLOT   #########################
############################################################

# LATENCY PLOT
# ggplot(ltoherrorstats, aes(x=Amp, y=LtoH, colour = Age)) + 
#   geom_errorbar(aes(ymin=LtoH-se, ymax=LtoH+se), width=.1) +
#   geom_point(na.rm=T)+
#   theme_bw(20)+
#   ylab("Latency to Hatch (min)\n") +  
#   xlab("\nAmplitude (m/s^2)")
# ggplot(ltoherrorstats, aes(x=Age, y=LtoH, colour = Amp)) + 
#   geom_errorbar(aes(ymin=LtoH-se, ymax=LtoH+se), width=.1) +
#   geom_point(na.rm=T)+
#   theme_bw(20)+
#   ylab("Latency to Hatch (min)\n") +  
#   xlab("\nAge(d)")

str(ltoherrorstats$Age)
ltoherrorstats$Age<-as.factor(ltoherrorstats$Age)
ltoherrorstats$Amp<-as.numeric(ltoherrorstats$Amp)

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) 
ggplot(ltoherrorstats, aes(x=Amp, y=LtoH, colour=Age, group=Age)) + 
  geom_errorbar(aes(ymin=LtoH-se, ymax=LtoH+se), colour="black", width=.1, position=pd) +
  theme_bw(20)+
  theme(axis.line.x = element_line(),
        axis.line.y = element_line(), legend.position = c(.9,.8))+
  geom_line(position=pd, size = 3) +
  geom_point(position=pd, size=5, cex= 4, shape=21, fill="white") + # 21 is filled circle
  ylab("Latency to Hatch (min)\n") + 
  #xlab("\nAmplitude (m/s^2)")+
  #xlab(bquote('surface of' ~ .(type) ~ '/' ~ m^{2}))
  xlab(bquote('Amplitude' ~ '(m/' ~ s^{2} ~ ')')) +
  scale_x_continuous(breaks= c(1,2,4,7), labels = c("1","2","4","7"))+
  scale_color_manual(values=cols[1:3], name=NULL, labels=c("4d", "5d", "6d"))
#   +annotate(geom="text",label=c("*"), x=1.3, y=2.28, cex=8, col=cols[2]) +
#   annotate(geom="text",label=c("**"), x=1.3, y=2.08, cex=8, col=cols[3])+
#   annotate(geom="text",label=c("*"), x=2.3, y=1.75, cex=8, col=cols[3])+
#   annotate(geom="text",label=c("*"), x=4.3, y=2.1, cex=8, col=cols[1])

######## Post-hoc test for LtoH
AncovaModelLatency<-lm(LtoH~Amp, data=amplitude.df)
Anova(AncovaModelLatency)

##Separate by ages

four$Amp<-as.factor(four$Amp)
fourAncovaModelLtoH<-lm(LtoH~Amp, data=four)
fourph1LtoH<-glht(fourAncovaModelLtoH, linfct=mcp(Amp="Tukey"))
cld(fourph1LtoH) 

five$Amp<-as.factor(five$Amp)
fiveAncovaModelLtoH<-lm(LtoH~Amp, data=five)
fiveph1LtoH<-glht(fiveAncovaModelLtoH, linfct=mcp(Amp="Tukey"))
cld(fiveph1LtoH) 

six$Amp<-as.factor(six$Amp)
sixAncovaModelLtoH<-lm(LtoH~Amp, data=six)
sixph1LtoH<-glht(sixAncovaModelLtoH, linfct=mcp(Amp="Tukey"))
cld(sixph1LtoH) 




###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###############        Part II                                           #######################################################################################################################################################################################################
###############    m e d     a m p s   (taken 2016)                      #######################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################

loweramps.df<-read.csv(file="loweramps.csv")
hist(loweramps.df$PropH)
hist(log(loweramps.df$PropH))

sum(loweramps.df$EP5)
sum(amplitude.df$EP5)

lowerampsprophstats <- summarySE(loweramps.df, measurevar="PropH", groupvars=c("AudacityAmp", "Age"), na.rm=T)
#lowerampsprophstats
lowerampsltohstats <- summarySE(loweramps.df, measurevar="LtoH", groupvars=c("AudacityAmp", "Age"), na.rm=T)
#lowerampsltohstats

#loweramps.df$Age<-as.factor(loweramps.df$Age)
#loweramps.df$Amp<-as.factor(loweramps.df$Amp)
str(loweramps.df)

ggplot(lowerampsprophstats, aes(x=AudacityAmp, y=PropH)) + 
  geom_errorbar(aes(ymin=PropH-se, ymax=PropH+se), width=.1) +
  geom_point()+
  theme_bw(20)+
  theme(axis.line.x = element_line(),
        axis.line.y = element_line(), legend.position = c(.75,.25)) +
  ylab("Proportion Hatched\n") +  
  xlab(bquote('Amplitude' ~ '(m/' ~ s^{2} ~ ')'))

ggplot(lowerampsltohstats, aes(x=AudacityAmp, y=LtoH)) + 
  geom_errorbar(aes(ymin=LtoH-se, ymax=LtoH+se), width=.1) +
  geom_point()+
  theme_bw(20)+
  theme(axis.line.x = element_line(),
        axis.line.y = element_line(), legend.position = c(.75,.25)) +
  ylab("Latency to Hatch\n") +  
  xlab(bquote('Amplitude' ~ '(m/' ~ s^{2} ~ ')'))

lowerampsprophstats$AudacityAmp<-as.factor(lowerampsprophstats$AudacityAmp)
blegh <-glm(PropH~AudacityAmp, data=lowerampsprophstats)
blerg<-glht(blegh, linfct=mcp(AudacityAmp="Tukey"))
cld(blerg) 


## should i exclude some of the rounds where we have only a few eggs left in the box?
plot(PropH~EP5, data=loweramps.df)
plot(LtoH~EP5, data=loweramps.df)

loweramps.df$EP5[loweramps.df$EP5 < 4] <- NA
loweramps.df$EP5 <- replace(loweramps.df$EP5, is.na(loweramps.df$EP5), NA)
plot(PropH~EP5, data=loweramps.df)
plot(LtoH~EP5, data=loweramps.df)



##########################################
##                                      ##
##        Read in main data file        ##
##                                      ##
##########################################                     

AllAmps.df<-read.csv(file="AllAmps.csv")

allpropherrorstats <- summarySE(AllAmps.df, measurevar="PropH", groupvars=c("Amp", "Age"), na.rm=T)
#propherrorstats
allltoherrorstats <- summarySE(AllAmps.df, measurevar="LtoH", groupvars=c("Amp", "Age"), na.rm=T)
#ltoherrorstats
allfour <- subset(AllAmps.df, Age == "4") #want them as numbers? put in quotes if want as factor. 
allfive <- subset(AllAmps.df, Age == "5")
allsix <- subset(AllAmps.df, Age == "6")

allpropherrorstats$Age <- as.factor(allpropherrorstats$Age)
library(ggplot2)

cols=c("darkgreen", "limegreen", "yellowgreen")
ggplot(allpropherrorstats, aes(x=Amp, y=PropH, colour = Age)) + 
  geom_errorbar(aes(ymin=PropH-se, ymax=PropH+se), width=.15, size=1.5) +
  geom_line(size = 3) +
  geom_point(size=5, cex= 4, shape=21, fill="white")+
  theme_bw(20)+
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=90, vjust=1),
        axis.line.x = element_line(),
        axis.line.y = element_line())+
        #axis.line.y = element_line(), legend.position = c(.75,.25))+
  scale_x_continuous(breaks= c(0.1,0.2,0.5,1,1.4,2,4,7), labels = c("0.1","0.2","0.5","1","1.4","2","4","7"))+
  scale_color_manual(values=cols[1:3], name=NULL)+
  #scale_color_manual(values=cols[1:3], name=NULL, labels=c("4d", "5d", "6d"))+
  ylab("Proportion Hatched\n") +  
  xlab(bquote('Amplitude' ~ '(m/' ~ s^{2} ~ ')')) 

ggplot(allpropherrorstats, aes(x=Amp, y=PropH, colour = Age)) + 
  geom_errorbar(aes(ymin=PropH-se, ymax=PropH+se), width=.15, size=1.5) +
  geom_line(size = 3) +
  geom_point(size=5, cex= 4, shape=21, fill="white")+
  theme_bw(20)+
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=90, vjust=1),
        axis.line.x = element_line(),
        axis.line.y = element_line())+
  #axis.line.y = element_line(), legend.position = c(.75,.25))+
  scale_x_continuous(breaks= c(0.1,0.2,0.5,1,1.4,2,4,7), labels = c("0.1","0.2","0.5","1","1.4","2","4","7"))+
  scale_color_manual(values=cols[1:3], name=NULL)+
  #scale_color_manual(values=cols[1:3], name=NULL, labels=c("4d", "5d", "6d"))+
  ylab("Proportion Hatched\n") +  
  xlab(bquote('Log of Amplitude' ~ '(log m/' ~ s^{2} ~ ')')) 

str(allpropherrorstats$Amp)

##########################################
##                                      ##
## Can I combine lower and higher amps? ##
##                                      ##
##########################################  

#subset to 5d eggs where Amp (real amp, not audacity amp) = 1.4, 0.54 (lower amps), 1.0, and 2.0 (higher amps)
#see if any (PropH) is significantly different 

allfive <- subset(AllAmps.df, Age == "5")
fivedayamp1.4<- subset(allfive, Amp == 1.4)
fivedayamp0.5<- subset(allfive, Amp == 0.5)
fivedayamp1.0<- subset(allfive, Amp == 1.0)
fivedayamp2.0<- subset(allfive, Amp == 2.0)
all5dMIDamps<-rbind(fivedayamp1.4, fivedayamp0.5, fivedayamp1.0, fivedayamp2.0)

midpropherrorstats <- summarySE(all5dMIDamps, measurevar="PropH", groupvars=c("Amp", "Age"), na.rm=T)
midltoherrorstats <- summarySE(all5dMIDamps, measurevar="LtoH", groupvars=c("Amp", "Age"), na.rm=T)

ggplot(midpropherrorstats, aes(x=Amp, y=PropH)) + 
  geom_errorbar(aes(ymin=PropH-se, ymax=PropH+se), width=.1) +
  geom_point()+
  theme_bw(20)+
  theme(axis.line.x = element_line(),
        axis.line.y = element_line(), legend.position = c(.75,.25))+
  ylab("Proportion Hatched\n") +  
  xlab(bquote('Amplitude' ~ '(m/' ~ s^{2} ~ ')')) 

##### Anova strategy
hist(all5dMIDamps$PropH)
modelA<-lm(PropH ~ Amp, data=all5dMIDamps)
Anova(modelA) #no significant differences in PropH among amps

##### post-hoc strategy
all5dMIDamps$Amp<-as.factor(all5dMIDamps$Amp)
blegh <-glm(PropH~Amp, data=all5dMIDamps)
blerg<-glht(blegh, linfct=mcp(Amp="Tukey"))
cld(blerg) 


###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###############        Part III                                           #######################################################################################################################################################################################################
###############     a m p    t h r e s h o l d   (taken 2017)             #######################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################
###################################################################################################################################################################################################################################################################################

amp_threshold_2017.df<-read.csv(file="amp_threshold_2017.csv")

###### important plot ######

ggplot(amp_threshold_2017.df, aes(x=as.factor(Amp), y=PropH)) + 
  geom_boxplot(data=amp_threshold_2017.df, size=2) +
  ylab("Proportion of tray hatched\n")+
  theme_bw(20) +
  xlab(bquote('Amplitude' ~ '(m/' ~ s^{2} ~ ')')) +
  theme(legend.position="none")

amp_threshold_2017.df$Amp<-as.factor(amp_threshold_2017.df$Amp)
lm1<-lm(PropH~Amp, data=amp_threshold_2017.df)
lm2<-glht(lm1, linfct=mcp(Amp="Tukey"))
summary(lm2)
cld(lm2) 

ggplot(amp_threshold_2017.df, aes(x=AvgStage, y=PropH)) + 
  geom_point(data=amp_threshold_2017.df, size=3) +
  ylab("Proportion of tray hatched\n")+
  theme_bw(20) +
  xlab("\n Developmental stage")+
  theme(legend.position="none")




amp0 <- subset(amp_threshold_2017.df, AudacityAmp == 0)
amp001 <- subset(amp_threshold_2017.df, AudacityAmp == 0.001)
amp005 <- subset(amp_threshold_2017.df, AudacityAmp == 0.005)
amp01 <- subset(amp_threshold_2017.df, AudacityAmp == 0.01)
amp1 <- subset(amp_threshold_2017.df, AudacityAmp == 0.1)
amp220 <- subset(amp_threshold_2017.df, AudacityAmp == 0.22)

mean(amp0$PropH)
mean(amp001$PropH)
mean(amp005$PropH)
mean(amp01$PropH)
mean(amp1$PropH)
mean(amp220$PropH)




