ls()
rm(list=ls())
ls()
setwd('/Users/juliejung/Desktop/box playback m.s.') 
getwd()         


dat<-read.csv(file="forprism2.csv")

#binomial glm
glm1<-glm(cbind(NumHat,TestEggs-NumHat)~AgeCat, family=binomial(logit), data=dat)
glm2<-glm(cbind(NumHat,TestEggs-NumHat)~STIMULUS, family=binomial(logit), data=dat)
glm3<-glm(cbind(NumHat,TestEggs-NumHat)~AgeCat+STIMULUS, family=binomial(logit), data=dat)
glm4<-glm(cbind(NumHat,TestEggs-NumHat)~AgeCat*STIMULUS, family=binomial(logit), data=dat)

library("AICcmodavg")
glms<-list(glm1, glm2, glm3, glm4)
aictab(glms)

library(car)
Anova(glm4)
Anova(glm1)
Anova(glm2)
Anova(glm3)

par(mfrow=c(1,2))
plot(PropH ~ STIMULUS + AgeCat, data = dat)

interaction.plot(dat$STIMULUS, dat$AgeCat, dat$PropH)
interaction.plot(dat$AgeCat, dat$STIMULUS, dat$PropH)

results = lm(PropH ~ STIMULUS + AgeCat + STIMULUS*AgeCat, data=dat)
anova(results)

qqnorm(results$res)
plot(results$fitted, results$res, xlab="Fitted", ylab="Residuals")

# SOURCE: http://www.stat.columbia.edu/~martin/W2024/R8.pdf
#http://www.graphpad.com/guides/prism/6/statistics/index.htm?how_to_think_about_results_from_two-way_anova.htm

#Model I (fixed effects) vs. Model II (random effects) ANOVA
#To understand the difference between fixed and random factors, consider an example of comparing responses in three species at three times. If you were interested in those three particular species, then species is considered to be a fixed factor. It would be a random factor if you were interested in differences between species in general, and you randomly selected those three species. Time is considered to be a fixed factor if you chose time points to span the interval you are interested in. Time would be a random factor if you picked those three time points at random. Since this is not likely, time is almost always considered to be a fixed factor.
#When both row and column variables are fixed factors, the analysis is called Model I ANOVA. When both row and column variables are random factors, the analysis is called Model II ANOVA. When one is random and one is fixed, it is termed mixed effects (Model III) ANOVA. Prism calculates only Model I two-way ANOVA. Since most experiments deal with fixed-factor variables, this is rarely a limitation.
##################################################################
############################# BY BOX #############################
##################################################################

library(aod)

glm1<-betabin(cbind(Hatched, TestEggs - Hatched)~1, ~1, data=Box.df)
glm2<-betabin(cbind(Hatched, TestEggs - Hatched)~Stimulus, ~1, data=Box.df) 
glm3<-betabin(cbind(Hatched, TestEggs - Hatched)~Age, ~1, data=Box.df)
glm4<-betabin(cbind(Hatched, TestEggs - Hatched)~Stimulus + Age, ~1, data=Box.df)
glm5<-betabin(cbind(Hatched, TestEggs - Hatched)~Stimulus * Age, ~1, data=Box.df)
#Pred*Res is a shortcut for Pred+Res+Pred:Res, Thus, the only difference between lmm1 and lmm2 is that lmm2 does not contain the interaction. 

anova(glm2, glm4, test="chi") #age effect
anova(glm3,glm4, test="chi") #stimulus effect
anova(glm4,glm5, test="chi") #no interaction effect
anova(glm1, glm2, glm3, glm4, glm5, test="chi") #same result

glm6<-betabin(cbind(Hatched, TestEggs - Hatched)~Stimulus + Age, ~1, data=Box.df)
glm7<-betabin(cbind(Hatched, TestEggs - Hatched)~Stimulus +Age + Stimulus * Age, ~1, data=Box.df)
anova(glm6,glm7)
#no interaction effect (same result)

#do chi or another test??

##################################################################
############################# BY EGG #############################
##################################################################

library(lme4)
Egg.df<-read.csv(file="EggPlayback.csv")

glmer1<-
#glm7<-glmer(PropH~Age.d. + (1|Box), weights = TestEggs, family=binomial, data=Box.df)
#glm8<-glmer(PropH~STIMULUS + (1|Box), weights = TestEggs, family=binomial, data=Box.df)

glm9<-glmer(PropH~Age.d.*STIMULUS + (1|Box), weights = TestEggs, family=binomial, data=Box.df)
summary(glm9) #we want to get rid of the variance in the random effect. 

glm9<-glmer(PropH~Age.d.*STIMULUS + (1|Box), weights = TestEggs, family=binomial, data=Box.df)
glm10<-glmer(PropH~Age.d.+STIMULUS + (1|Box), weights = TestEggs, family=binomial, data=Box.df)
anova(glm9, glm10)
#Note that because of the heirarchical nature of mixed e↵ects models, it is impossible to accurately know the degrees of freedom. Thus, you simply do not report them.

glm11<-glmer(PropH~Age.d. + (1|Box), weights = TestEggs, family=binomial, data=Box.df)
glm12<-glmer(PropH~STIMULUS + (1|Box), weights = TestEggs, family=binomial, data=Box.df)
anova(glm10, glm11)
anova(glm10, glm12)


