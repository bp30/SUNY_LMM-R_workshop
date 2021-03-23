###
# This is the second of the two workshops on multilevel modeling and R
###

# Set directory
setwd('C:/Users/Bruce Peng/Google Drive/Phd (1)/Stats/Multilevel modeling/Multilevel modeling workshop for SUNY/')


# Loading necessary packages and custom functions
library(lme4)
library(lmerTest)
library(emmeans)
library (ggeffects)
library(lavaan)
library(sjstats)
library(sjPlot)
source("basenull_logtest.R")
source("group_center.R")
source("means.R")
#``````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
IPE.df <- read.csv('IPE&2_fullData_N=40.csv', header=T)
IPE.df<- IPE.df[complete.cases(IPE.df$Condition),]
IPE.df$Story_ID<- as.factor(IPE.df$Story_ID)
IPE.df$Gender<- as.factor (IPE.df$Gender)
IPE.df$Version <- as.factor(IPE.df$Version)
IPE.df$Condition<- relevel(IPE.df$Condition, ref= 'Identify')

#dummy coding
IPE.df$dummy_img <- as.numeric(IPE.df$Condition=='Imagine') 
IPE.df$dummy_est <- as.numeric(IPE.df$Condition=='Estimate')
IPE.df$dummy_ident <- as.numeric(IPE.df$Condition=='Identify')

#centring 
IPE.df$Perspective_C<- group_center(IPE.df$Perspective.response, IPE.df$Participant)
IPE.df$Detail_C<- group_center(IPE.df$Detail.response, IPE.df$Participant)
IPE.df$Coherence_C<- group_center(IPE.df$Coherence.response, IPE.df$Participant)

#centre with story_ID
IPE.df$help_SC<- group_center(IPE.df$help.response, IPE.df$Story_ID)
#calculate the means
IPE.df$Coherence_M<- means(IPE.df$Coherence.response, IPE.df$Participant)
IPE.df$Coherence_MC<- IPE.df$Coherence_M- mean(IPE.df$Coherence_M)

#````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
#determine if clustering is needed
base<- lm(help.response~1, data=IPE.df)
summary(base)
null<- lmer (help.response~(1|Participant), data=IPE.df, REML=F)
basenull_logtest(base,null)
summary(null)
VarCorr(null)
ICC_P<-0.91/(2.3+0.91)

null_full<- lmer (help.response~(1|Participant)+(1|Story_ID), data=IPE.df, REML=F)
anova(null,null_full)
ranova(null_full)
#````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
#significance testing
randint<- lmer (help.response~Condition+(1|Participant)+(1|Story_ID), data=IPE.df, REML=F)

## Satterthwaite approximate of df (lmerTest)
summary(randint)
VarCorr(null_full);VarCorr(randint)

## Kenward roger
anova(randint,  ddf='Kenward-Roger')
## Chisquare
anova(null_full,randint)
## Post hoc testing
emmeans(randint, pairwise~Condition, adjust = 'bonferroni')
ggpredict (randint, c('Condition'))%>% plot()
## Confidence interval
confint (randint);VarCorr(randint)
x<-confint(randint, method = "boot", boot.type = "perc", nsim = 1000, oldNames = F)
## r2
r2(randint)
## Assumption checking
plots<-plot_model (randint, type ='diag')
plots[[1]]
plots[[2]]$Participant
plots[[2]]$Story_ID
plots[[3]]
plots[[4]]

plot_model (randint, type='re')[[2]]
#````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
# determining random slope:backward exculsion -best path 
## maximal model
randslope_max<- lmer (help.response~Condition+(dummy_img+dummy_est|Participant)+(dummy_img+dummy_est|Story_ID), 
                      data=IPE.df, REML=F)
summary(rePCA(randslope_max)); VarCorr(randslope_max)
# Zero correlation parameter model
randslope_zcp<- lmer (help.response~Condition+(dummy_img+dummy_est||Participant)+(dummy_img+dummy_est||Story_ID), data=IPE.df, REML=F)
summary(rePCA(randslope_zcp));VarCorr(randslope_zcp)
step(randslope_zcp)
## reduced model
randslope_rm<- lmer (help.response~Condition+(dummy_img+dummy_est||Participant)+(1|Story_ID), data=IPE.df, REML=F)
## add in correlation
randslope_opt<- lmer (help.response~Condition+(dummy_img+dummy_est|Participant)+(1|Story_ID), data=IPE.df, REML=F,control=lmerControl(optCtrl=list(maxfun=1e6)))
randslope_opt2<-update(randslope_opt, .~., start= getME(randslope_opt, 'theta'))
randslope_opt3<- lmer (help.response~Condition+(dummy_img|Participant) +(1|Story_ID), data=IPE.df, REML=F)
anova(randslope_rm,randslope_opt3)
## final model
randslope_final<- lmer (help.response~Condition+(dummy_img|Participant) +(1|Story_ID), data=IPE.df, REML=T)
summary(randint);summary(randslope_final)
VarCorr(randint);VarCorr(randslope_final)
# post hoc
emmeans(randslope_final, pairwise~Condition, adjust = 'bonferroni')
ggpredict (randslope_final, c('Condition'))%>% plot()

#``````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
interaction.df<- IPE.df[IPE.df$Condition!='Identify',]
interaction.df$Condition
interaction.df$Condition<- droplevels(interaction.df$Condition)
levels(interaction.df$Condition)
# With continuous variable
## maximal model
coherence_max<- lmer (help.response~Condition*Coherence_C+(dummy_img*Coherence_C|Participant)+(dummy_img*Coherence_C|Story_ID), data=interaction.df, REML=F)
summary(rePCA(coherence_max))
# Zero correlation parameter
coherence_zcp<- lmer (help.response~Condition*Coherence_C+(dummy_img*Coherence_C||Participant)+(dummy_img*Coherence_C||Story_ID), data=interaction.df, REML=F)
summary(rePCA(coherence_zcp));VarCorr(coherence_zcp)

coherence_rm<- lmer (help.response~Condition*Coherence_C+(dummy_img+dummy_img:Coherence_C||Participant)+(Coherence_C+dummy_img:Coherence_C||Story_ID), data=interaction.df, REML=F)
step(coherence_rm)

coherence_rm2<- lmer (help.response~Condition*Coherence_C+(dummy_img||Participant)+(Coherence_C||Story_ID), data=interaction.df, REML=F)
summary(rePCA(coherence_rm2));VarCorr(coherence_rm2)
## add correlation 
coherence_opt<- lmer (help.response~Condition*Coherence_C+(dummy_img|Participant)+(Coherence_C|Story_ID), data=interaction.df, REML=F,control=lmerControl(optCtrl=list(maxfun=1e6)))
summary(rePCA(coherence_opt));VarCorr(coherence_opt)

coherence_opt2<- lmer (help.response~Condition*Coherence_C+(dummy_img|Participant)+(1|Story_ID), data=interaction.df, REML=F)
anova(coherence_rm2,coherence_opt2)
# final model
coherence_final<- lmer (help.response~Condition*Coherence_C+(dummy_img|Participant)+(1|Story_ID), data=interaction.df, REML=T)
summary(coherence_final)

#post hoc
emtrends(coherence_final, pairwise~Condition, var='Coherence_C')
sd<-sd(interaction.df$Coherence_C)
m<-mean(interaction.df$Coherence_C)
m-sd;m;m+sd;
ggpredict(coherence_final, c("Condition","Coherence_C [-1.43,0,1.43]"))%>% plot()
ggpredict(coherence_final, c("Coherence_C[-1.43,0,1.43]","Condition"))%>% plot()

#````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
# Reintroduce the mean
## maximal model
coherence_meanmax<- lmer (help.response~Condition*(Coherence_C+Coherence_MC)+(dummy_img*Coherence_C|Participant)+(dummy_img*(Coherence_MC+Coherence_C)|Story_ID), data=interaction.df, REML=F)
summary(rePCA(coherence_meanmax))
## Zero correlation parameter
coherence_meanzcp<- lmer (help.response~Condition*(Coherence_C+Coherence_MC)+(dummy_img*Coherence_C||Participant)+(dummy_img*(Coherence_MC+Coherence_C)||Story_ID), data=interaction.df, REML=F)
summary(rePCA(coherence_meanzcp));VarCorr(coherence_meanzcp)

coherence_meanrm<- lmer (help.response~Condition*(Coherence_C+Coherence_MC)+(dummy_img+dummy_img:Coherence_C||Participant)+(Coherence_C||Story_ID), data=interaction.df, REML=F)
step(coherence_meanrm)

coherence_meanrm2<- lmer (help.response~Condition*(Coherence_C+Coherence_MC)+(dummy_img||Participant)+(Coherence_C||Story_ID), data=interaction.df, REML=F)
summary(rePCA(coherence_meanrm2));VarCorr(coherence_meanrm2)
# add in correlation parameters
coherence_meanopt<- lmer (help.response~Condition*(Coherence_C+Coherence_MC)+(dummy_img|Participant)+(1|Story_ID), data=interaction.df, REML=F)
anova(coherence_meanrm2,coherence_meanopt)
# final 
coherence_meanfinal<- lmer (help.response~Condition*(Coherence_C+Coherence_MC)+(dummy_img|Participant)+(1|Story_ID), data=interaction.df, REML=T)
summary(coherence_meanfinal)


#````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````

# Power analysis
library(simr)
install.packages('simr')
power<- powerSim(randslope_final, nsim= 100, test=fixed('Condition', 'f'))
power
powercurve <- powerCurve(randslope_final, along="Participant", test= fixed ('Condition', 'f'))
plot(powercurve)
print(powercurve)

model_extend<- extend(randslope_final, along="Participant", n=80)
powercurve_n80 <- powerCurve(model_extend, along="Participant", test= fixed ('Condition', 'f'))
plot(powercurve_n80)

# https://humburg.github.io/Power-Analysis/simr_power_analysis.html