library(readr)
library(lme4)
library(tidyverse)
library(ggplot2)
library(car)
library(irr)
library(ggthemes)
library(MuMIn)
library(ggeffects)

#load data
trees <- read_csv("trees_WaibiraWW.csv", col_select=c(1:11))

#remove unique drumming tree ids, as these have only one tree in that drum's plot
#not used for GLMM so also not included in descriptives
trees <- trees %>%
  filter(duplicated(tree_id))

#change use to 1/0
trees %>%
  mutate(use = replace(use, use == 'Y', 1)) %>%
  mutate(use = replace(use, use == 'N', 0)) -> trees
trees$use <- as.numeric(trees$use) #make 1/0 numeric

#set context levels to display and other
for(ii in 1:nrow(trees)){
  if(trees$context[ii]!='display')
    trees$context[ii] <- 'other'
}

#add z-scores to outome variables: (datapoint-mean)/standard deviation
trees$diam_meas.z <- (trees$DBH_measure-mean(trees$DBH_measure,na.rm=T))/sd(trees$DBH_measure,na.rm=T) #calculated diameter
trees$n_buttresses.z <- (trees$n_buttresses-mean(trees$n_buttresses,na.rm=T))/sd(trees$n_buttresses,na.rm=T) #n buttresses


## DESCRIPTIVES ##
#create dataframe with replicated trees removed for descriptives
trees %>%
  filter(!duplicated(tree_all_id)) -> trees.uniqueonly

nrow(trees.uniqueonly) #number of unique trees
trees.uniqueonly %>%
  count(use==1) #number of unique used trees

#n buttresses
summary(trees.uniqueonly$n_buttresses)
sd(trees.uniqueonly$n_buttresses)
#number of buttresses histogram
ggplot(data=trees.uniqueonly, aes(x=n_buttresses)) +
  geom_histogram(binwidth=1,color="black",fill="grey")+
  theme_classic()+
  labs(x='number of buttresses',y="frequency")+
  scale_y_continuous(breaks=seq(0,40,5),limits=c(0,40))+
  scale_x_continuous(breaks=seq(0,20))

#dbh
summary(trees.uniqueonly$diam_measure)
sd(trees$diam_measure)
#dbh histogram
ggplot(data=trees.uniqueonly, aes(x=diam_measure)) +
  geom_histogram(binwidth=10,color="black",fill="grey")+
  theme_classic()+
  ylab("frequency")+
  xlab("diameter (cm)")+
  scale_y_continuous(breaks=seq(0,50,5),limits=c(0,50))+
  scale_x_continuous(breaks=seq(0,130,10),limits=c(0,125))

#scatterplot with dbh and nbuttress
#calculate correlation coefficient buttresses and diameter
cor.test(trees.uniqueonly$diam_measure,trees.uniqueonly$n_buttresses,method="spearman")
#create plot
ggplot(trees.uniqueonly, aes(x=n_buttresses,y=diam_measure)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y~x, se = TRUE, color = "blue") +  # Add linear regression line
  theme_classic()+
  labs(x='number of buttresses',y='DBH (cm)')+
  scale_y_continuous(limits=c(0,120),breaks=seq(0,120,20))+
  scale_x_continuous(limits=c(0,15),breaks=seq(0,15,1))


## GLMM ##
#using BoByQa optimiser for consistency in all models, since it fixes convergence issues in some, using in all for consistency
#null model -> for comparison with other models
tree.null.z <- glmer(use~context+(1|tree_id)+(1|species)+(1|ind_id),
                     data=trees,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa"))

#interaction between DBH, n buttresses, and context
tree.3context.z <- glmer(use~diam_meas.z*n_buttresses.z*context+(1|tree_id)+(1|species)+(1|ind_id),
                         data=trees,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa"))
summary(tree.3context.z)
Anova(tree.3context.z)

#all two-way interactions
tree.2context.z <- glmer(use~diam_meas.z:context+n_buttresses.z:context+diam_meas.z+n_buttresses.z+context+(1|tree_id)+(1|species)+(1|ind_id),
                         data=trees,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa"))
summary(tree.2context.z)
Anova(tree.2context.z)

#interaction between n buttresses and DBH
tree.interaction.z <- glmer(use~n_buttresses.z*diam_meas.z+context+(1|tree_id)+(1|species)+(1|ind_id),
                            data=trees,family=binomial(link="logit"), control=glmerControl(optimizer="bobyqa"))
summary(tree.interaction.z)
Anova(tree.interaction.z)

#full model without interaction terms
tree.full.z <- glmer(use~n_buttresses.z+diam_meas.z+context+(1|tree_id)+(1|species)+(1|ind_id),
                     data=trees,family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa"))

#null-full comparison for model without interaction
anova(tree.null.z,tree.full.z)

#assess model in detail
S(tree.full.z) #gives exponentiated fixed effects
summary(tree.full.z) #general summary of results
Anova(tree.full.z)
r.squaredGLMM(tree.full.z) #from MuMIn package, r squared values
vif(tree.full.z)


## PLOTS ##
#diameters by use - includes duplicate trees as well, since these were used multiple times
ggplot(data=trees, aes(x=as.factor(use),y=diam_measure)) +
  geom_boxplot() +
  theme_classic() +
  geom_jitter(width=0.3,alpha=0.4) +
  ylab("DBH (cm)") +
  xlab("used for drumming") +
  scale_y_continuous(breaks=seq(0,125,25),labels=seq(0,125,25),limits=c(0,125))

#number of buttresses by use
ggplot(data=trees, aes(x=as.factor(use),y=n_buttresses)) +
  geom_boxplot() +
  geom_jitter(width=0.3,alpha=0.4) +
  theme_classic() +
  ylab("number of buttresses") +
  xlab("used for drumming")+
  scale_y_continuous(breaks=seq(0,20,2),limits=c(0,20))


#residuals vs fitted
resid(tree.full.z,type="response") -> tree.resid #compute residuals
ggplot(data.frame(Fitted = fitted(tree.full.z), Residuals = tree.resid), aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +  # Add a loess smoother
  geom_abline(slope=0,intercept=0)+
  labs(x = "Fitted Values", y = "Residuals") +
  theme_few()
