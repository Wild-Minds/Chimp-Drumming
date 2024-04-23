library(readr)
library(lme4)
library(tidyverse)
library(ggplot2)
library(car)
library(ggthemes)
library(MuMIn)
library(effects)

#load and prepare data
buttress <- read_csv("buttresses_WaibiraWW.csv")

#change y/n for use to 1/0
buttress %>%
  mutate(use = replace(use, use == 'Y', 1)) %>%
  mutate(use = replace(use, use == 'N', 0)) -> buttress
buttress$use <- as.numeric(buttress$use) #make 1/0 numeric

#set context levels to display and other
for(ii in 1:nrow(buttress)){
  if(buttress$context[ii]!='display')
    buttress$context[ii] <- 'other'
}

#add z-scores to each variable: (datapoint-mean)/standard deviation
buttress$area.z <- (buttress$area_m2-mean(buttress$area_m2,na.rm=T))/sd(buttress$area_m2,na.rm=T) #area
buttress$width.z <- (buttress$width_cm-mean(buttress$width_cm,na.rm=T))/sd(buttress$width_cm,na.rm=T) #width
buttress$diam.z <- (buttress$DBH_measure-mean(buttress$DBH_measure,na.rm=T))/sd(buttress$DBH_measure,na.rm=T) #tree diameter

## DESCRIPTIVES ##
#counting the number of buttresses used or unused
buttress %>%
  count(use==1)

#create separate data frame with only unique buttresses
buttress.uniqueonly <- buttress %>%
  distinct(tree_id, buttress_id, .keep_all = TRUE)
nrow(buttress.uniqueonly)
buttress.uniqueonly %>%
  count(use==1) #amount of unique buttresses used

#area
summary(buttress.uniqueonly$area_m2)
sd(buttress.uniqueonly$area_m2)
#area histogram
ggplot(data=buttress.uniqueonly, aes(x=area_m2)) +
  geom_histogram(binwidth=1,color="black",fill="grey")+
  theme_classic()+
  labs(x=bquote('area'~(m^2)),y="frequency")+
  scale_y_continuous(breaks=seq(0,150,10))+
  scale_x_continuous(breaks=seq(0,14,1))

#width
summary(buttress.uniqueonly$width_cm)
sd(buttress.uniqueonly$width_cm)
#width histogram
ggplot(data=buttress.uniqueonly, aes(x=width_cm)) +
  geom_histogram(binwidth=1,color="black",fill="grey")+
  theme_classic()+
  labs(x='width (cm)',y="frequency")+
  scale_y_continuous(breaks=seq(0,100,10),limits=c(0,90))+
  scale_x_continuous(breaks=seq(0,20,1))


##CORRELATIONS BETWEEN PREDICTORS##
#test assumptions
shapiro.test(buttress.uniqueonly$area_m2)
shapiro.test(buttress.uniqueonly$width_cm)
#use spearman correlations
cor.test(buttress.uniqueonly$area_m2,buttress.uniqueonly$width_cm,method="spearman")
#scatterplot width and area
ggplot(buttress.uniqueonly, aes(x=area,y=width)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y~x, se = TRUE, color = "blue") +  # Add linear regression line
  theme_classic()+
  labs(x='width (cm)',y=bquote('area'~(m^2)))+
  scale_y_continuous(limits=c(0,22),breaks=seq(0,22,2))+
  scale_x_continuous(limits=c(0,11),breaks=seq(0,11,1))


##correlations buttress area and tree diameter
#calculate average buttress size for each tree
ave_area <- buttress.uniqueonly %>%
  group_by(tree_id) %>%
  summarize(ave_area = mean(area_m2), diameter=first(DBH_measure))
#correlation test between area and DBH
cor.test(ave_area$diameter,ave_area$ave_area,method="spearman")
#plot
ggplot(ave_area, aes(x=diameter,y=ave_area)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y~x, se = TRUE, color = "blue") +  # Add linear regression line
  theme_classic()+
  labs(x='DBH (cm)',y=bquote('area'~(m^2)))


## GLMM ##
#for consistency use Bobyqa optimiser here like in the tree models

#create null model for comparison with other models
buttress.null.z <- glmer(use~context+(1|ind_id)+(1|species)+(1|tree_id)+(1|drum_id),
                         data=buttress, family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa"))

#check for context interactions
#three way interaction
buttress.3context.z <- glmer(use~area.z*width.z*context+(1|ind_id)+(1|species)+(1|tree_id)+(1|drum_id),
                                data=buttress, family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa"))
summary(buttress.3context.z)
Anova(buttress.3context.z)

#interactions between area and width, area and context, width and context
buttress.2context.z <- glmer(use~area.z:width.z+area.z:context+width.z:context+area.z+width.z+context+(1|ind_id)+(1|species)+(1|tree_id)+(1|drum_id),
                             data=buttress, family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa"))
summary(buttress.2context.z)
Anova(buttress.2context.z)

#interaction between area and width only
buttress.interaction.z <- glmer(use~area.z*width.z+context+(1|ind_id)+(1|species)+(1|tree_id)+(1|drum_id),
                                data=buttress, family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa"))
summary(buttress.interaction.z)
Anova(buttress.interaction.z)

#main effects only
buttress.full.z <- glmer(use~area.z+width.z+context+(1|ind_id)+(1|species)+(1|tree_id)+(1|drum_id),
                     data=buttress, family=binomial(link="logit"),control=glmerControl(optimizer="bobyqa"))
#compare full and null models
anova(buttress.null.z,buttress.full.z)

#assess results in detail
S(buttress.full.z)
summary(buttress.full.z)
Anova(buttress.full.z)
r.squaredGLMM(buttress.full.z)
vif(buttress.full.z)

## PLOTS
#residuals vs fitted
resid(buttress.full.z,type="response") -> buttress.resid #compute residuals
ggplot(data.frame(Fitted = fitted(buttress.full.z), Residuals = buttress.resid), aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +  # Add a loess smoother
  geom_abline(slope=0,intercept=0)+
  labs(x = "Fitted Values", y = "Residuals") +
  theme_few()

#area by use
ggplot(data=buttress, aes(x=as.factor(use),y=area_m2)) +
  geom_boxplot() +
  geom_jitter(width=0.3,alpha=0.3)+
  theme_classic() +
  labs(x=('used for drumming'),y=bquote('area'~(m^2)))

#width by use
ggplot(data=buttress, aes(x=as.factor(use),y=width_cm)) +
  geom_boxplot() +
  geom_jitter(width=0.3,alpha=0.3)+
  theme_classic() +
  ylab("width") +
  xlab("used for drumming")+
  scale_y_continuous(breaks=seq(0,16,2),limits=c(0,16))

