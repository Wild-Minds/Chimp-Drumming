## Prepare session, import data ----------------------------------------
#load packages
library(readr)
library(tidyverse)
library(lme4)
library(car)
library(effects)
library(optimx)
library(MuMIn)

#load data
trees <- read_csv("Trees_WaibiraWW.csv", trim_ws = TRUE)
buttress <- read_csv("Buttresses_WaibiraWW.csv", trim_ws = TRUE)


### SCRIPT FROM FITZGERALD ET AL 2022 ####

#Change  variables to factors where needed
trees$tree_all_id <- as.factor(trees$tree_all_id)
trees$tree_id <- as.factor(trees$tree_id)
buttress$tree_id <- as.factor(buttress$buttress_id)
buttress$buttress_id <- as.factor(buttress$buttress_id)
buttress$use <- as.factor(buttress$use)

#change use to 1/0
trees %>%
  mutate(use = replace(use, use == 'Y', 1)) %>%
  mutate(use = replace(use, use == 'N', 0)) -> trees
trees$use <- as.numeric(trees$use) #make 1/0 numeric

## Tree-level analysis restricted to plots with n trees >=2 --------------------
#remove unique drum ids, as these have only one tree in that drum's plot
trees.nover2 <- trees %>%
  filter(duplicated(tree_id))

#models - for tree level analysis Fitzgerald et al used plot_id as RE, in my data this is best described by tree_id
mod.t20<- glmer(use ~ 1 + (1|tree_id), data=trees.nover2, family= "binomial") #singular fit error but this is not a huge deal

mod.t22<- glmer(use ~ DBH_measure + n_buttresses + (1|tree_id), data=trees.nover2,
                family= "binomial")

anova(mod.t20, mod.t22)

Anova(mod.t22)
S(mod.t22)
plot(predictorEffects(mod.t22), type= "response", ylim=c(0,1))
r.squaredGLMM(mod.t22)

plot(mod.t22,xlab="fitted",ylab="residuals")
vif(mod.t22)



## Buttress-level analysis restricted to plots with n trees >=2 ----------------

# All trees have at least 1 used and 1 unused buttress

#In our buttress dataset plot_id and tree_id are equal (never two drumming trees with known buttresses within one plot)
#So only tree_id is added as a random effect, not plot_id

mod.b2b0<- glmer(use~ 1 + (1|tree_id), data=buttress,
                 family= "binomial")

mod.b2b3<- glmer(use~ area_m2 + width_cm + (1|tree_id), data=buttress,
                 family= "binomial")

anova(mod.b2b0, mod.b2b3)

Anova(mod.b2b3)
S(mod.b2b3)
plot(predictorEffects(mod.b2b3), type= "response",ylim=c(0,1))
r.squaredGLMM(mod.b2b3)

plot(mod.b2b3,xlab='fitted',ylab='residuals')
vif(mod.b2b3)

## _____ -----------------------------------------------------------------------
