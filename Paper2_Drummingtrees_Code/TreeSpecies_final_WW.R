#load packages
library(readr)
library(lme4)
library(tidyverse)
library(ggplot2)
library(car)
library(irr)
library(ggthemes)

#load data
trees <- read_csv("trees_WaibiraWW.csv", trim_ws = TRUE)
buttress <- read_csv("buttresses_WaibiraWW.csv", trim_ws = TRUE)
druminfo <- read_csv("druminfo_WaibiraWW.csv", trim_ws = TRUE)


#create dataframe with unique trees only
trees %>%
  filter(!duplicated(tree_all_id)) -> trees.uniqueonly

##USE OF TREE SPECIES##
#species counts among used trees only
trees %>%
  filter(use=='Y') %>%
  group_by(species) %>%
  count(species) -> count.usedtrees
totalused <- sum(count.usedtrees$n)

#species counts among all trees
trees.uniqueonly %>%
  group_by(species) %>%
  count(species) -> count.alltrees

#calculate proportion of each species among the used dataset
count.usedtrees$proportion <- count.usedtrees$n/totalused

#calculate proportion of each species among the total dataset
count.alltrees$proportion <- count.alltrees$n/sum(count.alltrees$n)

#create a subset of the total dataset with only used tree species
count.alltrees.used <- count.alltrees %>%
  filter(species %in% count.usedtrees$species)

##Binomial tests
#merge columns for used and total dataset into one df
foranalysis <- merge(count.usedtrees, count.alltrees.used, by = "species", all = TRUE)
colnames(foranalysis) <- c('species','n.used','prop.used','n.all','prop.all')

#binomial tests with p correction
##for tests: use count among used trees, total used trees, proportion among total dataset
binom.test(foranalysis$n.used[1],totalused,foranalysis$prop.all[1]) -> binom.bsa
binom.test(foranalysis$n.used[2],totalused,foranalysis$prop.all[2]) -> binom.cal
binom.test(foranalysis$n.used[3],totalused,foranalysis$prop.all[3]) -> binom.cmi
binom.test(foranalysis$n.used[4],totalused,foranalysis$prop.all[4]) -> binom.cmu
binom.test(foranalysis$n.used[5],totalused,foranalysis$prop.all[5]) -> binom.cya
binom.test(foranalysis$n.used[6],totalused,foranalysis$prop.all[6]) -> binom.cze
binom.test(foranalysis$n.used[7],totalused,foranalysis$prop.all[7]) -> binom.fe
binom.test(foranalysis$n.used[8],totalused,foranalysis$prop.all[8]) -> binom.fsu
#print results
binom.bsa
binom.cal
binom.cmi
binom.cmu
binom.cya
binom.cze
binom.fe
binom.fsu

#combine p.values into single vector
p.values <- c(binom.bsa$p.value,binom.cal$p.value,binom.cmi$p.value,binom.cmu$p.value,binom.cya$p.value,binom.cze$p.value,binom.fe$p.value,binom.fsu$p.value)
#adjust p values with BH method
p.adjust(p.values,"BH")

##Stacked barplot
#create dataframe for plot
count.usedtrees$use <- 1
count.alltrees.used$use <- 0
forplot <- rbind(count.usedtrees, count.alltrees.used)

#add 'other' species (comprising all unused species) to the dataset
#proportion of unused tree species
1-sum(count.alltrees.used$n)/sum(count.alltrees$n) -> prop.unused
#number of unused tree species
sum(count.alltrees$n)-sum(count.alltrees.used$n) -> n.unused
#bind dataframe
forplot <- rbind(forplot,
                 data.frame(species='Other',n=n.unused,proportion=prop.unused,use=0))

#plot with the proportion each species among used trees and among total measured trees
ggplot(data=forplot, aes(x=as.character(use),y=proportion,fill=species)) +
  geom_bar(stat="identity") +
  ylab("proportion") +
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.1))+
  theme_classic() +
  scale_fill_viridis_d()+
  scale_x_discrete(labels=c('overall', 'drumming'))+
  labs(x=NULL)


## VARIABLES PER SPECIES ##
##trees
#create trees dataframe with only used tree species
subset <- trees.uniqueonly %>%
  filter(species=='BSA'|species=='CAL'|species=='CMI'|species=='CMU'|species=='CYA'|species=='CZE'|species=='FE'|species=='FSU')
#set colours for species, red for preferred and blue for nonpreferred
colors_trees <- c("blue","red","blue","blue","red","blue","blue","blue")

#boxplot DBH
ggplot(subset, aes(x=species,y=DBH_measure,fill=species))+
  geom_boxplot()+
  theme_classic()+
  labs(y='DBH (cm)',x="tree species")+
  scale_y_continuous(limits=c(0,130),breaks=seq(0,130,10))+
  scale_fill_manual(values = colors_trees) +
  geom_hline(yintercept=mean(subset$DBH_measure),linetype="dashed")

#boxplot nr of buttresses
ggplot(subset, aes(x = species, y = n_buttresses, fill = species)) +
  geom_boxplot()+
  theme_few() +
  labs(y = 'buttress number', x = 'tree species') +
  scale_y_continuous(limits = c(0, 20)) +
  scale_fill_manual(values = colors_trees) +
  geom_hline(yintercept=mean(subset$n_buttresses),linetype="dashed")

#descriptive data
#dbh
subset %>%
  group_by(species) %>%
  summarize(
    n = n(),
    median = median(DBH_measure),
    q1 = quantile(DBH_measure, 0.25),
    q3 = quantile(DBH_measure, 0.75),
    mean = mean(DBH_measure),
    sd = sd(DBH_measure)
  )

#n_buttresses
subset %>%
  group_by(species) %>%
  summarize(
    n = n(),
    median = median(n_buttresses),
    q1 = quantile(n_buttresses, 0.25),
    q3 = quantile(n_buttresses, 0.75),
    mean = mean(n_buttresses),
    sd = sd(n_buttresses)
  )

##buttresses
#create buttress df with only unique tree/buttress combinations
buttress.uniqueonly <- buttress %>%
  distinct(tree_id, buttress_id, .keep_all = TRUE)
#set colours for species, red for preferred and blue for nonpreferred
colors_buttress <- c("red","blue","blue","red","blue","blue")

#boxplot buttress area
ggplot(buttress.uniqueonly, aes(x=species,y=area_m2,fill=species)) +
  geom_boxplot()+
  theme_classic()+
  labs(y=bquote('area '~(m^2)),x="tree species") +
  scale_y_continuous(breaks=seq(0,14,2),limits=c(0,14))+
  scale_fill_manual(values = colors_buttress) +
  geom_hline(yintercept=mean(buttress.uniqueonly$area_m2),linetype="dashed")

  
#boxplot buttress width
ggplot(buttress.uniqueonly, aes(x=species,y=width_cm,fill=species)) +
  geom_boxplot()+
  theme_classic()+
  labs(y='width (cm)',x="tree species") +
  scale_y_continuous(breaks=seq(0,14,2),limits=c(0,14))+
  scale_fill_manual(values = colors_buttress) +
  geom_hline(yintercept=mean(buttress.uniqueonly$width_cm),linetype="dashed")

#descriptives
#area
buttress.uniqueonly %>%
  group_by(species) %>%
  summarize(
    n = n(),
    median = median(area_m2),
    q1 = quantile(area_m2, 0.25),
    q3 = quantile(area_m2, 0.75),
    mean = mean(area_m2),
    sd = sd(area_m2)
  )

#width
buttress.uniqueonly %>%
  group_by(species) %>%
  summarize(
    n = n(),
    median = median(width_cm),
    q1 = quantile(width_cm, 0.25),
    q3 = quantile(width_cm, 0.75),
    mean = mean(width_cm),
    sd = sd(width_cm)
  )


## TREE SPECIES PROPORTIONS PER CONTEXT
#set context levels to 'display' and 'other'
for(ii in 1:nrow(druminfo)){
  if(druminfo$context_use[ii]!='display')
    druminfo$context_use[ii] <- 'other'
}

#species counts among display drums only
druminfo %>%
  filter(context_use=='display') %>%
  group_by(tree_species) %>%
  count(tree_species) -> count.displaytrees
#calculate proportions among display drums
count.displaytrees$proportion <- count.displaytrees$n/sum(count.displaytrees$n)

#species counts among non-display drums
druminfo %>%
  filter(context_use=='other') %>%
  group_by(tree_species) %>%
  count(tree_species) -> count.othertrees
#calculate proportion of each species among the non-display drums
count.othertrees$proportion <- count.othertrees$n/sum(count.othertrees$n)

##stacked barplot
#create dataframe for plot
count.displaytrees$context <- 'display'
count.othertrees$context <- 'other'
contextplot <- rbind(count.displaytrees, count.othertrees)

#plot with the proportion each species among used trees and among total measured trees
ggplot(data=contextplot, aes(x=context,y=proportion,fill=tree_species)) +
  geom_bar(stat="identity") +
  ylab("proportion") +
  scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.1))+
  theme_classic() +
  scale_fill_viridis_d()+
  labs(x=NULL)

