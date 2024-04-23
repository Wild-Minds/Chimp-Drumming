library(readr)
library(tidyverse)
library(car)
library(ggplot2)
library(ggthemes)

#load data
trees <- read_csv("trees_WaibiraWW.csv", col_select=c(1:11),trim_ws = TRUE)

#create dataframe with unique trees only
trees %>%
  filter(!duplicated(tree_all_id)) -> trees.uniqueonly
trees.uniqueonly %>%
  count(use=='Y') #number of unique used trees

#create df with calculated and measured diameters, only including rows with both available
trees.uniqueonly %>%
  drop_na(DBH_calc) %>%
  select(DBH_calc,DBH_measure) -> diams

#create a vertical dataset to use for the Levene's Test
rbind(cbind(diams$DBH_calc,'calc'),cbind(diams$DBH_measure,'measure')) -> diams.vertical

#test parametric assumptions
shapiro.test(diams$DBH_calc)
shapiro.test(diams$DBH_measure)
leveneTest(as.numeric(V1)~as.factor(V2),data=as.data.frame(diams.vertical)) #equal variances

#run Spearman correlation
cor.test(diams$DBH_calc,diams$DBH_measure,alternative="two.sided",method="spearman")

#scatter plot
ggplot(diams, aes(x=DBH_calc,y=DBH_measure)) +
  geom_point()+
  geom_smooth(method = "lm", formula = y~x, se = TRUE, color = "blue") +  # Add linear regression line
  theme_classic()+
  labs(x='calculated DBH (cm)',y='measured DBH (cm)')+
  scale_y_continuous(limits=c(0,120),breaks=seq(0,120,20))+
  scale_x_continuous(limits=c(0,120),breaks=seq(0,120,20))

