#load packages
library(readr)
library(lme4)
library(tidyverse)
library(ggplot2)
library(car)
library(irr)
library(ggthemes)

#load data
druminfo <- read_csv("druminfo_WaibiraWW.csv", trim_ws = TRUE)

#basic descriptives for each variable
summary(druminfo,na.rm=TRUE)
sd(druminfo$n_buttresses)
sd(druminfo$DBH_measure,na.rm=TRUE)
sd(druminfo$area_m2,na.rm=TRUE)
sd(druminfo$width_cm,na.rm=TRUE)

##DRUM COUNTS##
druminfo %>% count(context_use) #drums per context
druminfo %>% count(ind_id) #drums per individual
druminfo %>% count(ind_id,context_use) #drums per context per individual
druminfo %>% count(panthoot) #drums with or without pant-hoot
druminfo %>% count(chorus) #drums with or without chorus


#stacked barplot with the counts per context per individual
#for this the contexts are set to display and other as in other analyses
for(ii in 1:nrow(druminfo)){
  if(druminfo$context_use[ii]!='display')
    druminfo$context_use[ii] <- 'other'
}
ggplot(data=druminfo, aes(x=ind_id, fill=context_use)) +
  geom_bar()+
  theme_classic()+
  scale_fill_viridis_d()+
  labs(y='number of drums',x='individual',fill='context')+
  scale_y_continuous(breaks=seq(0,13,1))


##HISTOGRAMS##
#DBH
ggplot(data=druminfo, aes(x=DBH_measure)) +
  geom_histogram(binwidth=10,color="black",fill="grey")+
  theme_few()+
  ylab("frequency")+
  xlab("DBH (cm)")+
  scale_y_continuous(breaks=seq(0,21,1))+
  scale_x_continuous(breaks=seq(0,140,10),limits=c(0,140))

#area
ggplot(data=druminfo, aes(x=area_m2)) +
  geom_histogram(binwidth=1,color="black",fill="grey")+
  theme_few()+
  labs(x=bquote('area'~(m^2)),y="frequency")+
  scale_y_continuous(breaks=seq(0,22,2))+
  scale_x_continuous(breaks=seq(0,15,1),limits=c(0,15))

#width
ggplot(data=druminfo, aes(x=width_cm)) +
  geom_histogram(binwidth=1,color="black",fill="grey")+
  theme_few()+
  labs(x='width (cm)',y="frequency")+
  scale_y_continuous(breaks=seq(0,24,2))+
  scale_x_continuous(breaks=seq(0,20,1))


##DRUMS PER HOUR##
# Filter data for rows where ind_id and focal_id are equal
filtered_df <- druminfo %>%
  filter(ind_id == focal_id)

# Count instances for each ind_id
ind_id_counts <- filtered_df %>%
  group_by(ind_id) %>%
  summarise(instance_count = n())
#used to create datasheet with focal times and nr of drums during focal follow: DrumsFocalTimes_WW.csv
focalcount <- read_csv("DrumsFocalTimes_WW.csv", trim_ws = TRUE)

#calculate average per ind per hour
focalcount <- focalcount %>%
  mutate(per_hour = nr_drums / focal_time)
summary(focalcount$per_hour)
sd(focalcount$per_hour)


##drums collected during focal follows

#percentage of drums collected during focal follows
# Step 1: Count the number of rows where ind_id is equal to focal_id
nrow(filtered_df) -> same_id_count
# Step 2: Calculate the percentage
total_rows <- nrow(druminfo)
(same_id_count / total_rows) * 100


#number of response drums
filtered_df %>%
  group_by(response) %>%
  summarise(response_inst=n()) %>%
  filter(response=='y') -> count_response
count_response$response_inst/nrow(filtered_df) #percentage response drums

#number of chorus drums
filtered_df %>%
  group_by(chorus) %>%
  summarise(chorus_inst=n()) %>%
  filter(chorus=='join'|chorus=='start') -> count_chorus
sum(count_chorus$chorus_inst)/nrow(filtered_df) #percentage chorus drums


##WOOD DENSITY##
density <- read_csv("WoodDensity_WW.csv", trim_ws = TRUE)
summary(density$density,na.rm=TRUE)
sd(density$density,na.rm=TRUE)


