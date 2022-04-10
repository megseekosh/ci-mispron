library('tidyverse')

setwd('/Users/megcychosz/Google Drive/CI_mispron/')

# first for all kids
prepre_looks <- read.csv("results/BU2017_results/data/model.csv.gz") %>% 
  mutate(
    Cond_Lab = Condition %>% 
      factor(c("real", "MP", "nonsense"),
             c("Real word", "Mispronunciation", "Nonword")))

all_kids <- read.csv("results/BU2017_results/data/scores.csv") %>%
  dplyr::select(ChildStudyID, Maternal_Education_Level, Female) %>%
  merge(., prepre_looks, by="ChildStudyID")

all_ages <- read.csv('results/participant_info/JASA_audio_info.csv')  %>% # this dataset is from JASA (2021) paper w audiological info
  filter(Cimplant==1) %>%
  rename(ResearchID=Speaker) %>%
  mutate(ResearchID=substring(ResearchID,1,4))

all_CI_kids <- all_kids %>%
  merge(., all_ages, by='ResearchID') %>%
  mutate(hearing_age=EVT_Age-age_at_activation) %>%
  select(ResearchID,ChildStudyID,EVT_Age,age_at_hearing_loss,age_at_activation,hearing_age,etiology,device_formation,activation_order) %>%
  distinct(ChildStudyID, .keep_all = T)

# write out data to create audiological table
write.csv(all_CI_kids, 'results/participant_info/audiological_info.csv')



