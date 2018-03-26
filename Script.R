#Kaggle 2017 dataset analysis


#load packages
library(tidyverse)
library(broom)



#load data
responses <- read_csv("./multipleChoiceResponses.csv")


#subset of the responses
sr <- responses %>% select(WorkToolsSelect, LanguageRecommendationSelect, EmployerIndustry,
                      WorkAlgorithmsSelect) %>%
                     mutate(WorkToolsSelect = str_split(WorkToolsSelect, pattern = ',')) %>%
                      unnest()