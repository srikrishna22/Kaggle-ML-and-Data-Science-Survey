#Kaggle 2017 dataset analysis


#load packages
library(tidyverse)
library(broom)



#load data
responses <- read_csv("./multipleChoiceResponses.csv")


#subset of the responses selected and  str_split() to get individual tools that were locked in 
#strings. This will act as main data frame for rest of analysis.

sr <- responses %>% select(WorkToolsSelect, LanguageRecommendationSelect, EmployerIndustry,
                      WorkAlgorithmsSelect) %>%
                     mutate(WorkToolsSelect = str_split(WorkToolsSelect, pattern = ',')) %>%
                      unnest()

# arrange the tools that were used most often which tools were used

tools_used <- sr %>%
                group_by(tool = WorkToolsSelect) %>%
                summarize(count = n()) %>%
                arrange(desc(count))


# from the bar chart can see that R and Python are the most used tools.

ggplot(tools_used, aes(x = tool, y = count)) +
  geom_bar(stat = 'identity') +
  coord_flip()
  
# R vs Python?

debate_tools <- sr %>%
                  mutate(lang_pref = case_when(
                    # exclusive R case      
                    grepl(pattern = 'R', x = WorkToolsSelect) & !grepl(pattern = 'Python',
                                x = WorkToolsSelect) ~ 'R',
                    # exclusive Python case      
                    grepl(pattern = 'Python', x = WorkToolsSelect) & !grepl(pattern = 'R', 
                                x = WorkToolsSelect) ~ 'Python',
                    # neither Python or R case
                    !grepl(pattern = 'R', WorkToolsSelect) & !grepl(pattern = 'Python', WorkToolsSelect)
                      ~ 'Neither')
                    )
                    






