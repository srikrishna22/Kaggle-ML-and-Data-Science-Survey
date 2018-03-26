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
                      ~ 'Neither',
                    # both Python and R..can't use this expression
                    grepl(pattern = 'R | Python', WorkToolsSelect) ~ 'Both')
                    )
                  
                  
#remove the Neither column and visualize 

debate_plot <- debate_tools %>%
                      group_by(lang_pref) %>%
                      summarize(count = n()) %>%
                      filter(lang_pref != 'Neither')

ggplot(debate_plot, aes(x = lang_pref, y = count)) +
  geom_bar(stat = 'identity')



recommendations <- debate_tools %>%
                    group_by(use = lang_pref, recom = LanguageRecommendationSelect) %>%
                    summarise(count = n()) %>%
                    arrange(desc(count)) %>%
                    filter(recom != 'NA')
                    






