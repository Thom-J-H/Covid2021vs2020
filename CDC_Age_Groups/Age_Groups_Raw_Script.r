library(tidyverse)
library(here)
library(visdat)
library(scales)
library(glue)

load(file = here::here("data", "tidy_data", "CDC_Comorbid_dat.rda"))

#rename(By_year, Age_Group = `Age Group`)

C19_Deaths_Age_State <- rename(C19_Deaths_Age_State , 
                               Covid_Deaths = `COVID-19 Deaths`,
                               Start_Date = `Start Date`,
                               End_Date = `End Date`,
                               Data_As_Of = `Data As Of`,
                               Mentions_N = `Number of Mentions`)


C19_Deaths_Age_State_Year_Only <- rename(C19_Deaths_Age_State_Year_Only, 
                                         Covid_Deaths = `COVID-19 Deaths`,
                                         Start_Date = `Start Date`,
                                         End_Date = `End Date`,
                                         Data_As_Of = `Data As Of`,
                                         Mentions_N = `Number of Mentions`)


C19_Deaths_Age_State_Totals_Monthly <- rename(C19_Deaths_Age_State_Totals_Monthly, 
                                              Covid_Deaths = `COVID-19 Deaths`,
                                              Start_Date = `Start Date`,
                                              End_Date = `End Date`,
                                              Data_As_Of = `Data As Of`,
                                              Mentions_N = `Number of Mentions`)




# more cleanup ------------------------------------------------------------



C19_Deaths_Age_State$Condition <- recode(C19_Deaths_Age_State$Condition, 
                            `All other conditions and causes (residual)` = "All other (residual)" ,
                            `Intentional and unintentional injury, poisoning, and other adverse events` = "Injury, poisoning, other adverse events",
                            `Other diseases of the circulatory system` = "Circulatory system diseases (other)",
                            `Other diseases of the respiratory system` = "Respiratory system diseases (other)" ) 



C19_Deaths_Age_State_Year_Only$Condition <- recode(C19_Deaths_Age_State_Year_Only$Condition, 
                                                   `All other conditions and causes (residual)` = "All other (residual)" ,
                                                   `Intentional and unintentional injury, poisoning, and other adverse events` = "Injury, poisoning, other adverse events",
                                                   `Other diseases of the circulatory system` = "Circulatory system diseases (other)",
                                                   `Other diseases of the respiratory system` = "Respiratory system diseases (other)" ) 



C19_Deaths_Age_State_Totals_Monthly$Condition  <- recode(C19_Deaths_Age_State_Totals_Monthly$Condition, 
                                              `All other conditions and causes (residual)` = "All other (residual)" ,
                                              `Intentional and unintentional injury, poisoning, and other adverse events` = "Injury, poisoning, other adverse events",
                                              `Other diseases of the circulatory system` = "Circulatory system diseases (other)",
                                              `Other diseases of the respiratory system` = "Respiratory system diseases (other)" ) 




these_two_save <- c("C19_Deaths_Age_State_Totals_Monthly", 
                    "C19_Deaths_Age_State_Year_Only",
                    "C19_Deaths_Age_State")

save(list = these_two_save, file = here::here("data", "tidy_data", "CDC_Comorbid_dat.rda"))


# Work --------------------------------------------------------------------

C19_Deaths_Age_State_Year_Only %>% vis_dat()


Comorbidity_2020 <- C19_Deaths_Age_State_Year_Only %>%
  filter(Year == 2020)

Comorbidity_2020 %>% 
  filter(Age_Group == "All Ages", Condition != "COVID-19") %>%
  ggplot( aes(x = Condition, y = `COVID-19 Deaths`, fill = Condition)) +
  geom_col() +
  guides(fill = "none") +
  coord_flip() +
  labs( title = "Comorbidities and other conditions. For 2020, Age Group: All Ages",
        x = "Comorbodity / Condition (excluding C-19)", y = "Covid-19 Deaths")



Comorbidity_2021 <- C19_Deaths_Age_State_Year_Only %>%
  filter(Year == 2021)


Comorbidity_2022_April <- C19_Deaths_Age_State_Year_Only %>%
  filter(Year == 2022)

new_three <- c("Comorbidity_2020", "Comorbidity_2021", "Comorbidity_2022_April")

save(list = new_three, 
     \file = here::here("data", "tidy_data", "Comorbid_Yearly.rda" ))




# Simplify ----------------------------------------------------------------

rm(list = ls())


load(file = here::here("data", "tidy_data", "Comorbid_Yearly.rda" ))


# Percentages -------------------------------------------------------------

Comorbidity_2020_USA <- Comorbidity_2020 %>% filter(State == "United States")


check_per_2020 <- Comorbidity_2020_USA %>%
  filter(Condition == "COVID-19") %>%
  group_by(Age_Group) %>%
  mutate(Per_Death = `COVID-19 Deaths` / 385293 ) %>%
  ungroup()


remove_two <- c("Not stated", "All Ages")

check_per_2020 %>%
  filter(!Age_Group %in% remove_two ) %>%
  ggplot( aes(x = Age_Group, y = Per_Death, fill = rev(Per_Death))) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_c(option = "B") +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "USA, 2020: Covid Deaths (%) by Age Group",
       subtitle =" Source: cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source updated: 8 May 2022",
       y = "Percentage of Covid Deaths", x = "Age Group") +
  geom_text(aes(label = paste0(" ", ({Per_Death}  * 100) %>% round(2), "% ") ), size = 2.5, 
            vjust = -0.5, color= "red") 
  
  
Comorbidity_2021_USA <- Comorbidity_2021 %>% filter(State == "United States")


check_per_2021 <- Comorbidity_2021_USA %>%
  filter(Condition == "COVID-19") %>%
  group_by(Age_Group) %>%
  mutate(Per_Death = `COVID-19 Deaths` / 461706 ) %>%
  ungroup()


check_per_2020_plot <- check_per_2020 %>% 
  filter(!Age_Group %in% remove_two ) %>%
  select(Age_Group, Per_Death) %>%
  mutate(Per_Death = Per_Death * 100)

check_per_2021 %>% 
  select(Age_Group, Per_Death) %>%
  mutate(Per_Death = Per_Death * 100)

check_per_2021_plot <- check_per_2021 %>% 
  filter(!Age_Group %in% remove_two ) %>%
  select(Age_Group, Per_Death) %>%
  mutate(Per_Death = Per_Death * 100)

check_per_2021  %>%
  filter(!Age_Group %in% remove_two ) %>%
  ggplot( aes(x = Age_Group, y = Per_Death, fill = rev(Per_Death))) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_c(option = "B") +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "USA, 2021: Covid Deaths (%) by Age Group",
       subtitle =" Source: cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source updated: 8 May 2022",
       y = "Percentage of Covid Deaths", x = "Age Group") +
  geom_text(aes(label = paste0(" ", ({Per_Death}  * 100) %>% round(2), "% ") ), size = 2.5, 
            vjust = -0.5, color= "red") 


pop_age <- read_csv("raw_data/pop_age.txt")
sum(pop_age$Pop_Per)


pop_age$Deaths_2020 <- check_per_2020_plot$Per_Death 

pop_age$Deaths_2021 <- check_per_2021_plot$Per_Death 

pop_age

pop_age <- pop_age %>%
  mutate(ratio_2020 = (Deaths_2020 / Pop_Per),
         ratio_2021 = (Deaths_2021 / Pop_Per) )

pop_age

pop_age %>%
ggplot( aes(x = Age_Group, y = Pop_Per, fill = Pop_Per) ) +
  geom_col() +
  scale_fill_viridis_c(option = "C") +
  guides(fill = "none") +
  labs(title = "USA Population Percentage by CDC Age Group",
       subtitle = "Source: census.gov/data/tables/2019/demo/age-and-sex/2019-age-sex-composition.html",
       x = "Age Group", y = "Population Percentage",
       caption ="Data Humanist, CC0 (Public Domain)\nUSA Census Data, 2019") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(plot.subtitle = element_text (size = 7))



pop_age %>% 
  ggplot( aes(x = Age_Group, y = ratio_2020, fill = rev(ratio_2020) ) )+
  geom_col() +
  scale_fill_viridis_c(option = "A") +
  guides(fill = "none") +
  labs(title = "USA, 2020: Ratio of Covid Deaths % to Age Group Population %",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source updated: 8 May 2022",
       x = "CDC Age Groups", y = "Deaths % / Age Group Pop %") +
  geom_text(aes(label =  ({ratio_2020} %>% round(3) )), size = 3, 
            vjust = -0.5, color= "red") +
  theme_minimal()
  

 


# Plots -------------------------------------------------------------------


g_2020 <- check_per_2020 %>%
  filter(!Age_Group %in% remove_two ) %>%
  ggplot( aes(x = Age_Group, y = Per_Death, fill = rev(Per_Death))) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_c(option = "B") +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "USA, 2020: Covid Deaths (%) by Age Group",
       subtitle =" Source: cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source updated: 8 May 2022",
       y = "Percentage of Covid Deaths", x = "Age Group") +
  geom_text(aes(label = paste0(" ", ({Per_Death}  * 100) %>% round(2), "% ") ),
            size = 3, vjust = -0.5, color= "red") 




g_2021 <-check_per_2021  %>%
  filter(!Age_Group %in% remove_two ) %>%
  ggplot( aes(x = Age_Group, y = Per_Death, fill = rev(Per_Death))) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_c(option = "B") +
  theme_minimal() +
  guides(fill = "none") +
  labs(title = "USA, 2021: Covid Deaths (%) by Age Group",
       subtitle =" Source: cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source updated: 8 May 2022",
       y = "Percentage of Covid Deaths", x = "Age Group") +
  geom_text(aes(label = paste0(" ", ({Per_Death}  * 100) %>% round(2), "% ") ), 
            size = 3, 
            vjust = -0.5, color= "red") 




pop_base <- pop_age %>%
  ggplot( aes(x = Age_Group, y = Pop_Per, fill = Pop_Per) ) +
  geom_col() +
  scale_fill_viridis_c(option = "C") +
  guides(fill = "none") +
  labs(title = "USA Population Percentage by CDC Age Group",
       subtitle = "Source: census.gov/data/tables/2019/demo/age-and-sex/2019-age-sex-composition.html",
       x = "Age Group", y = "Population Percentage",
       caption ="Data Humanist, CC0 (Public Domain)\nUSA Census Data, 2019") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(plot.subtitle = element_text (size = 7))
# If this plot looks different than the graph say at Wi


pop_2020 <- pop_age %>% 
  ggplot( aes(x = Age_Group, y = ratio_2020, fill = rev(ratio_2020) ) )+
  geom_col() +
  scale_fill_viridis_c(option = "A") +
  guides(fill = "none") +
  labs(title = "USA, 2020: Ratio of Covid Deaths % to Age Group Population %",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source updated: 8 May 2022",
       subtitle =" Sources: cdc.gov/nchs/ & census.gov/data",
       x = "CDC Age Groups", y = "Deaths % / Age Group Pop %") +
  geom_text(aes(label =  ({ratio_2020} %>% round(3) )), size = 3, 
            vjust = -0.5, color= "red") +
  theme_minimal()


pop_2021 <- pop_age %>% 
  ggplot( aes(x = Age_Group, y = ratio_2021, fill = rev(ratio_2021 ))) +
  geom_col() +
  guides(fill = "none") +
  scale_fill_viridis_c(option = "A") +
  labs(title = "USA, 2021: Ratio of Covid Deaths % to Age Group Population %",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source updated: 8 May 2022",
       subtitle =" Sources: cdc.gov/nchs/ & census.gov/data",
       x = "CDC Age Groups", y = "Deaths % / Age Group Pop %") +
  geom_text(aes(label =  ({ratio_2021} %>% round(3) )), size = 3, 
            vjust = -0.5, color= "red") +
  theme_minimal()




com_20_024 <- Comorbidity_2020 %>% 
  filter(Age_Group == "0-24",  Condition != "COVID-19") %>%
  ggplot( aes(x = Condition, y = `COVID-19 Deaths`, fill = Condition)) +
  geom_col() +
  guides(fill = "none") +
  coord_flip() +
  labs( title = "USA, 2020. Covid Deaths: Comorbidities & Conditions. Age Group: 0-24",
        x = "Comorbodity / Condition (excluding C-19)", 
        y = "Death Certificate Report Incidents",
        subtitle =" Source: cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm",
        caption ="Data Humanist, CC0 (Public Domain)\nCDC source updated: 8 May 2022") +
  geom_hline(yintercept = 759, lty = 2, color = "red") +
  annotate(geom = "text", label = "759\nDeaths\nTotal",
           y = 730, x = "Influenza and pneumonia" , size = 3) 



com_20_075 <- Comorbidity_2020 %>% 
  filter(Age_Group == "75-84",  Condition != "COVID-19") %>%
  ggplot( aes(x = Condition, y = `COVID-19 Deaths`, fill = Condition)) +
  geom_col() +
  guides(fill = "none") +
  coord_flip() +
  labs( title = "USA, 2020. Covid Deaths: Comorbidities & Conditions. Age Group: 75-84",
        x = "Comorbodity / Condition (excluding C-19)", 
        y = "Death Certificate Report Incidents",
        subtitle =" Source: cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm",
        caption ="Data Humanist, CC0 (Public Domain)\nCDC source updated: 8 May 2022") +
  geom_hline(yintercept = 106207, lty = 2, color = "red") +
  annotate(geom = "text", label = "106207\nDeaths\nTotal",
           y = 101200, x = "Sepsis" , size = 3) 






save.image("~/R_STUDIO/Covid/data/mess_pediatrics/my_mess.RData")


g_2020 

g_2021 

pop_2020 
pop_base
pop_2021

com_20_024 
com_20_075









pop_basea <- pop_age %>%
  ggplot( aes(x = Age_Group, y = Pop_Per, fill = Age_Group) ) +
  geom_col() +
  scale_fill_viridis_c(option = "C") +
  guides(fill = "none") +
  labs(title = "USA Population Percentage by CDC Age Group",
       subtitle = "Source: census.gov/data/tables/2019/demo/age-and-sex/2019-age-sex-composition.html",
       x = "Age Group", y = "Population Percentage",
       caption ="Data Humanist, CC0 (Public Domain)\nUSA Census Data, 2019") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(plot.subtitle = element_text (size = 7))


pop_2020a <- pop_age %>% 
  ggplot( aes(x = Age_Group, y = ratio_2020, fill = Age_Group ) )+
  geom_col() +
  #scale_fill_viridis_c(option = "A") +
  guides(fill = "none") +
  labs(title = "USA, 2020: Ratio of Covid Deaths % to Age Group Population %",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source updated: 8 May 2022",
       subtitle =" Sources: cdc.gov/nchs/ & census.gov/data",
       x = "CDC Age Groups", y = "Deaths % / Age Group Pop %") +
  geom_text(aes(label =  ({ratio_2020} %>% round(3) )), size = 3, 
            vjust = -0.5, color= "red") +
  theme_minimal()


pop_2021a <- pop_age %>% 
  ggplot( aes(x = Age_Group, y = ratio_2021, fill =  Age_Group)) +
  geom_col() +
  guides(fill = "none") +
  #scale_fill_viridis_c(option = "A") +
  labs(title = "USA, 2021: Ratio of Covid Deaths % to Age Group Population %",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source updated: 8 May 2022",
       subtitle =" Sources: cdc.gov/nchs/ & census.gov/data",
       x = "CDC Age Groups", y = "Deaths % / Age Group Pop %") +
  geom_text(aes(label =  ({ratio_2021} %>% round(3) )), size = 3, 
            vjust = -0.5, color= "red") +
  theme_minimal()

pop_basea 
pop_2020a 
pop_2021a 


check <- Comorbidity_2020 %>% 
  filter(Age_Group == "0-24",  Condition != "COVID-19") %>%
  group_by(Condition) %>%
  replace_na(list(`COVID-19 Deaths` = 0) ) %>%
  summarize(ct = sum(`COVID-19 Deaths`))





Comorbidity_2020 %>% 
  filter(Age_Group == "0-24",  Condition != "COVID-19") %>%
  group_by(Condition) %>% count()


load(file = here::here("data", "tidy_data", "CDC_Comorbid_dat.rda"))



# Working on Yearly Totals Only -------------------------------------------

C19_Deaths_Age_State_Year_Only %>% glimpse()

C19_Deaths_Age_State_Year_Only %>% vis_dat()

C19_Deaths_Age_State_Year_Only <- C19_Deaths_Age_State_Year_Only %>% 
  replace_na(list(Covid_Deaths = 0) )

C19_Deaths_Age_State_Year_Only <- C19_Deaths_Age_State_Year_Only %>% 
  replace_na(list(Mentions_N = 0) )

C19_Deaths_Age_State_Year_Only %>% vis_dat()

C19_Deaths_Age_State <- C19_Deaths_Age_State %>%
  replace_na(list(Covid_Deaths = 0, Mentions_N = 0) )

C19_Deaths_Age_State_Totals_Monthly <- C19_Deaths_Age_State_Totals_Monthly %>%
  replace_na(list(Covid_Deaths = 0, Mentions_N = 0) )




these_three_save <- c("C19_Deaths_Age_State_Totals_Monthly", 
                    "C19_Deaths_Age_State_Year_Only",

                    "C19_Deaths_Age_State")


save(list = these_three_save, file = here::here("data", "tidy_data", "CDC_Comorbid_dat.rda"))


