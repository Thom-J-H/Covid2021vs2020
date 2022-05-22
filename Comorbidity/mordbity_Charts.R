library(tidyverse)
library(here)
library(visdat)
library(scales)
library(glue)
library(ggthemes)

#save(C19_Deaths_Age_USA_Year, 
#     file = here::here("data", "tidy_data", "C19_Deaths_Age_USA_Year.rda") )

#write.csv(C19_Deaths_Age_USA_Year, 
#          file = here::here("data", "tidy_data", "C19_Deaths_Age_USA_Year.csv"))


load(file = here::here("data", "tidy_data", "C19_Deaths_Age_USA_Year.rda"))


Deaths_All <- C19_Deaths_Age_USA_Year %>%
  filter(Condition == "COVID-19") %>%
  group_by(Year, Age_Group, Condition) %>%
  summarize(Deaths_Covid = sum(Covid_Deaths))

Condition_Reports_All <- C19_Deaths_Age_USA_Year %>%
  filter(Condition != "COVID-19") %>%
  group_by(Year, Age_Group, Condition) %>%
  summarize(Reports = sum(Covid_Deaths),
            Mentions = sum(Mentions_N))

Deaths_All <- Deaths_All %>% ungroup()
Condition_Reports_All <- Condition_Reports_All %>% ungroup()


# Start -------------------------------------------------------------------


report_20_24 <- Condition_Reports_All %>% 
  filter(Age_Group == "0-24") %>%
  filter(Year == 2020) %>% 
  ggplot( aes(x = Condition, y = Reports, fill = Condition)) +
  geom_col() +
  guides(fill = "none") +
  coord_flip() +
  labs(title = "Covid Deaths in 2020, Age Group 0-24: Other Conditions Reported",
        x = "", 
        y = "Reported on Death Certificate (Tally)",
        subtitle ="Source: cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm#Comorbidities",
    caption ="Data Humanist, CC0 (Public Domain)\nCDC source accessed: 18 May 2022") +
  geom_text(aes(label = {Reports} ), size = 2.5, 
            hjust = -0.5, color= "red") +
  theme_minimal() 


report_20_24 <- report_20_24 +
  geom_hline(yintercept = 759, lty = 2, color = "red") +
  annotate(geom = "text", label = "759\nDeaths\nTotal",
           y = 725, x = "Influenza and pneumonia" , size = 3) 

report_20_24 +
  scale_y_continuous(breaks =c(0, 100, seq(200, 800, by = 200)))





# next --------------------------------------------------------------------



report_20_34 <- Condition_Reports_All %>% 
  filter(Age_Group == "25-34") %>%
  filter(Year == 2020) %>% 
  ggplot( aes(x = Condition, y = Reports, fill = Condition)) +
  geom_col() +
  guides(fill = "none") +
  coord_flip() +
  labs(title = "Covid Deaths in 2020, Age Group 25-34: Other Conditions Reported",
       x = "", 
       y = "Reported on Death Certificate (Tally)",
       subtitle = "Source: cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm#Comorbidities",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source accessed: 18 May 2022") +
  geom_text(aes(label = {Reports} ), size = 2.5, 
            hjust = -0.5, color= "red") +
  theme_minimal()   


report_20_34 <- report_20_34 +
  geom_hline(yintercept = 2620, lty = 2, color = "red") +
  annotate(geom = "text", label = "2,620\nDeaths\nTotal",
           y = 2500, x = "Influenza and pneumonia" , size = 3) 

report_20_34 +
  scale_y_continuous(breaks =c(0, 500, seq(1000, 3000, by = 1000)))





# next --------------------------------------------------------------------


report_20_44 <- Condition_Reports_All %>% 
  filter(Age_Group == "35-44") %>%
  filter(Year == 2020) %>% 
  ggplot( aes(x = Condition, y = Reports, fill = Condition)) +
  geom_col() +
  guides(fill = "none") +
  coord_flip() +
  labs(title = "Covid Deaths in 2020, Age Group 35-44: Other Conditions Reported",
       x = "", 
       y = "Reported on Death Certificate (Tally)",
       subtitle = "Source: cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm#Comorbidities",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source accessed: 18 May 2022") +
  geom_text(aes(label = {Reports} ), size = 2.5, 
            hjust = -0.5, color= "red") +
  theme_minimal()   


report_20_44 <- report_20_44  +
  geom_hline(yintercept = 6779, lty = 2, color = "red") +
  annotate(geom = "text", label = "6,779\nDeaths\nTotal",
           y = 6460, x = "Influenza and pneumonia" , size = 3) 

report_20_44  +
  scale_y_continuous(breaks =c(0, 1000, seq(2000, 8000, by = 2000)))





# next --------------------------------------------------------------------


report_20_54 <- Condition_Reports_All %>% 
  filter(Age_Group == "45-54") %>%
  filter(Year == 2020) %>% 
  ggplot( aes(x = Condition, y = Reports, fill = Condition)) +
  geom_col() +
  guides(fill = "none") +
  coord_flip() +
  labs(title = "Covid Deaths in 2020, Age Group 45-54: Other Conditions Reported",
       x = "", 
       y = "Reported on Death Certificate (Tally)",
       subtitle = "Source: cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm#Comorbidities",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source accessed: 18 May 2022") +
  geom_text(aes(label = {Reports} ), size = 2.5, 
            hjust = -0.5, color= "red") +
  theme_minimal()   


report_20_54  <- report_20_54  +
  geom_hline(yintercept = 18320, lty = 2, color = "red") +
  annotate(geom = "text", label = "18,320\nDeaths\nTotal",
           y = 17390, x = "Influenza and pneumonia" , size = 3) 

report_20_54 +
  scale_y_continuous(breaks =c(0, 2500, seq(5000, 20000, by = 5000)))





# Next --------------------------------------------------------------------



report_20_64 <- Condition_Reports_All %>% 
  filter(Age_Group == "55-64") %>%
  filter(Year == 2020) %>% 
  ggplot( aes(x = Condition, y = Reports, fill = Condition)) +
  geom_col() +
  guides(fill = "none") +
  coord_flip() +
  labs(title = "Covid Deaths in 2020, Age Group 55-64: Other Conditions Reported",
       x = "", 
       y = "Reported on Death Certificate (Tally)",
       subtitle = "Source: cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm#Comorbidities",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source accessed: 18 May 2022") +
  geom_text(aes(label = {Reports} ), size = 2.5, 
            hjust = -0.5, color= "red") +
  theme_minimal()   


report_20_64  <- report_20_64  +
  geom_hline(yintercept = 45553, lty = 2, color = "red") +
  annotate(geom = "text", label = "45,553\nDeaths\nTotal",
           y = 43420, x = "Influenza and pneumonia" , size = 3) 

report_20_64 +
  scale_y_continuous(breaks =c(0, 5000, seq(10000, 50000, by = 10000)))









# next --------------------------------------------------------------------




report_20_74 <- Condition_Reports_All %>% 
  filter(Age_Group == "65-74") %>%
  filter(Year == 2020) %>% 
  ggplot( aes(x = Condition, y = Reports, fill = Condition)) +
  geom_col() +
  guides(fill = "none") +
  coord_flip() +
  labs(title = "Covid Deaths in 2020, Age Group 65-74: Other Conditions Reported",
       x = "", 
       y = "Reported on Death Certificate (Tally)",
       subtitle = "Source: cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm#Comorbidities",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source accessed: 18 May 2022") +
  geom_text(aes(label = {Reports} ), size = 2.5, 
            hjust = -0.5, color= "red") +
  theme_minimal()   


report_20_74   <- report_20_74   +
  geom_hline(yintercept = 82259, lty = 2, color = "red") +
  annotate(geom = "text", label = "82,259\nDeaths\nTotal",
           y = 78200, x = "Influenza and pneumonia" , size = 3) 

report_20_74 +
  scale_y_continuous(breaks =c(0, 10000, seq(20000, 100000, by = 20000)))







# Next --------------------------------------------------------------------




report_20_84 <- Condition_Reports_All %>% 
  filter(Age_Group == "75-84") %>%
  filter(Year == 2020) %>% 
  ggplot( aes(x = Condition, y = Reports, fill = Condition)) +
  geom_col() +
  guides(fill = "none") +
  coord_flip() +
  labs(title = "Covid Deaths in 2020, Age Group 75-84: Other Conditions Reported",
       x = "", 
       y = "Reported on Death Certificate (Tally)",
       subtitle = "Source: cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm#Comorbidities",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source accessed: 18 May 2022") +
  geom_text(aes(label = {Reports} ), size = 2.5, 
            hjust = -0.5, color= "red") +
  theme_minimal()   


report_20_84   <- report_20_84    +
  geom_hline(yintercept = 106207, lty = 2, color = "red") +
  annotate(geom = "text", label = "106,207\nDeaths\nTotal",
           y = 100600, x = "Influenza and pneumonia" , size = 3) 

report_20_84 +
  scale_y_continuous(breaks =c(0, 15000, 30000, 60000, 90000, 120000))



# next --------------------------------------------------------------------



report_20_85 <- Condition_Reports_All %>% 
  filter(Age_Group == "85+") %>%
  filter(Year == 2020) %>% 
  ggplot( aes(x = Condition, y = Reports, fill = Condition)) +
  geom_col() +
  guides(fill = "none") +
  coord_flip() +
  labs(title =  "Covid Deaths in 2020, Age Group 85+: Other Conditions Reported",
       x = "", 
       y = "Reported on Death Certificate (Tally)",
       subtitle = "Source: cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm#Comorbidities",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source accessed: 18 May 2022") +
  geom_text(aes(label = {Reports} ), size = 2.5, 
            hjust = -0.5, color= "red") +
  theme_minimal()   


report_20_85   <- report_20_85    +
  geom_hline(yintercept = 122785, lty = 2, color = "red") +
  annotate(geom = "text", label = "122,785\nDeaths\nTotal",
           y = 115600, x = "Influenza and pneumonia" , size = 3) 


report_20_85 +  
  scale_y_continuous(breaks =c(0, 12500, seq(25000, 150000, by = 25000)))



# 2021 --------------------------------------------------------------------





report_21_24 <- Condition_Reports_All %>% 
  filter(Age_Group == "0-24") %>%
  filter(Year == 2021) %>% 
  ggplot( aes(x = Condition, y = Reports, fill = Condition)) +
  geom_col() +
  guides(fill = "none") +
  coord_flip() +
  labs(title =  "Covid Deaths in 2021, Age Group 0-24: Other Conditions Reported",
       x = "", 
       y = "Reported on Death Certificate (Tally)",
       subtitle ="Source: cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm#Comorbidities",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source accessed: 18 May 2022") +
  geom_text(aes(label = {Reports} ), size = 2.5, 
            hjust = -0.5, color= "red") +
  theme_minimal()   


report_21_24 <- report_21_24 +
  geom_hline(yintercept = 2021, lty = 2, color = "red") +
  annotate(geom = "text", label = "2,021\nDeaths\nTotal",
           y = 1920, x = "Influenza and pneumonia" , size = 3) 

report_21_24 +
  scale_y_continuous(breaks =c(0, 250, seq(500, 2500, by = 500)))




# next --------------------------------------------------------------------






report_21_34 <- Condition_Reports_All %>% 
  filter(Age_Group == "25-34") %>%
  filter(Year == 2021) %>% 
  ggplot( aes(x = Condition, y = Reports, fill = Condition)) +
  geom_col() +
  guides(fill = "none") +
  coord_flip() +
  labs(title = "Covid Deaths in 2021, Age Group 25-34: Other Conditions Reported",
       x = "", 
       y = "Reported on Death Certificate (Tally)",
       subtitle = "Source: cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm#Comorbidities",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source accessed: 18 May 2022") +
  geom_text(aes(label = {Reports} ), size = 2.5, 
            hjust = -0.5, color= "red") +
  theme_minimal()   


report_21_34  <- report_21_34  +
  geom_hline(yintercept =  6871, lty = 2, color = "red") +
  annotate(geom = "text", label = "6,871\nDeaths\nTotal",
           y = 6520, x = "Influenza and pneumonia" , size = 3) 

report_21_34 +
  scale_y_continuous(breaks =c(0, 1000, seq(2000, 8000, by = 2000)))







report_21_44 <- Condition_Reports_All %>% 
  filter(Age_Group == "35-44") %>%
  filter(Year == 2021) %>% 
  ggplot( aes(x = Condition, y = Reports, fill = Condition)) +
  geom_col() +
  guides(fill = "none") +
  coord_flip() +
  labs(x = "", 
       y = "Reported on Death Certificate (Tally)",
       subtitle = "Source: cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm#Comorbidities",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source accessed: 18 May 2022",
       title = "Covid Deaths in 2021, Age Group 35-44: Other Conditions Reported")  +
  geom_text(aes(label = {Reports} ), size = 2.5, 
            hjust = -0.5, color= "red") +
  theme_minimal()   


report_21_44 <- report_21_44 +
  geom_hline(yintercept = 17291, lty = 2, color = "red") +
  annotate(geom = "text", label = "17,291\nDeaths\nTotal",
           y = 16450, x = "Influenza and pneumonia" , size = 3) 

report_21_44 +
  scale_y_continuous(breaks =c(0, 2500, seq(5000, 20000, by = 5000)))


# Next --------------------------------------------------------------------



report_21_54 <- Condition_Reports_All %>% 
  filter(Age_Group == "45-54") %>%
  filter(Year == 2021) %>% 
  ggplot( aes(x = Condition, y = Reports, fill = Condition)) +
  geom_col() +
  guides(fill = "none") +
  coord_flip() +
  labs(title = "Covid Deaths in 2021, Age Group 45-54: Other Conditions Reported",
       x = "", 
       y = "Reported on Death Certificate (Tally)",
       subtitle = "Source: cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm#Comorbidities",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source accessed: 18 May 2022") +
  geom_text(aes(label = {Reports} ), size = 2.5, 
            hjust = -0.5, color= "red") +
  theme_minimal()   


report_21_54  <- report_21_54  +
  geom_hline(yintercept = 39378, lty = 2, color = "red") +
  annotate(geom = "text", label = "39,378\nDeaths\nTotal",
           y = 37200, x = "Influenza and pneumonia" , size = 3) 

report_21_54  +
  scale_y_continuous(breaks =c(0, 5000, seq(10000, 50000, by = 10000)))





# Next --------------------------------------------------------------------



report_21_64 <- Condition_Reports_All %>% 
  filter(Age_Group == "55-64") %>%
  filter(Year == 2021) %>% 
  ggplot( aes(x = Condition, y = Reports, fill = Condition)) +
  geom_col() +
  guides(fill = "none") +
  coord_flip() +
  labs(title = "Covid Deaths in 2021, Age Group 55-64: Other Conditions Reported",
       x = "", 
       y = "Reported on Death Certificate (Tally)",
       subtitle = "Source: cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm#Comorbidities",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source accessed: 18 May 2022") +
  geom_text(aes(label = {Reports} ), size = 2.5, 
            hjust = -0.5, color= "red") +
  theme_minimal()   


report_21_64  <- report_21_64  +
  geom_hline(yintercept = 79226, lty = 2, color = "red") +
  annotate(geom = "text", label = "79,226\nDeaths\nTotal",
           y = 75200, x = "Influenza and pneumonia" , size = 3) 

report_21_64 +
  scale_y_continuous(breaks =c(0, 10000, seq(20000, 100000, by = 20000)))



# Next ---------------------------------------------------------------------



report_21_74 <- Condition_Reports_All %>% 
  filter(Age_Group == "65-74") %>%
  filter(Year == 2021) %>% 
  ggplot( aes(x = Condition, y = Reports, fill = Condition)) +
  geom_col() +
  guides(fill = "none") +
  coord_flip() +
  labs(title = "Covid Deaths in 2021, Age Group 65-74: Other Conditions Reported",
       x = "", 
       y = "Reported on Death Certificate (Tally)",
       subtitle = "Source: cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm#Comorbidities",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source accessed: 18 May 2022") +
  geom_text(aes(label = {Reports} ), size = 2.5, 
            hjust = -0.5, color= "red") +
  theme_minimal()   


report_21_74   <- report_21_74   +
  geom_hline(yintercept = 111443, lty = 2, color = "red") +
  annotate(geom = "text", label = "111,443\nDeaths\nTotal",
           y = 105050, x = "Influenza and pneumonia" , size = 3) 

report_21_74 +
  scale_y_continuous(breaks =c(0, 15000, seq(30000, 120000, by = 30000)))



# Next --------------------------------------------------------------------


report_21_84 <- Condition_Reports_All %>% 
  filter(Age_Group == "75-84") %>%
  filter(Year == 2021) %>% 
  ggplot( aes(x = Condition, y = Reports, fill = Condition)) +
  geom_col() +
  guides(fill = "none") +
  coord_flip() +
  labs(title = "Covid Deaths in 2021, Age Group 75-84: Other Conditions Reported",
       x = "", 
       y = "Reported on Death Certificate (Tally)",
       subtitle = "Source: cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm#Comorbidities",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source accessed: 18 May 2022") +
  geom_text(aes(label = {Reports} ), size = 2.5, 
            hjust = -0.5, color= "red") +
  theme_minimal()   


report_21_84   <- report_21_84    +
  geom_hline(yintercept = 110512, lty = 2, color = "red") +
  annotate(geom = "text", label = "110,512\nDeaths\nTotal",
           y = 104000, x = "Influenza and pneumonia" , size = 3) 

report_21_84 +
  scale_y_continuous(breaks =c(0, 15000, seq(30000, 120000, by = 30000)))





# ext ---------------------------------------------------------------------



report_21_85 <- Condition_Reports_All %>% 
  filter(Age_Group == "85+") %>%
  filter(Year == 2021) %>% 
  ggplot( aes(x = Condition, y = Reports, fill = Condition)) +
  geom_col() +
  guides(fill = "none") +
  coord_flip() +
  labs(title =  "Covid Deaths in 2021, Age Group 85+: Other Conditions Reported",
       x = "", 
       y = "Reported on Death Certificate (Tally)",
       subtitle = "Source: cdc.gov/nchs/nvss/vsrr/covid_weekly/index.htm#Comorbidities",
       caption ="Data Humanist, CC0 (Public Domain)\nCDC source accessed: 18 May 2022") +
  geom_text(aes(label = {Reports} ), size = 2.5, 
            hjust = -0.5, color= "red") +
  theme_minimal()   


report_21_85   <- report_21_85    +
  geom_hline(yintercept = 95037, lty = 2, color = "red") +
  annotate(geom = "text", label = "95,037\nDeaths\nTotal",
           y = 90450, x = "Influenza and pneumonia" , size = 3) 


report_21_85 +  
  scale_y_continuous(breaks =c(0, 12500, seq(25000, 100000, by = 25000)))



# My final versions -------------------------------------------------------







#0-24
report_20_24 +
  scale_y_continuous(breaks =c(0, 100, seq(200, 800, by = 200)))

#25-34
report_20_34 +
  scale_y_continuous(breaks =c(0, 500, seq(1000, 3000, by = 1000)))

#35-44
report_20_44  +
  scale_y_continuous(breaks =c(0, 1000, seq(2000, 8000, by = 2000)))

#45-54
report_20_54 +
  scale_y_continuous(breaks =c(0, 2500, seq(5000, 20000, by = 5000)))

#55-64
report_20_64 +
  scale_y_continuous(breaks =c(0, 5000, seq(10000, 50000, by = 10000)))

#65-74
report_20_74 +
  scale_y_continuous(breaks =c(0, 10000, seq(20000, 100000, by = 20000)))

#75-84
report_20_84 +
  scale_y_continuous(breaks =c(0, 15000, 30000, 60000, 90000, 120000))

#85+
report_20_85 +  
  scale_y_continuous(breaks =c(0, 12500, seq(25000, 150000, by = 25000)))


#
#

#0-24
report_21_24 +
  scale_y_continuous(breaks =c(0, 250, seq(500, 2500, by = 500)))

#25-34
report_21_34 +
  scale_y_continuous(breaks =c(0, 1000, seq(2000, 8000, by = 2000)))

#35-44
report_21_44 +
  scale_y_continuous(breaks =c(0, 2500, seq(5000, 20000, by = 5000)))

#45-54
report_21_54  +
  scale_y_continuous(breaks =c(0, 5000, seq(10000, 50000, by = 10000)))

#55-64
report_21_64 +
  scale_y_continuous(breaks =c(0, 10000, seq(20000, 100000, by = 20000)))


#65-74
report_21_74 +
  scale_y_continuous(breaks =c(0, 15000, seq(30000, 130000, by = 30000)))

#75-84
report_21_84 +
  scale_y_continuous(breaks =c(0, 15000, seq(30000, 120000, by = 30000)))



#85+
report_21_85 +  
  scale_y_continuous(breaks =c(0, 12500, seq(25000, 100000, by = 25000)))


# data tables -------------------------------------------------------------


Condition_Reports_All %>%
  filter(Year != 2022) %>%
  group_by(Age_Group, Year) %>%
  slice_max(Reports, n = 5) %>%
  DT::datatable()

Condition_Reports_All %>%
  filter(Year != 2022) %>%
  group_by(Age_Group, Year) %>%
  slice_min(Reports, n = 5) %>%
  DT::datatable()


