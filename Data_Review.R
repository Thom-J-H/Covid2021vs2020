
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(here)
library(visdat)

here()

load(here::here("tidy_data", "project_data.RData"))



# Data Sets ---------------------------------------------------------------
#
# ## PLEASE NOTE :  My_data derives DIRECTLY from the CDC Source :
#                 Provisional Death Counts for COVID-19
## The risk ratio were calculated and added.  For plotting purposes, I have stacked data
#      for the year 2021 ABOVE data for year 2020.  The retrieved data is otherwise
#      is UNCHANGED.  You may check against the original for the Sex, Year, and Age Ranges
#     listed below
#   
#     Sex == Male & Female.  This data set does NOT include the "All Sexes" data
#
##  The CDC claims the data valid as of 2022-January-19


My_data %>% vis_dat()

My_data %>% glimpse()

My_data %>% skimr::skim()


## We also want the data in long format.  I have done this work elsewhere.

My_data_long %>% vis_dat()

My_data_long %>% glimpse()

My_data_long %>% skimr::skim()


#  All Sexes


All_Sexes %>% vis_dat()

All_Sexes %>% glimpse()

All_Sexes %>% skimr::skim()


# And we also want a long form -- Tidy work done elsewhere

All_Sexes_long %>% vis_dat()

All_Sexes_long %>% glimpse()

All_Sexes_long %>% skimr::skim()

# Basic Manipulation ------------------------------------------------------


## We want the relative risk from one year to the next
## These means comparing the Covid death percentages as a ratio

# This is being reproduced here.  The results are already part of
# the saved Tidy data image: project_data.RData


My_data_2020_base <- My_data %>% 
  filter(Year == 2020) %>%
  select(Year, Sex, Age_Range, C19_Ratio, Not_C19_Rat)

My_data_2021_base <- My_data %>% 
  filter(Year == 2021) %>%
  select(Year, Sex, Age_Range, C19_Ratio, Not_C19_Rat)

Percent_Change <- (My_data_2021_base$C19_Ratio - My_data_2020_base$C19_Ratio) %>%
  enframe(name = NULL, value = "Per_Change")

Relative_Risk <- (My_data_2021_base$C19_Ratio / My_data_2020_base$C19_Ratio) %>%
  enframe(name = NULL, value = "Relative_Risk")


Comp_2021_vs_2020 <- My_data_2021_base %>%
  select(-c("C19_Ratio" , "Not_C19_Rat"))  %>%
  add_column(Percent_Change ,Relative_Risk )

# We can now clean up a bit.

del_list = c("My_data_2020_base", "My_data_2021_base",
            "Percent_Change", "Relative_Risk")

rm(list = del_list)

rm(del_list)



# All Sexes data ----------------------------------------------------------

All_Sexes_2020_base <- All_Sexes %>% 
  filter(Year == 2020) %>%
  select(Year, Age_Range, C19_Ratio, Not_C19_Rat)

All_Sexes_2021_base <- All_Sexes  %>% 
  filter(Year == 2021) %>%
  select(Year, Age_Range, C19_Ratio, Not_C19_Rat)

Percent_Change <- (All_Sexes_2021_base$C19_Ratio - All_Sexes_2020_base$C19_Ratio) %>%
  enframe(name = NULL, value = "Per_Change")

Relative_Risk <- (All_Sexes_2021_base$C19_Ratio / All_Sexes_2020_base$C19_Ratio) %>%
  enframe(name = NULL, value = "Relative_Risk")


Comp_2021_vs_2020_ALL <- All_Sexes_2021_base %>%
  select(-c("C19_Ratio" , "Not_C19_Rat"))  %>%
  add_column(Percent_Change ,Relative_Risk )


## Check!
Comp_2021_vs_2020_ALL 

# Clean UP

del_list = c("All_Sexes_2020_base", "All_Sexes_2021_base",
             "Percent_Change", "Relative_Risk")

rm(list = del_list)

rm(del_list)

# Save and get on to graphs and more
save.image(here::here("tidy_data", "project_data.RData"))






