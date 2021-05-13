library(tidyverse)
library(readxl)
library(httr)
library("feather")

pop_eng_2019 <- read_excel('/data/data-lake/raw/ons-populstion-estimates-mid-year-2019/2021-04-12-10-30-27/ons-populstion-estimates-mid-year-2019.xlsx', sheet = "Mid-2019 Persons", skip = 4)
# in vac data - this population data is referenced - https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates

ages <- pop_eng_2019 %>% select(8:ncol(.))
under_45 <- ages %>% select(19:45) %>%  mutate(total_pop_by_area = rowSums(across(where(is.numeric)))) %>%
  mutate(total_pop_age_range = sum(total_pop_by_area)) %>%
  select(total_pop_age_range) %>%
  mutate('age_range'='under 45') %>%
  head(n=1)
age_45_49 <- ages %>% select(46:50)  %>%  mutate(total_pop_by_area = rowSums(across(where(is.numeric)))) %>%
  mutate(total_pop_age_range = sum(total_pop_by_area)) %>%
  select(total_pop_age_range) %>%
  mutate('age_range'='45-49')  %>%
  head(n=1)
age_50_54 <- ages %>% select(51:55) %>%  mutate(total_pop_by_area = rowSums(across(where(is.numeric)))) %>%
  mutate(total_pop_age_range = sum(total_pop_by_area)) %>%
  select(total_pop_age_range) %>%
  mutate('age_range'='50-54')  %>%
  head(n=1)
age_55_59 <- ages %>% select(56:60) %>%  mutate(total_pop_by_area = rowSums(across(where(is.numeric)))) %>%
  mutate(total_pop_age_range = sum(total_pop_by_area)) %>%
  select(total_pop_age_range) %>%
  mutate('age_range'='55-59')  %>%
  head(n=1)
age_60_64 <- ages %>% select(61:65) %>%  mutate(total_pop_by_area = rowSums(across(where(is.numeric)))) %>%
  mutate(total_pop_age_range = sum(total_pop_by_area)) %>%
  select(total_pop_age_range) %>%
  mutate('age_range'='60-64')  %>%
  head(n=1)
age_65_69 <- ages %>% select(66:70) %>%  mutate(total_pop_by_area = rowSums(across(where(is.numeric)))) %>%
  mutate(total_pop_age_range = sum(total_pop_by_area)) %>%
  select(total_pop_age_range) %>%
  mutate('age_range'='65-69')  %>%
  head(n=1)
age_70_74 <- ages %>% select(71:75) %>%  mutate(total_pop_by_area = rowSums(across(where(is.numeric)))) %>%
  mutate(total_pop_age_range = sum(total_pop_by_area)) %>%
  select(total_pop_age_range) %>%
  mutate('age_range'='70-74')  %>%
  head(n=1)
age_75_79 <- ages %>% select(75:79) %>%  mutate(total_pop_by_area = rowSums(across(where(is.numeric)))) %>%
  mutate(total_pop_age_range = sum(total_pop_by_area)) %>%
  select(total_pop_age_range) %>%
  mutate('age_range'='75-79')  %>%
  head(n=1)
age_80_over <- ages %>% select(81:ncol(.)) %>% mutate(total_pop_by_area = rowSums(across(where(is.numeric)))) %>%
  mutate(total_pop_age_range = sum(total_pop_by_area)) %>%
  select(total_pop_age_range) %>%
  mutate('age_range'='80+')  %>%
  head(n=1)


populations_by_age <- rbind(under_45,
                            age_45_49,
                            age_50_54,
                            age_55_59,
                            age_60_64,
                            age_65_69,
                            age_70_74,
                            age_75_79,
                            age_80_over)

# --- vaccination data ---
# --- now retrieving from raw section --- 
get_requests <- list.dirs('/data/data-lake/raw/nhs-weekly-vaccination-data/')

file_name <- paste(tail(get_requests, n=1), 'nhs_weekly_vaccination_data.xlsx', sep='/')

vac_data <- read_excel(file_name,
                       sheet = "Gender, Age & Region", skip = 13)

get_age_total <- head(vac_data, 1)

# not repeatable
first_dose_vaccines <- get_age_total %>% select(3:20)
second_dose_vaccines <- get_age_total %>% select(22:39)

# sum the pairs of vaccinations
#https://stackoverflow.com/questions/35083166/sum-column-every-n-column-in-a-data-frame-r
first_dose <- (first_dose_vaccines[1:(ncol(first_dose_vaccines)-1)] + first_dose_vaccines[2:ncol(first_dose_vaccines)])[c(T,F)]
second_dose <- (second_dose_vaccines[1:(ncol(second_dose_vaccines)-1)] + second_dose_vaccines[2:ncol(second_dose_vaccines)])[c(T,F)]

# combine both doses 
first_dose <- first_dose %>% mutate('dose'='First dose')
second_dose <- second_dose %>% mutate('dose'='Second dose')

# add headers
col_headers<- c("under 45", "45-49", "50-54", '55-59',"60-64",'65-69','70-74','75-79','80+', 'dose')

colnames(first_dose) <- col_headers
colnames(second_dose) <- col_headers

both_doses <- rbind(first_dose, second_dose)

# transpose longer
both_doses_tr <- pivot_longer(both_doses, cols=1:9, names_to='age_range', values_to='number_of_doses')

# join data sets
doses_by_population <- left_join(both_doses_tr, populations_by_age, by='age_range')

final_doses_by_population <- doses_by_population %>%
  mutate(prop_of_population = round((number_of_doses/total_pop_age_range)*100,1))

#glimpse(final_doses_by_population)
write_feather(final_doses_by_population, '/home/izzy-everall/areas2focus_data/vaccination_rate.feather')




