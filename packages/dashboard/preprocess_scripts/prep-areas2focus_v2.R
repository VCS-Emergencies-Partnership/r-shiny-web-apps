library(tidyverse)
library(feather)

# look in raw section 
covid_dirs <- list.dirs(path = '/data/data-lake/raw/coronavirus-cases/', full.names=TRUE)

covid_file <- paste0(tail(covid_dirs, n=1), '/coronavirus_cases.csv')

retrieve_covid_data <- read_csv(covid_file)

# direct from API
#retrieve_covid_data <- read_csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDate&metric=newCasesBySpecimenDateChangePercentage&metric=newCasesBySpecimenDateRollingRate&metric=newCasesBySpecimenDateRollingSum&format=csv')

# rolling seven day average data based on covid specimen date is only complete for up to 5 days before current date
# so need to filter on 5 days before current day - but it is updated late in the day we really need 6 days prior
latest_specimen_date <- as.Date(Sys.Date()) - 6

# retrieve latest 7 day rolling cases per 100,000 and percentage change in cases
latest_covid_data <- retrieve_covid_data %>% rename(LAD19CD=areaCode) %>% 
  # todays seven day rolling average
  filter(as.Date(date) == as.Date(latest_specimen_date) & str_detect(LAD19CD, "^E"))


# ---- read in area lookups ---
area_lookup <- read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/lookup%20mosa11%20to%20lad17%20to%20lad19%20to%20tactical%20cell.csv")
area_lookup_tc2lad <- area_lookup %>% select('LAD19CD', 'TacticalCell') %>% unique()


# align with tactical cells 
latest_covid_data2tactical_cell <- left_join(area_lookup_tc2lad, latest_covid_data, by='LAD19CD') %>%
  filter(TacticalCell != 'Scotland' & TacticalCell != 'Wales' & TacticalCell != 'Northern Ireland and the Isle of Man')

# --- local file ---
write_csv(latest_covid_data2tactical_cell, '/home/izzy-everall/areas2focus_data/areas2focus_covid.csv')
write_feather(latest_covid_data2tactical_cell, '/home/izzy-everall/areas2focus_data/areas2focus_covid.feather')










# # read in vulnerability index 
# LA_vi <- read_csv('https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/output/vulnerability-LA.csv')
# LA_vi <- LA_vi %>% rename('LAD19CD'=Code)
# # -- read in look up table ---
# area_lookup <- read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/lookup%20mosa11%20to%20lad17%20to%20lad19%20to%20tactical%20cell.csv")
# area_lookup_tc2lad <- area_lookup %>% select('LAD19CD', 'TacticalCell') 
# 
# # read in latest covid case rates 
# covid_latest <- read_csv('/data/data-lake/curated/coronavirus-cases-eng/covid-rates-la.csv')
# 
# # merge with overall vulnerability index
# vi_needed <- LA_vi %>% select('LAD19CD','Name',`Vulnerability quintile`)
# covid2vulnerability <- left_join(vi_needed, area_lookup_tc2lad, by='LAD19CD', keep=F) %>% 
#   unique() %>%
#   # remove wales/scotland/NI for now
#   filter(TacticalCell != "Northern Ireland and the Isle of Man",
#          TacticalCell != "Scotland",
#          TacticalCell != "Wales")
# 
# # --- retrieve cases rates per 100,000 for last two weeks --- 
# covid_needed <- covid_latest %>% select('LAD19CD', tail(names(.), 2)) %>%
#   # difference between rates
#   mutate('cases_difference'=round(.[[3]]-.[[2]],2)) %>%
#   # percentage change
#   mutate(`% change in covid cases`=round((cases_difference/.[[2]])*100, 1))
# 
# covid2vulnerability <- left_join(covid2vulnerability, covid_needed, by='LAD19CD', keep=F) %>% unique()
# 
# # --- local file ---
# write_csv(covid2vulnerability, './areas2focus_data/areas2focus_covid.csv')
# write_feather(covid2vulnerability, './areas2focus_data/areas2focus_covid.feather')
# 
# date_time <- Sys.time()
# date_time <- str_split(date_time, ' ')
# note = paste(date_time[[1]][1], date_time[[1]][2], 'areas2focus', 'data refreshed', sep='\t')
# write(note, './areas2focus_data/areas2focus_crontab_log.txt', append=T)
# 
# 
# # --- app file ----
# #write_csv(covid2vulnerability, './r-shiny-web-apps/packages/dashboard/data/areas_to_focus/areas2focus_covid.csv')

