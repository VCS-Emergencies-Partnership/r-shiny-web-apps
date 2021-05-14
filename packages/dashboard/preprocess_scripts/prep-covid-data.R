library(tidyverse)
library(feather)

# look in raw section 
covid_dirs <- list.dirs(path = '/data/data-lake/raw/coronavirus-cases/', full.names=TRUE)
covid_file <- paste0(tail(covid_dirs, n=1), '/coronavirus_cases.csv')

# Does the file exist 
if (!file.exists(covid_file)) {
  
  print("Problem - file doesn't exist")
  
} else {
  
  retrieve_covid_data <- read_csv(covid_file)
  
  # CHECK - columns used present 
  # CHECK - ARE THE COLUMNS I NEED IN THERE? --> 
  cols_needed <- c('areaCode','areaName', 'date', 'newCasesBySpecimenDate', 'newCasesBySpecimenDateChangePercentage', 'newCasesBySpecimenDateRollingRate')
  # are all the columns i use with easy names there?
  cols_still_present <- (cols_needed %in% colnames(retrieve_covid_data))

  # if logical vector is not all T - there's a missing column name
  
  if (all(cols_still_present)==F) {
    
    print("Problem - column names changed")
    
  } else {
  
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

  #glimpse(latest_covid_data2tactical_cell)
# --- local file ---
write_feather(latest_covid_data2tactical_cell, '/home/izzy-everall/r-shiny-web-apps/packages/dashboard/data/areas_to_focus/areas2focus_covid.feather')

  }
}





