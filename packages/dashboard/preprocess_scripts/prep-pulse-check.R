library('tidyverse')
library(feather)

# proportion respondends reporting need function
proportion_responded <- function(survey_file) {

  pulse_needed <- read_csv(survey_file)
  
  # CHECK - ARE THE COLUMNS I NEED IN THERE? --> 
  cols_needed <- c('_index','What is your Multi-Agency Cell area?', 'In which county/unitary authority does your organisation operate?')
  # are all the columns i use with easy names there?
  area_names_still_present <- (cols_needed %in% colnames(pulse_needed))
  # is there still a question containing this string:
  question_still_present <- grepl('following sectors are the top three priority concerns in your area in the next 14 days', colnames(pulse_needed))
  question_still_present_cols <- pulse_needed[question_still_present] 
  
  # if logical vector is not all T - there's a missing column name
  # of if theres no columns containing the following sectors comment the question has changed
  if (all(area_names_still_present)==F ||
      dim(question_still_present_cols)[2] == 0) {
    stop("Couldn't find required columns in pulse check survey")
    
  } else {
  
# filter by column number to get concerns in next 14 days other relating to this is col 404 - not needed as this is counted but column on what was required is provided which is other:
pulse_needed <- pulse_needed %>% select(438,6,7, contains("following sectors are the top three priority concerns in your area in the next 14 days?/"))

# how many respondents
respondents <- nrow(pulse_needed)

# try remove ridiculous long names
pulse_needed_clean <- pulse_needed %>%
  rename_with(~paste0(sub("In your view, which of the following sectors are the top three priority concerns in your area in the next 14 days?/*", "", .)), 
              starts_with('In your'))


# group_by 
concerns <- pivot_longer(pulse_needed_clean, 4:15, names_to='Concerns', values_to='total')

concerns_by_type <- concerns %>% group_by(`Concerns`) %>% summarise(group_total=sum(total, na.rm=T)) %>%
  mutate(proportion_respondents = round((group_total/respondents)*100, 1))

concerns_by_type$clean_groups <- gsub("[[:punct:]]", '', concerns_by_type$Concerns)

# remove pulse none and other data and clean group names
not_wanted <- c('None', 'Not sure')
concerns_by_type <- concerns_by_type %>% filter(!clean_groups %in% not_wanted) %>% #, Concerns != '?/Other') %>%
  mutate('clean_names'=case_when(clean_groups == 'Employment advice  support' ~ 'Employment',
                                 clean_groups == 'IT access  digital divide' ~ 'Digital exclusion',
                                 clean_groups == 'Hardship  financial support' ~ 'Financial hardship',
                                 clean_groups == 'Housing temporary accommodation' ~ 'Housing',
                                 clean_groups == 'Protection  safeguarding' ~ 'Safegaurding',
                                 TRUE ~ as.character(.$clean_groups))) %>%
  mutate('total_respondents'=respondents)

return(concerns_by_type)

}

}



# --- Retrieving from raw section --- 
get_pulse <- list.dirs('/data/data-lake/raw/pulse_check_raw/')
latest_file_name <- paste(tail(get_pulse, n=1), 'pulse_check_raw.csv', sep='/')
last_but_one_file_name <- paste(tail(get_pulse, n=2), 'pulse_check_raw.csv', sep='/')
last_but_one_file_name <- last_but_one_file_name[1]

# Does file exist 
if (!file.exists(latest_file_name) || !file.exists(last_but_one_file_name)) {
  # break send warning 
  message <- paste0("Problem: a file is missing")
  stop(message)
} else {
    
  latest_survey <- proportion_responded(latest_file_name)
  previous_survey <- proportion_responded(last_but_one_file_name)
  #print('latest')
  #glimpse(latest_survey)
  
  #glimpse(previous_survey)
  #print('previous')
  
  joined_surveys <- left_join(latest_survey, previous_survey, by='clean_names')
  #glimpse(joined_surveys)
  greatest_change <- joined_surveys %>% mutate('greatest_diff'=proportion_respondents.x - proportion_respondents.y) %>%
    select(1:6,12) %>% rename_with(~str_remove(., '.x'))
  
  glimpse(greatest_change)


  # -- write to file ---
  write_feather(greatest_change, '~/r-shiny-web-apps/packages/dashboard/data/vcs_indicators/pulse_check_summary.feather')


 }

