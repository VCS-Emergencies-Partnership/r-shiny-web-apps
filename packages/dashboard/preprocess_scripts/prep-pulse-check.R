library('tidyverse')
library(feather)

# --- now retrieving from raw section --- 
get_pulse <- list.dirs('/data/data-lake/raw/pulse_check_raw/')

file_name <- paste(tail(get_pulse, n=1), 'pulse_check_raw.csv', sep='/')

pulse_needed <- read_csv(file_name)
glimpse(pulse_needed)
# filter by column number to get concerns in next 14 days other relating to this is col 404 - not needed as this is counted but column on what was required is provided which is other:
pulse_needed <- pulse_needed %>% select(438,6,7, contains("following sectors are the top three priority concerns in your area in the next 14 days?/"))

# how many respondents
respondents <- nrow(pulse_needed)

# try remove ridiculous long names
pulse_needed_clean <- pulse_needed %>%
rename_with(~paste0(sub("In your view, which of the following sectors are the top three priority concerns in your area in the next 14 days?/*", "", .)), 
                  starts_with('In your')) #%>% mutate(Other = case_when(is.na(`Other:`) ~ 0,
                                          #                                 TRUE ~ 1)) %>%
  #select(-`Other:`)

#glimpse(pulse_needed_clean)
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
                                                             TRUE ~ as.character(.$clean_groups)))

glimpse(concerns_by_type)

# -- write to file ---
write_feather(concerns_by_type, '/home/izzy-everall/vcs-indicators/pulse_check_summary.feather')




# # group by and sum number responded with particular concerns
# tc_proporiton_need <- pulse_needed %>%
#   group_by(`What is your Multi-Agency Cell area?`) %>%
#   count(`Thinking about the last fourteen days, do you have any concerns about the level of unmet needs in your area?`)
#   
# # sum total responses for each area 
# tc_proporiton_need_totals <- tc_proporiton_need %>%
#   group_by(`What is your Multi-Agency Cell area?`) %>%
#   summarise(area_total=sum(n))
# 
# # pivot so one row per mac
# tc_proporiton_need <- pivot_wider(tc_proporiton_need, names_from=`Thinking about the last fourteen days, do you have any concerns about the level of unmet needs in your area?`, values_from=`n`)
# 
# # join totals
# tc_group_total_and_area_total <- left_join(tc_proporiton_need, tc_proporiton_need_totals)
# # proportion of major concerns per area
# tc_prop_major_concern <- tc_group_total_and_area_total %>% mutate(prop_major_concern=round((`Major concerns`/`area_total`)*100,1))
# 
# eng_prop_major_concern <- round((sum(tc_prop_major_concern$`Major concerns`, na.rm=T)/sum(tc_prop_major_concern$area_total, na.rm=T))*100, 1)
# 
# tc_prop_major_concern <- tc_prop_major_concern %>% mutate('eng_prop_major_concer'=eng_prop_major_concenr)

# summary statistics for multi agency cells:
write_feather(tc_prop_major_concern, '/home/izzy-everall/vcs-indicators/pulse_check.feather')
