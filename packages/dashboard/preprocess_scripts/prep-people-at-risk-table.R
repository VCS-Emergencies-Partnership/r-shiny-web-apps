# --- pre process data for vcsep-shiny-dashboard ---
library(tidyverse)
library(readxl)
library(httr)
library("feather")


# --- get look up table 
area_lookup <- read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/lookup%20mosa11%20to%20lad17%20to%20lad19%20to%20tactical%20cell.csv")
area_lookup_tc2lad <- area_lookup %>% select('LAD19CD', 'TacticalCell')

# read in data 
bame <- read_csv('/data/data-lake/curated/annual-population-survey-ethnicity-data/bame-ethnicity-lad19.csv')
asylum <- read_csv('/data/data-lake/curated/asylum-section-95-support/asylum-lad19.csv')
digital_exclusion_tc <- read_csv('/data/data-lake/curated/digital-exclusion/digital-exclusion-tc.csv')
digital_exclusion_lad <- read_csv('/data/data-lake/curated/digital-exclusion/digital-exclusion-lad19.csv')
fuelp <- read_csv('/data/data-lake/curated/fuel-poverty-eng/fuel-poverty-lad19.csv')
homelessness <- read_csv('/data/data-lake/curated/homelessness-eng/homelessness-lad19.csv')
shielding <- read_csv('/data/data-lake/curated/shielding-patients-list-eng/shielding-lad19.csv')
ucred <- read_csv('/data/data-lake/curated/stat-xplore-people-on-universal-credit/universal-credit-lad19.csv')


# read in population files 
#pop_eng <- read_excel("./data/population/SAPE21DT3a-mid-2018-msoa-on-2019-LA-syoa-estimates-formatted.xlsx", sheet = "Mid-2018 Persons", skip = 4)
#pop_eng <- pop_eng %>% rename('LAD19CD'=`Area Codes`) %>% select('LAD19CD', `All Ages`)
#pop_eng_tc <- left_join(pop_eng, area_lookup_tc2lad, by='LAD19CD', keep=F) %>% unique() %>%
#  filter(!is.na(TacticalCell) & TacticalCell != 'Wales') 


pop_eng_2019 <- read_excel('/data/data-lake/raw/ons-populstion-estimates-mid-year-2019/2021-04-12-10-30-27/ons-populstion-estimates-mid-year-2019.xlsx', sheet = "Mid-2019 Persons", skip = 4)

# select LA Code (2019 boundaries) and All Ages
pop_eng <- pop_eng_2019 %>%
  select(LAD19CD=`LA Code (2019 boundaries)`, la_name=`LA name (2019 boundaries)`, population=`All Ages`) %>%
  group_by(LAD19CD) %>%
  summarise(`All Ages`=sum(`population`))

pop_eng_tc <- left_join(pop_eng, area_lookup_tc2lad, by='LAD19CD', keep=F) %>% unique() %>%
  filter(!is.na(TacticalCell) & TacticalCell != 'Wales') 

# i think there are 317 lads in England
#glimpse(pop_eng_tc)
# Calculate population of tactical cells 
pop_tc <- pop_eng_tc %>% group_by(TacticalCell) %>% 
  summarise_at(vars(`All Ages`), list(sum)) %>%
  mutate('eng_pop'=sum(`All Ages`))

pop_eng_lad_tc <- left_join(pop_eng_tc, pop_tc, by='TacticalCell', keep=F) %>% 
  rename('lad_pop'=`All Ages.x`, 'tc_pop'=`All Ages.y`)

# store eng pop seperately
eng_pop <- unique(pop_eng_lad_tc$eng_pop)

# 2019 estimates population size 


# bame data 
# -- BAME population in LAD 
# --> already calculated % of lad population which is bame 
# only have bame data for 200 out of the 317 LADs in England
# add in tactical cell 
bame_data <- left_join(bame, area_lookup_tc2lad, by='LAD19CD', keep=F) %>% 
  filter(TacticalCell != 'Wales' & TacticalCell != 'Scotland') %>% unique() 

# -- BAME population in England (based upon annual population survey - obviously some data missing)
# -- data for 200 local 
england_proportion_bame <- ((sum(bame_data$`numerator-bame-not-uk-born`, na.rm=T) + sum(bame_data$`numerator-bame-uk-born`, na.rm=T)) / sum(bame_data$Denominator, na.rm=T)) * 100
# according to the annual population survey - 19.9% of Englands population is bame
bame_data <- bame_data %>% mutate(england_proportion_bame = round(england_proportion_bame,1))

# group_by tactical cell and sum 
groupColumns = c('TacticalCell')
dataColumns = c('numerator-bame-not-uk-born','numerator-bame-uk-born', 'Denominator')
bame_lad_values2tc_total = plyr::ddply(bame_data, groupColumns, function(x) colSums(x[dataColumns], na.rm=T)) 
bame_lad_values2tc_total_final = bame_lad_values2tc_total %>% mutate(total_bame_in_tc = `numerator-bame-not-uk-born`+`numerator-bame-uk-born`) %>%
  mutate(tc_proportion = round((total_bame_in_tc /Denominator)*100,1)) %>%
  rename('tc_denominator'=Denominator) %>%
  select(-`numerator-bame-not-uk-born`, -`numerator-bame-uk-born`)

# join to LAD data 
bame_data <- left_join(bame_data, bame_lad_values2tc_total_final, by='TacticalCell', keep=F)
# write bame indicator to people at risk file
#write_csv(bame_data, './people_at_risk_table/BAME-indicator.csv')

# --- asylum data ---
# --- calculate proportion receiving support ---
# --- join to areas2uk --- 
asylum_data <- left_join(area_lookup_tc2lad, asylum, by='LAD19CD', keep=F) %>%
  unique() %>%
  left_join(., pop_eng_lad_tc, by=c("LAD19CD", "TacticalCell"))
# Currently we are just aggregating England 
asylum_data <- asylum_data %>% filter(TacticalCell != "Northern Ireland and the Isle of Man",
                                      TacticalCell != "Scotland",
                                      TacticalCell != "Wales")


asylum_data <- asylum_data %>% 
  mutate('lad_prop_receving_section95_support'=round((`People receiving Section 95 support`/lad_pop)*100,2)) %>%
  mutate('People receiving Section 95 support (people per 1000)'=round((`People receiving Section 95 support`/lad_pop)*1000, 2))

  
# Do they want to know what proprtion of the population that is
#or geographically where the highest proportion of section95s 
# one divides the number receiving support by population of LAD 
#the other by total receiving support
# based on how small the proportions would be if i did of the total population i'm going for of the people recieving section 95 support
#what proprotion are receving support in each tactical cell and lad
# however having discussed with Mike P going to do proportion of the population - the numbers will be small

# also note when calculation proportions of populations am using consistent figures across all.
# So I do NOT just calculate the population based on those that have data available for the indicator.

asylum_data_tc <- asylum_data %>% group_by(TacticalCell, tc_pop) %>%
  summarise_at(vars("People receiving Section 95 support"), list(sum), na.rm=T) %>%
  rename('tc_People receiving Section 95 support'=`People receiving Section 95 support`, 'People receiving Section 95 support') %>%
  mutate('tc_prop_people_recieving_section_95_support'=round((`tc_People receiving Section 95 support`/tc_pop)*100,2)) %>%
  mutate('tc_people_recieving_section_95_support_cases_per_1000'=round((`tc_People receiving Section 95 support`/tc_pop)*1000,2)) #%>%

asylum_data <- left_join(asylum_data, asylum_data_tc, by=c('TacticalCell','tc_pop'), keep=F)

asylum_data <- asylum_data %>%
  mutate('eng_people_recieving_section_95_support'=sum(`People receiving Section 95 support`, na.rm=T)) %>%
  mutate('prop_eng_receiving_section_95_support'=round((`eng_people_recieving_section_95_support`/eng_pop)*100,2)) %>%
  mutate('eng_receiving_section_95_support_cases_per_1000'=round((`eng_people_recieving_section_95_support`/eng_pop)*1000,2))

#write_csv(asylum_data, './people_at_risk_table/asylum-indicator.csv')


# --- Digital exclusion ---
# just going to combine the two file

digital_exclusion_data <- left_join(area_lookup_tc2lad, digital_exclusion_lad, by='LAD19CD', keep=F) %>%
  unique() %>% select('LAD19CD', "TacticalCell", everything()) %>%
  # remove wales/scotland/NI for now
  filter(TacticalCell != "Northern Ireland and the Isle of Man",
         TacticalCell != "Scotland",
         TacticalCell != "Wales") %>%
  mutate('percent_digitally_excluded'= round(`Proportion of neighbourhoods in 20% most digitally excluded`*100,1)) %>%
  select('LAD19CD', 'TacticalCell', `Proportion of neighbourhoods in 20% most digitally excluded`, 'percent_digitally_excluded')

digital_exclusion_data <- digital_exclusion_tc %>% rename(`tc_Proportion of neighbourhoods in 20% most digitally excluded` =`Proportion of neighbourhoods in 20% most digitally excluded`,
                                                               `tc_Extent of population living in highly digitally excluded areas`=`Extent of population living in highly digitally excluded areas`,
                                                               `tc_Population-weighted digitally exclusion score`=`Population-weighted digitally exclusion score`) %>%
  mutate('tc_percent_digitally_excluded'= round(`tc_Proportion of neighbourhoods in 20% most digitally excluded`*100,1)) %>%
  select('TacticalCell', `tc_Proportion of neighbourhoods in 20% most digitally excluded`, 'tc_percent_digitally_excluded') %>%
  left_join(digital_exclusion_data, ., by='TacticalCell', keep=F)


#write_csv(digital_exclusion_data, './people_at_risk_table/digital-exclusion-indicator.csv')

# --- shielding --- 
# join lad population
shielding_data <- left_join(area_lookup_tc2lad, shielding, by='LAD19CD', keep=F) %>%
  unique() %>% filter(TacticalCell != "Northern Ireland and the Isle of Man",
                      TacticalCell != "Scotland",
                      TacticalCell != "Wales")

# join population data
shielding_data <- left_join(pop_eng_lad_tc, shielding, by=c('LAD19CD'), keep=F)

# calculate number per tactical cell per 1000 
shielding_data_tc <- shielding_data %>% group_by(TacticalCell, tc_pop) %>%
  summarise_at(vars(`Clinically extremely vulnerable`), list(sum), na.rm=T) %>%
  # calculate number of people shielding per 1000
  mutate(`tc_Clinically vulnerable per 1000`=round((`Clinically extremely vulnerable`/tc_pop)*1000,1)) %>%
  mutate(`tc_Clinically vulnerable proportion of population`=round((`Clinically extremely vulnerable`/tc_pop)*100,1)) %>%
  rename(`tc_Clinically extremely vulnerable`=`Clinically extremely vulnerable`)

shielding_data <- left_join(shielding_data, shielding_data_tc, by=c('TacticalCell','tc_pop'), keep=F)

# shielding in england 
shielding_data <- shielding_data %>% 
  mutate('total_shielding_eng'=sum(`Clinically extremely vulnerable`, na.rm=T)) %>%
  #mutate('total_pop_eng'=sum(lad_pop)) %>%
  # proportion of englands population 
  mutate('proportion_total_shielding_Eng'= round((total_shielding_eng/eng_pop)*100,1))

#write_csv(shielding_data, './people_at_risk_table/shielding-indicator.csv')

# --- Homelessness ---
homelessness_data <- left_join(area_lookup_tc2lad, homelessness, by="LAD19CD", keep=F) %>%
  unique() %>%
  left_join(., pop_eng_lad_tc, by=c('LAD19CD',"TacticalCell"), keep=F) %>% 
  filter(TacticalCell != "Northern Ireland and the Isle of Man",
         TacticalCell != "Scotland",
         TacticalCell != "Wales")

# to get number of homeless people 
# divide by 1000 * by LAD population
#homelessness_data <- homelessness_data %>% 
#  mutate('lad_total_homeless'=round((`Homelessness (rate per 1000)`/1000)*lad_pop,0)) %>%
#  mutate('lad_prop_homeless'=round((lad_total_homeless/lad_pop)*100, 2))

# calculate the number of homeless people per tc
# homelessness_data_tc <- homelessness_data %>%
#   group_by(TacticalCell, tc_pop) %>%
#   summarise_at(vars('lad_total_homeless'), list(sum), na.rm=T) %>%
#   rename('tc_total_homeless'=lad_total_homeless) %>%
#   mutate('tc_prop_homeless' = round((tc_total_homeless/tc_pop)*100,2)) %>%
#   mutate('Homelessness per 1000 in tc'=round((tc_total_homeless/tc_pop)*1000,1)) #%>%
#   #mutate('total_eng_pop'=sum(tc_pop_lad_with_homelessness, na.rm=T)) %>%

homelessness_data_tc <- homelessness_data %>%
  group_by(TacticalCell) %>%
  summarise_at(vars(`Homelessness (rate per 1000)`), list(mean), na.rm=T) %>%
  rename('tc_Homelessness (rate per 1000)'=`Homelessness (rate per 1000)`)
  #mutate('tc_prop_homeless' = round((tc_total_homeless/tc_pop)*100,2)) %>%
  #mutate('Homelessness per 1000 in tc'=round((tc_total_homeless/tc_pop)*1000,1)) #%>%
#mutate('total_eng_pop'=sum(tc_pop_lad_with_homelessness, na.rm=T)) %>%

# homelessness_data <- homelessness_data %>%
#   mutate('eng_total_homeless'=sum(lad_total_homeless, na.rm=T)) %>%
#   mutate('proprotion_homeless'=round((eng_total_homeless/eng_pop)*100,1))

homelessness_data <- left_join(homelessness_data, homelessness_data_tc, by=c('TacticalCell'), keep=F)

homelessness_data <- homelessness_data %>%
  mutate('eng_rate_per_1000'= mean(`Homelessness (rate per 1000)`, na.rm=T))

#write_csv(homelessness_data, './people_at_risk_table/homelessness-indicator.csv')

# ---- universal credit ---
# for both local authority, tactical cell and england
# need to determine the proportion of the population unemployed on universal credit
ucred_data <- left_join(area_lookup_tc2lad, ucred, by='LAD19CD', keep=F) %>%
  unique() %>%
  left_join(., pop_eng_lad_tc, by=c('LAD19CD', 'TacticalCell'), keep=F) %>%
  # remove wales/scotland/NI for now
  filter(TacticalCell != "Northern Ireland and the Isle of Man",
       TacticalCell != "Scotland",
       TacticalCell != "Wales")

# local authority proportion of population unemployed on universal credit 
ucred_data <- ucred_data %>% 
  mutate('lad_prop_upemployed_on_ucred'=round((`Not in employment`/lad_pop)*100,0))

# tc and eng level total and proportion unemployed on universal credit
ucred_data_tc <- ucred_data %>%
  group_by(TacticalCell, tc_pop) %>%
  summarise_at(vars(`Not in employment`), list(sum), na.rm=T) %>%
  rename(`tc_Not in employment`=`Not in employment`) %>%
  mutate('tc_prop_unemployed_on_universal_credit'=round((`tc_Not in employment`/tc_pop)*100,1)) #%>%
  #mutate('eng_pop_with_ucred_data'=sum(tc_pop_with_ucred_data)) %>%

ucred_data <- left_join(ucred_data, ucred_data_tc, by=c('TacticalCell','tc_pop'), keep=F) %>%
  unique()

ucred_data <- ucred_data %>%
  mutate('eng_total_unemployed_on_ucred'=sum(`Not in employment`, na.rm=T)) %>%
  mutate('prop_eng_pop_unemployed_on_ucred'=round((eng_total_unemployed_on_ucred/eng_pop)*100,1))


#write_csv(ucred_data, './people_at_risk_table/ucred-indicator.csv')

# --- fuel poverty ---
# join tactical cells (this is households so don't need population)
fuelp_data <- left_join(area_lookup_tc2lad, fuelp, by='LAD19CD', keep=F) %>%
  unique()  %>%
  # remove wales/scotland/NI for now
  filter(TacticalCell != "Northern Ireland and the Isle of Man",
         TacticalCell != "Scotland",
         TacticalCell != "Wales")


# caclulate proprotion of households fuel poor tactical
fuelp_data_tc_eng <- fuelp_data %>%
  group_by(TacticalCell) %>%
  summarise_at(vars(`Number of households1`,`Number of households in fuel poverty1`), list(sum), na.rm=T) %>%
  rename(`tc_Number of households1`=`Number of households1`, `tc_Number of households in fuel poverty1`=`Number of households in fuel poverty1`) %>%
  mutate('tc_prop_households_fuel_poor'=round((`tc_Number of households in fuel poverty1`/`tc_Number of households1`)*100,1)) %>%
  mutate('eng_total_households'=round(sum(`tc_Number of households1`),0)) %>%
  mutate('eng_total_fuel_poor_households'=round(sum(`tc_Number of households in fuel poverty1`),0)) %>%
  mutate('eng_prop_households_fuel_poor'=round((eng_total_fuel_poor_households/eng_total_households)*100,1))


fuelp_data <- left_join(fuelp_data, fuelp_data_tc_eng, by='TacticalCell', keep=F)

#write_csv(fuelp_data, './people_at_risk_table/fuel-poverty-indicator.csv')


# ---- do we want to join them all together ----
# join altogether
all_data <- left_join(digital_exclusion_data, asylum_data, by=c("LAD19CD","TacticalCell"), keep=F) %>%
  # add bame
  left_join(., bame_data, by=c("LAD19CD","TacticalCell"), keep=F) %>%
  # add covid 
  #left_join(., covid_data, by=c("LAD19CD","TacticalCell",'lad_pop','tc_pop','eng_pop'), keep=F) %>%
  # add fuel poverty
  left_join(., fuelp_data, by=c("LAD19CD","TacticalCell"), keep=F) %>%
  # add homelessness
  left_join(., homelessness_data, by=c("LAD19CD","TacticalCell",'lad_pop','tc_pop','eng_pop'), keep=F)  %>%
  # add ucred
  left_join(., ucred_data, by=c("LAD19CD","TacticalCell", 'lad_pop','tc_pop','eng_pop'), keep=F) %>%
  # clinically shielding
  left_join(., shielding_data,  by=c("LAD19CD","TacticalCell",'lad_pop','tc_pop','eng_pop'), keep=F)

# --- save all data as a .feather file ---
#write_csv(all_data, './people_at_risk_table/people-at-risk.csv')
write_feather(all_data, './r-shiny-web-apps/packages/dashboard/data/people_at_risk/people_at_risk_table/people-at-risk.feather')
# --- and to app file -- 
#write_feather(all_data, './r-shiny-web-apps/packages/dashboard/data/people_at_risk/people-at-risk.feather')
#write_feather(all_data, './r-shiny-web-apps/packages/dashboard/data/people_at_risk/people-at-risk.csv')



date_time <- Sys.time()
date_time <- str_split(date_time, ' ')
note = paste(date_time[[1]][1], date_time[[1]][2], 'people_at_risk_table', 'data refreshed', sep='\t')
write(note, './people_at_risk_table/people_at_risk_crontab_log.txt', append=T)

#test <- read_csv('./people-at-risk.csv')
