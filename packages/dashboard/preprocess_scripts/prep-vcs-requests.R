library(AzureStor)
library(tidyverse)
library(lubridate)
library(PostcodesioR)
library('secret')
library(feather)

# -- lookup table ---
lookup <- read_csv("https://raw.githubusercontent.com/britishredcrosssociety/covid-19-vulnerability/master/data/lookup%20mosa11%20to%20lad17%20to%20lad19%20to%20tactical%20cell.csv") %>% 
  select(LAD19CD, TacticalCell) %>% 
  mutate('TacticalCell'=case_when(TacticalCell == 'South West and the Channel Islands' ~ 'South and the Channel Islands',
                                                                    TacticalCell == 'Central' ~ 'Midlands and East',
                                                                    TRUE ~ (as.character(.$TacticalCell)))) %>%
  filter(TacticalCell != "Northern Ireland and the Isle of Man",
         TacticalCell != "Scotland",
         TacticalCell != "Wales") %>%
  unique()


# # -- todays date --
# # need to know current date to append to request for support
# 
 date_time <- Sys.time()
 date_time <- str_split(date_time, ' ')
 date <- format(as.Date(date_time[[1]][1]), '%d-%m-%Y')
# # latest data should be in this file name
# file_name <- paste0("RequestForSupport-",date,".csv")
# 
# # -- Secretly retrieve this information -- 
# # instructions https://blog.revolutionanalytics.com/2018/12/azurestor.html
# 
# my_secrets <- function() {
#   path <- "~/.secret/secret.file2"
#   if (!file.exists(path)) {
#     stop("Can't find secret file: '", path, "'")
#   }
#   read_csv(path)
# }
# 
# # connect to end point
# blob_endp <- blob_endpoint(
#   "https://vcsepuksbrclandingprodsa.blob.core.windows.net",
#   key =  colnames(my_secrets())[1])
# 
# # list containers in storage 
# #list_blob_containers(blob_endp)
# 
# # view blob contain
# cont <- blob_container(blob_endp,"requestforsupport")
# list_blobs(cont)
# 
# # file to read write
# out_name <- paste0('~/vcs-indicators/', file_name)
# #out_name <- paste0('~/vcs-indicators/', 'RequestForSupport-19-01-2021.csv')
# #file_name <- "RequestForSupport-19-01-2021.csv"
# # retrieve data
# download_blob(cont, file_name, dest=out_name, overwrite=TRUE)

 # --- now retrieving from raw section --- 
get_requests <- list.dirs('/data/data-lake/raw/vcsep-requests-for-support/')

file_name <- paste(tail(get_requests, n=1), 'vcsep-requests-for-support.csv', sep='/')

requests <- read_csv(file_name)

# -- filtering for current live requests -- 
requests <- requests %>% filter(!is.na(request_date)) %>%
  separate(request_date, c('clean_date',NA), remove=F, sep=' ') %>% 
  mutate('formatted_date' = ymd(clean_date)) 


# Total requests over last week 
# - filter by date -
# number of open
get_all_dates <- requests %>% arrange(formatted_date) %>% select(formatted_date)


#date_of_last_request <- tail(get_all_dates$formatted_date, 1)
today_date <- dmy(date)

# get all requests from last week 
seven_days_prior = today_date - 7
previous_week_start = seven_days_prior - 7

# filter for previous two weeks data 
last2weeks <- requests %>% filter(formatted_date <= today_date & formatted_date >= previous_week_start)

# extract postcodes
last2weeks_postcodes <- last2weeks$postcode
last2weeks_postcodes <- as.list(as.vector(last2weeks_postcodes))

#look up postcodes 
#lng_lat_last2weeks <- bulk_postcode_lookup(last2weeks_postcodes)

wanted_data <- list()

for (i in last2weeks_postcodes) {
  print(i)
  df <- tryCatch({
    # what i want returned
    postcode_lookup(i) },
    error = function(cond){
      return(NA)
    })
  
  if (!is.na(df) && !is.null(df)) {
  wanted <- df %>% select('postcode','admin_district_code','longitude','latitude')
  wanted_data[[i]] <- wanted
  }
  
}
# make dataframe
postcode2lad <- do.call(rbind, wanted_data)

# if no requests in last two weeks?
  if(is.null(postcode2lad)) {
  # read in previous requests and make all values NA
    no_data <- read_csv('./vcs-indicators/requests_this_week_and_last.csv')
    # replace old values with NA 
    no_data_update <- no_data %>% mutate_if(is.numeric, function(x) x=NA)
    # add in date
    out_name <- paste0('./vcs-indicators/requests_this_week_and_last-',date, '.csv')
    # write to vcs-indicators file
    write_csv(no_data_update, out_name)
    write_csv(no_data_update, './vcs-indicators/requests_this_week_and_last.csv')
    
  } else {
    # join to last2weeks 
    last2weeks_admins <- left_join(last2weeks, postcode2lad, by='postcode', keep=F) %>% rename('LAD19CD'=admin_district_code, 'TacticalCell' = multi_agency_cell) %>%
     # remove postcodes that were unable to return local authority - incorrectly formatted
      filter(!is.na(LAD19CD)) %>% mutate('TacticalCell'=case_when(TacticalCell == 'South West and the Channel Islands' ~ 'South and the Channel Islands',
                                                                  TacticalCell == 'Central' ~ 'Midlands and East',
                                                                  TRUE ~ (as.character(.$TacticalCell))))


    # filter requests into 
    past_weeks_requests <- last2weeks_admins %>% filter(formatted_date <= today_date & formatted_date > seven_days_prior) %>%
    group_by(TacticalCell, LAD19CD) %>% tally() %>% rename('total_requests_last_week'=n)


  compare2week_before <- last2weeks_admins %>% filter(formatted_date <= seven_days_prior & formatted_date > previous_week_start) %>%
  group_by(TacticalCell, LAD19CD) %>% tally() %>% rename('total_requests_previous_week'=n)


  # combine
  requests_this_week_and_last <- left_join(lookup, past_weeks_requests, by=c('TacticalCell','LAD19CD'), keep=F) %>%
  left_join(., compare2week_before, by=c('TacticalCell','LAD19CD'), keep=F) %>% unique()

  # add in date
  out_name <- paste0('./vcs-indicators/requests_this_week_and_last-',date, '.csv')
  
  # write to vcs-indicators file
  write_csv(requests_this_week_and_last, out_name)
  write_csv(requests_this_week_and_last, './vcs-indicators/requests_this_week_and_last.csv')
  write_feather(requests_this_week_and_last, './vcs-indicators/requests_this_week_and_last.feather')
  }

# write to 
#write_csv(requests_this_week_and_last, '/home/izzy-everall/r-shiny-web-apps/packages/dashboard/data/vcs_indicators/requests_this_week_and_last.csv')




