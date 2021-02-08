0~library(tidyverse)
library(httr)
library(jsonlite)
library('ghql')

#https://www.r-bloggers.com/2020/12/accessing-grahpql-from-r/

findcharities <- function(curr_bbox, search_term) {

  link <- 'https://charitybase.uk/api/graphql'

  conn <- GraphqlClient$new(url = link,
                          headers= list(Authorization='Apikey b9e235db-e6ab-43f4-9110-9b6da33560ad'))

  query <- '
  query Search_charities_in_Area($top: Float!, $left: Float!, $right:Float!, $bottom: Float!, $search: String) {
  CHC {
    getCharities(
      filters: { search: $search, geo:{boundingBox: {top: $top, left: $left, bottom: $bottom, right: $right}}}
    ) {
      list(limit:30) {
        id
        names {
          value
        }
        activities,
        causes{
          name
        }
        geo {
          admin_district,
          region,
          longitude,
          latitude
        }
      }
    }
  }
}'

  new <- Query$new()$query('link', query)

  variable <- list(
    top = curr_bbox$ymax, bottom=curr_bbox$ymin, left=curr_bbox$xmin, right=curr_bbox$xmax, 
    search = search_term)


  result <- conn$exec(new$link, variables=variable) %>%
    fromJSON(flatten = F)
  

  emergency_charity_data <- result$data$CHC$getCharities$list %>%
    as_tibble() %>% unnest(., names) %>% unnest(., causes)
  
  # work with causes 
  charity2cause <- emergency_charity_data %>% select('value', 'name') %>%
    group_by(value) %>% summarise(across(everything(), str_c, collapse=", ")) %>% rename('Causes'=name)
  
  charity_data <- left_join(emergency_charity_data, charity2cause, by='value') %>%
    # remove exploded column
    select(-'name') %>% rename('Charity name'='value')
  
  print(charity_data$geo$admin_district)
  
  charity_data_final <- distinct(charity_data, id, .keep_all=T) %>%
    select(`Charity name`, `Causes`, `activities`)
  
  
  return(charity_data_final)

}
