library(tidyverse)
library(httr)
library(jsonlite)
library('ghql')
library(R.utils)

#https://www.r-bloggers.com/2020/12/accessing-grahpql-from-r/

findcharities <- function(curr_bbox, search_term) {
  
  #tryCatch(
    
  #  expr = { withTimeout({
    
  

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
        contact{
          address,
          email,
          phone,
          postcode
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
    as_tibble() 
  
  
  if(dim(emergency_charity_data)[1] == 0) {
  
    return(emergency_charity_data)
    #out <- NULL
  } 
  else {
    emergency_charity_data <- emergency_charity_data %>% unnest(., names) %>% unnest(., causes)
  
    emergency_charity_data <- result$data$CHC$getCharities$list %>%
      as_tibble() %>% unnest(., names) %>% unnest(., causes)
    
    # work with causes 
    charity2cause <- emergency_charity_data %>% select('value', 'name') %>%
      group_by(value) %>% summarise(across(everything(), str_c, collapse=", ")) %>%
      rename('Causes'=name)
    
    charity_data <- left_join(emergency_charity_data, charity2cause, by='value') %>%
      # remove exploded column
      select(-'name') %>% rename('Charity name'=value, 'Activities'=activities) #%>%
    
    
    # remove duplicates
    charity_data_final <- distinct(charity_data, id, .keep_all=T)
    
    # extract address
    format_address <- charity_data_final$contact$address
    format_address <- sapply(format_address, paste, collapse=', ')
    
    charity_data_final$Phone <- charity_data_final$contact$phone
    charity_data_final$Email <- charity_data_final$contact$email
    charity_data_final$Address <- format_address 
    charity_data_final$Postcode <- charity_data_final$contact$postcode
    charity_data_final$`Local authority` <- charity_data_final$geo$admin_district
    
    charity_data2return <- charity_data_final %>% select(-id, -contact, -geo) %>%
      relocate(`Charity name`, `Causes`, `Phone`, `Email`, `Activities`, `Address`, `Postcode`, `Local authority`)
  
    return(charity_data2return)
  
        } # end of else 
      #}, # end of with timeout curly bracket
      #  timeout = 0.01, onTimeout = 'warning') 
      #}, # end of expression
      # TimeoutException
      #TimeoutException = function() {
      #  print('time out issue')
      #  test_error <- 'API running slowly'
      #  return(test_error)
      #}
    
    #)
  
}
