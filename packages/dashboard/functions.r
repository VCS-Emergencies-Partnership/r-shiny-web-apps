library(tidyverse)
library(httr)
library(jsonlite)
library('ghql')
library(R.utils)

# query charitybase
#https://www.r-bloggers.com/2020/12/accessing-grahpql-from-r/
#https://daattali.gitbooks.io/stat545-ubc-github-io/content/bit003_api-key-env-var.html
findcharities <- function(curr_bbox, search_term) {
  
  link <- 'https://charitybase.uk/api/graphql'
  
  #print(Sys.getenv("CHARITY_BASE_API_KEY"))
  conn <- GraphqlClient$new(url = link,
                          headers= list(Authorization=paste0('Apikey ', Sys.getenv("CHARITY_BASE_API_KEY"))))

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
      relocate(`Charity name`,`Phone`, `Activities`, `Causes`,  `Email`, `Address`, `Postcode`, `Local authority`)
  
    return(charity_data2return)
    
  }
  
}


# deomgraphic functions 

# --- BAME population text ---
# --- National level ---
ethnicity_stats_eng_text <- function(indicators) {
  bame_to_plot  <- indicators %>% select('england_proportion_bame') %>%
    pivot_longer(`england_proportion_bame`, names_to = "Indicator", values_to = "proportion")

  bame_to_plot <- bame_to_plot %>% filter(!is.na(proportion)) %>%
    unique() %>% mutate('proportion'=round(proportion,0))

  bame_to_show <- paste0(round(bame_to_plot$proportion,0), "%")


  return( 
    div(style= " text-align: center;margin-top:5px;",
      #hr(),
      p(bame_to_show, 
        tags$br(),
        "of the population are BAME"
      )
    )
  )
  
}

ethnicity_stats_eng_plot <- function(indicators) {

  bame_to_plot  <- indicators %>% select('england_proportion_bame') %>%
    pivot_longer(`england_proportion_bame`, names_to = "Indicator", values_to = "proportion")
  
  
  bame_to_plot <- bame_to_plot %>% filter(!is.na(proportion)) %>%
    unique() %>% mutate('proportion'=round(proportion,0))
  
return(
  # # Plot population statistics
  bame <- bame_to_plot %>%
    e_charts(x = Indicator) %>%
    e_bar(proportion, bar_width=0.1, showBackground=T) %>%
    e_labels(position = "right", color='black') %>%
    e_color(c('purple')) %>%
    e_hide_grid_lines() %>%
    e_flip_coords() %>%
    e_grid(containLabel = TRUE, left=30, right=30, top=10, bottom=5, height='60%') %>%
    e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
    e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
    e_y_axis(show=F) %>%
    e_legend(FALSE)
  )

}


# --- MAC level ----
ethnicity_stats_mac_text <- function(indicators, avgs) {
  
  bame_to_plot <- indicators %>% select(`tc_proportion`)

  # transpose dataframe
  bame_to_plot  <- bame_to_plot %>% pivot_longer(`tc_proportion`, names_to = "Indicator", values_to = "proportion") %>%
    unique() %>% filter(!is.na(proportion)) %>% 
    mutate('proportion'=round(proportion,0))
  
  # to format percentage
  bame_to_show <- paste0(round(bame_to_plot$proportion,0), "%")
  
  return(
    div(style= " text-align: center;margin-top:5px;",
        #hr(),
        p(bame_to_show,
          tags$br(),
          'of the population are BAME')
        
    )
    
  )
  
  
}

ethnicity_stats_mac_plot <- function(indicators, avgs){
  
  bame_to_plot <- indicators %>% select(`tc_proportion`)
  
  # transpose dataframe
  bame_to_plot  <- bame_to_plot %>% pivot_longer(`tc_proportion`, names_to = "Indicator", values_to = "proportion") %>%
    unique() %>% filter(!is.na(proportion)) %>% 
    mutate('proportion'=round(proportion,0))
  
  # for echarts
  tc_avg_bame <- avgs %>% select(`tc_proportion_mean`) %>%
    mutate(`tc_proportion`=round(`tc_proportion_mean`,0)) %>%
    select('xAxis' = `tc_proportion`) %>%
    as.list()
  
  # for echarts colour
  tc_avg_stdev_bame <- avgs %>% 
    select(`tc_proportion_mean`, `tc_proportion_stdev`) %>%
    mutate('plus_stdev'=round(`tc_proportion_mean`,1) + round(`tc_proportion_stdev`,1)) %>%
    mutate('minus_stdev'=round(`tc_proportion_mean`,1) - round(`tc_proportion_stdev`,1)) 
  
  
  tc_bame_plot_colour <- bame_to_plot %>% mutate("rag_rating"=case_when((proportion <=  tc_avg_stdev_bame$plus_stdev & proportion >= tc_avg_stdev_bame$minus_stdev) ~ 'orange',
                                                                        proportion >= tc_avg_stdev_bame$plus_stdev ~ 'red',
                                                                        proportion <= tc_avg_stdev_bame$minus_stdev ~ 'green'))
  
  
  # label to show
  label_to_show <- paste(round(avgs$`tc_proportion_mean`,0), '%', '\n','(regional avg)')
  
  return(
    bame <- bame_to_plot %>%
      e_charts(x = Indicator) %>%
      e_bar(proportion, bar_width=0.1, showBackground=T) %>%
      e_labels(position = "right", color='black') %>%
      e_color(c(tc_bame_plot_colour$rag_rating)) %>%
      #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
      #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
      e_mark_line(data=tc_avg_bame, symbol = "none", lineStyle = list(color = "black"), title=label_to_show, label=list(formatter='label',fontSize=10)) %>%
      e_hide_grid_lines() %>%
      e_flip_coords() %>%
      e_grid(containLabel = TRUE, left=30, right=30, top=10, bottom=5, height='60%') %>%
      
      #e_rm_axis(axis="x") %>%
      #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
      e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
      e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
      e_y_axis(show=F) %>%
      e_legend(FALSE)
  )
}




# --- national level section 95 ---
sec95_stats_eng_text <- function(indicators){
  eng_sec95_to_write <- indicators %>% select('eng_people_recieving_section_95_support','prop_eng_receiving_section_95_support') %>%
    unique()
  
  eng_sec95_to_write <- eng_sec95_to_write %>% filter(!is.na(eng_people_recieving_section_95_support) & !is.na('prop_eng_receiving_section_95_support'))
  
  write_eng_sec95 <- paste0("(",eng_sec95_to_write$prop_eng_receiving_section_95_support,"% of the population)")
  
  return(
    div(style= " text-align: center;margin-top:5px;",
        #hr(),
        p(format(eng_sec95_to_write$eng_people_recieving_section_95_support, big.mark=',', scientific = F), tags$br(),
          "people receiving support whilst seeking asylum (Section 95 support)")
        #p(tags$strong('No. of people receiving Section 95 support:'), format(eng_sec95_to_write$eng_people_recieving_section_95_support, big.mark=',', scientific = F), "people", tags$br(), write_eng_sec95)
    )
  )
  
}

sec95_stats_eng_plot <- function(indicators){
  eng_sec95_to_write <- indicators %>% select('eng_people_recieving_section_95_support','prop_eng_receiving_section_95_support') %>%
    unique()
  
  eng_sec95_to_plot <- eng_sec95_to_write %>% select('prop_eng_receiving_section_95_support') %>%
    pivot_longer('prop_eng_receiving_section_95_support', names_to = "Indicator", values_to = "proportion")
  
  eng_sec95_to_plot <- eng_sec95_to_plot %>% filter(!is.na(proportion)) %>%
    unique()
  
  return(
    sec95 <- eng_sec95_to_plot %>%
      e_charts(x = Indicator) %>%
      e_bar(proportion, bar_width=0.1,showBackground=T) %>%
      e_labels(position = "right", color='black') %>%
      e_color(c('purple')) %>%
      e_hide_grid_lines() %>%
      e_flip_coords() %>%
      e_grid(containLabel = TRUE, left=30, right=30, top=5, bottom=5, height='60%') %>%
      e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
      e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
      e_y_axis(show=F) %>%
      e_legend(FALSE)
  )
  
}


# --- national level homelessness ---
homelessness_stats_eng_text <- function(indicators){
  
  eng_homeless_to_write <- indicators %>% select('eng_rate_per_1000') %>%
    unique()
  
  eng_homeless_to_write <- eng_homeless_to_write %>% filter(!is.na('eng_rate_per_1000'))
  
  write_eng_homeless <- paste0("(",eng_homeless_to_write$eng_rate_per_1000," homelessness rate per 1000)")
  
  
  return( div(style= " text-align: center;margin-top:5px;",
          #hr(),
          p(format(round(eng_homeless_to_write$eng_rate_per_1000,2), big.mark=',', scientific = F), tags$br(),
          'homeless people per 1000')
          #p(tags$strong('No. of people homeless:'), format(eng_homeless_to_write$eng_total_homeless, big.mark=',', scientific = F), "people", tags$br(), write_eng_homeless),
      )
  )
}

homelessness_stats_eng_plot <- function(indicators){
  eng_homeless_to_write <- indicators %>% select('eng_rate_per_1000') %>%
    unique()
  
  eng_homeless_to_plot <- eng_homeless_to_write %>% select('eng_rate_per_1000') %>%
    pivot_longer('eng_rate_per_1000', names_to = "Indicator", values_to = "proportion")
  
  eng_homeless_to_plot <- eng_homeless_to_plot %>% filter(!is.na(proportion)) %>%
    unique() %>%
    mutate('proportion'=round(proportion,2))
  
  return(  # Plot population statistics
    homelessness <- eng_homeless_to_plot %>%
      e_charts(x = Indicator) %>%
      e_bar(proportion, bar_width=0.1,showBackground=T) %>%
      e_labels(position = "right", color='black') %>%
      e_color(c('purple')) %>%
      e_hide_grid_lines() %>%
      e_flip_coords() %>%
      e_grid(containLabel = TRUE, left=30, right=30, top=5, bottom=5, height='60%') %>%
      e_x_axis(position='top', axisLabel=list(show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=1000, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=1000) %>%
      e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
      e_y_axis(show=F) %>%
      e_legend(FALSE)
  )
}

# --- National fuel poverty stats ----
fuelp_stats_eng_text <- function(indicators){
  
  eng_fuelp_to_write <- indicators %>% select('eng_total_fuel_poor_households','eng_prop_households_fuel_poor') %>%
    unique()
  
  eng_fuelp_to_write <- eng_fuelp_to_write %>% filter(!is.na(eng_total_fuel_poor_households) & !is.na('eng_prop_households_fuel_poor'))
  
  write_eng_fuelp <- paste0("(",eng_fuelp_to_write$eng_prop_households_fuel_poor,"% of households)")
  
  return(
    
    div(style= " text-align: center;margin-top:5px;",
        p(format(eng_fuelp_to_write$eng_total_fuel_poor_households, big.mark=',', scientific = F),
          tags$br(),
          "households in fuel poverty"
        )
    )
    
  )
  
}

fuelp_stats_eng_plot <- function(indicators){
  
  eng_fuelp_to_write <- indicators %>% select('eng_total_fuel_poor_households','eng_prop_households_fuel_poor') %>%
    unique()
  
  eng_fuelp_to_plot <- eng_fuelp_to_write %>% select('eng_prop_households_fuel_poor') %>%
    pivot_longer('eng_prop_households_fuel_poor', names_to = "Indicator", values_to = "proportion")
  
  eng_fuelp_to_plot <- eng_fuelp_to_plot %>% filter(!is.na(proportion)) %>%
    unique() %>%
    mutate('proportion'=round(proportion,0))
  
  
  return(
    
    # # Plot population statistics
    fuelp_t <- eng_fuelp_to_plot %>%
      e_charts(x = Indicator) %>%
      e_bar(proportion, bar_width=0.1,showBackground=T) %>%
      e_labels(position = "right", color='black') %>%
      e_color(c('purple')) %>%
      e_hide_grid_lines() %>%
      e_flip_coords() %>%
      e_grid(containLabel = TRUE, left=30, right=30, top=5, bottom=5, height='60%') %>%
      e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
      e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
      e_y_axis(show=F) %>%
      e_legend(FALSE)

    
  )
  
}

# --- National unemployment ---

unemployment_stats_eng_text <- function(indicators){

  eng_unem_to_write <- indicators %>% select('eng_total_unemployed_on_ucred','prop_eng_pop_unemployed_on_ucred') %>%
    unique()

  eng_unem_to_write <- eng_unem_to_write %>% filter(!is.na(eng_total_unemployed_on_ucred) & !is.na('prop_eng_pop_unemployed_on_ucred'))

  write_eng_unem <- paste0("(",eng_unem_to_write$prop_eng_pop_unemployed_on_ucred,"% of people)")
  
  return( 
    div(style= " text-align: center;margin-top:5px;",
         #hr(),
        p(format(eng_unem_to_write$eng_total_unemployed_on_ucred, big.mark=',', scientific = F),
        tags$br(),
        "people unemployed on universal credit"
         )
  ))

}


unemployment_stats_eng_plot <- function(indicators){
  
  eng_unem_to_write <- indicators %>% select('eng_total_unemployed_on_ucred','prop_eng_pop_unemployed_on_ucred') %>%
    unique()
  
  eng_unem_to_plot <- eng_unem_to_write %>% select('prop_eng_pop_unemployed_on_ucred') %>%
    pivot_longer('prop_eng_pop_unemployed_on_ucred', names_to = "Indicator", values_to = "proportion")
  
  eng_unem_to_plot <- eng_unem_to_plot %>% filter(!is.na(proportion)) %>%
    unique() %>%
    mutate('proportion'=round(proportion,0))
  
  return(
    # # Plot population statistics
    unem_t <- eng_unem_to_plot %>%
      e_charts(x = Indicator) %>%
      e_bar(proportion, bar_width=0.1,showBackground=T) %>%
      e_labels(position = "right", color='black') %>%
      e_color(c('purple')) %>%
      e_hide_grid_lines() %>%
      e_flip_coords() %>%
      e_grid(containLabel = TRUE, left=30, right=30, top=5, bottom=5, height='60%') %>%
      e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
      e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
      e_y_axis(show=F) %>%
      e_legend(FALSE)
    
  )
  
}


# --- National Digital exclusion ---
ditigal_exclusion_eng_text <- function(indicators) {
  
  return( 
    div(style= " text-align: center;margin-top:5px;",
    #hr(),
    p(tags$strong('Data not currently available at national level for digital exclusion'))
    )
  )
}



# --- National Digital exclusion ---
shielding_eng_text <- function(indicators) {
  
  eng_shielding_to_write <- indicators %>% select('total_shielding_eng','proportion_total_shielding_Eng') %>%
    unique()
  
  eng_shielding_to_write <- eng_shielding_to_write %>% filter(!is.na(total_shielding_eng) & !is.na('proportion_total_shielding_Eng'))
  
  write_eng_shielding <- paste0("(",eng_shielding_to_write$proportion_total_shielding_Eng,"% of people)")
  
  
  return(
    div(style= " text-align: center;margin-top:5px;",
        p(format(eng_shielding_to_write$total_shielding_eng, big.mark=',', scientific = F),
          tags$br(),
          "people clinically extremely vulnerable")
    )
  )
  
}


shielding_eng_plot <- function(indicators) {
  
  eng_shielding_to_write <- indicators %>% select('total_shielding_eng','proportion_total_shielding_Eng') %>%
    unique()
  
  eng_shielding_to_plot <- eng_shielding_to_write %>% select('proportion_total_shielding_Eng') %>%
    pivot_longer('proportion_total_shielding_Eng', names_to = "Indicator", values_to = "proportion")
  
  eng_shielding_to_plot <- eng_shielding_to_plot %>% filter(!is.na(proportion)) %>%
    unique()
  
  return(
    # # Plot population statistics
    shielding_t <- eng_shielding_to_plot %>%
      e_charts(x = Indicator) %>%
      e_bar(proportion, bar_width=0.1,showBackground=T) %>%
      e_labels(position = "right", color='black') %>%
      e_color(c('purple')) %>%
      e_hide_grid_lines() %>%
      e_flip_coords() %>%
      e_grid(containLabel = TRUE, left=30, right=30, top=5, bottom=5, height='60%') %>%
      e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
      e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
      e_y_axis(show=F) %>%
      e_legend(FALSE)
    
  )
  
}





