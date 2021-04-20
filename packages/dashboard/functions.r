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


# --- Deomgraphic functions ---

# --- BAME population ---
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
      e_mark_line(data=tc_avg_bame, symbol = "none", lineStyle = list(color = "black"), title=label_to_show, label=list(formatter='label',fontSize=10)) %>%
      e_hide_grid_lines() %>%
      e_flip_coords() %>%
      e_grid(containLabel = TRUE, left=30, right=30, top=10, bottom=5, height='60%') %>%
      e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
      e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
      e_y_axis(show=F) %>%
      e_legend(FALSE)
  )
}


# --- LAD level ----
ethnicity_stats_lad_text <- function(indicators, avgs, lad) {
  lad_bame_to_plot <- indicators %>% select('LAD19CD',`Percentage of population who are ethnic minority`) %>%
    filter(LAD19CD == lad$LAD19CD)
  
  # transpose dataframe
  lad_bame_to_plot  <- lad_bame_to_plot %>% pivot_longer(`Percentage of population who are ethnic minority`, names_to = "Indicator", values_to = "proportion") %>%
    unique() %>% filter(!is.na(proportion)) %>%
    mutate('proportion'=round(proportion,0))
  
  # to format percentage
  lad_bame_to_show <- paste0(round(lad_bame_to_plot$proportion,0), "%")
  
  if (dim(lad_bame_to_plot)[1] != 0) {
    return(
      div(style= "text-align: center;margin-top:5px;",
          p(lad_bame_to_show,
            tags$br(),
            'of the population are BAME')
      )
    )
  }
  
  else {
    lad_bame_to_show <- "Data unavailable"
    
    return(
      div(style= "text-align: center;margin-top:5px;",
          p(lad_bame_to_show,
            tags$br(),
            'for % of the population who are BAME')
      )
    )
    
  }
  
}

ethnicity_stats_lad_plot <- function(indicators, avgs, lad) {
  
  lad_bame_to_plot <- indicators %>% select('LAD19CD',`Percentage of population who are ethnic minority`) %>%
    filter(LAD19CD == lad$LAD19CD)
  
  # transpose dataframe
  lad_bame_to_plot  <- lad_bame_to_plot %>% pivot_longer(`Percentage of population who are ethnic minority`, names_to = "Indicator", values_to = "proportion") %>%
    unique() %>% filter(!is.na(proportion)) %>%
    mutate('proportion'=round(proportion,0))
  
  
  # for echarts
  lad_avg_bame <- avgs %>% select(`Percentage of population who are ethnic minority_mean`) %>%
    select('xAxis' = `Percentage of population who are ethnic minority_mean`) %>%
    mutate('xAxis'= round(xAxis,0)) %>%
    as.list()
  
  # for echarts colour
  lad_avg_stdev_bame <- avgs %>% 
    select(`Percentage of population who are ethnic minority_mean`, `Percentage of population who are ethnic minority_stdev`) %>%
    mutate('plus_stdev'=round(`Percentage of population who are ethnic minority_mean`,1)+round(`Percentage of population who are ethnic minority_stdev`,1)) %>%
    mutate('minus_stdev'=round(`Percentage of population who are ethnic minority_mean`,1) - round(`Percentage of population who are ethnic minority_stdev`,1)) 
  
  
  bame_plot_colour <- lad_bame_to_plot %>% mutate("rag_rating"=case_when((proportion <=  lad_avg_stdev_bame$plus_stdev & proportion >= lad_avg_stdev_bame$minus_stdev) ~ 'orange',
                                                                         proportion >= lad_avg_stdev_bame$plus_stdev ~ 'red',
                                                                         proportion <= lad_avg_stdev_bame$minus_stdev ~ 'green'))
  
  # label to sho
  lad_label_to_show <- paste0(round(avgs$`Percentage of population who are ethnic minority_mean`,0), '%', '\n','(eng avg)')
  
  if (dim(lad_bame_to_plot)[1] != 0) {
    return(
      # # Plot population statistics
      bame <- lad_bame_to_plot %>%
        e_charts(x = Indicator) %>%
        e_bar(proportion, bar_width=0.1, showBackground=T, label=list(show=T, color='black', position='right')) %>%
        #e_labels(position = "right", color='black') %>%
        e_color(c(bame_plot_colour$rag_rating)) %>%
        #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
        #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
        e_mark_line(data=lad_avg_bame, symbol = "none", lineStyle = list(color = "black"), title=lad_label_to_show, label=list(formatter='label',fontSize=10)) %>%
        
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
  
  else {
    return()
  }
  
  
}



# --- section 95 statistics ---
# --- National Level ---
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


# --- MAC Level ---
sec95_stats_mac_text <- function(indicators, avgs) {
  
  tc_sec95_to_write <- indicators %>% select(`tc_People receiving Section 95 support`,'tc_prop_people_recieving_section_95_support') %>%
    unique()
  
  tc_sec95_to_write <- tc_sec95_to_write %>% filter(!is.na(`tc_People receiving Section 95 support`) & !is.na('tc_prop_people_recieving_section_95_support'))
  
  write_tc_sec95 <- paste0("(",tc_sec95_to_write$tc_prop_people_recieving_section_95_support,"% of the population)")
  
  return(
    div(style= " text-align: center;margin-top:5px;",
        p(format(tc_sec95_to_write$`tc_People receiving Section 95 support`, big.mark=',', scientific = F),
          tags$br(),
          'people receiving support whilst seeking asylum (Section 95 support)')
    )
  )
}


sec95_stats_mac_plot <- function(indicators, avgs) {
  
  tc_sec95_to_write <- indicators %>% select(`tc_People receiving Section 95 support`,'tc_prop_people_recieving_section_95_support') %>%
    unique()
  
  tc_sec95_to_plot <- tc_sec95_to_write %>% select('tc_prop_people_recieving_section_95_support') %>%
    pivot_longer('tc_prop_people_recieving_section_95_support', names_to = "Indicator", values_to = "proportion")
  
  tc_sec95_to_plot <- tc_sec95_to_plot %>% filter(!is.na(proportion)) %>%
    unique()
  
  # for echarts
  tc_avg_section95 <- avgs %>% select('tc_prop_people_recieving_section_95_support_mean') %>%
    select('xAxis' = 'tc_prop_people_recieving_section_95_support_mean') %>%
    as.list()
  
  tc_avg_stdev_sec95 <- avgs %>% 
    select('tc_prop_people_recieving_section_95_support_mean', 'tc_prop_people_recieving_section_95_support_stdev') %>%
    mutate('plus_stdev'=round(tc_prop_people_recieving_section_95_support_mean,2)+round(tc_prop_people_recieving_section_95_support_stdev,2)) %>%
    mutate('minus_stdev'=round(tc_prop_people_recieving_section_95_support_mean,2)- round(tc_prop_people_recieving_section_95_support_stdev,2)) 
  
  
  
  tc_sec95_plot_colour <- tc_sec95_to_plot %>% mutate("rag_rating"=case_when((proportion <=  tc_avg_stdev_sec95$plus_stdev & proportion >= tc_avg_stdev_sec95$minus_stdev) ~ 'orange',
                                                                             proportion >= tc_avg_stdev_sec95$plus_stdev ~ 'red',
                                                                             proportion <= tc_avg_stdev_sec95$minus_stdev ~ 'green'))
  
  
  
  tc_sec95_for_avg = paste0(avgs$tc_prop_people_recieving_section_95_support_mean, '%', '\n','(regional avg)')
  

  return(
    # # Plot population statistics
    sec95 <- tc_sec95_to_plot %>%
      e_charts(x = Indicator) %>%
      e_bar(proportion, bar_width=0.1,showBackground=T) %>%
      e_labels(position = "right", color='black') %>%
      e_color(c(tc_sec95_plot_colour$rag_rating)) %>%
      e_mark_line(data=tc_avg_section95, symbol = "none", lineStyle = list(color = "black"), title=tc_sec95_for_avg, label=list(formatter='label',fontSize=10)) %>%
      e_hide_grid_lines() %>%
      e_flip_coords() %>%
      e_grid(containLabel = TRUE, left=30, right=30, top=10, bottom=5, height='60%') %>%
      e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=1, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=1) %>%
      e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
      e_y_axis(show=F) %>%
      e_legend(FALSE)
  )
  
}


sec95_stats_lad_text <- function(indicators, avgs, lad) {
  
  lad_sec95_to_write <- indicators %>% select('LAD19CD',`People receiving Section 95 support`,'lad_prop_recieving_section_95_support') %>%
    filter(LAD19CD == lad$LAD19CD) %>%
    unique()
  
  lad_sec95_to_plot <- lad_sec95_to_write %>% select('lad_prop_recieving_section_95_support') %>%
    pivot_longer('lad_prop_recieving_section_95_support', names_to = "Indicator", values_to = "proportion")
  
  lad_sec95_to_plot <- lad_sec95_to_plot %>% filter(!is.na(proportion)) %>%
    unique() 
  
  lad_sec95_to_write <- lad_sec95_to_write %>% filter(!is.na(`People receiving Section 95 support`) & !is.na('lad_prop_recieving_section_95_support'))
  
  write_lad_sec95 <- paste0("(",lad_sec95_to_write$lad_prop_recieving_section_95_support,"% of the population)")
  
  if(dim(lad_sec95_to_plot)[1] != 0) {
    return(
      div(style= " text-align: center;margin-top:5px;",
          p(format(lad_sec95_to_write$`People receiving Section 95 support`, big.mark=',', scientific = F),
            tags$br(),
            'people receiving support whilst seeking asylum (Section 95 support)')
      )
    )
  }
  
  else {
    return(
      div(style= " text-align: center;margin-top:5px;",
          #hr(),
          p("Data unavailable",
            tags$br(),
            'people receiving support whilst seeking asylum (Section 95 support)')
      )
    )
  }
  
}


sec95_stats_lad_plot <- function(indicators, avgs, lad) {
  
  lad_sec95_to_write <- indicators %>% select('LAD19CD',`People receiving Section 95 support`,'lad_prop_recieving_section_95_support') %>%
    filter(LAD19CD == lad$LAD19CD) %>%
    unique()
  
  lad_sec95_to_plot <- lad_sec95_to_write %>% select('lad_prop_recieving_section_95_support') %>%
    pivot_longer('lad_prop_recieving_section_95_support', names_to = "Indicator", values_to = "proportion")
  
  lad_sec95_to_plot <- lad_sec95_to_plot %>% filter(!is.na(proportion)) %>%
    unique() 
  
  # for echarts
  lad_avg_section95 <- avgs %>% select(`lad_prop_recieving_section_95_support_mean`) %>%
    select('xAxis' = `lad_prop_recieving_section_95_support_mean`) %>%
    as.list()
  
  lad_avg_stdev_sec95 <- avgs %>% 
    select(`lad_prop_recieving_section_95_support_mean`, `lad_prop_recieving_section_95_support_stdev`) %>%
    mutate('plus_stdev'=round(`lad_prop_recieving_section_95_support_mean`,1)+round(`lad_prop_recieving_section_95_support_stdev`,1)) %>%
    mutate('minus_stdev'=round(`lad_prop_recieving_section_95_support_mean`,1) - round(`lad_prop_recieving_section_95_support_stdev`,1)) 
  
  
  sec95_plot_colour <- lad_sec95_to_plot %>% mutate("rag_rating"=case_when((proportion <=  lad_avg_stdev_sec95$plus_stdev & proportion >= lad_avg_stdev_sec95$minus_stdev) ~ 'orange',
                                                                           proportion >= lad_avg_stdev_sec95$plus_stdev ~ 'red',
                                                                           proportion <= lad_avg_stdev_sec95$minus_stdev ~ 'green'))
  
  
  lad_sec95_for_avg = paste0(round(avgs$lad_prop_recieving_section_95_support_mean,2), '%', '\n','(eng avg)')
  
  if(dim(lad_sec95_to_plot)[1] != 0) {
    return(
      # # Plot population statistics
      sec95 <- lad_sec95_to_plot %>%
        
        e_charts(x = Indicator) %>%
        e_bar(proportion, bar_width=0.1,showBackground=T) %>%
        e_labels(position = "right", color='black') %>%
        e_color(c(sec95_plot_colour$rag_rating)) %>%
        #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
        #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
        e_mark_line(data=lad_avg_section95, symbol = "none", lineStyle = list(color = "black"), title=lad_sec95_for_avg,label=list(formatter='label',fontSize=10)) %>%
        e_hide_grid_lines() %>%
        e_flip_coords() %>%
        e_grid(containLabel = TRUE, left=30, right=30, top=10, bottom=5, height='60%') %>%
        
        #e_rm_axis(axis="x") %>%
        #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
        e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=1, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=1) %>%
        e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
        e_y_axis(show=F) %>%
        e_legend(FALSE)
    )
  }
  
  else {
    return()
  }
  
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


# --- MAC level ---
homelessness_stats_mac_text <- function(indicators, avgs) {
  
  tc_homeless_to_write <- indicators %>% select(`tc_Homelessness (rate per 1000)`) %>%
    unique()
  
  tc_homeless_to_write <- tc_homeless_to_write %>% filter(!is.na(`tc_Homelessness (rate per 1000)`))
  
  write_tc_homeless <- paste0("(",tc_homeless_to_write$`tc_Homelessness (rate per 1000)`,"homelessness rate per 1000)")
  
  return(
    div(style= " text-align: center;margin-top:5px;",
        #hr(),
        p(format(round(tc_homeless_to_write$`tc_Homelessness (rate per 1000)`,2), big.mark=',', scientific = F),
          tags$br(),
          "homeless per 1000")
        #p(tags$strong('No. of homeless people:'), format(tc_homeless_to_write$tc_total_homeless, big.mark=',', scientific = F), "people", tags$br(), write_tc_homeless)
    )
  )
  
}


homelessness_stats_mac_plot <- function(indicators, avgs) {
  
  tc_homeless_to_write <- indicators %>% select(`tc_Homelessness (rate per 1000)`) %>%
    unique()
  
  tc_homeless_to_plot <- tc_homeless_to_write %>% select(`tc_Homelessness (rate per 1000)`) %>%
    pivot_longer(`tc_Homelessness (rate per 1000)`, names_to = "Indicator", values_to = "proportion")
  
  tc_homeless_to_plot <- tc_homeless_to_plot %>% filter(!is.na(proportion)) %>%
    unique() %>% mutate('proportion'=round(proportion,2))
  
  # for echarts
  tc_avg_homeless <- avgs %>% select(`tc_Homelessness (rate per 1000)_mean`) %>%
    select('xAxis' = `tc_Homelessness (rate per 1000)_mean`) %>%
    as.list()
  
  tc_avg_stdev_homeless <- avgs %>% 
    select(`tc_Homelessness (rate per 1000)_mean`, `tc_Homelessness (rate per 1000)_stdev`) %>%
    mutate('plus_stdev'=round(`tc_Homelessness (rate per 1000)_mean`,2)+round(`tc_Homelessness (rate per 1000)_stdev`,2)) %>%
    mutate('minus_stdev'=round(`tc_Homelessness (rate per 1000)_mean`,2)- round(`tc_Homelessness (rate per 1000)_stdev`,2)) 
  
  
  
  tc_homeless_plot_colour <- tc_homeless_to_plot %>% mutate("rag_rating"=case_when((proportion <=  tc_avg_stdev_homeless$plus_stdev & proportion >= tc_avg_stdev_homeless$minus_stdev) ~ 'orange',
                                                                                   proportion >= tc_avg_stdev_homeless$plus_stdev ~ 'red',
                                                                                   proportion <= tc_avg_stdev_homeless$minus_stdev ~ 'green'))
  
  
  
  
  tc_homeless_for_avg = paste0(round(avgs$`tc_Homelessness (rate per 1000)_mean`,2), '\n','(regional avg)')

  return(
    homeless <- tc_homeless_to_plot %>%
      e_charts(x = Indicator) %>%
      e_bar(proportion, bar_width=0.1,showBackground=T) %>%
      e_labels(position = "right", color='black') %>%
      e_color(c(tc_homeless_plot_colour$rag_rating)) %>%
      e_mark_line(data=tc_avg_homeless, symbol = "none", lineStyle = list(color = "black"), title=tc_homeless_for_avg, label=list(formatter='label',fontSize=10)) %>%
      e_hide_grid_lines() %>%
      e_flip_coords() %>%
      e_grid(containLabel = TRUE, left=30, right=30, top=10, bottom=5, height='60%') %>%
      e_x_axis(position='top', axisLabel=list(show=T, fontSize=8, formatter = "{value}\nper 1000",showMinLabel=F, fontWeight='bold', margin=2),min=0, max=20, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=20) %>%
      e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
      e_y_axis(show=F) %>%
      e_legend(FALSE)
    
  )
}

# --- LAD Level ---
homelessness_stats_lad_text <- function(indicators, avgs, lad) {
  lad_homeless_to_write <- indicators %>% select('LAD19CD',`Homelessness (rate per 1000)`) %>%
    filter(LAD19CD == lad$LAD19CD) %>%
    unique()
  
  lad_homeless_to_plot <- lad_homeless_to_write %>% select(`Homelessness (rate per 1000)`) %>%
    pivot_longer(`Homelessness (rate per 1000)`, names_to = "Indicator", values_to = "proportion")
  
  lad_homeless_to_plot <- lad_homeless_to_plot %>% filter(!is.na(proportion)) %>%
    unique() %>%
    mutate('proportion'=round(proportion,2))
  
  lad_homeless_to_write <- lad_homeless_to_write %>% filter(!is.na(`Homelessness (rate per 1000)`))
  
  write_lad_homeless <- paste0("(",lad_homeless_to_write$`Homelessness (rate per 1000)`,"homelessness per 1000)")
  
  
  if(dim(lad_homeless_to_plot)[1] != 0) {
    return(
      div(style= " text-align: center;margin-top:5px;",
          #hr(),
          p(format(round(lad_homeless_to_write$`Homelessness (rate per 1000)`,2), big.mark=',', scientific = F),
            tags$br(),
            'homeless per 1000')
          #p(tags$strong('No. of homeless people:'), format(lad_homeless_to_write$lad_total_homeless, big.mark=',', scientific = F), "people", tags$br(), write_lad_homeless)
      )
    )
    
  }
  else {
    return(
      div(style= " text-align: center;margin-top:5px;",
          #hr(),
          p("Data unavailable",
            tags$br(),
            'for homeless people')
          #p(tags$strong('No. of homeless people:'), format(lad_homeless_to_write$lad_total_homeless, big.mark=',', scientific = F), "people", tags$br(), write_lad_homeless)
      )
    )
  }
  
}


homelessness_stats_lad_plot <- function(indicators, avgs, lad) {
  lad_homeless_to_write <- indicators %>% select('LAD19CD',`Homelessness (rate per 1000)`) %>%
    filter(LAD19CD == lad$LAD19CD) %>%
    unique()
  
  lad_homeless_to_plot <- lad_homeless_to_write %>% select(`Homelessness (rate per 1000)`) %>%
    pivot_longer(`Homelessness (rate per 1000)`, names_to = "Indicator", values_to = "proportion")
  
  lad_homeless_to_plot <- lad_homeless_to_plot %>% filter(!is.na(proportion)) %>%
    unique() %>%
    mutate('proportion'=round(proportion,2))
  
  lad_homeless_to_write <- lad_homeless_to_write %>% filter(!is.na(`Homelessness (rate per 1000)`))
  
  write_lad_homeless <- paste0("(",lad_homeless_to_write$`Homelessness (rate per 1000)`,"homelessness per 1000)")
  
  # for echarts
  lad_avg_homeless <- avgs %>% select(`Homelessness (rate per 1000)_mean`) %>%
    select('xAxis' = `Homelessness (rate per 1000)_mean`) %>%
    as.list()
  
  lad_avg_stdev_homeless <- avgs %>% 
    select(`Homelessness (rate per 1000)_mean`, `Homelessness (rate per 1000)_stdev`) %>%
    mutate('plus_stdev'=round(`Homelessness (rate per 1000)_mean`,2)+round(`Homelessness (rate per 1000)_stdev`,2)) %>%
    mutate('minus_stdev'=round(`Homelessness (rate per 1000)_mean`,2) - round(`Homelessness (rate per 1000)_stdev`,2)) 
  
  
  homeless_plot_colour <- lad_homeless_to_plot %>% mutate("rag_rating"=case_when((proportion <=  lad_avg_stdev_homeless$plus_stdev & proportion >= lad_avg_stdev_homeless$minus_stdev) ~ 'orange',
                                                                                 proportion >= lad_avg_stdev_homeless$plus_stdev ~ 'red',
                                                                                 proportion <= lad_avg_stdev_homeless$minus_stdev ~ 'green'))
  
  
  
  lad_homeless_for_avg = paste0(round(avgs$`Homelessness (rate per 1000)_mean`,2), '\n','(eng avg)')
  
  
  if(dim(lad_homeless_to_plot)[1] != 0) {
    return(
      # # Plot population statistics
      sec95 <- lad_homeless_to_plot %>%
        e_charts(x = Indicator) %>%
        e_bar(proportion, bar_width=0.1,showBackground=T) %>%
        e_labels(position = "right", color='black') %>%
        e_color(c(homeless_plot_colour$rag_rating)) %>%
        #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
        #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
        e_mark_line(data=lad_avg_homeless, symbol = "none", lineStyle = list(color = "black"), title=lad_homeless_for_avg, label=list(formatter='label',fontSize=10)) %>%
        e_hide_grid_lines() %>%
        e_flip_coords() %>%
        e_grid(containLabel = TRUE, left=30, right=30, top=10, bottom=5, height='60%') %>%
        
        #e_rm_axis(axis="x") %>%
        #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
        e_x_axis(position='top', axisLabel=list(show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2, suffix='per 1000'),min=0, max=20, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=20) %>%
        e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
        e_y_axis(show=F) %>%
        e_legend(FALSE)
    )
    
  }
  else {
    return()
  }
  
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


# --- MAC level ---
fuelp_stats_mac_text <- function(indicators, avgs){
  
  tc_fuelp_to_write <- indicators %>% select(`tc_Number of households in fuel poverty1`,'tc_prop_households_fuel_poor') %>%
    unique()
  
  tc_fuelp_to_write <- tc_fuelp_to_write %>% 
    filter(!is.na(`tc_Number of households in fuel poverty1`) & !is.na('tc_prop_households_fuel_poor')) %>%
    mutate(`tc_Number of households in fuel poverty1`=round(`tc_Number of households in fuel poverty1`,0))
  
  write_tc_fuelp <- paste0("(",tc_fuelp_to_write$tc_prop_households_fuel_poor,"% of households)")
  
  return(
    div(style= " text-align: center;margin-top:5px;",
        p(format(tc_fuelp_to_write$`tc_Number of households in fuel poverty1`, big.mark=',', scientific = F),
          tags$br(),
          'households in fuel poverty')
    )
  )
}

fuelp_stats_mac_plot <- function(indicators, avgs){
  tc_fuelp_to_write <- indicators %>% select(`tc_Number of households in fuel poverty1`,'tc_prop_households_fuel_poor') %>%
    unique()
  
  tc_fuelp_to_plot <- tc_fuelp_to_write %>% select('tc_prop_households_fuel_poor') %>%
    pivot_longer('tc_prop_households_fuel_poor', names_to = "Indicator", values_to = "proportion") 
  
  tc_fuelp_to_plot <- tc_fuelp_to_plot %>% filter(!is.na(proportion)) %>%
    unique() %>%
    mutate('proportion'=round(proportion,0))
  
  # for echarts
  tc_avg_fuelp <- avgs %>% select(`tc_prop_households_fuel_poor_mean`) %>%
    mutate(`tc_prop_households_fuel_poor`=round(`tc_prop_households_fuel_poor_mean`,0)) %>%
    select('xAxis' = `tc_prop_households_fuel_poor_mean`) %>%
    as.list()
  
  tc_avg_stdev_fuelp <- avgs %>% 
    select(`tc_prop_households_fuel_poor_mean`, `tc_prop_households_fuel_poor_stdev`) %>%
    mutate('plus_stdev'=round(`tc_prop_households_fuel_poor_mean`,2)+round(`tc_prop_households_fuel_poor_stdev`,2)) %>%
    mutate('minus_stdev'=round(`tc_prop_households_fuel_poor_mean`,2)- round(`tc_prop_households_fuel_poor_stdev`,2)) 
  
  
  
  tc_fuelp_plot_colour <- tc_fuelp_to_plot %>% mutate("rag_rating"=case_when((proportion <=  tc_avg_stdev_fuelp$plus_stdev & proportion >= tc_avg_stdev_fuelp$minus_stdev) ~ 'orange',
                                                                             proportion >= tc_avg_stdev_fuelp$plus_stdev ~ 'red',
                                                                             proportion <= tc_avg_stdev_fuelp$minus_stdev ~ 'green'))
  
  
  
  tc_fuelp_for_avg = paste0(round(avgs$tc_prop_households_fuel_poor_mean,0), '%', '\n','(regional avg)')
  
  return(
    # # Plot population statistics
    sec95 <- tc_fuelp_to_plot %>%
      e_charts(x = Indicator) %>%
      e_bar(proportion, bar_width=0.1,showBackground=T) %>%
      e_labels(position = "right", color='black') %>%
      e_color(c(tc_fuelp_plot_colour$rag_rating)) %>%
      e_mark_line(data=tc_avg_fuelp, symbol = "none", lineStyle = list(color = "black"), title=tc_fuelp_for_avg, label=list(formatter='label',fontSize=10)) %>%
      e_hide_grid_lines() %>%
      e_flip_coords() %>%
      e_grid(containLabel = TRUE, left=30, right=30, top=10, bottom=5, height='60%') %>%
      e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=25, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=25) %>%
      e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
      e_y_axis(show=F) %>%
      e_legend(FALSE)
  )
  
}

fuelp_stats_lad_text <- function(indicators, avgs, lad) {
  
  lad_fuelp_to_write <- indicators %>% select('LAD19CD',`Number of households in fuel poverty1`,`Proportion of households fuel poor (%)`) %>%
    filter(LAD19CD == lad$LAD19CD) %>%
    unique()
  
  lad_fuelp_to_plot <- lad_fuelp_to_write %>% select(`Proportion of households fuel poor (%)`) %>%
    pivot_longer(`Proportion of households fuel poor (%)`, names_to = "Indicator", values_to = "proportion")
  
  lad_fuelp_to_plot <- lad_fuelp_to_plot %>% filter(!is.na(proportion)) %>%
    unique() %>%
    mutate('proportion'=round(proportion,0))
  
  
  lad_fuelp_to_write <- lad_fuelp_to_write %>% filter(!is.na(`Number of households in fuel poverty1`) & !is.na(`Proportion of households fuel poor (%)`))
  
  
  write_lad_fuelp <- paste0("(",lad_fuelp_to_write$`Proportion of households fuel poor (%)`,"% of households)")
  
  if (dim(lad_fuelp_to_plot)[1] != 0) {
    return(
      div(style= " text-align: center;margin-top:5px;",
          #hr(),
          p(format(round(lad_fuelp_to_write$`Number of households in fuel poverty1`,0), big.mark=',', scientific = F),
            tags$br(),
            "households in fuel poverty")
          #p(tags$strong('No. of households in fuel poverty:'), format(lad_fuelp_to_write$`Number of households in fuel poverty1`, big.mark=',', scientific = F), "households", tags$br(), write_lad_fuelp)
          
      )
    )
  }
  else{
    return(
      div(style= " text-align: center;margin-top:5px;",
          #hr(),
          p("Data unavailable",
            tags$br(),
            "for households in fuel poverty")
      )
    )
  }
  
}

fuelp_stats_lad_plot <- function(indicators, avgs, lad) {
  
  lad_fuelp_to_write <- indicators %>% select('LAD19CD',`Number of households in fuel poverty1`,`Proportion of households fuel poor (%)`) %>%
    filter(LAD19CD == lad$LAD19CD) %>%
    unique()
  
  lad_fuelp_to_plot <- lad_fuelp_to_write %>% select(`Proportion of households fuel poor (%)`) %>%
    pivot_longer(`Proportion of households fuel poor (%)`, names_to = "Indicator", values_to = "proportion")
  
  lad_fuelp_to_plot <- lad_fuelp_to_plot %>% filter(!is.na(proportion)) %>%
    unique() %>%
    mutate('proportion'=round(proportion,0))
  
  # for echarts
  lad_avg_fuelp <- avgs %>% select(`Proportion of households fuel poor (%)_mean`) %>%
    select('xAxis' = `Proportion of households fuel poor (%)_mean`) %>%
    mutate('xAxis'=round(xAxis,0)) %>%
    as.list()
  
  
  lad_avg_stdev_fuelp <- avgs %>% 
    select(`Proportion of households fuel poor (%)_mean`, `Proportion of households fuel poor (%)_stdev`) %>%
    mutate('plus_stdev'=round(`Proportion of households fuel poor (%)_mean`,1)+round(`Proportion of households fuel poor (%)_stdev`,1)) %>%
    mutate('minus_stdev'=round(`Proportion of households fuel poor (%)_mean`,1) - round(`Proportion of households fuel poor (%)_stdev`,1)) 
  
  
  fuelp_plot_colour <- lad_fuelp_to_plot %>% mutate("rag_rating"=case_when((proportion <=  lad_avg_stdev_fuelp$plus_stdev & proportion >= lad_avg_stdev_fuelp$minus_stdev) ~ 'orange',
                                                                           proportion >= lad_avg_stdev_fuelp$plus_stdev ~ 'red',
                                                                           proportion <= lad_avg_stdev_fuelp$minus_stdev ~ 'green'))
  
  lad_fuelp_for_avg = paste0(round(avgs$`Proportion of households fuel poor (%)_mean`,0), '%', '\n','(eng avg)')
  
  if (dim(lad_fuelp_to_plot)[1] != 0) {
    return(
      # # Plot population statistics
      lad_fuelp <-  lad_fuelp_to_plot %>%
        e_charts(x = Indicator) %>%
        e_bar(proportion, bar_width=0.1,showBackground=T) %>%
        e_labels(position = "right", color='black') %>%
        e_color(c(fuelp_plot_colour$rag_rating)) %>%
        #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
        #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
        e_mark_line(data=lad_avg_fuelp, symbol = "none", lineStyle = list(color = "black"), title=lad_fuelp_for_avg, label=list(formatter='label',fontSize=10)) %>%
        e_hide_grid_lines() %>%
        e_flip_coords() %>%
        e_grid(containLabel = TRUE, left=30, right=30, top=10, bottom=5, height='60%') %>%
        
        #e_rm_axis(axis="x") %>%
        #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
        e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=25, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=25) %>%
        e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
        e_y_axis(show=F) %>%
        e_legend(FALSE)
    )
  }
  else{
    return()
  }
}


# --- unemployment ---
# --- National ---
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

# --- MAC level ---
unemployment_stats_mac_text <- function(indicators, avgs){
  
  tc_unem_to_write <- indicators %>% select(`tc_Not in employment`,'tc_prop_unemployed_on_universal_credit') %>%
    unique()
  
  tc_unem_to_write <- tc_unem_to_write %>% filter(!is.na(`tc_Not in employment`) & !is.na('tc_prop_unemployed_on_universal_credit'))
  
  write_tc_unem <- paste0("(",tc_unem_to_write$tc_prop_unemployed_on_universal_credit,"% of people)")
  
  return(
    div(style= " text-align: center;margin-top:5px;",
        p(format(tc_unem_to_write$`tc_Not in employment`, big.mark=',', scientific = F),
          tags$br(),
          "people unemployed on universal credit")
    )
  )
  
}

unemployment_stats_mac_plot <- function(indicators, avgs){
  
  tc_unem_to_write <- indicators %>% select(`tc_Not in employment`,'tc_prop_unemployed_on_universal_credit') %>%
    unique()
  
  tc_unem_to_plot <- tc_unem_to_write %>% select('tc_prop_unemployed_on_universal_credit') %>%
    pivot_longer('tc_prop_unemployed_on_universal_credit', names_to = "Indicator", values_to = "proportion") 
  
  tc_unem_to_plot <- tc_unem_to_plot %>% filter(!is.na(proportion)) %>%
    unique() %>%
    mutate('proportion'=round(proportion,0))
  
  # for echarts
  tc_avg_unem <- avgs %>% select(`tc_prop_unemployed_on_universal_credit_mean`) %>%
    mutate(`tc_prop_unemployed_on_universal_credit_mean`=round(`tc_prop_unemployed_on_universal_credit_mean`,0)) %>%
    select('xAxis' = `tc_prop_unemployed_on_universal_credit_mean`) %>%
    as.list()
  
  tc_avg_stdev_unem <- avgs %>% 
    select(`tc_prop_unemployed_on_universal_credit_mean`, `tc_prop_unemployed_on_universal_credit_stdev`) %>%
    mutate('plus_stdev'=round(`tc_prop_unemployed_on_universal_credit_mean`,2)+round(`tc_prop_unemployed_on_universal_credit_stdev`,2)) %>%
    mutate('minus_stdev'=round(`tc_prop_unemployed_on_universal_credit_mean`,2)- round(`tc_prop_unemployed_on_universal_credit_stdev`,2)) 
  
  
  tc_unem_plot_colour <- tc_unem_to_plot %>% mutate("rag_rating"=case_when((proportion <=  tc_avg_stdev_unem$plus_stdev & proportion >= tc_avg_stdev_unem$minus_stdev) ~ 'orange',
                                                                           proportion >= tc_avg_stdev_unem$plus_stdev ~ 'red',
                                                                           proportion <= tc_avg_stdev_unem$minus_stdev ~ 'green'))
  
  tc_unem_for_avg = paste0(round(avgs$tc_prop_unemployed_on_universal_credit_mean,0), '%', '\n','(regional avg)')
  
  return(
    # # Plot population statistics
    unem <- tc_unem_to_plot %>%
      e_charts(x = Indicator) %>%
      e_bar(proportion, bar_width=0.1,showBackground=T) %>%
      e_labels(position = "right", color='black') %>%
      e_color(c(tc_unem_plot_colour$rag_rating)) %>%
      e_mark_line(data=tc_avg_unem, symbol = "none", lineStyle = list(color = "black"), title=tc_unem_for_avg,label=list(formatter='label',fontSize=10)) %>%
      e_hide_grid_lines() %>%
      e_flip_coords() %>%
      e_grid(containLabel = TRUE, left=30, right=30, top=10, bottom=5, height='60%') %>%
      e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=10, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=10) %>%
      e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
      e_y_axis(show=F) %>%
      e_legend(FALSE)
    
  )
}

# --- LAD level ---

unemployment_stats_lad_text <- function(indicators, avgs, lad){
  lad_unem_to_write <- indicators %>% select('LAD19CD',`Not in employment`,'lad_prop_unemployed_on_ucred') %>%
    filter(LAD19CD == lad$LAD19CD) %>%
    unique()
  
  lad_unem_to_plot <- lad_unem_to_write %>% select('lad_prop_unemployed_on_ucred') %>%
    pivot_longer('lad_prop_unemployed_on_ucred', names_to = "Indicator", values_to = "proportion")
  
  lad_unem_to_plot <- lad_unem_to_plot %>% filter(!is.na(proportion)) %>%
    unique() %>%
    mutate('proportion'=round(proportion,0))
  
  lad_unem_to_write <- lad_unem_to_write %>% filter(!is.na(`Not in employment`) & !is.na('lad_prop_unemployed_on_ucred'))
  
  #write_lad_unem <- paste0("(",lad_unem_to_write$lad_prop_unemployed_on_ucred,"% of people)")

  if (dim(lad_unem_to_plot)[1] != 0) {
    return(
      div(style= " text-align: center;margin-top:5px;",
          p(format(lad_unem_to_write$`Not in employment`, big.mark=',', scientific = F),
            tags$br(),
            'people unemployed on universal credit')
      )
    )
  }
  
  else{
    return(
      div(style= " text-align: center;margin-top:5px;",
          p('Data unavailable',
            tags$br(),
            'for people unemployed on universal credit')
      )
    )
  }
}

unemployment_stats_lad_plot <- function(indicators, avgs, lad){
  
  lad_unem_to_write <- indicators %>% select('LAD19CD',`Not in employment`,'lad_prop_unemployed_on_ucred') %>%
    filter(LAD19CD == lad$LAD19CD) %>%
    unique()
  
  lad_unem_to_plot <- lad_unem_to_write %>% select('lad_prop_unemployed_on_ucred') %>%
    pivot_longer('lad_prop_unemployed_on_ucred', names_to = "Indicator", values_to = "proportion")
  
  lad_unem_to_plot <- lad_unem_to_plot %>% filter(!is.na(proportion)) %>%
    unique() %>%
    mutate('proportion'=round(proportion,0))
  
  # for echarts
  lad_avg_unem <- avgs %>% select(`lad_prop_unemployed_on_ucred_mean`) %>%
    select('xAxis' = `lad_prop_unemployed_on_ucred_mean`) %>%
    mutate('xAxis'=round(xAxis,0)) %>%
    as.list()
  
  lad_avg_stdev_unem <- avgs %>% 
    select(`lad_prop_unemployed_on_ucred_mean`, `lad_prop_unemployed_on_ucred_stdev`) %>%
    mutate('plus_stdev'=round(`lad_prop_unemployed_on_ucred_mean`,1)+round(`lad_prop_unemployed_on_ucred_stdev`,1)) %>%
    mutate('minus_stdev'=round(`lad_prop_unemployed_on_ucred_mean`,1) - round(`lad_prop_unemployed_on_ucred_stdev`,1)) 
  
  
  unem_plot_colour <- lad_unem_to_plot %>% mutate("rag_rating"=case_when((proportion <=  lad_avg_stdev_unem$plus_stdev & proportion >= lad_avg_stdev_unem$minus_stdev) ~ 'orange',
                                                                         proportion >= lad_avg_stdev_unem$plus_stdev ~ 'red',
                                                                         proportion <= lad_avg_stdev_unem$minus_stdev ~ 'green'))
  
  lad_unem_for_avg = paste0(round(avgs$lad_prop_unemployed_on_ucred_mean,0), '%', '\n','(eng avg)')
  
  if (dim(lad_unem_to_plot)[1] != 0) {
    return(
      # # Plot population statistics
      lad_unem <- lad_unem_to_plot %>%
        e_charts(x = Indicator) %>%
        e_bar(proportion, bar_width=0.1,showBackground=T) %>%
        e_labels(position = "right", color='black') %>%
        e_color(c(unem_plot_colour$rag_rating)) %>%
        e_mark_line(data=lad_avg_unem, symbol = "none", lineStyle = list(color = "black"), title=lad_unem_for_avg, label=list(formatter='label',fontSize=10)) %>%
        e_hide_grid_lines() %>%
        e_flip_coords() %>%
        e_grid(containLabel = TRUE, left=30, right=30, top=10, bottom=5, height='60%') %>%
        e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=10, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=10) %>%
        e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
        e_y_axis(show=F) %>%
        e_legend(FALSE)
    )
  }
  
  else{
    return()
  }
}


# --- Digital exclusion ---

# -- National Level ---
digital_exclusion_eng_text <- function(indicators) {
  return( 
    div(style= " text-align: center;margin-top:5px;",
    p(tags$strong('Data not currently available at national level for digital exclusion'))
    )
  )
}

# --- MAC Level ---
digital_exclusion_mac_text <- function(indicators, avgs) {
  tc_de_to_write <- indicators %>% select('tc_percent_digitally_excluded') %>%
    unique()
  
  tc_de_to_write <- tc_de_to_write %>% filter(!is.na(tc_percent_digitally_excluded))
  
  write_tc_de <- paste0(round(tc_de_to_write$tc_percent_digitally_excluded,0), "%")
  
  return(
    div(style= " text-align: center;margin-top:5px;",
        p(write_tc_de,
          tags$br(),
          "neighbourhoods in 20% most digitally excluded")
    )
  )
}

digital_exclusion_mac_plot <- function(indicators, avgs) {
  
  tc_de_to_write <- indicators %>% select('tc_percent_digitally_excluded') %>%
    unique()
  
  tc_de_to_plot <- tc_de_to_write %>% select('tc_percent_digitally_excluded') %>%
    pivot_longer('tc_percent_digitally_excluded', names_to = "Indicator", values_to = "proportion")
  
  tc_de_to_plot <- tc_de_to_plot %>% filter(!is.na(proportion)) %>%
    unique() %>%
    mutate('proportion'=round(proportion,0))
  
  # for echarts
  tc_avg_de <- avgs %>% select('tc_percent_digitally_excluded_mean') %>%
    #mutate('tc_percent_digitally_excluded'=round(tc_percent_digitally_excluded,0))
    select('xAxis' = `tc_percent_digitally_excluded_mean`) %>%
    mutate('xAxis' = round(xAxis,0)) %>%
    as.list()
  
  tc_avg_stdev_de <- avgs %>% 
    select(`tc_percent_digitally_excluded_mean`, `tc_percent_digitally_excluded_stdev`) %>%
    mutate('plus_stdev'=round(`tc_percent_digitally_excluded_mean`,2)+round(`tc_percent_digitally_excluded_stdev`,2)) %>%
    mutate('minus_stdev'=round(`tc_percent_digitally_excluded_mean`,2)- round(`tc_percent_digitally_excluded_stdev`,2)) 
  
  
  
  tc_de_plot_colour <- tc_de_to_plot %>% mutate("rag_rating"=case_when((proportion <=  tc_avg_stdev_de$plus_stdev & proportion >= tc_avg_stdev_de$minus_stdev) ~ 'orange',
                                                                       proportion >= tc_avg_stdev_de$plus_stdev ~ 'red',
                                                                       proportion <= tc_avg_stdev_de$minus_stdev ~ 'green'))
  
  
  
  tc_de_for_avg = paste0(round(avgs$tc_percent_digitally_excluded_mean,0), '%', '\n','(regional avg)')
  
  return(
    # # Plot population statistics
    de <- tc_de_to_plot %>%
      e_charts(x = Indicator) %>%
      e_bar(proportion, bar_width=0.1,showBackground=T) %>%
      e_labels(position = "right", color='black') %>%
      e_color(c(tc_de_plot_colour$rag_rating)) %>%
      e_mark_line(data=tc_avg_de, symbol = "none", lineStyle = list(color = "black"), title=tc_de_for_avg, label=list(formatter='label',fontSize=10)) %>%
      e_hide_grid_lines() %>%
      e_flip_coords() %>%
      e_grid(containLabel = TRUE, left=30, right=30, top=10, bottom=5, height='60%') %>%
      e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
      e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
      e_y_axis(show=F) %>%
      e_legend(FALSE)
  )
  
}


# --- LAD Level ---
digital_exclusion_lad_text <- function(indicators, avgs, lad) {
  lad_de_to_write <- indicators %>% select('LAD19CD','percent_digitally_excluded') %>%
    filter(LAD19CD == lad$LAD19CD) %>%
    unique()
  
  lad_de_to_plot <- lad_de_to_write %>% select('percent_digitally_excluded') %>%
    pivot_longer('percent_digitally_excluded', names_to = "Indicator", values_to = "proportion")
  
  lad_de_to_plot <- lad_de_to_plot %>% filter(!is.na(proportion)) %>%
    unique() %>% 
    mutate('proportion'=round(proportion,2))
  
  lad_de_to_write <- lad_de_to_write %>% filter(!is.na('percent_digitally_excluded'))
  
  write_lad_de <- paste0(round(lad_de_to_write$percent_digitally_excluded,2), "%")
  
  if (dim(lad_de_to_plot)[1] != 0) {
    return(
      div(style= " text-align: center;margin-top:5px;",
          #hr(),
          p(write_lad_de,
            tags$br(),
            'neighbourhoods in the 20% most digitally excluded')
      )
    )
  }
  
  else{
    return(
      div(style= " text-align: center;margin-top:5px;",
          #hr(),
          p("Data unavailable",
            tags$br(),
            'for neighbourhoods in the 20% most digitally excluded')
      )
    )
  }
}

digital_exclusion_lad_plot <- function(indicators, avgs, lad) {
  lad_de_to_write <- indicators %>% select('LAD19CD','percent_digitally_excluded') %>%
    filter(LAD19CD == lad$LAD19CD) %>%
    unique()
  
  lad_de_to_plot <- lad_de_to_write %>% select('percent_digitally_excluded') %>%
    pivot_longer('percent_digitally_excluded', names_to = "Indicator", values_to = "proportion")
  
  lad_de_to_plot <- lad_de_to_plot %>% filter(!is.na(proportion)) %>%
    unique() %>% 
    mutate('proportion'=round(proportion,2))
  
  lad_de_to_write <- lad_de_to_write %>% filter(!is.na('percent_digitally_excluded'))
  
  write_lad_de <- paste0(round(lad_de_to_write$percent_digitally_excluded,2), "%")
  
  # for echarts
  lad_avg_de <- avgs %>% select(`percent_digitally_excluded_mean`) %>%
    select('xAxis' = `percent_digitally_excluded_mean`) %>%
    mutate('xAxis'=round(xAxis,0)) %>%
    as.list()
  
  lad_avg_stdev_de <- avgs %>% 
    select(`percent_digitally_excluded_mean`, `percent_digitally_excluded_stdev`) %>%
    mutate('plus_stdev'=round(`percent_digitally_excluded_mean`,1)+round(`percent_digitally_excluded_stdev`,1)) %>%
    mutate('minus_stdev'=round(`percent_digitally_excluded_mean`,1) - round(`percent_digitally_excluded_stdev`,1)) 
  
  
  de_plot_colour <- lad_de_to_plot %>% mutate("rag_rating"=case_when((proportion <=  lad_avg_stdev_de$plus_stdev & proportion >= lad_avg_stdev_de$minus_stdev) ~ 'orange',
                                                                     proportion >= lad_avg_stdev_de$plus_stdev ~ 'red',
                                                                     proportion <= lad_avg_stdev_de$minus_stdev ~ 'green'))
  
  
  
  lad_de_for_avg = paste0(round(avgs$percent_digitally_excluded_mean,0), '%', '\n','(eng avg)')
  
  if (dim(lad_de_to_plot)[1] != 0) {
    return(
      # # Plot population statistics
      lad_de <- lad_de_to_plot %>%
        e_charts(x = Indicator) %>%
        e_bar(proportion, bar_width=0.1,showBackground=T) %>%
        e_labels(position = "right", color='black') %>%
        e_color(c(de_plot_colour$rag_rating)) %>%
        e_mark_line(data=lad_avg_de, symbol = "none", lineStyle = list(color = "black"), title=lad_de_for_avg, label=list(formatter='label',fontSize=10)) %>%
        e_hide_grid_lines() %>%
        e_flip_coords() %>%
        e_grid(containLabel = TRUE, left=30, right=30, top=10, bottom=5, height='60%') %>%
        e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
        e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
        e_y_axis(show=F) %>%
        e_legend(FALSE)
    )
  }
  
  else{
    return()
  }
}

# --- Shielding ---

# --- National level ---
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

# --- MAC Level ---
shielding_mac_text <- function(indicators, avgs) {
  
  tc_shielding_to_write <- indicators %>% select(`tc_Clinically extremely vulnerable`,`tc_Clinically vulnerable proportion of population`) %>%
    unique()
  
  tc_shielding_to_write <- tc_shielding_to_write %>% filter(!is.na(`tc_Clinically extremely vulnerable`) & !is.na(`tc_Clinically vulnerable proportion of population`))
  
  write_tc_shielding <- paste0("(",tc_shielding_to_write$`tc_Clinically vulnerable proportion of population`,"% of the population)")

  return(
    div(style= " text-align: center;margin-top:5px;",
        p(format(tc_shielding_to_write$`tc_Clinically extremely vulnerable`, big.mark=',', scientific = F),
          tags$br(),
          'people clinically extremely vulnerable')
  ))
  
}

shielding_mac_plot <- function(indicators, avgs) {
  tc_shielding_to_write <- indicators %>% select(`tc_Clinically extremely vulnerable`,`tc_Clinically vulnerable proportion of population`) %>%
    unique()
  
  tc_shielding_to_plot <- tc_shielding_to_write %>% select(`tc_Clinically vulnerable proportion of population`) %>%
    pivot_longer(`tc_Clinically vulnerable proportion of population`, names_to = "Indicator", values_to = "proportion")
  
  tc_shielding_to_plot <- tc_shielding_to_plot %>% filter(!is.na(proportion)) %>%
    unique() %>%
    mutate('proportion'=round(proportion,0))
  
  # for echarts
  tc_avg_shielding <- avgs %>% 
    select(`tc_Clinically vulnerable proportion of population_mean`) %>%
    select('xAxis' = `tc_Clinically vulnerable proportion of population_mean`) %>%
    mutate('xAxis' = round(xAxis,0)) %>%
    as.list()
  
  tc_avg_stdev_shielding <- avgs %>% 
    select(`tc_Clinically vulnerable proportion of population_mean`, `tc_Clinically vulnerable proportion of population_stdev`) %>%
    mutate('plus_stdev'=round(`tc_Clinically vulnerable proportion of population_mean`,2)+round(`tc_Clinically vulnerable proportion of population_stdev`,2)) %>%
    mutate('minus_stdev'=round(`tc_Clinically vulnerable proportion of population_mean`,2)- round(`tc_Clinically vulnerable proportion of population_stdev`,2)) 
  

  tc_shielding_plot_colour <- tc_shielding_to_plot %>% mutate("rag_rating"=case_when((proportion <=  tc_avg_stdev_shielding$plus_stdev & proportion >= tc_avg_stdev_shielding$minus_stdev) ~ 'orange',
                                                                                     proportion >= tc_avg_stdev_shielding$plus_stdev ~ 'red',
                                                                                     proportion <= tc_avg_stdev_shielding$minus_stdev ~ 'green'))
  

  tc_shielding_for_avg = paste0(round(avgs$`tc_Clinically vulnerable proportion of population_mean`,0), '%', '\n','(regional avg)')
  
  return(
    # # Plot population statistics
    shielding <- tc_shielding_to_plot %>%
      e_charts(x = Indicator) %>%
      e_bar(proportion, bar_width=0.1,showBackground=T) %>%
      e_labels(position = "right", color='black') %>%
      e_color(c(tc_shielding_plot_colour$rag_rating)) %>%
      e_mark_line(data=tc_avg_shielding, symbol = "none", lineStyle = list(color = "black"), title=tc_shielding_for_avg, label=list(formatter='label',fontSize=10)) %>%
      e_hide_grid_lines() %>%
      e_flip_coords() %>%
      e_grid(containLabel = TRUE, left=30, right=30, top=10, bottom=5, height='60%') %>%
      e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=25, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=25) %>%
      e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
      e_y_axis(show=F) %>%
      e_legend(FALSE)
  )
}

# --- LAD Level ---
shielding_lad_text <- function(indicators, avgs, lad) {
  
  lad_shielding_to_write <- indicators %>% select('LAD19CD',`Clinically extremely vulnerable`,`Proportion Clinically extremely vulnerable`) %>%
    filter(LAD19CD == lad$LAD19CD) %>%
    unique()
  
  lad_shielding_to_plot <- lad_shielding_to_write %>% select(`Proportion Clinically extremely vulnerable`) %>%
    pivot_longer(`Proportion Clinically extremely vulnerable`, names_to = "Indicator", values_to = "proportion")
  
  lad_shielding_to_plot <- lad_shielding_to_plot %>% filter(!is.na(proportion)) %>%
    unique() %>%
    mutate('proportion'=round(proportion,0))
  
  lad_shielding_to_write <- lad_shielding_to_write %>% filter(!is.na(`Clinically extremely vulnerable`) & !is.na(`Proportion Clinically extremely vulnerable`))
  
  write_lad_shielding <- paste0("(",lad_shielding_to_write$`Proportion Clinically extremely vulnerable`,"% of the population)")
  
  if (dim(lad_shielding_to_plot)[1] != 0) {
    return(
      div(style= " text-align: center;margin-top:5px;",
          p(format(lad_shielding_to_write$`Clinically extremely vulnerable`, big.mark=',', scientific = F),
            tags$br(),
            'people clinically extremely vulnerable')
      )
    )
  }
  
  else{
    return(
      div(style= " text-align: center;margin-top:5px;",
          p('Data unavailable',
            tags$br(),
            'for people clinically extremely vulnerable')
      )
    )
  }
  
}

shielding_lad_plot <- function(indicators, avgs, lad) {
  lad_shielding_to_write <- indicators %>% select('LAD19CD',`Clinically extremely vulnerable`,`Proportion Clinically extremely vulnerable`) %>%
    filter(LAD19CD == lad$LAD19CD) %>%
    unique()
  
  lad_shielding_to_plot <- lad_shielding_to_write %>% select(`Proportion Clinically extremely vulnerable`) %>%
    pivot_longer(`Proportion Clinically extremely vulnerable`, names_to = "Indicator", values_to = "proportion")
  
  lad_shielding_to_plot <- lad_shielding_to_plot %>% filter(!is.na(proportion)) %>%
    unique() %>%
    mutate('proportion'=round(proportion,0))
  
  # for echarts
  lad_avg_shielding <- avgs %>% select(`Proportion Clinically extremely vulnerable_mean`) %>%
    select('xAxis' = `Proportion Clinically extremely vulnerable_mean`) %>%
    mutate('xAxis'=round(xAxis,0)) %>%
    as.list()
  
  lad_avg_stdev_shielding <- avgs %>% 
    select(`Proportion Clinically extremely vulnerable_mean`, `Proportion Clinically extremely vulnerable_stdev`) %>%
    mutate('plus_stdev'=round(`Proportion Clinically extremely vulnerable_mean`,1)+round(`Proportion Clinically extremely vulnerable_stdev`,1)) %>%
    mutate('minus_stdev'=round(`Proportion Clinically extremely vulnerable_mean`,1) - round(`Proportion Clinically extremely vulnerable_stdev`,1)) 
  
  
  shielding_plot_colour <- lad_shielding_to_plot %>% mutate("rag_rating"=case_when((proportion <=  lad_avg_stdev_shielding$plus_stdev & proportion >= lad_avg_stdev_shielding$minus_stdev) ~ 'orange',
                                                                                   proportion >= lad_avg_stdev_shielding$plus_stdev ~ 'red',
                                                                                   proportion <= lad_avg_stdev_shielding$minus_stdev ~ 'green'))
  
  
  lad_sheilding_for_avg = paste0(round(avgs$`Proportion Clinically extremely vulnerable_mean`,0), '%', '\n','(eng avg)')
  
  if (dim(lad_shielding_to_plot)[1] != 0) {
    return(
      # # Plot population statistics
      sec95 <- lad_shielding_to_plot %>%
        e_charts(x = Indicator) %>%
        e_bar(proportion, bar_width=0.1,showBackground=T) %>%
        e_labels(position = "right", color='black') %>%
        e_color(c(shielding_plot_colour$rag_rating)) %>%
        e_mark_line(data=lad_avg_shielding, symbol = "none", lineStyle = list(color = "black"), title=lad_sheilding_for_avg, label=list(formatter='label',fontSize=10)) %>%
        e_hide_grid_lines() %>%
        e_flip_coords() %>%
        e_grid(containLabel = TRUE, left=30, right=30, top=10, bottom=5, height='60%') %>%
        e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=25, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=25) %>%
        e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
        e_y_axis(show=F) %>%
        e_legend(FALSE)
    )
  }
  
  else{
    return()
  }
}



