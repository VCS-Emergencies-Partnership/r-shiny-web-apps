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


# --- function to write html ---
lots_of_text <- function(file_name) {
  return(includeHTML(file_name))
}

# --- TEXT FUNCTIONS FOR HELP SECTION --- 
# -- about us page ---
create_about_us <- function(time, date) {

  return(div(
    h5(tags$strong("The dashboard was last updated at", time, "on", date)),
    p("The update frequency of individual datasets vary. 
      We are working on a sharable method for viewing this information but get", 
      tags$a(href="https://vcsep.org.uk/",target="_blank",'in touch'),
      "if you have any questions."),
    tags$br(),
    h4(tags$strong("Welcome to the Voluntary and Community Emergency Partnership Risk Indicator Tool Kit")),
    p("We created this toolkit to recongise the role data and insight have in helping organisations 
      plan and prepare for emergencies with the ultimate aim of reducing the impact of an emergency.
      It doesn’t have answer the problems we face in emergency response, but it can help spark 
      questions and discussions to explore your emergency response work. In its current form, 
      the toolkit provides demographic estimates for different areas and people. 
      We’ve included top 10 lists where we can, but these are based on sorting data and should
      not be taken as an opinion."),
    tags$br(),
    h4(tags$strong("Key questions behind creating these tools")),
    tags$li("Where is the need for support greatest?"),
    tags$li("Who is in greatest need of support?"),
    tags$li("What type of support do they need?"),
    tags$li("How are those people and areas being supported?"),
    tags$br(),
    h4(tags$strong("Beta group to test and learn")),
    p("This toolkit is in its infancy. We are grateful for your patience and support and ask 
      for your", tags$a(href="https://vcsep.org.uk/", target="_blank","feedback"), "for its continual development. It’s critical to sensitively test how 
      insight can inform being prepared for an emergency as well as responding to it and we are 
      delighted to have our group of testers doing this. As with all testing, this takes time and 
      it is only once we’re happy with the results that we will continue to release these tools to wider audiences to inform broader action to either prepare for, respond to, or help communities recover better from an emergency."),
    tags$br(),
    p(tags$a(href="https://vcsep.org.uk/",target="_blank",'Contact us'), "through our website.")
    
  )
  )
}

# -- dashboard about page --
create_about_areas_at_risk <- function(time, date) {

  return( div(
        #tags$br(),
    h4(tags$strong("Brief introduction to the risk indicator tool")),
    h5(tags$strong("Identify, focus and tailor")),
    p("Do you want to better inform the focusing and tailoring of resources to prepare for, 
      respond to, and help communities recover from an emergency? The tool helps those who 
      wish to target their efforts in areas of highest risk and least capacity to cope, 
      should an emergency occur. Users can select an emergency type and drill down to Local 
      Authority."),
    tags$br(),
    #h5(tags$strong("Dashboard elements:")),
    p(tags$li(tags$strong('Areas to focus list:'), "Highlights 10 areas where an emergency is most 
            prevalent by local authority based on an emergency type. Filter the map to view the 
            resilience of just these areas, by selecting the toggle below the zoom buttons on the 
            map."),
      tags$br(),
      tags$li(tags$strong('Area Demographics:'), "Provides estimates the of people, households and incidents 
                    to give insight around who is impacted and how. These are largely based on the indicators within 
                    the vulnerability index (e.g. digital exclusion), but is complimented with other data (e.g. universal credit)."),
                   
      tags$br(),
      tags$li(tags$strong('Areas at risk map:'), "Highlights either the BRC Resilience Index score or Vulnerability Index score for local authority districts. See map tab for further information."),
      tags$br(),
      tags$li(tags$strong('Find local resources:'), "This tool allows you to search the database", tags$a(href='https://charitybase.uk/', target="_blank", "CharityBase"), "returning a table of information about 
                    charities in the area selected. The search bar allows you to look for charities with a particular purpose."),
      tags$br(),
      tags$li(tags$strong("Areas to focus table:"), "This table displays the data relevant to the emergency selected for all 
              local authority districts in the area and can be ordered and searched.")
    ),
    tags$br(),
    h5(tags$strong("Learn, evaluate, respond, repeat")),
    p("We continuously learn and prepare, so if an emergency should happen, 
            we are better placed to respond and help communities recover. 
            In our Beta Group we are testing the sharing of  partner data, shown alongside open 
            data used in this tool, to inform and refine our work. We have a general partnership 
            agreement to share data and we are working with partners to collaborate on a bespoke 
            agreement to build both our collaboration and connectivity.", 
      tags$a(href="https://vcsep.org.uk/", target="_blank", "Contact us or book a zoom call"), "to feedback.")
    
  )
  )
}

create_map_help_top <- function() {  
  return(
    div(h4(tags$strong("What the map shows")),
        p("The British Red Cross developed a series of", 
          tags$a(href="https://britishredcrosssociety.github.io/covid-19-vulnerability/",target="_blank","indices"), "to identify UK areas vulnerable 
            to the effects of COVID-19, and a resilience index which overlays capacity to cope.
            Using statistical modelling of data from a range of", tags$a(href="https://github.com/britishredcrosssociety/covid-19-vulnerability/blob/master/output/metadata_vi.csv", target="_blank", "(mostly open) sources,"), "the indices
            provide an area rating, which is then used to map areas of need.", tags$br(),
          "Over time, we will also used the",tags$a(href="https://www.gov.uk/government/collections/english-indices-of-deprivation",target="_blank", "Indices of Multiple Deprivation"), "and continue to develop bespoke maps."),
        tags$br())
  )
}

create_map_help_middle <- function() {
  return(
    div(style="overflow:auto;",
        p(img(src = "bivar-legend_v2.png", width = 225), style="float:left;"), 
        p(tags$strong("Explore by resilience", style='font-size:18px'), 
          tags$strong('(hover over the legend to enlarge)', style='font-size:12px'),
          tags$br(),
          "The default map shows the BRC Resilience Index (vulnerability vs capacity to cope). 
          The black highlights those areas that are most in need areas (i.e. highest vulnerability
          and least capacity to cope). This lightens to grey for the least in need 
          (i.e. lowest vulnerability and highest capacity). We selected this colour scheme to try
          and avoid confusion with the RAG colour scheme that is commonly used in operational 
          response."))
  )
}

# -- dashbaord map page --
create_map_help_bottom <- function() {
  return( div(
    h4(tags$strong("Explore by type of vulnerability")),
    p("When the Covid-19 emergency is selected users can also explore different layers of the map based on vulnerability type, by selecting the button in the top right corner of the map.
        Data is shown by local authority, where purple indicates the most vulnerable, yellow the least vulnerable only (i.e. not resilience)",
      tags$li("Economic vulnerability"),
      tags$li("Socioeconomic vulnerability"),
      tags$li("Social vulnerability"),
      tags$li("Health/wellbeing vulnerability"),
      tags$li("Clinical vulnerability")),
    tags$br(),
    h4(tags$strong("See the latest flood warnings and alerts")),
    p("When the Flooding emergency type is selected users can view flood warnings from the",
      tags$a(href="https://flood-warning-information.service.gov.uk/warnings", target="_blank", 'Environment Agency.'),
      "The colour of the points (red - severe warning or warning, orange - alert) represent the severity of the warning.
            When a Local authority district is selected the outline of the potentially flood zone can be seen. 
            See Emergency-Flooding tab for more information."),
    tags$br(),
    h4(tags$strong("Sources")),
    p("For licensing and sources, including for geographical boundaries, see the references page.")

    
  ))
}

# -- dashboard covid help page ---
create_emergency_covid <- function(covid_data_update) {
  return(div(h4(tags$strong("When you filter by emergency type - Covid-19")),
      h5(tags$strong("Areas to focus")),
      p(tags$li("The 10 areas with the highest number of cases per 100,000"),
        tags$li("The 10 areas with the greatest % change in total covid cases over rolling 7-day period")),
      tags$br(),
      h5(tags$strong("Area demographics")),
      p("Displays the values of indicators used by the", tags$a(href="https://github.com/britishredcrosssociety/covid-19-vulnerability", target="_blank", "BRC Vulnerability Index"),
        "and further open source data.",
        tags$li("Number of people and proportion of the population clinically extremely vulnerable"),
        tags$li("Number of people and  proportion of the population recieving support whilst seeking asylum"),
        tags$li("Homeless people per 1000"),
        tags$li("Number of households and proportion of households in fuel poverty"),
        tags$li("Number of people and proportion of the population unemployed claiming universal credit"),
        tags$li("Proportion of neighbourhoods in the 20% most digitally excluded"),
        tags$li("Proportion of the population who are of an ethnic minority")),
      
      tags$br(),
      
      # p("Based on the indicators used in the BRC Vulnerability and Resilience
      #   indices and complimented with open data.", tags$a(href='https://britishredcross.shinyapps.io/resilience-index/',target="_blank",'View the indices'),
      #   "or", tags$a(href="https://vcsep.org.uk/",target="_blank",'Contact us'), "for more information"),
      # tags$br(),
      h5(tags$strong("Areas at risk (ie. the map)")),
      p("This displays the resilience (vulnerability vs capacity to cope) of local authority districts based on", tags$a(href="https://github.com/britishredcrosssociety/resilience-index", target="_blank", "BRC Resilience Index."),
        "By clicking on the layers button in the top right hand corner of the map you can view either
            the social, socioeconomic, economic, health/wellbeing or clinical vulnerability of an area based upon the", tags$a(href="https://github.com/britishredcrosssociety/covid-19-vulnerability", target="_blank", "BRC Vulnerability Index")),
      tags$br(),
      h4(tags$strong("About the data and how it's represented")),
      h5(tags$strong("Covid cases per 100,000")),
      p("A rolling 7-day average of positive covid tests for each local authority district
              (lower tier local authority). To convert the rolling average of cases to average of 
              cases per 100,000, the average number of cases is divided by the population of the
              local authority district and multiplied by 100,000."),
      tags$br(),
      h5(tags$strong("Total covid cases")),
      p("The total number of new cases (by specimen date) seen over the relevant 7 day period."),
      tags$br(),
      h5(tags$strong("% change in covid cases")),
      p("The % change in new cases (by sepecimen date) between the most recent 
              relevant 7-day period and the 7-day period prior to that."),
      tags$br(),
      h5(tags$strong("Rolling average")),
      p("This is determined by averaging the number new cases by specimen date on the day itself, the 3 days prior and the 3 days after.
              For this to be possible this data is the rolling average of 5 days prior to the current day (i.e. starting from 5 days prior to today - the 7 days prior to that).
              The current relevant 7 day period is up to:", covid_data_update),
      tags$br(),
      h5(tags$strong("Source")),
      p("For more information see", tags$a(href="https://coronavirus.data.gov.uk/", target="_blank", "https://coronavirus.data.gov.uk/."), 
        "This data contains Public Health England data © Crown copyright and database right 2020 and is licensed under the", tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target="_blank", " Open Government Licence v3.0."), 
        "See a full list of sources and licensing, including for the map on the references page.")
 
  )
  )
}


# --- dashboard flooding help page ---
create_flooding_help <- function() {
  return(div(h4(tags$strong("When you filter by emergency type - flooding")),
                   h5(tags$strong("Areas to focus")),
                   p(tags$li("The 10 areas impacted by the greatest number of flood warnings ranked by the severity of warning. 
                    If there are no flood alerts or warnings the list displays the 10 areas with the highest number of historical flood incidents per 10,000 people."),
                     tags$li("The 10 areas with the highest number of historical flood incidents per 10,000 people"),
                     tags$li("The 10 areas with the highest proportion of population living in flood risk areas")),
                   tags$br(),
                   h5(tags$strong("Area demographics")),
                   p("Displays the values of indicators used by the", tags$a(href="https://github.com/britishredcrosssociety/covid-19-vulnerability", target="_blank", "BRC Vulnerability Index"),
                     "and futher open source data.",
                     tags$li("Number of people and proportion of the population clinically extremely vulnerable"),
                     tags$li("Number of people and  proportion of the population recieving support whilst seeking asylum"),
                     tags$li("Homeless people per 1000"),
                     tags$li("Number of households and proportion of households in fuel poverty"),
                     tags$li("Number of people and proportion of the population unemployed claiming universal credit"),
                     tags$li("Proportion of neighbourhoods in the 20% most digitally excluded"),
                     tags$li("Proportion of the population who are of an ethnic minority")),
                   tags$br(),
                   h5(tags$strong("Areas at risk (i.e the map)")),
                   p("This shows the BRC Resilience index (vulnerability vs capacity to cope), 
            overlayed with points showing", tags$a(href="https://flood-warning-information.service.gov.uk/warnings", target="_blank", 'Environment Agency'), "flood warnings and alerts. 
            If a local authority is selected the Environment's Agency predicted flood outline is also displayed."),
                   tags$br(),
                   h4(tags$strong("About the data and how it's represented")),
                   h5(tags$strong("Environment agency flood warnings and alerts:")),
                   p("Local authority districts are deemed to be affected by a flood alert or warning if 
            the predicted most extreme scenario (represented by the flood outline provided by the 
            environment agency) overlaps their local authority district. On occasions the flood 
            outline shown in the app may not appear to overlay a local authority. This is to help 
            the performance of the app.", 
                     tags$strong("Please note,"), "currently these warnings are updated daily as opposed to 
            every 15 minutes as per the environment agency - we aim to change this in an upcoming release."),
                   tags$br(),
                   h5(tags$strong("Historical flood events per 10,000 people")),
                   p("Originally determined and represented by the BRC resilience index, 
            the historical flood incidents are determined by the number of Fire and Rescue Service 
            call outs to flooding incidents."),
                   tags$br(),
                   h5(tags$strong("Proportion of the population living in flood risk areas")),
                   p("Originally determined and represented by the BRC resilience index, this shows the proportion of the population of each local authority district living 
            in areas where there is a greater than 1% chance a year of flooding. 
            This is calculated by determining using the number of people living in the flood zones 
            as defined by the Environment Agency."),
                   tags$br(),
                   h5(tags$strong("Source")),
             p("The",  tags$a(href="https://environment.data.gov.uk/flood-monitoring/doc/reference#data", target="_blank","Environment Agency flood warnings and alerts"), "are licensened under", tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target="_blank", "Open Government Licence v3.0."), "See a full list of sources and licensing, 
      including for the map on the references page.")
 
  ))
}

# --- dashboard data and licensing page ---
create_data_license_help <- function() {
  return( div(
    #h3(tags$strong("Data, licenses and privacy policy")),
    #hr(),
    h4(tags$strong("We use a range of data sources to bring insight to you")),
    tags$br(),
    h4(tags$strong("Open Data")),
    p("We utilise a range of open data, including sources shared under the", tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target="_blank", " Open Government Licence v3.0"),
            "and the", tags$a(href="https://creativecommons.org/licenses/by/4.0/legalcode", target="_blank", "Creative Commons By Attribution 4.0."), "For example, the number of people shielding, 
            seeking asylum or homelessness rate per 1000 people. We will continually add to this 
            list as things develop and are working on user friendly way of displaying this 
            information for each dataset. In the meantime, see the help sections on the individual emergency types  or", tags$a(href="https://vcsep.org.uk/",target="_blank",'contact us'), "if you have a question."),
    p("The", tags$a(href="https://github.com/britishredcrosssociety/resilience-index", target="_blank", "BRC Resilience Index (RI)"), "and", tags$a(href="https://github.com/britishredcrosssociety/covid-19-vulnerability", target="_blank", "BRC Vulnerability Index (VI)"),
    "ranks areas based on a range of factors against an areas ability to cope. For information on
            what indicators are used, including a", tags$a(href="https://github.com/britishredcrosssociety/covid-19-vulnerability/blob/master/output/metadata_vi.csv", target="_blank", "full list of indicators,"), "see the", tags$a(href="https://github.com/britishredcrosssociety/resilience-index", target="_blank", "BRC Resilience Index"), 
            "and", tags$a(href="https://github.com/britishredcrosssociety/covid-19-vulnerability", target="_blank", "BRC Vulnerability Index."),
            "These are the foundation for the Covid and Flooding emergency maps. We 
            include a range of open data to tell a more detailed story. 
            For example, a ranked list of areas to focus based on coronavirus cases, estimates for 
            number of households digitally excluded, or number of people living below the poverty 
            line."),
    tags$br(),
    h4(tags$strong("Partner Data")),
    p("In our Beta Group we are testing how to combine this with partner data to deliver 
            insight that informs action. Where possible, we will release these insights publicly 
            on this tool, or in our insight archive. We are working closely with partners to put 
            in place organisation specific data agreements. For those who want to share their 
            data publicly, we recommend getting familiar with choosealicense.com and we can speak 
            to you about how we’re doing it."),
    tags$br(),
    h4(tags$strong("Disclaimer")),
    p("We work our best to ensure the regular update of the information included in this tool.
              However, we are a very small team in the early development stage at the British Red Cross,
              and we do not guarantee the accuracy of this information. 
              See references section for each dashboard for more information."),
    tags$br(),
    h4(tags$strong("License for Risk Indicator Tool Kit")),
    p("We are preparing to share our tool with others using the GPL3 license, following on 
            from the BRC Vulnerability and Resilience indices being released under the same license. 
            The benefit is that analysts and engineers can review what we’re doing to help make it
            better. We’re taking a collaborative approach and working closely with those who want 
            to be involved. The requirements is that this tool cannot be used for  monetary gain 
            and if the development code is used the source must be referenced, including this tool
            and the above BRC VI and RI. Before reproducing or adapting this tool, please read the
            license or contact us if you have a question."),
    tags$br(),
    h4(tags$strong("Privacy and Cookies Policy")),
    p("We use Google Analytics to collect information on visits and behaviour on the site, 
      to help us improve our tool.This requires the use of cookies, and we give you the option to decline or accept these. 
      The VCSEP privacy and cookies policies are avalailable", tags$a(href='https://vcsep.org.uk/privacy-policy', target="_blank", "here.")),
      tags$br(),
    h4(tags$strong("List of data sources, attributions and licences")),
    p(tags$li(tags$strong(tags$a(href="https://github.com/britishredcrosssociety/resilience-index", target="_blank", "BRC resilience index data:")),"this data is licensed under", tags$a(href="https://creativecommons.org/licenses/by/4.0/legalcode", target="_blank", "Creative Commons By Attribution 4.0")),
      tags$li(tags$strong(tags$a(href="https://github.com/britishredcrosssociety/resilience-index", target="_blank", "Code to generate the BRC resilience index:")), "this is licensed under", tags$a(href="https://www.gnu.org/licenses/gpl-3.0.html", target="_blank", "GNU General Public License version 3")),
      tags$li(tags$strong(tags$a(href="https://britishredcross.shinyapps.io/resilience-index/", target="_blank", "Code to generate the BRC resilience index app:")), "this is licensed under", tags$a(href="https://www.gnu.org/licenses/gpl-3.0.html", target="_blank", "GNU General Public License version 3")),
      tags$li(tags$strong(tags$a(href="https://github.com/britishredcrosssociety/covid-19-vulnerability", target="_blank", "BRC vulnerability index data:")), "this data is licensed under", tags$a(href="https://creativecommons.org/licenses/by/4.0/legalcode", target="_blank", "Creative Commons By Attribution 4.0")),
      tags$li(tags$strong(tags$a(href="https://github.com/britishredcrosssociety/covid-19-vulnerability", target="_blank", "Code to generate the BRC vulnerability index:")),  "this is licensed under", tags$a(href="https://www.gnu.org/licenses/gpl-3.0.html", target="_blank", "GNU General Public License version 3")),
      tags$li(tags$strong(tags$a(href="https://coronavirus.data.gov.uk/", target="_blank", "Coronavirus data:")), 
              "This data contains Public Health England data © Crown copyright and database right 2020 and is available under the",
              tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target="_blank", " Open Government Licence v3.0.")),
      tags$li(tags$strong(tags$a(href="https://environment.data.gov.uk/flood-monitoring/doc/reference#data", target="_blank","Environment Agency flood warnings and alerts:")),"this contains public sector information licensed under the", tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target="_blank", "Open Government Licence v3.0"), "and uses Environment Agency flood and river level data from the real-time data API (Beta)"),
      tags$li(tags$strong(tags$a(href="", target="_blank","Environment Agency number of people living in flood risk areas:")), "this contains public sector information licensed under the", tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target="_blank", "Open Government Licence v3.0")),
      tags$li(tags$strong(tags$a(href="https://digital.nhs.uk/data-and-information/publications/statistical/mi-english-coronavirus-covid-19-shielded-patient-list-summary-totals/latest",target="_blank", "Clinically extremely vulnerable people data:")), "this contains public sector information licensed under the", tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target="_blank", "Open Government Licence v3.0")),
      tags$li(tags$strong(tags$a(href="https://www.gov.uk/government/collections/universal-credit-statistics#latest-monthly-release",target="_blank", "Unemployed on universal credit:")), "this contains public sector information licensed under the", tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target="_blank", "Open Government Licence v3.0")),
      tags$li(tags$strong(tags$a(href="https://www.nomisweb.co.uk/datasets/apsnew",target="_blank", "Ethinicity data:")), "This data is from the ONS Annual Population survey contains public sector information licensed under the",tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target="_blank", "Open Government Licence v3.0")),
      tags$li(tags$strong(tags$a(href="https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019",target="_blank", "Homelessness data:")), "This data is from the English indices of multiple deprivation which is licensed under the",tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target="_blank", "Open Government Licence v3.0"), "In this instance it was taken from the", tags$a(href="https://github.com/britishredcrosssociety/covid-19-vulnerability", target="_blank", "BRC Vulnerability Index")),
      tags$li(tags$strong(tags$a(href="https://www.gov.uk/government/statistical-data-sets/asylum-and-resettlement-datasets",target="_blank", "People receiving support whilst seeking asylum data:")), "this is licensed under the", tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target="_blank", "Open Government Licence v3.0")),
      tags$li(tags$strong(tags$a(href="https://github.com/britishredcrosssociety/covid-19-vulnerability",target="_blank", "Digital exclusion data:")), "this is a bespoke indicator created by and used in the",tags$a(href="https://github.com/britishredcrosssociety/covid-19-vulnerability", target="_blank", "BRC Vulnerability Index"), "which is licensed under",tags$a(href="https://creativecommons.org/licenses/by/4.0/legalcode", target="_blank", "Creative Commons By Attribution 4.0")),
      tags$li(tags$strong(tags$a(href="https://www.gov.uk/government/statistics/sub-regional-fuel-poverty-data-2020",target="_blank", "Fuel poverty data:")), "this contains public sector information licensed under the", tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target="_blank", "Open Government Licence v3.0")),
      tags$li(tags$strong(tags$a(href="https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/middlesuperoutputareamidyearpopulationestimates",target="_blank", "ONS Population statistics Mid-2019:")), "this is from the Office for National Statistics and licesenced under the", tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target="_blank", "Open Government Licence v3.0")),
      tags$li(tags$strong(tags$a(href="https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2019-boundaries-uk-bfc/data?geometry=-33.636%2C51.101%2C28.767%2C59.782", target="_blank", "Local authority district 2019 boundaries")), "this is from the Office for National Statistics licesenced under the", tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target="_blank", "Open Government Licence v3.0")),
      
    )

  ))
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


internal_report_link <- function() {
  return(showModal(modalDialog(
    title = "You are about to leave this page to access an internal dashboard",
    div(
      p("If you are a", tags$strong("member of the VCS Emergencies Partnership then please continue"), 
        "to view the internal dashboard."),
      tags$br(),
      p("If you are a", tags$strong("member of the public,"), "visit our
            website to request support or to learn more about our work.")),
    
    
    footer = tagList(
      actionButton('rfs_continue',label='Continue', onclick ="window.open('https://dashboards.vcsep.org.uk/', '_blank')"),
      #actionButton('rfs_access',label='Request access', onclick="window.location.href='mailto:itsupport@vcsep.org.uk?subject=Request access to VCSEP RFS dashboard'"),
      #actionButton('rfs_access',label='Request access', onclick=mailToLink),
      actionButton('VCS website', label='Visit website', onclick ="window.open('https://vcsep.org.uk/', '_blank')"),
      modalButton('Close')
        )
      )
    )
  )
}


rfs_highlights <- function(dataset, which_highlight) {
  if(which_highlight == 1) {
    no_requests <- nrow(dataset)
    currently_active <- dataset %>% filter(request_status == 'Active') %>%
      nrow()

    return(div(p("We have responded to", tags$strong(no_requests, 
                                          "requests for support"),
              'during our response to the COVID-19 pandemic.',
              tags$em("(source: requests for support data)"))))
  }
  else {
    if(which_highlight == 2) {
      # select 
      requests_for <- dataset %>% select(starts_with("cat_")) 
      rm_last <- requests_for %>% select(-'cat_other')
      convert_tf <- rm_last * 1
      total_requests_for <- convert_tf %>%
        summarise(across(1:7, sum, na.rm=T))
      
      total_requests_for <- pivot_longer(total_requests_for, cols=1:7, names_to='requests_for', values_to='total',
                                         names_prefix="cat_")
      
      total_requests_for <- total_requests_for %>% 
        mutate('prop_requests'=round((total/nrow(dataset))*100,1))
      
      most_common_request <- total_requests_for %>% filter(prop_requests == max(prop_requests))
      
      return(div(
        p("The highest proportion of requests,",
        tags$strong(paste0(most_common_request$prop_requests, "%")),
        paste0("(",most_common_request$total,")"),
        "have been for", tags$strong(paste0(most_common_request$requests_for, "."))
      )))
        
    }
  }
}
  

