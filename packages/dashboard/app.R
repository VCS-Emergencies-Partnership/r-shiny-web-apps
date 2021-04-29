library(shiny) # - not in Docker 
library(shinydashboard) # - Docker
library(httr) # - Docker
library(sf) # - Docker
library(tidyverse) # Docker
library(jsonlite) # - Docker
library(shinydashboardPlus) # Docker
library(leaflet) # Docker
library(viridis) # Docker
library(DT) # Docker
library(echarts4r) # Docker 
library(feather) # Docker
library(scales) # Docker
library(htmlwidgets) # Docker
library(shinyjs) # Docker
library(shinycssloaders) # docker
library(shinyWidgets) #-- ADD TO DOCKER
library(R.utils) # -- ADD TO DOCKER 
library('ghql') # -- ADD TO DOCKER

readRenviron(".Renviron")

options(shiny.trace = F)

source("./functions.r")

# function for table sorting 
clearSorting <- function(proxy) {
  runjs(paste0("$('#' + document.getElementById('", proxy$id,"').getElementsByTagName('table')[0].id).dataTable().fnSort([]);"))
}


# --- read in vulnerablity indices ---
# # --- local authority level ---
LA_vi <- read_feather('./data/vulnerability_index/vulnerability-LA.feather')
LA_vi <- LA_vi %>% rename('LAD19CD'=Code)

# --- Middle super output area level ---
#msoa_vi <- read_csv('https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/output/vulnerability-MSOA-UK.csv')
#msoa_vi <- msoa_vi %>% rename('MSOA11CD'=Code)

# -- Area lookup table ---
#area_lookup <- read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/lookup%20mosa11%20to%20lad17%20to%20lad19%20to%20tactical%20cell.csv")
area_lookup <- read_feather('./data/vulnerability_index/lookup_msoa11_to_lad19_to_tactical_cell.feather')
area_lookup_tc2lad <- area_lookup %>% select('LAD19CD', 'TacticalCell') %>% 
  mutate('TacticalCell_update'=case_when(TacticalCell == 'South and the Channel Islands' ~ 'South West',
                                         TacticalCell == 'Central' ~ 'Midlands & East',
                                         TRUE ~ as.character(.$TacticalCell))) %>%
  select(-'TacticalCell') %>% rename("TacticalCell"=TacticalCell_update)


# ---- Read in the resilience index ----
#LA_res <- read_csv('https://github.com/britishredcrosssociety/resilience-index/raw/main/data/processed/resilience%20index.csv')
LA_res <- read_feather('data/resilience_index_bivar.feather')


# --- read in shape files with minimal metadata ---
# --- Local authorities ---
lad_uk <- read_sf('data/reduced_boundaries/lad19_eng_wales_sc_ni.geojson')
lad_uk <- lad_uk %>% rename('LAD19CD'=lad19cd)
# --- Middle admin level ---
# --- read in England and wales msoa ---
#msoa_ew <- read_sf('data/reduced_boundaries/MSOA2011_EW.geojson')
# --- read in NI soa file ---
#msoa_ni <- read_sf('data/reduced_boundaries/NI_soa.geojson')
# --- read in Scotland Intermediade zones ---
#msoa_scot <- read_sf('data/reduced_boundaries/Scotland_intzones.geojson')
# --- join middle admin level data ---
#all_msoas <- bind_rows(msoa_ew, msoa_ni, msoa_scot)
# -- rename to merge with lookup table ---
#all_msoas <- all_msoas %>% rename('MSOA11CD'=Code)

# -- tactical cell boundaries --
#tc_shp <- st_read('./data/reduced_boundaries/vcsep_multiagencycells_wo-iom-ci_BFE.geojson') %>%
#  st_transform('+proj=longlat +datum=WGS84')
tc_shp <- read_sf('data/reduced_boundaries/vcsep_multiagencycells_wo-iom-ci_BFE.shp') %>%
  st_transform('+proj=longlat +datum=WGS84')

tc_shp <- tc_shp %>% mutate("TacticalCell"=case_when(lookup_loc == 'Midlands and the East' ~ 'Midlands & East',
                                                     lookup_loc == 'South and the Channel Islands' ~ 'South West',
                                                     TRUE ~ as.character(.$lookup_loc)))



# --- join lookup table ---
# --- to la shapefile  ---
lad_uk2areas <- left_join(lad_uk, area_lookup_tc2lad, by='LAD19CD', keep=F)
# --- to msoa shapefile ---
#all_msoas2areas <- left_join(all_msoas, area_lookup, by='MSOA11CD', keep=F)

# --- join vulnerability index to shapefiles ---
# --- local authority level ---
lad_uk2areas2vulnerability_full <- left_join(unique(lad_uk2areas), LA_vi, by="LAD19CD", keep=F)
lad_uk2areas2vulnerability <- lad_uk2areas2vulnerability_full %>% select('LAD19CD', 'Name', 'Country', `Clinical Vulnerability quintile`, `Health/Wellbeing Vulnerability quintile`,`Economic Vulnerability quintile`,`Social Vulnerability quintile`,`Socioeconomic Vulnerability quintile`,`Vulnerability quintile`,'TacticalCell')

# --- msoa level ----
#all_msoas2areas2vulnerability <- left_join(all_msoas2areas, msoa_vi, by='MSOA11CD', keep=F)

# res shapefile
lad_uk2vuln_resilience <- left_join(unique(lad_uk2areas), LA_res, by='LAD19CD', keep=F)

lad_uk2vuln_resilience <- lad_uk2vuln_resilience %>% filter(!is.na(fill))  %>%
  mutate('TacticalCell_update'=case_when(TacticalCell == 'South and the Channel Islands' ~ 'South West',
                                         TacticalCell == 'Central' ~ 'Midlands & East',
                                         TRUE ~ as.character(.$TacticalCell))) %>%
  select(-'TacticalCell') %>% rename("TacticalCell"=TacticalCell_update)


# for mapping click add in id column
lad_uk2vuln_resilience <- lad_uk2vuln_resilience %>%
  mutate('res_id' = paste0('covidres_',LAD19CD)) %>%
  mutate('econ_id' = paste0('econ_',LAD19CD)) %>%
  mutate('soc_id' = paste0('soc_',LAD19CD)) %>%
  mutate('socecon_id' = paste0('socecon_',LAD19CD)) %>%
  mutate('clin_id' = paste0('clin_',LAD19CD)) %>%
  mutate('health_id' = paste0('health_',LAD19CD)) %>%
  mutate('floodres_id' = paste0('floodres_',LAD19CD)) %>%
  mutate('incd_id' = paste0('incd_',LAD19CD)) %>%
  mutate('risk_id' = paste0('risk_',LAD19CD)) 
  

# tactical cells
tactical_cells <- area_lookup_tc2lad %>% filter(TacticalCell != 'Wales' & TacticalCell != 'Northern Ireland and the Isle of Man' & TacticalCell != 'Scotland')
tactical_cells <- unique(tactical_cells$TacticalCell)
tactical_cells <- c('-- England --', tactical_cells)

#lads_needed <- unique(lad_uk2vuln_resilience$lad19nm)
#lads_needed <- c('All local authorities in region', sort(lads_needed))

# --- Metadata ----
# --- people at risk data ---
par_table <- read_feather('data/people_at_risk/people-at-risk.feather')

# temp fix of typo 
par_table <- par_table %>% rename('lad_prop_recieving_section_95_support'=lad_prop_receving_section95_support,
                                  'lad_prop_unemployed_on_ucred' = 'lad_prop_upemployed_on_ucred') %>% 
  mutate('TacticalCell_update'=case_when(TacticalCell == 'South and the Channel Islands' ~ 'South West',
                                         TacticalCell == 'Central' ~ 'Midlands & East',
                                         TRUE ~ as.character(.$TacticalCell))) %>%
  select(-'TacticalCell') %>% rename("TacticalCell"=TacticalCell_update)


# just working with engalnd for now
england_regions = c("North", "Midlands & East", "London", "South West", "South East")


# just working with engalnd for now
england_regions = c("North", 'Midlands & East', "London", "South West", "South East")


# calculate averages for all local authorities across england - to me this is the avg across england
# need to select 
par_table_lad_avg <- par_table %>%
  select(
         'Proportion of neighbourhoods in 20% most digitally excluded',
         'percent_digitally_excluded',
         'People receiving Section 95 support',
         'lad_prop_recieving_section_95_support',
         `Percentage of population who are ethnic minority`,
         `Number of households in fuel poverty1`,
         `Proportion of households fuel poor (%)`,
         `Homelessness (rate per 1000)`,
         #`lad_total_homeless`,
         #'lad_prop_homeless',
         `Not in employment`,
         'lad_prop_unemployed_on_ucred',
         `Clinically extremely vulnerable`,
         `Proportion Clinically extremely vulnerable`
         ) %>%
  summarise_all(., list('mean' = mean, 'stdev'=sd), na.rm=T) %>%
  mutate('LAD19CD'='lad_avg', 'TacticalCell'='lad_avg') %>%
  mutate('TacticalCell_update'=case_when(TacticalCell == 'South and the Channel Islands' ~ 'South West',
                                         TacticalCell == 'Central' ~ 'Midlands & East',
                                         TRUE ~ as.character(.$TacticalCell))) %>%
  select(-'TacticalCell') %>% rename("TacticalCell"=TacticalCell_update) %>%
  select('LAD19CD', 'TacticalCell', everything())

# calculate the tactical cell average.
par_table_tc_avg <- par_table %>% 
  select(
    `tc_Proportion of neighbourhoods in 20% most digitally excluded`,
    `tc_percent_digitally_excluded`,
    `tc_People receiving Section 95 support`,
    `tc_prop_people_recieving_section_95_support`,
    `tc_proportion`,
    `tc_cases_per_10000_for_current_week`,
    `tc_Number of households in fuel poverty1`,
    'tc_prop_households_fuel_poor',
    #'tc_total_homeless',
    `tc_Homelessness (rate per 1000)`,
    #'tc_prop_homeless',
    `tc_Not in employment`,
    `tc_prop_unemployed_on_universal_credit`,
    `tc_Clinically extremely vulnerable`,
    `tc_Clinically vulnerable proportion of population`
    ) %>%
  unique() %>%
  summarise_all(., list('mean'=mean, 'stdev'=sd), na.rm=T) %>%
  summarise_all(., list(round), 2) %>%
  mutate('LAD19CD'='tc_avg', 'TacticalCell'='tc_avg') %>%
  mutate('TacticalCell_update'=case_when(TacticalCell == 'South and the Channel Islands' ~ 'South West',
                                         TacticalCell == 'Central' ~ 'Midlands & East',
                                         TRUE ~ as.character(.$TacticalCell))) %>%
  select(-'TacticalCell') %>% rename("TacticalCell"=TacticalCell_update) %>%
  select('LAD19CD', 'TacticalCell', everything())

# --- areas to focus ---
# --- Covid ----
covid_area2focus <- read_feather('data/areas_to_focus/areas2focus_covid.feather')

# covid prefix for name for table
#covid_week = colnames(covid_area2focus)[6]
#covid_week = strsplit(covid_week, " ")
#covid_week = paste('Week', covid_week[[1]][2], ' 2021\n')
covid_data_date = format(covid_area2focus$date[1], '%d/%m/%Y')
# possible weird repetitive unknown unititialised column error work around. 
remove <- c("date")
covid_area2focus <- covid_area2focus[, !(names(covid_area2focus) %in% remove)]

# rename with suffix for time being. 
covid_area2focus <- covid_area2focus %>%
  rename('covid cases per 100,000'=newCasesBySpecimenDateRollingRate) %>%
  rename('Name' = areaName) %>%
  rename('Total cases' = newCasesBySpecimenDateRollingSum) %>%
  rename('% change in covid cases' = newCasesBySpecimenDateChangePercentage) %>%
  mutate('TacticalCell_update'=case_when(TacticalCell == 'South and the Channel Islands' ~ 'South West',
                                         TacticalCell == 'Central' ~ 'Midlands & East',
                                         TRUE ~ as.character(.$TacticalCell))) %>%
  select(-'TacticalCell', -'areaType', -'newCasesBySpecimenDate') %>%
  rename("TacticalCell"=TacticalCell_update)
  



# ---- Flooding ---
# flooding stats within resilience index
flooding_area2focus <- lad_uk2vuln_resilience %>% st_drop_geometry() %>%
  mutate('TacticalCell_update'=case_when(TacticalCell == 'South and the Channel Islands' ~ 'South West',
                                         TacticalCell == 'Central' ~ 'Midlands & East',
                                         TRUE ~ as.character(.$TacticalCell))) %>%
  select(-'TacticalCell') %>% rename("TacticalCell"=TacticalCell_update) %>%
  select('LAD19CD','LAD19NM','TacticalCell',`Vulnerability quintile`, `Total people in flood risk areas`, 
         `% people in flood risk areas`, `Flood risk quintile`,
         `Total historical flooding incidents`, `Flooding incidents per 10,000 people`,
         `Flood incidents quintile`)


# flood outlines metoffice warnings 
flood_warning_polygons <- read_sf('./data/areas_to_focus/current_live_warnings_polygons.geojson')
flood_warning_points <- read_sf('./data/areas_to_focus/current_live_warnings_points.geojson')
flood_warning_meta <- read_feather('./data/areas_to_focus/current_live_warnings_metadata.feather')


# for areas to focus list 
total_type_of_warning_per_authority <- flood_warning_meta %>% group_by(lad19nm, severity) %>%
  count() 
# how many of each type of alert
total_type_of_warning_per_aurthority_trans <- pivot_wider(total_type_of_warning_per_authority, names_from=severity, values_from=n, names_prefix = 'Total live ') %>%
  rename('LAD19NM'=lad19nm) %>% replace(is.na(.), 0)

# join to flooding areas to focus 
flooding_area2focus <- left_join(flooding_area2focus, total_type_of_warning_per_aurthority_trans, by='LAD19NM', keep=F)


# ensure always have columns for all alerts
# if column not there
if(!"Total live severe Flood warning" %in% colnames(flooding_area2focus)) {
  flooding_area2focus <- flooding_area2focus %>% mutate('Total live severe Flood warning' = 0)
}

if(!"Total live Flood warning" %in% colnames(flooding_area2focus)) {
  flooding_area2focus <- flooding_area2focus %>% mutate('Total live Flood warning' = 0)
}

if(!"Total live Flood alert" %in% colnames(flooding_area2focus)) {
  flooding_area2focus <- flooding_area2focus %>% mutate('Total live severe Flood alert' = 0)
}

# ensure order is consistent
flooding_area2focus <- flooding_area2focus %>% relocate(`Total live severe Flood warning`, `Total live Flood warning`, -`Total live Flood alert`, .after=`Flood incidents quintile`) %>%
  mutate(`Total live severe Flood warning`=replace_na(`Total live severe Flood warning`,0)) %>%
  mutate(`Total live Flood warning`=replace_na(`Total live Flood warning`,0)) %>%
  mutate(`Total live Flood alert`=replace_na(`Total live Flood alert`,0))



# join dfs for mapping
flood_warning_polygons <- left_join(flood_warning_meta, flood_warning_polygons, by='floodAreaID', keep=F) %>%
  mutate('TacticalCell_update'=case_when(TacticalCell == 'South and the Channel Islands' ~ 'South West',
                                         TacticalCell == 'Central' ~ 'Midlands & East',
                                         TRUE ~ as.character(.$TacticalCell))) %>%
  select(-'TacticalCell') %>% rename("TacticalCell"=TacticalCell_update) 

flood_warning_points <- left_join(flood_warning_points,flood_warning_meta, by='floodAreaID', keep=F) %>%
  mutate('TacticalCell_update'=case_when(TacticalCell == 'South and the Channel Islands' ~ 'South West',
                                         TacticalCell == 'Central' ~ 'Midlands & East',
                                         TRUE ~ as.character(.$TacticalCell))) %>%
  select(-'TacticalCell') %>% rename("TacticalCell"=TacticalCell_update) 

flood_warning_meta <- flood_warning_meta %>%
  mutate('TacticalCell_update'=case_when(TacticalCell == 'South and the Channel Islands' ~ 'South West',
                                         TacticalCell == 'Central' ~ 'Midlands & East',
                                         TRUE ~ as.character(.$TacticalCell))) %>%
  select(-'TacticalCell') %>% rename("TacticalCell"=TacticalCell_update) 
  

# -- vcs indicators
requests <- read_feather('data/vcs_indicators/requests_this_week_and_last.feather')
requests <- requests %>%
  mutate('TacticalCell_update'=case_when(TacticalCell == 'South and the Channel Islands' ~ 'South West',
                                         TacticalCell == 'Central' ~ 'Midlands & East',
                                         TRUE ~ as.character(.$TacticalCell))) %>%
  select(-'TacticalCell') %>% rename("TacticalCell"=TacticalCell_update)


# -- for home page maybe replacing initial requests --
requests_home <- read_feather("data/vcs_indicators/all_requests.feather")
pulse <- read_feather("data/vcs_indicators/pulse_check_summary.feather")


# latest insight
vac_data <- read_feather("data/areas_to_focus/vaccination_rate.feather")

### --- update time --- 
time_of_update <- Sys.time()
time_and_date <- str_split(time_of_update, " ")
last_updated_time <- paste0(time_and_date[[1]][2],",")
last_updated_date <- paste(format(as.Date(time_and_date[[1]][1], format="%Y-%m-%d"), "%d/%m/%Y"))


# ---  dashboard --- #
# --- header --- #
header <- dashboardHeader(title = "", titleWidth = "300px",
                         tags$li(class = "dropdown", 
                                   tags$li(class="dropdown",
                                   actionLink("home", "Home", icon=icon("home"))),
                                  tags$li(class="dropdown",
                                          actionLink("RI_tool","Risk Indicator Tool", icon=icon("fas fa-map-signs"))
                                  ),
                                 tags$li(class="dropdown",
                                         actionLink("e_catalogue", "Insight catalouge", icon=icon("fas fa-book-open"))),
                                 tags$li(class="dropdown",
                                         actionLink("internal_reports", "Partnership insight", icon=icon("fas fa-lock"))),
                                 tags$li(class="dropdown",
                                         actionLink("community_assets", "Community assets map", icon=icon("fas fa-map-marked"), 
                                                    onclick ="window.open('https://britishredcross.maps.arcgis.com/apps/webappviewer/index.html?id=b2fec0e028554a5aac99d3519c81ab44', '_blank')")),
                                 tags$li(class="dropdown",
                                         actionLink("help", "Help", icon=icon("far fa-question-circle"))),
                                 tags$li(class="dropdown",
                                         actionLink("references", "References", icon=icon('fas fa-feather-alt')))
                                 ))
                  


# --- side bar --- #
sidebar <- dashboardSidebar(
  width = "300px",
  minified = T,
  collapsed=T,
  tags$style(HTML(".sidebar-menu li a { font-size: 16px; }")),
  sidebarMenu(id="sidebar_id",
              
              # -- Home page ---
              menuItem('Home', tabName='home', icon=icon("home")),
              # -- Unmet need insight --
              menuItem(HTML("Risk Indicator Tool"), tabName="unmetneed", icon=icon("fas fa-map-signs")),
              
              # -- trying conditional panel ---
              
              menuItem(HTML("Insight catalouge"), tabName="resource_catalogue", icon=icon("fas fa-book-open")),
              menuItem(HTML("Partnership insight"), tabName="internal_reports_from_sidebar", icon=icon("fas fa-lock")),
              menuItem(HTML("Community assets web map"), tabName="community_assets", icon=icon("fas fa-map-marked")),
    
              menuItem("Help", tabName="Help", icon=icon("far fa-question-circle")),
              menuItem("References", tabName='references', icon=icon('fas fa-feather-alt'))


  # - display vcsep logo -
  #div(p("Developed by"), img(src = "vcs-logo-text.png", width = 225),style="position:fixed; bottom:0; padding:15px; text-align: center;")
    
  )
)

body <- dashboardBody(
  useShinyjs(),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  tags$head(includeCSS("styles.css"), includeHTML("cookie_consent_v2.html"),
            tags$link(rel="icon", sizes="32X32", href="img/favicon-32x32.png"),
            tags$link(rel="icon",  sizes="16X16", href="img/favicon-16x16.png"),
            tags$link(rel="apple-touch-icon", sizes="180x180", href="img/img/apple-touch-icon.png"),
            tags$link(rel="manifest", href="img/site.webmanifest")),
  tabItems(
    # --- Home page ---
    tabItem(tabName="home", selected=T,
            # - row 1 -
            fluidRow(style="padding-right:20px; padding-left:20px; padding-bottom:40px",
              # column 1
              column(width = 10,
                     h1("Welcome to the Emergencies Partnership Toolkit"),
                              h4("Sharing insight and resources amongst the VCS to help before, during and after an emergency."),
              ),
              column(width = 2,
                     div(img(src = "vcs-logo-text.png", width = 350),
                         style="position:fixed; padding-right:20px; padding-left:20px;padding-top:0px;padding-bottom:50px"))),
              # row two
              fluidRow(style="padding-right:20px; padding-left:20px; padding-bottom:20px",  
                     
                     column(width=3,
                            box(title=actionLink("RI_tool_box","Risk Indicator Tool"), width=NULL,
                                collapsible = T, collapsed=T,
                                icon = icon("fas fa-map-signs"),
                                p("The Risk indicator tool shows...
                                  click on the header to go to the tool"))),
                     
                     column(width=3,
                            box(title="Insight catalogue",width=NULL, 
                                collapsible = T, collapsed=T,
                                icon = icon("fas fa-book-open"),
                                p("The Insight catalogue lists publicly available resources...
                                  click on the header to go to the tool"))),
                    
                    column(width=3, 
                        box(title=actionLink("internal_reports_from_box", "Partnership insight"),
                          width=NULL,
                          collapsible = T, 
                          collapsed=T,
                            icon = icon("fas fa-lock"),
                          p("Internal reports are for VCS members.
                          Current reports include the RFS, Pulse survey and vaccine uptake.
                                  click on the header to go to the tool"))),
            
                       column(width=3,
                              box(title=actionLink("community_assets", "Community assets map", 
                                                   onclick ="window.open('https://britishredcross.maps.arcgis.com/apps/webappviewer/index.html?id=b2fec0e028554a5aac99d3519c81ab44', '_blank')")
                                  ,width=NULL,
                                  collapsible = T, collapsed=T,
                                  icon=icon("fas fa-map-marked"),
                                  p("Community assets map allows you to explore and understand 
                                    assets in your area..
                                    click on the title to go to the tool")))),
                      
              # row three 
                  fluidRow(style="padding-right:20px; padding-left:20px; padding-bottom:20px",
                     column(width=4,
                            box(title="Where we're working", width=NULL,
                                uiOutput("home_map_headlines", height='60px'),
                                leafletOutput('home_map', height = "400px"))),
                     column(width=4,
                            box(title="Latest concerns raised by our network", width=NULL,
                                uiOutput("latest_concerns_headline", height='60px', width=NULL),
                                
                                echarts4rOutput('concerns', height="400px"))),
                      
                    column(width = 4,
                           box(title="Latest insight", width=NULL,
                               uiOutput("latest_insight_headline", height='60px'),
                               echarts4rOutput("latest_insight", height="400px"))
              )
            )
          ),

    # -- areas in need --
    tabItem(tabName = "unmetneed",
  # - row 1 -
  fluidRow(style="padding-right:20px; padding-left:20px",
    column(width=12,
        panel(
            width=NULL,
            header=NULL,
            height='30px',
            fluidRow(
              column(width = 3,
            div(h4(tags$strong("Risk indicator tool")),
                p("This tool is to help answer the question of what",
                tags$strong("areas and people"), "would be/are at risk should the",
                                   tags$strong("emergency"), "scenario selected occur"))),
            column(width=3,
                    selectInput("theme",
                                         label="1. Select an emergency",
                                         #choices = sort(c("Covid-19","Winter Pressures","Economic Hardship", "Mental Health","Flooding","Food Insecurity")),
                                         choices = sort(c("Covid-19","Flooding")),
                                         selected="Covid-19")),
            
            column(width=3,
                             selectInput("tactical_cell",
                                         label = "2. Choose a region",
                                         choices = sort(tactical_cells),
                                         selected = "-- England --")),
            column(width=3,
                
                    uiOutput("secondSelection")),
                             
                             )))),
    # - row 2  -
      fluidRow(style="padding-right:20px; padding-left:20px",
      # - column 1 -
      column(width = 4,
                      # - row 3 -
                      fluidRow( 
                        # - column 1 -
                        column(
                          width = 12,
                          tabBox(height='610px',
                            width = NULL, #collapsible = T, collapsed=F,#solidHeader = TRUE, status='primary',
                            title = "", 
                            id = 'people_focus',
                            tabPanel('Areas to focus',
                                     fluidRow(width=NULL, 
                                              column(width=12,style='margin-bottom:-5px;padding-bottom:-5px;',
                                                     uiOutput("title_focus_list", height='30px'))),
                                     fluidRow(width=NULL,
                                              column(width=12, #tags$style("#top10options {font-size:10pt;height:30px;}"),
                                                     style='margin-top:-30px;margin-bottom:-30px;padding-top:-30px;padding-bottom:-30px;padding-left:20px;font-size:10pt',
                                              #HTML("label{float:left;}")
                                           
                                     uiOutput('top10options', height='20px'))),
                                     
                                     # top 10 options
                                     fluidRow(width=NULL,
                                     column(width=12, style = "height:450px; overflow-y: scroll;overflow-x: scroll;",
                                    fluidRow(width=NULL,
                                              column(width=12,
                                                     uiOutput('top_10_1'))),
                                     fluidRow(width=NULL,
                                              column(width=12,
                                                     uiOutput('top_10_2'))),
                                     fluidRow(width=NULL,
                                              column(width=12,
                                                     uiOutput('top_10_3'))),
                                     fluidRow(width=NULL,
                                              column(width=12, 
                                                     uiOutput('top_10_4'))),
                                     fluidRow(width=NULL,
                                              column(width=12,
                                                     uiOutput('top_10_5'))),
                                     fluidRow(width=NULL,
                                              column(width=12,
                                                     uiOutput('top_10_6'))),
                                     fluidRow(width=NULL,
                                              column(width=12,
                                                     uiOutput('top_10_7'))),
                                     fluidRow(width=NULL,
                                              column(width=12,
                                                     uiOutput('top_10_8'))),
                                     fluidRow(width=NULL,
                                              column(width=12,
                                                     uiOutput('top_10_9'))),
                                     fluidRow(width=NULL,
                                              column(width=12,
                                                     uiOutput('top_10_10')))
                                     ))
                                     
                                     ),
                            tabPanel("Area demographics", 
                          # multi columned box - bame row
                          # -- shielding row ---
                          fluidRow(style = "border-top: 1px solid #D3D3D3;",
                                   column(
                                     width = 12,
                                     uiOutput('shielding_text'),
                                     echarts4rOutput('shielding_f', height='40px'),
                                     rightBorder=F,
                                     marginBottom =T
                                   )
                          ),
                            
                            # -- section 95 row ---
                            fluidRow(style = "border-top: 1px solid #D3D3D3;",
                              column(
                                width = 12,
                                uiOutput('section95_text'),
                                echarts4rOutput('section95', height='40px'),
                                rightBorder=F,
                                marginBottom =T
                              )
                            ),
                          
                          # -- homeless row ---
                          fluidRow(style = "border-top: 1px solid #D3D3D3;",
                                   column(
                                     width = 12,
                                     uiOutput('homeless_text'),
                                     echarts4rOutput('homeless', height='40px'),
                                     rightBorder=F,
                                     marginBottom =T
                                   )
                          ),
                          
                          # -- fuel poverty row ---
                          fluidRow(style = "border-top: 1px solid #D3D3D3;",
                                   column(
                                     width = 12,
                                     uiOutput('fuelp_text'),
                                     echarts4rOutput('fuelp', height='40px'),
                                     rightBorder=F,
                                     marginBottom =T
                                   )
                          ),
                          
                          # -- universal credit row ---
                          fluidRow(style = "border-top: 1px solid #D3D3D3;",
                                   column(
                                     width = 12,
                                     uiOutput('unemployment_text'),
                                     echarts4rOutput('unemployment', height='40px'),
                                   )
                          ),
                          
                          # -- digital exclusion row ---
                          fluidRow(style = "border-top: 1px solid #D3D3D3;",
                                   column(
                                     width = 12,
                                     uiOutput('digital_text'),
                                     echarts4rOutput('digital', height='40px'),
                                     rightBorder=F,
                                     marginBottom =T
                                   )
                          ),
                          
                         
                          fluidRow(style = "border-top: 1px solid #D3D3D3;",
                                   column(
                                     width = 12,
                                     uiOutput('bame_population_text', height='40px'),
                                     echarts4rOutput('bame_population', height='40px'),
                                     rightBorder=F,
                                     marginBottom =T
                                   )
                          ),
                          style = "height:550px; overflow-y: scroll;overflow-x: scroll;"
                          )
                          )
                          
                        )
                    ),
                ),

              # column - 2
              column( width = 8,
                    # - row 1 -
                    tabBox(
                      id = 'areas',
                      # bivariate
                      title = "",
                      width = NULL, height = "610px", #solidHeader = TRUE, status='primary',
                      #closable = F,
                      tabPanel('Areas at risk', 
                      withSpinner(leafletOutput("map", height = "550px"))),
                      tabPanel("Find local resources",
                                 fluidRow( 
                                   column(width=8,
                                          uiOutput("search_needed", height='50px')),
                               
                                  column(width=4,
                                          uiOutput("expand_search_needed", height='50px'))),
                                
                                 fluidRow(
                                   column(width=12,
                                          uiOutput('local_orgs_ui', height='350px'))),
                                 
                                 style = "height:550px; overflow-y: scroll;overflow-x: scroll;"
                               ),
                      tabPanel('Areas to focus table', 
                      withSpinner(DT::dataTableOutput('areas2focus', height='350px')),
                      style = "height:550px; overflow-y: scroll;overflow-x: scroll;")
                )
              )
        )
    ),

  tabItem(tabName='Help',
          column(width = 12,
                   tabBox(id='information about dashboards', width=NULL,
                          tabPanel("Help - areas at risk in an emergency",
                                   uiOutput('about_needs'),
                                  style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
                                  )))),
  tabItem(tabName = 'references',
          fluidRow(style="padding-right:100px;padding-left:100px;padding-top:20px;padding-bottom:20px",
                   # column 1
                   column(width = 12,
                          box(width=NULL, headerBorder=F,
                              uiOutput('refs'),
                              style = "height:650px; overflow-y: scroll;overflow-x: scroll;margin-top:-50px;padding-top:-50px"
                              #style = 'overflow-y:scroll; height: calc(100vh - 200px) !important;'
                          )))
    )
  )
)



# --- build user interface --- #
ui <- function(request) {
  dashboardPage(
  #sidebar_fullCollapse = TRUE,
  skin='purple',
  header,
  sidebar,
  body,
)
}


# ---- server ---- #
server = function(input, output, session) {

  observeEvent(input$home, {
    newtab <- switch(input$sidebar_id, "home")
    updateTabItems(session, "sidebar_id", newtab)
    
  })
  
  
  observeEvent(input$references, {
    newtab <- switch(input$sidebar_id, "references")
    updateTabItems(session, "sidebar_id", newtab)

  })
  
  observeEvent(input$RI_tool, {
    newtab <- switch(input$sidebar_id, "unmetneed")
    updateTabItems(session, "sidebar_id", newtab)
    
  })
  
  observeEvent(input$RI_tool_box, {
    newtab <- switch(input$sidebar_id, "unmetneed")
    updateTabItems(session, "sidebar_id", newtab)
    
  })
  
  
  observeEvent(input$help, {
    newtab <- switch(input$sidebar_id, "Help")
    print(newtab)
    updateTabItems(session, "sidebar_id", newtab)
    
  })
  
  # --- observe if references tab selected ---
  observeEvent(input$references, {
      output$refs <- renderUI({
        div(
          h2(tags$strong("Data contributors")),
          hr(),
          p("We make use of a range of data sources to bring you this insight,
            including", tags$strong("data that is open source"), "as well as", tags$strong("data from our contributing partners.")),
          p("The platform uses data and code from the", tags$a(href="https://github.com/britishredcrosssociety/covid-19-vulnerability", target="_blank", 'British Red Cross COVID-19 Vulnerability Index,'), tags$a(href="https://github.com/britishredcrosssociety/resilience-index", target="_blank", 'British Red Cross Resilience Index'), "and the British Red Cross Local Lockdown tool. 
            The code from these tools is distributed under GPL-3 (GNU GENERAL PUBLIC LICENSE version 3). Outputs related to the vulnerability index (e.g., vulnerability scores) are distributed under CC-BY-4.0 (Creative Commons Attribution 4.0 International), unless otherwise stated."),
          p(
            "Data is also included in the platform from",
            tags$a(href="https://www.ons.gov.uk/", target="_blank", "Office of National Statistics"), "and", tags$a(href="https://digital.nhs.uk/", target="_blank", "NHS Digital"), "shared under an", tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target="_blank", "Open Government Licence.")),
            
          br()
        )

      })

  })
  
  
  # --- help for about needs dashboard ---
  observe({
    
    req(input$sidebar_id)
    
    if (input$sidebar_id == 'Help') {
      
      output$about_needs <- renderUI({
        div(h4(tags$strong("Last updated:")),
          p("The dashboard was last updated at", last_updated_time, "on", last_updated_date),
          tags$br(),
          
          # -- about section ---
          h4(tags$strong('About:')),
          p("This tool highlights areas that are in greatest need across a range of emergencies and gives information to support outreach in those areas (e.g. people at risk by demographic). 
            It has been designed with and for members of the Voluntary and Community Emergencies Sector Partnership to inform their emergency preparedness and response efforts, who’s four key questions are:"),
          
          tags$li("Where is the need for support greatest?"),
          tags$li("Who is in greatest need of support?"),
          tags$li("What type of support do they need?"),
          tags$li("How are those people and areas being supported?"),
          tags$br(),
          
          p("We utilise a range of sources (see references tab) to answer these questions, including the",
            tags$a(href="https://github.com/britishredcrosssociety/resilience-index", target="_blank", "British Red Cross Resilience Index,"),
            "which ranks the resilience of an area based on its vulnerability to a range of factors against an areas ability to cope."),
          
          tags$br(),
          h4(tags$strong("Areas to focus:")),
             p("Highlights 10 areas where an emergency is most prevalent in England and by region. 
               It is intended to support our multiagency network to focus their outreach and response efforts in those areas that are most impacted by an emergency.
               To view the the resilience of just these areas select the toggle below the zoom buttons on the map."),
            h5(tags$strong("Covid-19:", style="color:blue")),
               p("View the top 10 areas with either the highest number of cases per 100,000 or an area with the greatest  % change in total covid cases over a rolling 7 day period. 
                This current relevant 7 day period is up to:", covid_data_date), 
               
               p(
               tags$li(tags$strong("Covid cases per 100,000:"), "a rolling 7-day average of positive covid tests for each local authority district (lower tier local authority). 
                       To convert the rolling average of cases to average of cases per 100,000, the average number of cases is divided by the population of the local authority district and multiplied by 100,000."),
               tags$br(),
               tags$li(tags$strong("Total covid cases:"), "the total number of new cases (by specimen date) seen over the relevant 7 day period ."),
               tags$br(),
               tags$li(tags$strong('% change in covid cases:'), 'the % change in new cases (by specimen date) between the most recent relevant 7-day period and the 7-day period prior to that.'),
               tags$br(),
               tags$li(tags$strong("Rolling average:"), "This is determined by averaging the number new cases by specimen date on the day itself, the 3 days prior and the 3 days after.
                       For this to be possible this data is the rolling average of 5 days prior to the current day (i.e. starting from 5 days prior to today - the 7 days prior to that).")),
              
               p(tags$strong("Frequency of update:"), "Data is updated daily but note 5-day lag due to rolling average."),
               p(tags$strong("Source:"), "For more information see", tags$a(href="https://coronavirus.data.gov.uk/", target="_blank", "https://coronavirus.data.gov.uk/."),
                  "This data contains Public Health England data © Crown copyright and database right 2020 and is available under the", tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target="_blank", " Open Government Licence v3.0.")),
          #tags$br(),
          h5(tags$strong("Flooding:", style="color:blue")),
          p("View the top 10 areas with either the most flood warnings ranked by severity of warning, 
            the most historical flood incidents per 10,000 people, or with the highest proportion of population living in flood risk areas."),
          p(tags$li(tags$strong('Flood warnings and alerts:'), "local authority districts are deemed to be affected by a flood alert or warning if the predicted most extreme scenario (represented by the flood outline provided by the environment agency) overlaps their local authority district. 
                    On occasions the flood outline shown in the app may not appear to overlay a local authority. This is to help the performance of the app by reducing the complexity of the flood outline thereby reducing the file size.
                    When there are no flood warnings active the areas are ordered by number of historical flood incidents per 10,000 people.", 
                    tags$strong("Caveat:"), "- currently these warnings are updated daily as opposed to every 15 minutes as per the environment agency - we aim to change this in an upcoming release."),
            tags$br(),
            tags$li(tags$strong("Historical flood events per 10,000 people:"), "Historical flood incidents are determined by the number of Fire and Rescue Service call outs to flooding incidents."),
            tags$br(),
            tags$li(tags$strong("Proportion of the population living in flood risk areas:"), "This shows the proportion of the population of each local authority district living in areas where there is a greater than 1% chance a year of flooding.
                    This is calculated by determining the number of people living in the flood zones defined by the environment agency. ")),
            
          p(tags$strong("Frequency of update:"), "TBC"),
          p(tags$strong("Source:"), 
            "The", tags$a(href="https://flood-warning-information.service.gov.uk/warnings",target="_blank", "environment agency"), "flood warnings and alerts are available under the", tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target="_blank", " Open Government Licence v3.0."),
            tags$br(),
            "This representation of historical flood incidents is based upon the BRC Resilience index, released under the GLP-v3 licence. The code is available",
            tags$a(href="https://github.com/britishredcrosssociety/resilience-index", target="_blank", "here."),
            "The areas at greater than 1% chance of flooding were defined by the",tags$a(href="https://data.gov.uk/dataset/bad20199-6d39-4aad-8564-26a46778fd94/risk-of-flooding-from-rivers-and-sea", target="_blank", "environment agency risk of flooding from rivers and sea"),"This representation of the proportion of people living in flood risk areas is based upon the BRC resilience index, released under the GLP-v3 licence. The code is available",
            tags$a(href="https://github.com/britishredcrosssociety/resilience-index", target="_blank", "here.")),
            tags$br(),
          
          
          
          h4(tags$strong("Areas at Risk - interpreting the map:")), 
          tags$br(),
          p(img(src = "bivar-legend_v2.png", width = 225, style="float:left;text-align:left;vertical-align:middle;"),
          "We use a map to answer the key question of “where is the need greatest”. 
            You’ll notice a legend in the bottom left corner of the map, with 9 different colours
            to show the varying levels of greatest need. The colour pallet has been specifically chosen 
            to a) avoid confusing with Red, Amber and Green which is typically used to assess performance,
            and b) as it is a recommended", 
            tags$a(href="https://nowosad.github.io/post/cbc-bp2/", target="_blank", "color-blind friendly pallet.")),
          
         p(
         "Darker colours reflect those areas in greater need of support, with Black highlighting areas thought to be in greatest need. 
           By this we mean, what areas have the most vulnerability to an emergency with the least capacity to cope, should an emergency occur." 
           
          ),#For each emergency, the areas flagged as being in greatest need will differ, depending on the indicators chosen to understand vulnerability and resilience.",
         tags$br(),
         tags$br(),
           
          h4(tags$strong("Map layers:")), 
          p(tags$strong("Covid-19 emergency map:", style="color:blue"),
            tags$br(),
            "The British Red Cross developed a series of indices to identify UK areas vulnerable 
            to the effects of COVID-19, and a resilience index which overlays capacity to cope.
            Using statistical modelling of data from a range of", tags$a(href="https://github.com/britishredcrosssociety/covid-19-vulnerability/blob/master/output/metadata_vi.csv", target="_blank", "(mostly open) sources,"), "the indices
            provide an area rating, which is then used to map areas of need. 
            Users can also explore different layers of the map based on vulnerability type, 
            by selecting the button in the top right corner of the map."),
            p("These layers include:",
              tags$br(),
          
            tags$li(tags$strong(tags$em("Resilience: vulnerablity vs capacity to cope: ")), "This layer shows the", tags$a(href="https://britishredcross.shinyapps.io/resilience-index/", target="_blank", 'British Red Cross resilience index.'), 
                    "This shows the vulnerability vs the capacity to cope with an emergency of local authority districts in England. 
                    The ", tags$strong("black", style="color:#000000"), "highlights those areas that are", tags$strong("most in need - highest vulnerability and least capacity to cope." , style="color:#000000")), 
                                                                             #tags$strong("The brightest red", style="color:#AE3A4E"), "indicates areas that are", tags$strong("highly vulnerable but have high capactiy to cope.",style="color:#AE3A4E"), 
                                                                             #tags$strong("The darker blue", style="color:#4885C1"), "indicates", tags$strong("low vulnerability but low capacity to cope.", style="color:#4885C1"), 
                                                                             tags$strong("The lightest gray", style="color:#d9d9d9"), "indicates", tags$strong("the least in need - lowest vulnerability and highest capactiy.", style="color:#d9d9d9"), 
                    "We selected this colour scheme to try and avoid confusion with the RAG colour scheme that is commonly used in operational response."), 
                        
                        tags$li(tags$strong(tags$em("Economic vulnerability:")), "This layer shows the economic vulnerability of local authority districts based upon the", tags$a(href="https://github.com/britishredcrosssociety/covid-19-vulnerability/blob/master/README.md", target="_blank", "BRC vulnerability index."), "Purple indicates the most vulnerable, yellow the least vulnerable"),
                        tags$br(),
                        tags$li(tags$strong(tags$em("Socioeconomic vulnerability:")), "This layer shows the socioecomic vulnerability of local authority districts based upon the", tags$a(href="https://github.com/britishredcrosssociety/covid-19-vulnerability/blob/master/README.md", target="_blank", "BRC vulnerability index."), "Purple indicates the most vulnerable, yellow the least vulnerable"),
                        tags$br(),
                        tags$li(tags$strong(tags$em("Social vulnerability:")), "This layer shows the social vulnerability (i.e barriers to housing and services, poor living environment etc.) of local authority districts based upon the", tags$a(href="https://github.com/britishredcrosssociety/covid-19-vulnerability/blob/master/README.md", target="_blank", "BRC vulnerability index."), "Purple indicates the most vulnerable, yellow the least vulnerable"),
                        tags$br(),
                        tags$li(tags$strong(tags$em("Health/wellbeing vulnerability:")), "This layer shows the health/wellbeing vulnerability (i.e mental health and loneliness etc.) of local authority districts based upon the", tags$a(href="https://github.com/britishredcrosssociety/covid-19-vulnerability/blob/master/README.md", target="_blank", "BRC vulnerability index."), "Purple indicates the most vulnerable, yellow the least vulnerable"),
                        tags$br(),
                        tags$li(tags$strong(tags$em("Clinical vulnerability:")), "This layer shows the clinical vulnerability (i.e underlying health conditions etc.) of local authority districts based upon the", tags$a(href="https://github.com/britishredcrosssociety/covid-19-vulnerability/blob/master/README.md", target="_blank", "BRC vulnerability index."), "Purple indicates the most vulnerable, yellow the least vulnerable"),
          tags$br(),              
          p(tags$strong("Flooding emergency map layers:", style="color:blue"),
                        tags$li(tags$strong(tags$em("Resilience of all local authorities:")), "This shows the BRC Resilience index (vulnerability vs capacity to cope) using the same colour scheme as the for the same layer on the Covid-19 data."),
                        tags$br(),
                        tags$li(tags$strong(tags$em("Flood warnings/alerts:")), "The points and polygons displayed show the flood warnings and alerts from the", tags$a(href="https://flood-warning-information.service.gov.uk/warnings", target="_blank","environment agency"), "as of", last_updated_time, last_updated_date)),

        )
      })
      
    }
    
  })


  #set up polygons
  pal <- colorFactor("viridis", c(1:5), reverse = TRUE)
  # --- preparing base map ---- #

  # set up the static parts of the map (that don't change as user selects different options)
  output$map <- renderLeaflet({
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, attributionControl = T)) %>%
        setView(lat = 54.00366, lng = -2.547855, zoom = 5) %>% # maybe could Fenny drayton to make map sclighly closer initially --> centre map on lat = 54.00366, lng = -2.547855 Whitendale Hanging Stones, the centre of GB: https://en.wikipedia.org/wiki/Centre_points_of_the_United_Kingdom
        addProviderTiles(providers$CartoDB.Positron) %>%
        addTiles(urlTemplate = "", attribution = '2020 (c) British Red Cross') %>%
        addEasyButton(easyButton(
          states = list(
            easyButtonState(
              stateName="show-all",
              icon="ion-toggle",
              #icon="fas fa-bullseye",
              title="Show top 10 areas to focus",
              onClick = JS("
              function(btn, map) {
                btn.state('top-ten');
                Shiny.onInputChange('my_easy_button', 'show_top_ten');
              }")
            ),
            easyButtonState(
              stateName="top-ten",
              icon="ion-toggle-filled",
              #icon = "far fa-times-circle",
              title="Show all areas",
              onClick = JS("
              function(btn, map) {
                btn.state('show-all');
                Shiny.onInputChange('my_easy_button', 'show_all');
              }")
            )
          )
        ))
    
    })
  # to prevent map error in js console - not sure if necessary
  outputOptions(output,"map",suspendWhenHidden=FALSE)
  
  count <- 0
  autoInvalidate <- reactiveTimer(15000)
  
  
  # to change highlights
  observeEvent(autoInvalidate(), {
     if(input$sidebar_id == 'home') {
       
       if (count > 2) {
          count <<- 1
       }
       else {
         count <<- count + 1
       }
      }
   })
  
  output$home_map_headlines <- renderUI({
    autoInvalidate()
    print(count)
    rfs_highlights(requests_home, count)
    
  })
  
  output$home_map <- renderLeaflet ({
    
    requests_home_no_na <- requests_home %>%
      filter(!is.na(admin_district_code))
    
    leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, attributionControl = T)) %>%
    setView(lat = 54.00366, lng = -2.547855, zoom = 5) %>% # maybe could Fenny drayton to make map sclighly closer initially --> centre map on lat = 54.00366, lng = -2.547855 Whitendale Hanging Stones, the centre of GB: https://en.wikipedia.org/wiki/Centre_points_of_the_United_Kingdom
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data=tc_shp, layerId = ~TacticalCell,
                group='tactical cell boundary',
                stroke=T,
                weight = 1,
                opacity = 0.8,
                color = "red",
                dashArray = "3",
                fill=F) %>%
    addCircleMarkers(data=requests_home_no_na, 
                     lng=requests_home_no_na$longitude, 
                     lat=requests_home_no_na$latitude,
                     #radius=requests_home$request_status_radius,
                     #color=requests_home$request_status_col,
                     #fillOpacity = requests_home$request_status_opacity,
                     radius=4,
                     color='blue',
                     fillOpacity=0.6,
                     stroke=F)
               #clusterOptions = markerClusterOptions())
    
  })
  
  output$latest_concerns_headline <- renderUI({
    max_in_need <- pulse %>% arrange(-desc(proportion_respondents))
    max_in_need <- tail(max_in_need, 1)
    
    div(
    p(
    tags$strong(paste0(max_in_need$proportion_respondents,"%")), paste0("(",max_in_need$group_total, ")"), "of respondents
                  reported", tags$strong(max_in_need$clean_names), "as a concern
                  in the next 14 days.",
    tags$em(paste0("(source latest pulse check survey)"))))
    
  })
  
  # pulse major concerns
  output$concerns <- renderEcharts4r({
    
    pulse <- pulse %>% arrange(-desc(proportion_respondents))
    
    concerns_pulse <- pulse %>%
      e_charts(x = clean_names) %>%
      e_bar(proportion_respondents, bar_width=1, showBackground=T) %>%
      #e_labels(position = "right", color='black') %>%
      #e_color(c('red')) %>%
      #e_title('Respondents concerns in next 14 days (%)') %>%
      e_hide_grid_lines() %>%
      e_flip_coords() %>%
      e_grid(containLabel = TRUE, left=30, right=30, top=20, bottom=5, height='90%') %>%
      e_x_axis(name='% respondents', nameLocation="middle", position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
      e_y_axis(axisLabel = list(interval = 0, show = T)) %>%
      e_y_axis(show=T) %>%
      e_axis_labels(x="% respondents") %>%
      e_legend(FALSE)
    
  })
  
  
 output$latest_insight_headline <- renderUI({
   vac_second_dose_highest <- vac_data %>%
     filter(dose=='Second dose')
   
   
   
   vac_second_dose_highest <- vac_second_dose_highest %>%
     arrange(-desc(prop_of_population)) %>%
     tail(n=1)
   
   #print(vac_second_dose_highest)
   
   div(
       p(tags$strong(paste0(vac_second_dose_highest$prop_of_population, "%")),
         "of those aged", tags$strong(vac_second_dose_highest$age_range),
         "have received both doses of vaccine against COVID-19", 
         tags$em(paste0("(vaccine uptake dashboard)"))
         ))
   
})
  
  output$latest_insight <- renderEcharts4r({
    #pulse <- pulse %>% arrange(-desc(proportion_respondents)) 
    
    vaccines <- vac_data %>%
      group_by(dose) %>%
      e_charts(x = age_range) %>%
      e_bar(prop_of_population, bar_width=1, showBackground=T) %>%
      #e_labels(position = "right", color='black') %>%
      #e_color(c('red')) %>%
      e_hide_grid_lines() %>%
      #e_title("% population vaccinated", fontsize=12) %>%
      e_flip_coords() %>%
      e_grid(containLabel = TRUE, left=30, right=30, top=20, bottom=5, height='90%') %>%
      e_x_axis(name='% populoation', nameLocation="middle", position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
      e_y_axis(axisLabel = list(interval = 0, show = T)) %>%
      e_y_axis(show=T) %>%
      e_legend(show=F)
    
  })
    


  # --- Respond to users input on location ----
  # ---- Respond to users tactical cell ----
  filteredLA <- reactive({
    if(input$tactical_cell == '-- England --') {
      lad_uk2areas2vulnerability
    }
    
    else {
        lad_uk2areas2vulnerability %>% filter(TacticalCell == input$tactical_cell)
    }
  })
  

  
  observe( {
    if (input$tactical_cell == '-- England --') {
      output$secondSelection <- renderUI({
        #lads2select <- unique(lad_uk2vuln_resilience$Name)
        #lads2select <- c('All local authorities in region',sort(lads2select))
        lads2select <- c('All local authorities in region')
        selectInput("lad_selected", "3. Local authority district", choices = lads2select, selected='All local authorities in region')
      })
    }
    
    else {
      
      # has a local authority been selected 
      if (input$lad_selected != 'All local authorities in region') {
        output$secondSelection <- renderUI({
          lads2select <- unique(filteredLA()$Name)
          lads2select <- c('All local authorities in region',sort(lads2select))
          #print(dd_areas2focus$l)
          selectInput("lad_selected", "3. Local authority district", choices = lads2select, selected=dd_areas2focus$l)
        })
        
      }
      
      
      else {
        
        # ---- Adjust LAD options based on tactical cell ---
        output$secondSelection <- renderUI({
          lads2select <- unique(filteredLA()$Name)
          lads2select <- c('All local authorities in region',sort(lads2select))
          selectInput("lad_selected", "3. Local authority district", choices = lads2select, selected='All local authorities in region')
        })
        
      }
    }
  })
  

  
  
  # --- button on map ---
  showtop10_or_all <- reactiveValues(display_wanted='show_all')
  observeEvent(input$my_easy_button, {
    showtop10_or_all$display_wanted <- input$my_easy_button
  })
  
 
  # ---- Respond to users input on location and theme ----
  
  # for zoom
  filtered_tc <- reactive({
    
    req(input$sidebar_id)
    if (input$sidebar_id == 'unmetneed') {
      if (input$tactical_cell == '-- England --') {
        zoom_level <- tc_shp
      }
      else {
        zoom_level <- tc_shp %>% 
          filter(TacticalCell == input$tactical_cell)
      }
    }
    
  })
  
  
  # -- because of layers need to be sepearte reactors --
  filtered_areas_at_risk_covid <- reactive({
    
    # which tab is selected:
    req(input$sidebar_id)
    if (input$sidebar_id == 'unmetneed') {
      
        if(showtop10_or_all$display_wanted == 'show_all') {
          
          if (input$tactical_cell == '-- England --') {
            # --- filter to just areas most in need ---
            lad_uk_most_vuln <- lad_uk2vuln_resilience %>%
              mutate('opacity_val' = 0.8) %>%
              mutate('weight_val'=0.7)
          }
          
          else {
            # Filter to tactical cell
            if (input$lad_selected == 'All local authorities in region') {
              lad_uk_most_vuln <- lad_uk2vuln_resilience %>%
                filter(TacticalCell == input$tactical_cell) %>%
                mutate('opacity_val' = 0.8) %>%
                mutate('weight_val' = 0.7)
            }
            else {
              # Filter to local authority
              #lad_uk_most_vuln <- lad_uk2vuln_resilience %>%
              #  filter(Name == input$lad_selected)
              lad_uk_most_vuln <- lad_uk2vuln_resilience %>%
                filter(TacticalCell == input$tactical_cell) %>%
                mutate('opacity_val'=case_when(Name == input$lad_selected ~ 0.8,
                                               Name != input$lad_selected ~ 0.1)) %>%
                mutate('weight_val'=case_when(Name == input$lad_selected ~ 2,
                                               Name != input$lad_selected ~ 0.7))
              
              # add alpha values 
              
              #--- test ---
              #lad_of_interest <- lad_uk2vuln_resilience %>% filter(Name == input$lad_selected)
              #neighbours_of_interest <- lad_uk2vuln_resilience %>% filter(lengths(st_intersects(., lad_of_interest)) > 0)
              #bounding_wanted <- st_bbox(neighbours_of_interest)
              
            }
          }
          
        }
      
        else {
      
        # --- RESILIENCE index ---  
        # vulnerable colours
        # High income, High inequality --> #3F2949
        # High income, Medium inequality --> "#435786"
        # Medium income, medium inequality --> #806A8A
        # high inequality, medium income  --> "#77324C"
        # "#3F2949" -->
        #vuln_cols <- c("#77324C","#3F2949","#435786","#806A8A")
        #vuln_cols <- c("#000000","#b36600","#b3b3b3", "#376387")
        top_ten_cols <- head(filtered_areas2focus_list(), n=10)
        #top_ten_cols <- top_ten_cols %>% select('Local Authority')
        top_ten_cols <- as.vector(top_ten_cols$`Local Authority`)
        
        if (input$tactical_cell == '-- England --') {
        # --- filter to just areas most in need ---
        lad_uk_most_vuln <- lad_uk2vuln_resilience %>% #%>% filter(fill %in% vuln_cols)
          mutate('opacity_val' = case_when(Name %in% top_ten_cols ~ 0.8,
                                           !Name %in% top_ten_cols ~ 0.1)) %>%
          mutate('weight_val' = case_when(Name %in% top_ten_cols ~ 2,
                                           !Name %in% top_ten_cols ~ 0.7)) 
        }
        
        else {
          # Filter to tactical cell
          if (input$lad_selected == 'All local authorities in region') {
            lad_uk_most_vuln <- lad_uk2vuln_resilience %>%
              filter(TacticalCell == input$tactical_cell) %>%
              #filter(fill %in% vuln_cols)
              mutate('opacity_val' = case_when(Name %in% top_ten_cols ~ 0.8,
                                               !Name %in% top_ten_cols ~ 0.1)) %>%
              mutate('weight_val' = case_when(Name %in% top_ten_cols ~ 2,
                                              !Name %in% top_ten_cols ~ 0.7)) 
          }
          else {
            # Filter to local authority
            lad_uk_most_vuln <- lad_uk2vuln_resilience %>%
              filter(TacticalCell == input$tactical_cell) %>%
              mutate('opacity_val'=case_when(Name == input$lad_selected ~ 0.8,
                                             Name != input$lad_selected ~ 0.1)) %>%
              mutate('weight_val' = case_when(Name %in% top_ten_cols ~ 2,
                                              !Name %in% top_ten_cols ~ 0.7)) 
          }
        }
        }
    }
  })
  
 # --- covid labels ----
  filtered_labels_covid <- reactive({
    labels <- paste0(
                   sprintf("<strong>%s</strong><br/>",  filtered_areas_at_risk_covid()$lad19nm),
                   "Vulnerability (5 = highest vulnerability): ",  filtered_areas_at_risk_covid()$`Vulnerability quintile`, "<br/>",
                   "Capacity (5 = lowest capacity): ",  filtered_areas_at_risk_covid()$`Capacity quintile`
                 ) %>%
                 lapply(htmltools::HTML)
    
  })
  
  # --- to see other vulnerability sub domains ---
  filtered_econ_vuln <- reactive({
    
    
    top_ten_cols <- head(filtered_areas2focus_list(), n=10)
    top_ten_cols <- as.vector(top_ten_cols$`Local Authority`)
    
    # show all or just most vulnerable 
    if(showtop10_or_all$display_wanted == 'show_all') {
      
      # which region to display
      if (input$tactical_cell == '-- England --') {
        econ_vuln <- lad_uk2vuln_resilience %>%
          mutate('opacity_val' = 0.8) %>%
          mutate('weight_val' = 0.7)
        }
      else {
        if (input$lad_selected == 'All local authorities in region') {
          econ_vuln <- lad_uk2vuln_resilience %>% 
            filter(TacticalCell == input$tactical_cell) %>%
            mutate('opacity_val' = 0.8) %>%
            mutate('weight_val' = 0.7)
        }
        else {
          econ_vuln <- lad_uk2vuln_resilience %>%
            filter(TacticalCell == input$tactical_cell) %>%
            mutate('opacity_val'=case_when(Name == input$lad_selected ~ 0.8,
                                           Name != input$lad_selected ~ 0.1)) %>%
            mutate('weight_val' = case_when(Name == input$lad_selected ~ 2,
                                            Name != input$lad_selected ~ 0.7))
        }
        
      }
    }
    else {
    
    if (input$tactical_cell == '-- England --') {
    econ_vuln <- lad_uk2vuln_resilience %>% 
      mutate('opacity_val' = case_when(Name %in% top_ten_cols ~ 0.8,
                                        !Name %in% top_ten_cols ~ 0.1)) %>%
      mutate('weight_val' = case_when(Name %in% top_ten_cols ~ 2,
                                       !Name %in% top_ten_cols ~ 0.7))
    }
    
    else{
      if (input$lad_selected == 'All local authorities in region') {
        econ_vuln <- lad_uk2vuln_resilience %>% 
          filter(TacticalCell == input$tactical_cell) %>%
          mutate('opacity_val' = case_when(Name %in% top_ten_cols ~ 0.8,
                                           !Name %in% top_ten_cols ~ 0.1)) %>%
          mutate('weight_val' = case_when(Name %in% top_ten_cols ~ 2,
                                          !Name %in% top_ten_cols ~ 0.7))
      }
      else {
        econ_vuln <- lad_uk2vuln_resilience %>%
          filter(TacticalCell == input$tactical_cell) %>%
          mutate('opacity_val' = case_when(Name == input$lad_selected ~ 0.8,
                                           Name != input$lad_selected ~ 0.1)) %>%
          mutate('weight_val' = case_when(Name %in% top_ten_cols ~ 2,
                                          !Name %in% top_ten_cols ~ 0.7))
      }
    }
  }
  })
  
  filtered_socioecon_vuln <- reactive({
    
    top_ten_cols <- head(filtered_areas2focus_list(), n=10)
    top_ten_cols <- as.vector(top_ten_cols$`Local Authority`)
    
    # show all or just most vulnerable 
    if(showtop10_or_all$display_wanted == 'show_all') {
      
      if (input$tactical_cell == '-- England --') {
        socioecon_vuln <- lad_uk2vuln_resilience %>%
          mutate('opacity_val' = 0.8) %>% 
          mutate('weight_val' = 0.7)
      }
      
      else{
        if (input$lad_selected == 'All local authorities in region') {
          socioecon_vuln <- lad_uk2vuln_resilience %>% 
            filter(TacticalCell == input$tactical_cell) %>%
            mutate('opacity_val' = 0.8) %>% 
            mutate('weight_val' = 0.7)
        }
        else {
          socioecon_vuln <- lad_uk2vuln_resilience %>%
            filter(TacticalCell == input$tactical_cell) %>%
            mutate('opacity_val'=case_when(Name == input$lad_selected ~ 0.8,
                                           Name != input$lad_selected ~ 0.1)) %>%
            mutate('weight_val'=case_when(Name == input$lad_selected ~ 2,
                                           Name != input$lad_selected ~ 0.7))
          
        }
      }
      
    }
    
    else {
    
    if (input$tactical_cell == '-- England --') {
      socioecon_vuln <- lad_uk2vuln_resilience %>% 
        #filter(`Socioeconomic Vulnerability quintile` >= 4)
        mutate('opacity_val' = case_when(Name %in% top_ten_cols ~ 0.8,
                                         !Name %in% top_ten_cols ~ 0.1)) %>%
        mutate('weight_val' = case_when(Name %in% top_ten_cols ~ 2,
                                        !Name %in% top_ten_cols ~ 0.7))
    }
    
    else{
      if (input$lad_selected == 'All local authorities in region') {
        socioecon_vuln <- lad_uk2vuln_resilience %>% 
          filter(TacticalCell == input$tactical_cell) %>%
          mutate('opacity_val' = case_when(Name %in% top_ten_cols ~ 0.8,
                                           !Name %in% top_ten_cols ~ 0.1)) %>%
          mutate('weight_val' = case_when(Name %in% top_ten_cols ~ 2,
                                          !Name %in% top_ten_cols ~ 0.7))
      }
      else {
        socioecon_vuln <- lad_uk2vuln_resilience %>%
          filter(TacticalCell == input$tactical_cell) %>%
          mutate('opacity_val'=case_when(Name == input$lad_selected ~ 0.8,
                                         Name != input$lad_selected ~ 0.1)) %>%
          mutate('weight_val'=case_when(Name == input$lad_selected ~ 2,
                                        Name != input$lad_selected ~ 0.7))
      }
    }
    }
  })
  
  filtered_socio_vuln <- reactive({
    
    top_ten_cols <- head(filtered_areas2focus_list(), n=10)
    top_ten_cols <- as.vector(top_ten_cols$`Local Authority`)
    
    if(showtop10_or_all$display_wanted == 'show_all') {
      
      if (input$tactical_cell == '-- England --') {
        socio_vuln <- lad_uk2vuln_resilience %>%
          mutate('opacity_val' = 0.8) %>% 
          mutate('weight_val' = 0.7)
      }
      
      else{
        if (input$lad_selected == 'All local authorities in region') {
          socio_vuln <- lad_uk2vuln_resilience %>% 
            filter(TacticalCell == input$tactical_cell) %>%
            mutate('opacity_val' = 0.8) %>% 
            mutate('weight_val' = 0.7)
        }
        else {
          socio_vuln <- lad_uk2vuln_resilience %>%
            filter(TacticalCell == input$tactical_cell) %>%
            mutate('opacity_val'=case_when(Name == input$lad_selected ~ 0.8,
                                           Name != input$lad_selected ~ 0.1)) %>%
            mutate('weight_val'=case_when(Name == input$lad_selected ~ 2,
                                          Name != input$lad_selected ~ 0.7))
        }
      }
      
    }
    
    else {
    
    if (input$tactical_cell == '-- England --') {
      socio_vuln <- lad_uk2vuln_resilience %>% #filter(`Social Vulnerability quintile` >= 4)
        mutate('opacity_val' = case_when(Name %in% top_ten_cols ~ 0.8,
                                         !Name %in% top_ten_cols ~ 0.1)) %>%
        mutate('weight_val' = case_when(Name %in% top_ten_cols ~ 2,
                                        !Name %in% top_ten_cols ~ 0.7))
    }
    
    else{
      if (input$lad_selected == 'All local authorities in region') {
        socio_vuln <- lad_uk2vuln_resilience %>% 
          filter(TacticalCell == input$tactical_cell) %>%
          mutate('opacity_val' = case_when(Name %in% top_ten_cols ~ 0.8,
                                           !Name %in% top_ten_cols ~ 0.1)) %>%
          mutate('weight_val' = case_when(Name %in% top_ten_cols ~ 2,
                                          !Name %in% top_ten_cols ~ 0.7))
      }
      else {
        socio_vuln <- lad_uk2vuln_resilience %>%
          filter(TacticalCell == input$tactical_cell) %>%
          mutate('opacity_val'=case_when(Name == input$lad_selected ~ 0.8,
                                         Name != input$lad_selected ~ 0.1)) %>%
          mutate('weight_val'=case_when(Name == input$lad_selected ~ 2,
                                        Name != input$lad_selected ~ 0.7))
        }
      }
    }
  })
  
  filtered_health_vuln <- reactive({
    
    top_ten_cols <- head(filtered_areas2focus_list(), n=10)
    top_ten_cols <- as.vector(top_ten_cols$`Local Authority`)
    
    if(showtop10_or_all$display_wanted == 'show_all') {
      if (input$tactical_cell == '-- England --') {
        health_vuln <- lad_uk2vuln_resilience %>%
          mutate('opacity_val' = 0.8) %>% 
          mutate('weight_val' = 0.7)
      }
      
      else{
        if (input$lad_selected == 'All local authorities in region') {
          health_vuln <- lad_uk2vuln_resilience %>% 
            filter(TacticalCell == input$tactical_cell) %>%
            mutate('opacity_val' = 0.8) %>% 
            mutate('weight_val' = 0.7)
        }
        else {
          health_vuln <- lad_uk2vuln_resilience %>%
            filter(TacticalCell == input$tactical_cell) %>%
            mutate('opacity_val'=case_when(Name == input$lad_selected ~ 0.8,
                                           Name != input$lad_selected ~ 0.1)) %>%
            mutate('weight_val'=case_when(Name == input$lad_selected ~ 2,
                                          Name != input$lad_selected ~ 0.7))
        }
      }
    }
    
    else {
    
    if (input$tactical_cell == '-- England --') {
      health_vuln <- lad_uk2vuln_resilience %>% 
        mutate('opacity_val' = case_when(Name %in% top_ten_cols ~ 0.8,
                                         !Name %in% top_ten_cols ~ 0.1)) %>%
        mutate('weight_val' = case_when(Name %in% top_ten_cols ~ 2,
                                        !Name %in% top_ten_cols ~ 0.7))
        #filter(`Health/Wellbeing Vulnerability quintile` >= 4)
    }
    
    else{
      if (input$lad_selected == 'All local authorities in region') {
        health_vuln <- lad_uk2vuln_resilience %>% 
          filter(TacticalCell == input$tactical_cell) %>%
          mutate('opacity_val' = case_when(Name %in% top_ten_cols ~ 0.8,
                                           !Name %in% top_ten_cols ~ 0.1)) %>%
          mutate('weight_val' = case_when(Name %in% top_ten_cols ~ 2,
                                          !Name %in% top_ten_cols ~ 0.7))
      }
      else {
        health_vuln <- lad_uk2vuln_resilience %>%
          filter(TacticalCell == input$tactical_cell) %>%
          mutate('opacity_val'=case_when(Name == input$lad_selected ~ 0.8, 
                                         Name != input$lad_selected ~ 0.1)) %>%
          mutate('weight_val'=case_when(Name == input$lad_selected ~ 2,
                                        Name != input$lad_selected ~ 0.7))
        }
      }
    }
  })
  
  filtered_clin_vuln <- reactive({
    
    top_ten_cols <- head(filtered_areas2focus_list(), n=10)
    top_ten_cols <- as.vector(top_ten_cols$`Local Authority`)
    
    
    if(showtop10_or_all$display_wanted == 'show_all') {
      if (input$tactical_cell == '-- England --') {
        clin_vuln <- lad_uk2vuln_resilience %>%
          mutate('opacity_val' = 0.8) %>% 
          mutate('weight_val' = 0.7) 
      }
      
      else{
        if (input$lad_selected == 'All local authorities in region') {
          clin_vuln <- lad_uk2vuln_resilience %>% 
            filter(TacticalCell == input$tactical_cell) %>%
            mutate('opacity_val'= 0.8) %>% 
            mutate('weight_val' = 0.7)
        }
        else {
          clin_vuln <- lad_uk2vuln_resilience %>%
            filter(TacticalCell == input$tactical_cell) %>%
            mutate('opacity_val'=case_when(Name == input$lad_selected ~ 0.8,
                                           Name != input$lad_selected ~ 0.1)) %>%
            mutate('weight_val'=case_when(Name == input$lad_selected ~ 2,
                                          Name != input$lad_selected ~ 0.7))
        }
      }
    }
    
    else {
    
    if (input$tactical_cell == '-- England --') {
      clin_vuln <- lad_uk2vuln_resilience %>% #filter(`Clinical Vulnerability quintile` >= 4)
                  mutate('opacity_val' = case_when(Name %in% top_ten_cols ~ 0.8,
                                         !Name %in% top_ten_cols ~ 0.1)) %>%
        mutate('weight_val' = case_when(Name %in% top_ten_cols ~ 2,
                                        !Name %in% top_ten_cols ~ 0.7))
    }
    
    else{
      if (input$lad_selected == 'All local authorities in region') {
        clin_vuln <- lad_uk2vuln_resilience %>% 
          filter(TacticalCell == input$tactical_cell) %>%
          mutate('opacity_val' = case_when(Name %in% top_ten_cols ~ 0.8,
                                           !Name %in% top_ten_cols ~ 0.1)) %>%
          mutate('weight_val' = case_when(Name %in% top_ten_cols ~ 2,
                                          !Name %in% top_ten_cols ~ 0.7))
      }
      else {
        clin_vuln <- lad_uk2vuln_resilience %>%
          filter(TacticalCell == input$tactical_cell) %>%
          mutate('opacity_val'=case_when(Name == input$lad_selected ~ 0.8,
                                         Name != input$lad_selected ~ 0.1)) %>%
          mutate('weight_val'=case_when(Name == input$lad_selected ~ 2,
                                        Name != input$lad_selected ~ 0.7))
        }
      }
    }
  })
  
  # --- labels ---
  filtered_econ_labels <- reactive({
    
    econ_labels <-
          paste0(
          sprintf("<strong>%s</strong><br/>",  filtered_econ_vuln()$lad19nm),
          "Economic Vulnerability (5 = highest vulnerability): ",  filtered_econ_vuln()$`Economic Vulnerability quintile`, "<br/>",
          "Capacity (5 = lowest capacity): ",  filtered_econ_vuln()$`Capacity quintile`) %>%
          lapply(htmltools::HTML)
    
  })
  
  
  filtered_socioecon_labels <- reactive({
    
    socioecon_labels <-
                paste0(
                   sprintf("<strong>%s</strong><br/>",  filtered_socioecon_vuln()$lad19nm),
                   "Socioeconomic Vulnerability (5 = highest vulnerability): ",  filtered_socioecon_vuln()$`Socioeconomic Vulnerability quintile`, "<br/>",
                   "Capacity (5 = lowest capacity): ",  filtered_socioecon_vuln()$`Capacity quintile`) %>%
                 lapply(htmltools::HTML)
    
  })

  filtered_socio_labels <- reactive({
            socio_labels <-
              paste0(
                sprintf("<strong>%s</strong><br/>",  filtered_socio_vuln()$lad19nm),
                "Social Vulnerability (5 = highest vulnerability): ",  filtered_socio_vuln()$`Social Vulnerability quintile`, "<br/>",
                "Capacity (5 = lowest capacity): ",  filtered_socio_vuln()$`Capacity quintile`) %>%
              lapply(htmltools::HTML)
    
  })
  
  filtered_health_labels <- reactive({
    
    health_labels <- paste0(
                   sprintf("<strong>%s</strong><br/>",  filtered_health_vuln()$lad19nm),
                   "Health/Wellbeing Vulnerability (5 = highest vulnerability): ",  filtered_health_vuln()$`Health/Wellbeing Vulnerability quintile`, "<br/>",
                   "Capacity (5 = lowest capacity): ",  filtered_health_vuln()$`Capacity quintile`) %>%
                 lapply(htmltools::HTML)
    
  })
  
  filtered_clin_labels <- reactive({
    clin_labels <-  paste0(
                sprintf("<strong>%s</strong><br/>",  filtered_clin_vuln()$lad19nm),
                 "Clinical Vulnerability (5 = highest vulnerability): ",  filtered_clin_vuln()$`Clinical Vulnerability quintile`, "<br/>",
                 "Capacity (5 = lowest capacity): ",  filtered_clin_vuln()$`Capacity quintile`) %>%
                 lapply(htmltools::HTML)
  })

  
  # - if you want all - 
  filtered_areas_at_risk_flooding_resilience <- reactive({
    # which tab is selected:
    req(input$sidebar_id)
    if (input$sidebar_id == 'unmetneed') {
      
      if (input$theme == 'Flooding') {
        
        if(showtop10_or_all$display_wanted == 'show_all') {
        
          # show all levels of resilience
          if (input$tactical_cell == '-- England --') {
            fl_incd_lad_uk_most_vuln <- lad_uk2vuln_resilience %>% 
              select('lad19nm', `Vulnerability quintile`, `Capacity quintile`, `Total historical flooding incidents`, 
                     `Flooding incidents per 10,000 people`, `Flood risk quintile`, `Total people in flood risk areas`, `% people in flood risk areas`, `fill`, `floodres_id`, `incd_id`, `risk_id`, 'TacticalCell') %>%
              mutate('opacity_val'=0.8) %>%
              mutate('weight_val'=0.7)
          }
          
          else {
            # Filter to tactical cell
            if (input$lad_selected == 'All local authorities in region') {
              fl_incd_lad_uk_most_vuln <- lad_uk2vuln_resilience %>%
                filter(TacticalCell == input$tactical_cell) %>%
                select('lad19nm', `Vulnerability quintile`, `Capacity quintile`, `Total historical flooding incidents`, 
                       `Flooding incidents per 10,000 people`, `Flood risk quintile`, `Total people in flood risk areas`, `% people in flood risk areas`, `fill`, `floodres_id`, `incd_id`, `risk_id`, 'TacticalCell') %>%
                mutate('opacity_val'=0.8) %>%
                mutate('weight_val'=0.7)
              
            }
            
            else {
              # Filter to local authority
              fl_incd_lad_uk_most_vuln <- lad_uk2vuln_resilience %>%
                filter(TacticalCell == input$tactical_cell) %>%
                mutate('opacity_val'=case_when(Name == input$lad_selected ~ 0.8,
                                               Name != input$lad_selected ~ 0.1)) %>%
                mutate('weight_val'=case_when(Name == input$lad_selected ~ 2,
                                               Name != input$lad_selected ~ 0.7)) %>%
                select('lad19nm', `Vulnerability quintile`, `Capacity quintile`, `Total historical flooding incidents`, 
                       `Flooding incidents per 10,000 people`, `Flood risk quintile`, `Total people in flood risk areas`, `% people in flood risk areas`, `fill`, `floodres_id`, `incd_id`, `risk_id`,'TacticalCell',`opacity_val`,`weight_val`) 
            }
          }
        }
      
      else {
        # show all only most vulnerable areas
        # --- RESILIENCE index ---  
        # vulnerable colours
        # High income, High inequality --> #3F2949
        # High income, Medium inequality --> "#435786"
        # Medium income, medium inequality --> #806A8A
        # high inequality, medium income  --> "#77324C"
        # "#3F2949" -->
        #vuln_cols <- c("#77324C","#3F2949","#435786","#806A8A")
        #vuln_cols <- c("#000000","#b36600","#b3b3b3", "#376387")
        
        top_ten_cols <- head(filtered_areas2focus_list(), n=10)
        top_ten_cols <- as.vector(top_ten_cols$`Local Authority`)
        
        
        if (input$tactical_cell == '-- England --') {
          fl_incd_lad_uk_most_vuln <- lad_uk2vuln_resilience %>%
            select('lad19nm', `Vulnerability quintile`, `Capacity quintile`, `Total historical flooding incidents`, 
                   `Flooding incidents per 10,000 people`, `Flood risk quintile`, `Total people in flood risk areas`, `% people in flood risk areas`, `fill`, `floodres_id`, `incd_id`, `risk_id`, 'TacticalCell') %>%
            mutate('opacity_val'=case_when(lad19nm %in% top_ten_cols ~ 0.8,
                                           !lad19nm %in% top_ten_cols ~ 0.1)) %>%
            mutate('weight_val'=case_when(lad19nm %in% top_ten_cols ~ 2,
                                           !lad19nm %in% top_ten_cols ~ 0.7)) 
        }
        
        else {
          # Filter to tactical cell
          if (input$lad_selected == 'All local authorities in region') {
            fl_incd_lad_uk_most_vuln <- lad_uk2vuln_resilience %>%
              filter(TacticalCell == input$tactical_cell) %>%
              mutate('opacity_val'=case_when(lad19nm %in% top_ten_cols ~ 0.8,
                                             !lad19nm %in% top_ten_cols ~ 0.1)) %>%
              mutate('weight_val'=case_when(lad19nm %in% top_ten_cols ~ 2,
                                            !lad19nm %in% top_ten_cols ~ 0.7)) %>%
              select('lad19nm', `Vulnerability quintile`, `Capacity quintile`, `Total historical flooding incidents`, 
                     `Flooding incidents per 10,000 people`, `Flood risk quintile`, `Total people in flood risk areas`, `% people in flood risk areas`, `fill`, `floodres_id`, `incd_id`, `risk_id`, 'TacticalCell', `opacity_val`, `weight_val`)
            
          }
          
          else {
            # Filter to local authority
            fl_incd_lad_uk_most_vuln <- lad_uk2vuln_resilience %>%
              filter(TacticalCell == input$tactical_cell) %>%
              mutate('opacity_val'=case_when(Name == input$lad_selected ~ 0.8,
                                             Name != input$lad_selected ~ 0.1)) %>%
              mutate('weight_val'=case_when(Name == input$lad_selected ~ 2,
                                             Name != input$lad_selected ~ 0.7)) %>%
              select('lad19nm', `Vulnerability quintile`, `Capacity quintile`, `Total historical flooding incidents`, 
                     `Flooding incidents per 10,000 people`, `Flood risk quintile`, `Total people in flood risk areas`, `% people in flood risk areas`, `fill`, `floodres_id`, `incd_id`, `risk_id`, 'TacticalCell', `opacity_val`, `weight_val`)
          }
        }
        
        }
      }
    }
})
  
  # --- flood warning map points --- 
  filteredFlood_warnings_points <- reactive({
    
    req(input$sidebar_id)
    if (input$sidebar_id == 'unmetneed') {
    
    if (input$theme == 'Flooding') {
      if (input$tactical_cell == '-- England --') {
        
        flood_warnigns2show <- flood_warning_points %>% 
          select('floodAreaID','description','severity','severityLevel', 'alertlevelmeaning','lastupdatetime','lastupdateday','messageurl') %>%
          # case colour polgon based on severity
          mutate('warning_col'=case_when(severityLevel==3 ~ 'orange',
                                         severityLevel == 2 ~ 'red',
                                         severityLevel == 1 ~ 'red')) %>%
          # without this points are repeated because flood polgons overlap multiple LADS
          unique()
      }
      else {
        if (input$lad_selected == 'All local authorities in region') {
          flood_warnigns2show <- flood_warning_points  %>% 
            filter(TacticalCell == input$tactical_cell) %>% 
            select('floodAreaID','description','severity','severityLevel', 'alertlevelmeaning','lastupdatetime','lastupdateday','messageurl') %>%
            # case colour polgon based on severity
            mutate('warning_col'=case_when(severityLevel==3 ~ 'orange',
                                           severityLevel == 2 ~ 'red',
                                           severityLevel == 1 ~ 'red')) %>%
            # without this points are repeated because flood polygons overlap multiple LADS
            unique()
          
        }
        else {
          flood_warnings2show <- flood_warning_points %>%
            filter(lad19nm == input$lad_selected) %>% 
            select('lad19nm','description','severity','severityLevel', 'alertlevelmeaning','lastupdatetime','lastupdateday','messageurl') %>%
            # case colour polgon based on severity
            mutate('warning_col'=case_when(severityLevel==3 ~ 'orange',
                                           severityLevel == 2 ~ 'red',
                                           severityLevel == 1 ~ 'red'))
          }
        }
      }
    }
  })
  
  # --- flood warning map polygons --- 
  filteredFlood_warnings_polygons <- reactive({
    
    req(input$sidebar_id)
    if (input$sidebar_id == 'unmetneed') {
      
      if (input$theme == 'Flooding') {
        if (input$tactical_cell == '-- England --') {
          
          flood_warnigns2show <- flood_warning_polygons %>% 
            select('floodAreaID','description','severity','severityLevel', 'alertlevelmeaning','lastupdatetime','lastupdateday','messageurl','geometry') %>%
            # case colour polgon based on severity
            mutate('warning_col'=case_when(severityLevel==3 ~ 'orange',
                                           severityLevel == 2 ~ 'red',
                                           severityLevel == 1 ~ 'red')) %>%
            # without this points are repeated because flood polgons overlap multiple LADS
            unique()
        }
        else {
          if (input$lad_selected == 'All local authorities in region') {
            flood_warnigns2show <- flood_warning_polygons  %>% 
              filter(TacticalCell == input$tactical_cell) %>% 
              select('floodAreaID','description','severity','severityLevel', 'alertlevelmeaning','lastupdatetime','lastupdateday','messageurl', 'geometry') %>%
              # case colour polgon based on severity
              mutate('warning_col'=case_when(severityLevel==3 ~ 'orange',
                                             severityLevel == 2 ~ 'red',
                                             severityLevel == 1 ~ 'red')) %>%
              # without this points are repeated because flood polygons overlap multiple LADS
              unique()
            
          }
          else {
            flood_warnings2show <- flood_warning_polygons %>%
              filter(lad19nm == input$lad_selected) %>% 
              select('lad19nm','description','severity','severityLevel', 'alertlevelmeaning','lastupdatetime','lastupdateday','messageurl', 'geometry') %>%
              # case colour polgon based on severity
              mutate('warning_col'=case_when(severityLevel==3 ~ 'orange',
                                             severityLevel == 2 ~ 'red',
                                             severityLevel == 1 ~ 'red'))
          }
        }
      }
    }
  })
  
  
  filtered_flood_resilience_labels <- reactive({
    
    if (input$tactical_cell == '-- England --') {
    fl_resilience_lad_uk_most_vuln_for_labels <- filtered_areas_at_risk_flooding_resilience() %>%
      select('lad19nm', `Vulnerability quintile`, `Capacity quintile`, `Total people in flood risk areas`, 
             `% people in flood risk areas`, `Flood risk quintile`, `Total historical flooding incidents`, 
             `Flooding incidents per 10,000 people`) %>%
      st_drop_geometry() %>%
      mutate_all(list(~na_if(.,""))) %>%
      mutate(`% people in flood risk areas` = round(`% people in flood risk areas`, 2)) %>%
      mutate(`% people in flood risk areas` = case_when(`% people in flood risk areas` == 0.00 ~ '< 0.01',
                                                        TRUE ~ (as.character(.$`% people in flood risk areas`)))) %>%
      mutate(`Flooding incidents per 10,000 people` = round(`Flooding incidents per 10,000 people`,2))
    
    }
    
    else {
      
      if (input$tactical_cell != '-- England --' & input$lad_selected=='All local authorities in region') {
        fl_resilience_lad_uk_most_vuln_for_labels <- filtered_areas_at_risk_flooding_resilience() %>%
          filter(TacticalCell == input$tactical_cell) %>%
          select('lad19nm', `Vulnerability quintile`, `Capacity quintile`, `Total people in flood risk areas`, 
                 `% people in flood risk areas`, `Flood risk quintile`, `Total historical flooding incidents`, 
                 `Flooding incidents per 10,000 people`) %>%
          st_drop_geometry() %>%
          mutate_all(list(~na_if(.,""))) %>%
          mutate(`% people in flood risk areas` = round(`% people in flood risk areas`, 2)) %>%
          mutate(`% people in flood risk areas` = case_when(`% people in flood risk areas` == 0.00 ~ '< 0.01',
                                                            TRUE ~ (as.character(.$`% people in flood risk areas`)))) %>%
          mutate(`Flooding incidents per 10,000 people` = round(`Flooding incidents per 10,000 people`,2))
        
        
        
      }
      
      else {
        fl_resilience_lad_uk_most_vuln_for_labels <- filtered_areas_at_risk_flooding_resilience() %>%
          #filter(lad19nm == input$lad_selected) %>%
          select('lad19nm', `Vulnerability quintile`, `Capacity quintile`, `Total people in flood risk areas`, 
                 `% people in flood risk areas`, `Flood risk quintile`, `Total historical flooding incidents`, 
                 `Flooding incidents per 10,000 people`) %>%
          st_drop_geometry() %>%
          mutate_all(list(~na_if(.,""))) %>%
          mutate(`% people in flood risk areas` = round(`% people in flood risk areas`, 2)) %>%
          mutate(`% people in flood risk areas` = case_when(`% people in flood risk areas` == 0.00 ~ '< 0.01',
                                                            TRUE ~ (as.character(.$`% people in flood risk areas`)))) %>%
          mutate(`Flooding incidents per 10,000 people` = round(`Flooding incidents per 10,000 people`,2))
        
        
      }
    }
    
    fl_risk_labels <- paste0(
      sprintf("<strong>%s</strong><br/>",  fl_resilience_lad_uk_most_vuln_for_labels$lad19nm),
      "Vulnerability (5 = highest vulnerability): ",  fl_resilience_lad_uk_most_vuln_for_labels$`Vulnerability quintile`, "<br/>",
      "Capacity (5 = lowest capacity): ",  fl_resilience_lad_uk_most_vuln_for_labels$`Capacity quintile`, "<br/>",
      "Flood Risk (5 = most risk): ", fl_resilience_lad_uk_most_vuln_for_labels$`Flood risk quintile`, "<br/>",
      "Total people in flood risk areas: ", fl_resilience_lad_uk_most_vuln_for_labels$`Total people in flood risk areas`, "<br/>",
      "% people in flood risk areas: ", fl_resilience_lad_uk_most_vuln_for_labels$`% people in flood risk areas`, "<br/>",
      "Number of historical flooding incidents: ", fl_resilience_lad_uk_most_vuln_for_labels$`Total historical flooding incidents`, "<br/>",
      "Flooding incidents per 10,000 people: ", fl_resilience_lad_uk_most_vuln_for_labels$`Flooding incidents per 10,000 people`
    ) %>%
      lapply(htmltools::HTML)
    
  })
  
  filtered_flood_warning_labels <- reactive({
    
    flood_warning_labels <- paste0(
                    sprintf("<strong>%s</strong><br/>",  filteredFlood_warnings_points()$description),
                    filteredFlood_warnings_points()$severity, ": ", filteredFlood_warnings_points()$alertlevelmeaning, "<br/>",
                    "last updated (at time dashboard refreshed): ",  filteredFlood_warnings_points()$lastupdateday, " ", filteredFlood_warnings_points()$lastupdatetime) %>%
                   lapply(htmltools::HTML)
    
  })
  
  


  filterpar_tab <- reactive({
    for_tc <- par_table %>% filter(TacticalCell == input$tactical_cell)

  })
  


  # --- Areas to focus ----
  filtered_areas2focus <- reactive({
    
    #volunteers_available <- volunteers
    
    # -- covid ---
    if(input$theme == 'Covid-19') {
      
        covid_lads_in_tc <- covid_area2focus %>% arrange(-`covid cases per 100,000`) %>%
          select('LAD19CD', 'Local Authority'= Name, 'Region'='TacticalCell', `covid cases per 100,000`, `Total cases`, `% change in covid cases`)
      
        # no volunteer data
        covid_cases2volunteers <- covid_lads_in_tc %>% arrange(-`covid cases per 100,000`) %>%
          select(-'LAD19CD') # %>% 
          #rename_at(vars(`covid cases per 100,000`), ~ paste0(covid_week, .))
      
    }
    
    else{
      # Flooding 
      
      if(input$theme == 'Flooding') {
        
        # arrange so areas to focus table follows list
        if (store_rank_wanted$rank_wanted_flooding == 'Historical flood incidents per 10,000') {
        
          flooding_lads_in_tc <- flooding_area2focus %>% arrange(-`Total live severe Flood warning`, -`Total live Flood warning`,-`Total live Flood alert`, -`Flooding incidents per 10,000 people`) %>%
            mutate(`Flooding incidents per 10,000 people`=round(`Flooding incidents per 10,000 people`,2)) %>%
            mutate(`% people in flood risk areas`=round(`% people in flood risk areas`,2)) %>%
            rename('Local Authority'=LAD19NM, 'Region'=TacticalCell) %>% 
            select(-`Vulnerability quintile`, -`Flood risk quintile`, -`Flood incidents quintile`)
          
          
          flooding_cases2volunteers <- flooding_lads_in_tc %>% 
              arrange( -`Flooding incidents per 10,000 people`) %>%
              select(-`LAD19CD`) %>% 
              mutate(`% people in flood risk areas` = case_when(`% people in flood risk areas` == 0.00 ~ '< 0.01',
                                                              TRUE ~ (as.character(.$`% people in flood risk areas`))))
          
          
        }
        
        else {
          # order based on flood warnings
          if(store_rank_wanted$rank_wanted_flooding == 'Flood warnings/alerts') {
            
            
            flooding_lads_in_tc <- flooding_area2focus %>% arrange(-`Total live severe Flood warning`, -`Total live Flood warning`,-`Total live Flood alert`, -`Flooding incidents per 10,000 people`) %>%
              mutate(`Flooding incidents per 10,000 people`=round(`Flooding incidents per 10,000 people`,2)) %>%
              mutate(`% people in flood risk areas`=round(`% people in flood risk areas`,2)) %>%
              rename('Local Authority'=LAD19NM, 'Region'=TacticalCell) %>% 
              select(-`Vulnerability quintile`, -`Flood risk quintile`, -`Flood incidents quintile`)
            
            flooding_cases2volunteers <- flooding_lads_in_tc %>% 
              arrange(-`Total live Flood warning`,-`Total live Flood alert`) %>%
              select(-`LAD19CD`) %>% 
              mutate(`% people in flood risk areas` = case_when(`% people in flood risk areas` == 0.00 ~ '< 0.01',
                                                                TRUE ~ (as.character(.$`% people in flood risk areas`))))
            
        
            
          }
          
          else {
            if (store_rank_wanted$rank_wanted_flooding == '% of population living in flood risk areas') {
              
              flooding_lads_in_tc <- flooding_area2focus %>% arrange(-`% people in flood risk areas`) %>%
                mutate(`Flooding incidents per 10,000 people`=round(`Flooding incidents per 10,000 people`,2)) %>%
                mutate(`% people in flood risk areas`=round(`% people in flood risk areas`,2)) %>%
                rename('Local Authority'=LAD19NM, 'Region'=TacticalCell) %>% 
                select(-`Vulnerability quintile`, -`Flood risk quintile`, -`Flood incidents quintile`)
              
              flooding_cases2volunteers <- flooding_lads_in_tc %>% 
                arrange(-`% people in flood risk areas`) %>%
                select(-`LAD19CD`) %>% 
                mutate(`% people in flood risk areas` = case_when(`% people in flood risk areas` == 0.00 ~ '< 0.01',
                                                                  TRUE ~ (as.character(.$`% people in flood risk areas`))))
              
            }
          
          }
        }
      }
      # add new theme here 
    }

  })
  
  
  
  # --- Areas to focus list ----
  filtered_areas2focus_list <- reactive({
    #volunteers_available <- volunteers
    
    # -- covid ---
    if(input$theme == 'Covid-19') {
      
      # what list do they want:
      if (store_rank_wanted$rank_wanted_covid == 'cases per 100,000  ') {
      
        if (input$tactical_cell == '-- England --') {
      
        covid_lads_in_tc <- covid_area2focus %>% arrange(-`covid cases per 100,000`) %>%
          select('LAD19CD', 'Local Authority'= Name, 'Region'='TacticalCell', `covid cases per 100,000`, `Total cases`, `% change in covid cases`)
      
      covid_cases4list <- covid_lads_in_tc %>% arrange(-`covid cases per 100,000`) %>%
        select(-'LAD19CD') #%>% 
        #rename_at(vars(`covid cases per 100,000`), ~ paste0(covid_week, .)) 
    }
      else {
      if (input$tactical_cell != '-- England --' & input$lad_selected == 'All local authorities in region') {
        
        covid_lads_in_tc <- covid_area2focus %>% filter(TacticalCell == input$tactical_cell) %>%
          arrange(-`covid cases per 100,000`) %>%
          select('LAD19CD', 'Local Authority'= Name, 'Region'='TacticalCell', `covid cases per 100,000`, `Total cases`, `% change in covid cases`)
        
        covid_cases4list <- covid_lads_in_tc %>% arrange(-`covid cases per 100,000`) %>%
          select(-'LAD19CD') #%>% 
          #rename_at(vars(`covid cases per 100,000`), ~ paste0(covid_week, .)) 
      }
        
      else {
        covid_lads_in_tc <- covid_area2focus %>% filter(Name == input$lad_selected) %>%
          arrange(-`covid cases per 100,000`) %>%
          select('LAD19CD', 'Local Authority'= Name, 'Region'='TacticalCell', `covid cases per 100,000`, `Total cases`, `% change in covid cases`)
        
        covid_cases4list <- covid_lads_in_tc %>% arrange(-`covid cases per 100,000`) %>%
          select(-'LAD19CD') #%>% 
          #rename_at(vars(`covid cases per 100,000`), ~ paste0(covid_week, .)) 
        
      }
        
      }
      }
      
      else {
        # rank on % change
        if (input$tactical_cell == '-- England --') {
          
          covid_lads_in_tc <- covid_area2focus %>% arrange(-`% change in covid cases`) %>%
            select('LAD19CD', 'Local Authority'= Name, 'Region'='TacticalCell', `covid cases per 100,000`, `Total cases`, `% change in covid cases`)
          
          covid_cases4list <- covid_lads_in_tc %>% arrange(-`% change in covid cases`) %>%
            select(-'LAD19CD') # %>% 
            #rename_at(vars(`covid cases per 100,000`), ~ paste0(covid_week, .)) 
        }
        else {
          if (input$tactical_cell != '-- England --' & input$lad_selected == 'All local authorities in region') {
            
            covid_lads_in_tc <- covid_area2focus %>% filter(TacticalCell == input$tactical_cell) %>%
              arrange(-`% change in covid cases`) %>%
              select('LAD19CD', 'Local Authority'= Name, 'Region'='TacticalCell', `covid cases per 100,000`, `Total cases`,`% change in covid cases`)
            
            covid_cases4list <- covid_lads_in_tc %>% arrange(-`% change in covid cases`) %>%
              select(-'LAD19CD') #%>% 
              #rename_at(vars(`covid cases per 100,000`), ~ paste0(covid_week, .)) 
          }
          
          else {
            covid_lads_in_tc <- covid_area2focus %>% filter(Name == input$lad_selected) %>%
              arrange(-`% change in covid cases`) %>%
              select('LAD19CD', 'Local Authority'= Name, 'Region'='TacticalCell', `covid cases per 100,000`, `Total cases`, `% change in covid cases`)
            
            covid_cases4list <- covid_lads_in_tc %>% arrange(-`% change in covid cases`) %>%
              select(-'LAD19CD') #%>% 
              #rename_at(vars(`covid cases per 100,000`), ~ paste0(covid_week, .)) 
            
          }
          
        }
      }
      
    } # end of covid theme
    
    else {
      # Flooding 
      
      if(input$theme == 'Flooding') {
        
        # arrange for areas to focus list 
        if (store_rank_wanted$rank_wanted_flooding == 'Historical flood incidents per 10,000') {
          
          if(input$tactical_cell == '-- England --') {
          
          flooding_lads_in_tc <- flooding_area2focus %>% arrange(-`Flooding incidents per 10,000 people`) %>%
            mutate(`Flooding incidents per 10,000 people`=round(`Flooding incidents per 10,000 people`,2)) %>%
            mutate(`% people in flood risk areas`=round(`% people in flood risk areas`,2)) %>%
            rename('Local Authority'=LAD19NM, 'Region'=TacticalCell) %>% 
            select(-`Vulnerability quintile`, -`Flood risk quintile`, -`Flood incidents quintile`)
          
          flooding_cases2volunteers <- flooding_lads_in_tc %>% 
            arrange( -`Flooding incidents per 10,000 people`) %>%
            select(-`LAD19CD`) %>% 
            mutate(`% people in flood risk areas` = case_when(`% people in flood risk areas` == 0.00 ~ '< 0.01',
                                                              TRUE ~ (as.character(.$`% people in flood risk areas`))))
          
          }
          
          else {
            if (input$tactical_cell != '-- England --' & input$lad_selected == 'All local authorities in region') {
              
              flooding_lads_in_tc <- flooding_area2focus %>% filter(TacticalCell == input$tactical_cell) %>%
                arrange(-`Flooding incidents per 10,000 people`) %>%
                mutate(`Flooding incidents per 10,000 people`=round(`Flooding incidents per 10,000 people`,2)) %>%
                mutate(`% people in flood risk areas`=round(`% people in flood risk areas`,2)) %>%
                rename('Local Authority'=LAD19NM, 'Region'=TacticalCell) %>% 
                select(-`Vulnerability quintile`, -`Flood risk quintile`, -`Flood incidents quintile`)
              
              flooding_cases2volunteers <- flooding_lads_in_tc %>% 
                arrange( -`Flooding incidents per 10,000 people`) %>%
                select(-`LAD19CD`) %>% 
                mutate(`% people in flood risk areas` = case_when(`% people in flood risk areas` == 0.00 ~ '< 0.01',
                                                                  TRUE ~ (as.character(.$`% people in flood risk areas`))))
              
            }
            
            else {
              
              flooding_lads_in_tc <- flooding_area2focus %>% filter(LAD19NM == input$lad_selected) %>%
                arrange(-`Flooding incidents per 10,000 people`) %>%
                mutate(`Flooding incidents per 10,000 people`=round(`Flooding incidents per 10,000 people`,2)) %>%
                mutate(`% people in flood risk areas`=round(`% people in flood risk areas`,2)) %>%
                rename('Local Authority'=LAD19NM, 'Region'=TacticalCell) %>% 
                select(-`Vulnerability quintile`, -`Flood risk quintile`, -`Flood incidents quintile`)
              
              flooding_cases2volunteers <- flooding_lads_in_tc %>% 
                arrange( -`Flooding incidents per 10,000 people`) %>%
                select(-`LAD19CD`) %>% 
                mutate(`% people in flood risk areas` = case_when(`% people in flood risk areas` == 0.00 ~ '< 0.01',
                                                                  TRUE ~ (as.character(.$`% people in flood risk areas`))))
              
            }
            
          }
        }
        
        else {
          # order based on flood warnings
          if(store_rank_wanted$rank_wanted_flooding == 'Flood warnings/alerts') {
            
            
          if(input$tactical_cell == '-- England --') {
            
            flooding_lads_in_tc <- flooding_area2focus %>% arrange(-`Total live Flood warning`,-`Total live Flood alert`, -`Flooding incidents per 10,000 people`) %>%
              mutate(`Flooding incidents per 10,000 people`=round(`Flooding incidents per 10,000 people`,2)) %>%
              mutate(`% people in flood risk areas`=round(`% people in flood risk areas`,2)) %>%
              rename('Local Authority'=LAD19NM, 'Region'=TacticalCell) %>% 
              select(-`Vulnerability quintile`, -`Flood risk quintile`, -`Flood incidents quintile`)
            
            flooding_cases2volunteers <- flooding_lads_in_tc %>% 
              arrange(-`Total live Flood warning`,-`Total live Flood alert`) %>%
              select(-`LAD19CD`) %>% 
              mutate(`% people in flood risk areas` = case_when(`% people in flood risk areas` == 0.00 ~ '< 0.01',
                                                                TRUE ~ (as.character(.$`% people in flood risk areas`))))
            
            }
            
            else {
              if (input$tactical_cell != '-- England --' & input$lad_selected == 'All local authorities in region') {
                
                flooding_lads_in_tc <- flooding_area2focus %>% filter(TacticalCell == input$tactical_cell) %>%
                  arrange(-`Total live Flood warning`,-`Total live Flood alert`, -`Flooding incidents per 10,000 people`) %>%
                  mutate(`Flooding incidents per 10,000 people`=round(`Flooding incidents per 10,000 people`,2)) %>%
                  mutate(`% people in flood risk areas`=round(`% people in flood risk areas`,2)) %>%
                  rename('Local Authority'=LAD19NM, 'Region'=TacticalCell) %>% 
                  select(-`Vulnerability quintile`, -`Flood risk quintile`, -`Flood incidents quintile`)
                
                flooding_cases2volunteers <- flooding_lads_in_tc %>% 
                  arrange(-`Total live Flood warning`,-`Total live Flood alert`) %>%
                  select(-`LAD19CD`) %>% 
                  mutate(`% people in flood risk areas` = case_when(`% people in flood risk areas` == 0.00 ~ '< 0.01',
                                                                    TRUE ~ (as.character(.$`% people in flood risk areas`))))
                
              
              }
              
              else {
                flooding_lads_in_tc <- flooding_area2focus %>% filter(LAD19NM == input$lad_selected) %>%
                  arrange(-`Total live Flood warning`,-`Total live Flood alert`, -`Flooding incidents per 10,000 people`) %>%
                  mutate(`Flooding incidents per 10,000 people`=round(`Flooding incidents per 10,000 people`,2)) %>%
                  mutate(`% people in flood risk areas`=round(`% people in flood risk areas`,2)) %>%
                  rename('Local Authority'=LAD19NM, 'Region'=TacticalCell) %>% 
                  select(-`Vulnerability quintile`, -`Flood risk quintile`, -`Flood incidents quintile`)
                
                flooding_cases2volunteers <- flooding_lads_in_tc %>% 
                  arrange(-`Total live Flood warning`,-`Total live Flood alert`) %>%
                  select(-`LAD19CD`) %>% 
                  mutate(`% people in flood risk areas` = case_when(`% people in flood risk areas` == 0.00 ~ '< 0.01',
                                                                    TRUE ~ (as.character(.$`% people in flood risk areas`))))
                
              }
            }
            }#
  
          
          else {
            if (store_rank_wanted$rank_wanted_flooding == '% of population living in flood risk areas') {
              
              if(input$tactical_cell == '-- England --') {
              
              flooding_lads_in_tc <- flooding_area2focus %>% arrange(-`% people in flood risk areas`) %>%
                mutate(`Flooding incidents per 10,000 people`=round(`Flooding incidents per 10,000 people`,2)) %>%
                mutate(`% people in flood risk areas`=round(`% people in flood risk areas`,2)) %>%
                rename('Local Authority'=LAD19NM, 'Region'=TacticalCell) %>% 
                select(-`Vulnerability quintile`, -`Flood risk quintile`, -`Flood incidents quintile`)
              
              flooding_cases2volunteers <- flooding_lads_in_tc %>% 
                arrange(-`% people in flood risk areas`) %>%
                select(-`LAD19CD`) %>% 
                mutate(`% people in flood risk areas` = case_when(`% people in flood risk areas` == 0.00 ~ '< 0.01',
                                                                  TRUE ~ (as.character(.$`% people in flood risk areas`))))
              }
              
              else {
                if (input$tactical_cell != '-- England --' & input$lad_selected == 'All local authorities in region') {
                  
                  flooding_lads_in_tc <- flooding_area2focus %>% filter(TacticalCell == input$tactical_cell) %>%
                    arrange(-`% people in flood risk areas`) %>%
                    mutate(`Flooding incidents per 10,000 people`=round(`Flooding incidents per 10,000 people`,2)) %>%
                    mutate(`% people in flood risk areas`=round(`% people in flood risk areas`,2)) %>%
                    rename('Local Authority'=LAD19NM, 'Region'=TacticalCell) %>% 
                    select(-`Vulnerability quintile`, -`Flood risk quintile`, -`Flood incidents quintile`)
                  
                  flooding_cases2volunteers <- flooding_lads_in_tc %>% 
                    arrange(-`% people in flood risk areas`) %>%
                    select(-`LAD19CD`) %>% 
                    mutate(`% people in flood risk areas` = case_when(`% people in flood risk areas` == 0.00 ~ '< 0.01',
                                                                      TRUE ~ (as.character(.$`% people in flood risk areas`))))
                  
                }
                
                else {
                  
                  flooding_lads_in_tc <- flooding_area2focus %>% filter(LAD19NM == input$lad_selected) %>%
                    arrange(-`% people in flood risk areas`) %>%
                    mutate(`Flooding incidents per 10,000 people`=round(`Flooding incidents per 10,000 people`,2)) %>%
                    mutate(`% people in flood risk areas`=round(`% people in flood risk areas`,2)) %>%
                    rename('Local Authority'=LAD19NM, 'Region'=TacticalCell) %>% 
                    select(-`Vulnerability quintile`, -`Flood risk quintile`, -`Flood incidents quintile`)
                  
                  flooding_cases2volunteers <- flooding_lads_in_tc %>% 
                    arrange(-`% people in flood risk areas`) %>%
                    select(-`LAD19CD`) %>% 
                    mutate(`% people in flood risk areas` = case_when(`% people in flood risk areas` == 0.00 ~ '< 0.01',
                                                                      TRUE ~ (as.character(.$`% people in flood risk areas`))))
                  
                }
                
              }
            }
          }
          
        }
      } # add new theme here 
      
      } 

    
  })
  

  # --- Requests ----
  filtered_requests <- reactive({
    requests_tc <- requests %>% filter(TacticalCell==input$tactical_cell)

  })


  # --- Generate Map ----
  observe({
    
    # which tab is selected:
    req(input$sidebar_id)
    if (input$sidebar_id == 'unmetneed') {
      
      # --- which theme ---
      if (input$theme == 'Covid-19') {
        lad_uk_most_vuln <- filtered_areas_at_risk_covid()
        econ_vuln <- filtered_econ_vuln()
        socioecon_vuln <- filtered_socioecon_vuln()
        socio_vuln <- filtered_socio_vuln()
        clin_vuln <- filtered_clin_vuln()
        health_vuln <- filtered_health_vuln()
        
        
        if(input$tactical_cell == '-- England --') {
          # -- zoom for uk ---
          curr_bbox <- st_bbox(filtered_tc())
          
          leafletProxy("map") %>%
            clearShapes() %>%
            clearMarkerClusters() %>%
            clearMarkers() %>%
            #
            # addPolygons(data=tc_shp, layerId = ~TacticalCell,
            #             group='tactical cell boundary',
            #             stroke=T,
            #             weight = 3,
            #             opacity = 0.8,
            #             color = "grey",
            #             dashArray = "0.1",
            #             fill=F) %>%
            addPolygons(data=lad_uk_most_vuln, layerId = ~res_id,
                        group="Resilience: vulnerability vs capacity to cope", fillColor = ~fill,
                        weight = ~weight_val,
                        opacity = 0.8,
                        color = "black",
                        dashArray = "0.1",
                        fillOpacity = ~opacity_val,
                        highlight = highlightOptions(
                          weight = 5,
                          color = "#666",
                          dashArray = "",
                          fillOpacity = 0.7,
                          bringToFront = TRUE#,
                        ),
                        label= filtered_labels_covid(),
                        labelOptions = labelOptions(
                          style = list("font-weight" = "normal", padding = "3px 8px"),
                          textsize = "10px",
                          direction = "auto"
                        )
            ) %>%
             # economic vulnerability layer
            addPolygons(data=econ_vuln, layerId = ~econ_id,
                                group="Economic vulnerability", fillColor = ~pal(`Economic Vulnerability quintile`),
                                weight = ~weight_val,
                                opacity = 0.8,
                                color = "black",
                                dashArray = "0.1",
                                fillOpacity = ~opacity_val,
                                highlight = highlightOptions(
                                  weight = 5,
                                  color = "#666",
                                  dashArray = "",
                                  fillOpacity = 0.7,
                                  bringToFront = TRUE,
                                ),
                                label= filtered_econ_labels(),

                                labelOptions = labelOptions(
                                  style = list("font-weight" = "normal", padding = "3px 8px"),
                                  textsize = "10px",
                                  direction = "auto"
                                )
                    ) %>%
                    # socioeconomic vulnerability layer
                    addPolygons(data=socioecon_vuln, layerId = ~socecon_id,
                                group="Socioeconomic vulnerability", fillColor = ~pal(`Socioeconomic Vulnerability quintile` ),
                                weight = ~weight_val,
                                opacity = 0.8,
                                color = "black",
                                dashArray = "0.1",
                                fillOpacity = ~opacity_val,
                                highlight = highlightOptions(
                                  weight = 5,
                                  color = "#666",
                                  dashArray = "",
                                  fillOpacity = 0.7,
                                  bringToFront = TRUE,
                                ),
                                label= filtered_socioecon_labels(),

                                labelOptions = labelOptions(
                                  style = list("font-weight" = "normal", padding = "3px 8px"),
                                  textsize = "10px",
                                  direction = "auto"
                                )
                    ) %>%
                    # social vulnerability layer
                    addPolygons(data=socio_vuln, layerId = ~soc_id,
                                group="Social vulnerability", fillColor = ~pal(`Social Vulnerability quintile` ),
                                weight = ~weight_val,
                                opacity = 0.8,
                                color = "black",
                                dashArray = "0.1",
                                fillOpacity = ~opacity_val,
                                highlight = highlightOptions(
                                  weight = 5,
                                  color = "#666",
                                  dashArray = "",
                                  fillOpacity = 0.7,
                                  bringToFront = TRUE,
                                ),
                                label= filtered_socio_labels(),

                                labelOptions = labelOptions(
                                  style = list("font-weight" = "normal", padding = "3px 8px"),
                                  textsize = "10px",
                                  direction = "auto"
                                )
                    ) %>%
                    # Health/wellbeing vulnerability layer
                    addPolygons(data=health_vuln, layerId = ~health_id,
                                group="Health/Wellbeing vulnerability", fillColor = ~pal(`Health/Wellbeing Vulnerability quintile` ),
                                weight = ~weight_val,
                                opacity = 0.8,
                                color = "black",
                                dashArray = "0.1",
                                fillOpacity = ~opacity_val,
                                highlight = highlightOptions(
                                  weight = 5,
                                  color = "#666",
                                  dashArray = "",
                                  fillOpacity = 0.7,
                                  bringToFront = TRUE,
                                ),
                                label= filtered_health_labels(),

                                labelOptions = labelOptions(
                                  style = list("font-weight" = "normal", padding = "3px 8px"),
                                  textsize = "10px",
                                  direction = "auto"
                                )
                    ) %>%
                    # clin vulnerability layer
                    addPolygons(data=clin_vuln, layerId = ~clin_id,
                                group="Clinical vulnerability", fillColor = ~pal(`Clinical Vulnerability quintile` ),
                                weight = ~weight_val,
                                opacity = 0.8,
                                color = "black",
                                dashArray = "0.1",
                                fillOpacity = ~opacity_val,
                                highlight = highlightOptions(
                                  weight = 5,
                                  color = "#666",
                                  dashArray = "",
                                  fillOpacity = 0.7,
                                  bringToFront = TRUE,
                                ),
                                label= filtered_clin_labels(),

                                labelOptions = labelOptions(
                                  style = list("font-weight" = "normal", padding = "3px 8px"),
                                  textsize = "10px",
                                  direction = "auto"
                                )
                    ) %>%
            addPolygons(data=tc_shp, layerId = ~TacticalCell,
                        group='tactical cell boundary',
                        stroke=T,
                        weight = 2,
                        opacity = 0.8,
                        color = "black",
                        dashArray = "3",
                        fill=F) %>%
                    flyToBounds(lng1 = as.numeric(curr_bbox["xmin"]),
                                lat1 = as.numeric(curr_bbox["ymin"]),
                                lng2 = as.numeric(curr_bbox["xmax"]),
                                lat2 = as.numeric(curr_bbox["ymax"])) %>%
                    addLayersControl(baseGroups = c("Resilience: vulnerability vs capacity to cope","Economic vulnerability","Socioeconomic vulnerability","Social vulnerability","Health/Wellbeing vulnerability","Clinical vulnerability"),
                                     options= layersControlOptions(collapsed=T))

                        }
        
        else {
          # tactical cell level
          if (input$tactical_cell != '-- England --' & input$lad_selected == 'All local authorities in region') {
            curr_bbox <- st_bbox(filtered_tc())
            
            leafletProxy("map") %>%
              clearShapes() %>%
              clearMarkerClusters() %>%
              clearMarkers() %>%
              
              addPolygons(data=lad_uk_most_vuln, layerId = ~res_id,
                          group="Resilience: vulnerability vs capacity to cope", fillColor = ~fill,
                          weight = ~weight_val,
                          opacity = 0.8,
                          color = "black",
                          dashArray = "0.1",
                          fillOpacity = ~opacity_val,
                          highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE#,
                          ),
                          label= filtered_labels_covid(),
                          labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "10px",
                            direction = "auto"
                          )
              ) %>%
              # economic vulnerability layer
              addPolygons(data=econ_vuln, layerId = ~econ_id,
                          group="Economic vulnerability", fillColor = ~pal(`Economic Vulnerability quintile`),
                          weight = ~weight_val,
                          opacity = 0.8,
                          color = "black",
                          dashArray = "0.1",
                          fillOpacity = ~opacity_val,
                          highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE,
                          ),
                          label= filtered_econ_labels(),
                          
                          labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "10px",
                            direction = "auto"
                          )
              ) %>%
              # socioeconomic vulnerability layer
              addPolygons(data=socioecon_vuln, layerId = ~socecon_id,
                          group="Socioeconomic vulnerability", fillColor = ~pal(`Socioeconomic Vulnerability quintile` ),
                          weight = ~weight_val,
                          opacity = 0.8,
                          color = "black",
                          dashArray = "0.1",
                          fillOpacity = ~opacity_val,
                          highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE,
                          ),
                          label= filtered_socioecon_labels(),
                          
                          labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "10px",
                            direction = "auto"
                          )
              ) %>%
              # social vulnerability layer
              addPolygons(data=socio_vuln, layerId = ~soc_id,
                          group="Social vulnerability", fillColor = ~pal(`Social Vulnerability quintile` ),
                          weight = ~weight_val,
                          opacity = 0.8,
                          color = "black",
                          dashArray = "0.1",
                          fillOpacity = ~opacity_val,
                          highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE,
                          ),
                          label= filtered_socio_labels(),
                          
                          labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "10px",
                            direction = "auto"
                          )
              ) %>%
              # Health/wellbeing vulnerability layer
              addPolygons(data=health_vuln, layerId = ~health_id,
                          group="Health/Wellbeing vulnerability", fillColor = ~pal(`Health/Wellbeing Vulnerability quintile` ),
                          weight = ~weight_val,
                          opacity = 0.8,
                          color = "black",
                          dashArray = "0.1",
                          fillOpacity = ~opacity_val,
                          highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE,
                          ),
                          label= filtered_health_labels(),
                          
                          labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "10px",
                            direction = "auto"
                          )
              ) %>%
              # clin vulnerability layer
              addPolygons(data=clin_vuln, layerId = ~clin_id,
                          group="Clinical vulnerability", fillColor = ~pal(`Clinical Vulnerability quintile` ),
                          weight = ~weight_val,
                          opacity = 0.8,
                          color = "black",
                          dashArray = "0.1",
                          fillOpacity = ~opacity_val,
                          highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE,
                          ),
                          label= filtered_clin_labels(),
                          
                          labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "10px",
                            direction = "auto"
                          )
              ) %>%
              addPolygons(data=tc_shp, layerId = ~TacticalCell,
                          group='tactical cell boundary',
                          stroke=T,
                          weight = 2,
                          opacity = 0.8,
                          color = "black",
                          dashArray = "3",
                          fill=F) %>%
              flyToBounds(lng1 = as.numeric(curr_bbox["xmin"]),
                          lat1 = as.numeric(curr_bbox["ymin"]),
                          lng2 = as.numeric(curr_bbox["xmax"]),
                          lat2 = as.numeric(curr_bbox["ymax"])) %>%
              addLayersControl(baseGroups = c("Resilience: vulnerability vs capacity to cope","Economic vulnerability","Socioeconomic vulnerability","Social vulnerability","Health/Wellbeing vulnerability","Clinical vulnerability"),
                               options= layersControlOptions(collapsed=T))

          }
          
          # local authority level
          else {
            lad_bbox <- lad_uk_most_vuln %>% filter(Name == input$lad_selected)
            curr_bbox <- st_bbox(lad_bbox)
            
            leafletProxy("map") %>%
              clearShapes() %>%
              clearMarkerClusters() %>%
              clearMarkers() %>%
              addPolygons(data=lad_uk_most_vuln, layerId = ~res_id,
                          group="Resilience: vulnerability vs capacity to cope", fillColor = ~fill,
                          weight = ~weight_val, 
                          opacity = 0.8,
                          color = "black",
                          dashArray = "0.1",
                          fillOpacity = ~opacity_val,
                          highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE#,
                          ),
                          label= filtered_labels_covid(),
                          labelOptions = labelOptions(
                          style = list("font-weight" = "normal", padding = "3px 8px"),
                          textsize = "10px",
                          direction = "auto"
                          )
              ) %>%
              # economic vulnerability layer
              addPolygons(data=econ_vuln, layerId = ~econ_id,
                          group="Economic vulnerability", fillColor = ~pal(`Economic Vulnerability quintile`),
                          weight = ~weight_val,
                          opacity = 0.8,
                          color = "black",
                          dashArray = "0.1",
                          fillOpacity = ~opacity_val,
                          highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE,
                          ),
                          label= filtered_econ_labels(),
                          
                          labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "10px",
                            direction = "auto"
                          )
              ) %>%
              # socioeconomic vulnerability layer
              addPolygons(data=socioecon_vuln, layerId = ~socecon_id,
                          group="Socioeconomic vulnerability", fillColor = ~pal(`Socioeconomic Vulnerability quintile` ),
                          weight = ~weight_val,
                          opacity = 0.8,
                          color = "black",
                          dashArray = "0.1",
                          fillOpacity = ~opacity_val,
                          highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE,
                          ),
                          label= filtered_socioecon_labels(),
                          
                          labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "10px",
                            direction = "auto"
                          )
              ) %>%
              # social vulnerability layer
              addPolygons(data=socio_vuln, layerId = ~soc_id,
                          group="Social vulnerability", fillColor = ~pal(`Social Vulnerability quintile` ),
                          weight = ~weight_val,
                          opacity = 0.8,
                          color = "black",
                          dashArray = "0.1",
                          fillOpacity = ~opacity_val,
                          highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE,
                          ),
                          label= filtered_socio_labels(),
                          
                          labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "10px",
                            direction = "auto"
                          )
              ) %>%
              # Health/wellbeing vulnerability layer
              addPolygons(data=health_vuln, layerId = ~health_id,
                          group="Health/Wellbeing vulnerability", fillColor = ~pal(`Health/Wellbeing Vulnerability quintile` ),
                          weight = ~weight_val,
                          opacity = 0.8,
                          color = "black",
                          dashArray = "0.1",
                          fillOpacity = ~opacity_val,
                          highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE,
                          ),
                          label= filtered_health_labels(),
                          
                          labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "10px",
                            direction = "auto"
                          )
              ) %>%
              # clin vulnerability layer
              addPolygons(data=clin_vuln, layerId = ~clin_id,
                          group="Clinical vulnerability", fillColor = ~pal(`Clinical Vulnerability quintile` ),
                          weight = ~weight_val,
                          opacity = 0.8,
                          color = "black",
                          dashArray = "0.1",
                          fillOpacity = ~opacity_val,
                          highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE,
                          ),
                          label= filtered_clin_labels(),
                          
                          labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "10px",
                            direction = "auto"
                          )
              ) %>%
              addPolygons(data=tc_shp, layerId = ~TacticalCell,
                          group='tactical cell boundary',
                          stroke=T,
                          weight = 2,
                          opacity = 0.8,
                          color = "black",
                          dashArray = "3",
                          fill=F) %>%
              flyToBounds(lng1 = as.numeric(curr_bbox["xmin"]),
                          lat1 = as.numeric(curr_bbox["ymin"]),
                          lng2 = as.numeric(curr_bbox["xmax"]),
                          lat2 = as.numeric(curr_bbox["ymax"])) %>%
              addLayersControl(baseGroups = c("Resilience: vulnerability vs capacity to cope","Economic vulnerability","Socioeconomic vulnerability","Social vulnerability","Health/Wellbeing vulnerability","Clinical vulnerability"),
                               options= layersControlOptions(collapsed=T))
          }
        }
          
      }
    
    else {
      if (input$theme == 'Flooding') {
        
        #flood_incd <- filtered_areas_at_risk_flooding_incd()
        #flood_risk <- filtered_areas_at_risk_flooding_risk()
        flood_all <- filtered_areas_at_risk_flooding_resilience()
        plot_flood_warning_polygon <- st_as_sf(filteredFlood_warnings_polygons())
        plot_flood_warning_points <- filteredFlood_warnings_points()
        
        
        # -- if no flood warnigns -- 
        if (dim(plot_flood_warning_polygon)[1] == 0) {
          
          if (input$lad_selected != 'All local authorities in region') {
            lad_bbox <- lad_uk2vuln_resilience %>% filter(Name == input$lad_selected)
            curr_bbox <- st_bbox(lad_bbox)
            
            
            leafletProxy("map") %>%
              clearShapes() %>%
              clearMarkers() %>%
              clearMarkerClusters() %>%
              addPolygons(data=flood_all, layerId = ~floodres_id,
                          group="Resilience of local authority", fillColor = ~`fill`,
                          weight = ~weight_val,
                          opacity = 0.8,
                          color = "black",
                          dashArray = "0.1",
                          fillOpacity = ~opacity_val,
                          highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE,
                            ),
                            label= filtered_flood_resilience_labels(),
                            
                            labelOptions = labelOptions(
                              style = list("font-weight" = "normal", padding = "3px 8px"),
                              textsize = "10px",
                              direction = "auto"
                          )) %>%
              addPolygons(data=tc_shp, layerId = ~TacticalCell,
                          group='tactical cell boundary',
                          stroke=T,
                          weight = 2,
                          opacity = 0.8,
                          color = "black",
                          dashArray = "3",
                          fill=F) %>%
              flyToBounds(lng1 = as.numeric(curr_bbox["xmin"]),
                          lat1 = as.numeric(curr_bbox["ymin"]),
                          lng2 = as.numeric(curr_bbox["xmax"]),
                          lat2 = as.numeric(curr_bbox["ymax"]))  %>%
              addLayersControl(baseGroups = c("Resilience of local authority"),
                               options= layersControlOptions(collapsed=T))
            
            
          }
          
          else {
            curr_bbox <- st_bbox(filtered_tc())
          
          leafletProxy("map") %>%
            clearShapes() %>%
            clearMarkers() %>%
            clearMarkerClusters() %>%

            addPolygons(data=flood_all, layerId = ~floodres_id,
                        group="Resilience of all local authorities", fillColor = ~`fill`,
                        weight = ~weight_val,
                        opacity = 0.8,
                        color = "black",
                        dashArray = "0.1",
                        fillOpacity = ~opacity_val,
                        highlight = highlightOptions(
                          weight = 5,
                          color = "#666",
                          dashArray = "",
                          fillOpacity = 0.7,
                          bringToFront = TRUE,
                        ), 
                        label= filtered_flood_resilience_labels(),
                        labelOptions = labelOptions(
                          style = list("font-weight" = "normal", padding = "3px 8px"),
                          textsize = "10px",
                          direction = "auto"
                        ))  %>%
            addPolygons(data=tc_shp, layerId = ~TacticalCell,
                        group='tactical cell boundary',
                        stroke=T,
                        weight = 2,
                        opacity = 0.8,
                        color = "black",
                        dashArray = "3",
                        fill=F) %>%
            flyToBounds(lng1 = as.numeric(curr_bbox["xmin"]),
                        lat1 = as.numeric(curr_bbox["ymin"]),
                        lng2 = as.numeric(curr_bbox["xmax"]),
                        lat2 = as.numeric(curr_bbox["ymax"]))  %>%
            addLayersControl(baseGroups = c("Resilience of all local authorities"),
                             options= layersControlOptions(collapsed=T))
          
          } 
          
        }
        
        # flood warnign has occurred..   
        else {

          icons = awesomeIcons(
                  icon = "glyphicon glyphicon-warning-sign", 
                  iconColor = "black",
                  #library = "fa",
                  markerColor = plot_flood_warning_points$warning_col)
          
          
          if (input$tactical_cell == '-- England --') {
          
          # -- zoom for uk ---
          curr_bbox <- st_bbox(filtered_tc())
          
          leafletProxy("map") %>%
            clearShapes() %>%
            clearMarkers() %>%
            clearMarkerClusters() %>%
            addPolygons(data=flood_all, layerId = ~floodres_id,
                        group="Resilience of all local authorities", fillColor = ~fill,
                        weight = ~weight_val,
                        opacity = 0.8,
                        color = "black",
                        dashArray = "0.1",
                        fillOpacity = ~opacity_val,
                        highlight = highlightOptions(
                          weight = 5,
                          color = "#666",
                          dashArray = "",
                          fillOpacity = 0.7,
                          bringToFront = TRUE,
                          ),
                          label= filtered_flood_resilience_labels(),
                          
                          labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "10px",
                            direction = "auto"
                        )
            ) %>%
            addAwesomeMarkers(data=plot_flood_warning_points, layerId=~`description`,
                              group="Latest flood warnings",  icon=icons,
                              clusterOptions = markerClusterOptions(), label=filtered_flood_warning_labels(),
                              
                              ) %>%
            addPolygons(data=tc_shp, layerId = ~TacticalCell,
                        group='tactical cell boundary',
                        stroke=T,
                        weight = 2,
                        opacity = 0.8,
                        color = "black",
                        dashArray = "3",
                        fill=F) %>%
           #addPolygons(data=plot_flood_warning_polygon, layerId=~`description`,
          #          group="Latest flood warnings", fillColor = ~warning_col,
           #         weight = 0.7,
          #          opacity = 0.8,
           #         color = "black",
          #          dashArray = "0.1",
           #         fillOpacity = 0.7) %>%
            flyToBounds(lng1 = as.numeric(curr_bbox["xmin"]),
                        lat1 = as.numeric(curr_bbox["ymin"]),
                        lng2 = as.numeric(curr_bbox["xmax"]),
                        lat2 = as.numeric(curr_bbox["ymax"])) %>%
            addLayersControl(baseGroups = c("Resilience of all local authorities"),
                             overlayGroups = c("Latest flood warnings"),
                             options= layersControlOptions(collapsed=T))
          
          } #
          
          else {
            # show tactical cell 
            if (input$tactical_cell != '-- England --' & input$lad_selected == 'All local authorities in region') {
            # -- zoom for uk ---
            curr_bbox <- st_bbox(filtered_tc())
            
            leafletProxy("map") %>%
              clearShapes() %>%
              clearMarkers() %>%
              clearMarkerClusters() %>%
              addPolygons(data=flood_all, layerId = ~floodres_id,
                          group="Resilience of all local authorities", fillColor = ~fill,
                          weight = ~weight_val,
                          opacity = 0.8,
                          color = "black",
                          dashArray = "0.1",
                          fillOpacity = ~opacity_val,
                          highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE,
                            ),
                            label= filtered_flood_resilience_labels(),
                            
                            labelOptions = labelOptions(
                              style = list("font-weight" = "normal", padding = "3px 8px"),
                              textsize = "10px",
                              direction = "auto"
                          )
              ) %>%
              addAwesomeMarkers(data=plot_flood_warning_points, layerId=~`description`,
                                group="Latest flood warnings",  icon=icons,
                                clusterOptions = markerClusterOptions(), label=filtered_flood_warning_labels()
                                #lng=~long, lat=~lat
              ) %>%
              addPolygons(data=tc_shp, layerId = ~TacticalCell,
                          group='tactical cell boundary',
                          stroke=T,
                          weight = 2,
                          opacity = 0.8,
                          color = "black",
                          dashArray = "3",
                          fill=F) %>%
              #addPolygons(data=plot_flood_warning_polygon, layerId=~`description`,
              #            group="Latest flood warnings", fillColor = ~warning_col,
              #            weight = 0.7,
              #            opacity = 0.8,
              #            color = "black",
              #            dashArray = "0.1",
              #            fillOpacity = 0.7) %>%
              flyToBounds(lng1 = as.numeric(curr_bbox["xmin"]),
                          lat1 = as.numeric(curr_bbox["ymin"]),
                          lng2 = as.numeric(curr_bbox["xmax"]),
                          lat2 = as.numeric(curr_bbox["ymax"])) %>%
              addLayersControl(baseGroups = c("Resilience of all local authorities"),
                               overlayGroups = c("Latest flood warnings"),
                               options= layersControlOptions(collapsed=T))
            } 
            
            else {
              
              lad_bbox <- lad_uk2vuln_resilience %>% filter(Name == input$lad_selected)
              curr_bbox <- st_bbox(lad_bbox)
              
              leafletProxy("map") %>%
                clearShapes() %>%
                clearMarkers() %>%
                clearMarkerClusters() %>%
                addPolygons(data=flood_all, layerId = ~floodres_id,
                            group="Resilience of all local authorities", fillColor = ~fill,
                            weight = ~weight_val,
                            opacity = 0.8,
                            color = "black",
                            dashArray = "0.1",
                            fillOpacity = ~opacity_val,
                            highlight = highlightOptions(
                              weight = 5,
                              color = "#666",
                              dashArray = "",
                              fillOpacity = 0.7,
                              bringToFront = TRUE,
                              ),
                              label= filtered_flood_resilience_labels(),
                              
                              labelOptions = labelOptions(
                                style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "10px",
                                direction = "auto"
                            )
                ) %>%
                addAwesomeMarkers(data=plot_flood_warning_points, layerId=~`description`,
                                  group="Latest flood warnings",  icon=icons, label=filtered_flood_warning_labels()
                ) %>%
                addPolygons(data=plot_flood_warning_polygon, layerId=~`description`,
                            group="Latest flood warnings", fillColor = ~warning_col,
                            weight = 0.7,
                            opacity = 0.8,
                            color = "black",
                            dashArray = "0.1",
                            fillOpacity = 0.7) %>%
                addPolygons(data=tc_shp, layerId = ~TacticalCell,
                            group='tactical cell boundary',
                            stroke=T,
                            weight = 2,
                            opacity = 0.8,
                            color = "black",
                            dashArray = "3",
                            fill=F) %>%
                flyToBounds(lng1 = as.numeric(curr_bbox["xmin"]),
                            lat1 = as.numeric(curr_bbox["ymin"]),
                            lng2 = as.numeric(curr_bbox["xmax"]),
                            lat2 = as.numeric(curr_bbox["ymax"])) %>%
                addLayersControl(baseGroups = c("Resilience of all local authorities"),
                                 overlayGroups = c("Latest flood warnings"),
                                 options= layersControlOptions(collapsed=T))
              
            }
          }
        }
          # close else statement
        } # close england has flood warnings if else
      }
    }
})
  
  
#addLayersControl(baseGroups = c("Resilience: vulnerability vs capacity to cope","Economic vulnerability","Socioconomic vulnerability","Social vulnerability","Health/Wellbeing vulnerability","Clinical vulnerability"),
#Use a separate observer to recreate the legend as needed.
  observe({
    # clear current controls
    map_labels = c('Least','','','','Most')
    if (input$theme == 'Covid-19') {
      map <- leafletProxy("map") %>%
        clearControls()
    
      if (any(input$map_groups %in% 'Resilience: vulnerability vs capacity to cope')) {
      
        map <- map %>%
          addControl(html="<img src='bivar-legend_v2.png', width=200>", position="bottomleft",
                   className = "fieldset {border: 0;}")
    }
    
    if (any(input$map_groups %in% 'Economic vulnerability')) {
      map <- map %>%
        addLegend(position = "bottomleft", group='Economic vulnerability',
                  pal = pal,
                  values = lad_uk2vuln_resilience$`Socioeconomic Vulnerability quintile`,
                  #title = paste0("<b>", 'Economic vulnerability score', "</b></br>", '(5 = worst)', "<br/>"),
                  title = paste0("<b>", 'Economic vulnerability', "</b>"),
                  labFormat = function(type, cuts, p) {
                    paste0(map_labels)
                  },
                  opacity = 0.8)
      
    }
    
    if (any(input$map_groups %in% 'Socioeconomic vulnerability')) {
      map <- map %>%
        addLegend(position = "bottomleft", group='Socioeconomic vulnerability',
                  pal = pal,
                  values = lad_uk2vuln_resilience$`Socioeconomic Vulnerability quintile`,
                  title = paste0("<b>", 'Socioeconomic vulnerability', "</b></br>"),
                  opacity = 0.8,
                  labFormat = function(type, cuts, p) {
                    paste0(map_labels)
                  })
      
    }
    
    if (any(input$map_groups %in% 'Social vulnerability')) {
      map <- map %>%
        addLegend(position = "bottomleft", group='Social vulnerability',
                  pal = pal,
                  values = lad_uk2vuln_resilience$`Socioeconomic Vulnerability quintile`,
                  title = paste0("<b>", 'Social vulnerability', "</b></br>"),
                  opacity = 0.8,
                  labFormat = function(type, cuts, p) {
                    paste0(map_labels)
                  })
      
    }
    
    if (any(input$map_groups %in% 'Health/Wellbeing vulnerability')) {
      map <- map %>%
        addLegend(position = "bottomleft", group='Health/Wellbeing vulnerability',
                  pal = pal,
                  values = lad_uk2vuln_resilience$`Socioeconomic Vulnerability quintile`,
                  title = paste0("<b>", 'Health/wellbeing vulnerability', "</b></br>"),
                  opacity = 0.8,
                  labFormat = function(type, cuts, p) {
                    paste0(map_labels)
                  })
      
    }
    
    if (any(input$map_groups %in% 'Clinical vulnerability')) {
      map <- map %>%
        addLegend(position = "bottomleft", group='Clinical vulnerability',
                  pal = pal,
                  values = lad_uk2vuln_resilience$`Socioeconomic Vulnerability quintile`,
                  title = paste0("<b>", 'Clinical vulnerability', "</b></br>"),
                  opacity = 0.8,
                  labFormat = function(type, cuts, p) {
                    paste0(map_labels)
                  })
      
    }
  } # end of covid-19 map theme
    
    else {
      if (input$theme == 'Flooding') {
        map <- leafletProxy("map") %>%
          clearControls() %>%
          addControl(html="<img src='bivar-legend_v2.png', width=200>", position="bottomleft",
                     className = "fieldset {border: 0;}")
        
        #if ((any(input$map_groups %in% 'Resilience of high flood incident areas')) | (any(input$map_groups %in% 'Resilience of high flood risk areas'))) {
        #  
        #  map <- map %>%
        #    addControl(html="<img src='bivar-legend.png', width=200>", position="bottomleft",
        #               className = "fieldset {border: 0;}")
        #}
        
        
      }# end of flood theme
    }
  })


  # ------- People at Risk table -------
  # --- Store click ---
  #data <- reactiveValues(clickedShape=NULL)

  observe({

    # --- Which tab is selected ---
    req(input$sidebar_id)
    if (input$sidebar_id == 'unmetneed') {
      # -- this will get tactical cell table --
      curr_table <- filterpar_tab()

      if (input$tactical_cell == '-- England --') {
        
        # select england level figures 
        eng_table <- par_table %>% 
          select('eng_people_recieving_section_95_support',
                 'prop_eng_receiving_section_95_support',
                 'england_proportion_bame',
                 'eng_case_per_100000',
                 'total_las_in_eng_with_data',
                 'eng_total_fuel_poor_households',
                 'eng_prop_households_fuel_poor',
                 'eng_rate_per_1000',
                 'eng_total_unemployed_on_ucred',
                 'prop_eng_pop_unemployed_on_ucred',
                 'total_shielding_eng',
                 'proportion_total_shielding_Eng') 
        
        # --- National BAME Statistics ---
        output$bame_population_text <- renderUI({
          ethnicity_stats_eng_text(eng_table)
        })
        
        output$bame_population <- renderEcharts4r({
          ethnicity_stats_eng_plot(eng_table)
                  })

        # --- National Section 95 Support statistics ---
        output$section95_text <- renderUI({
          sec95_stats_eng_text(eng_table)
        })
        
        output$section95 <- renderEcharts4r({
                      # # Plot population statistics
                      sec95_stats_eng_plot(eng_table)
                    })
        
        # ---National  homelessness ---
        output$homeless_text <- renderUI({
          homelessness_stats_eng_text(eng_table)  
        })
        
        output$homeless <- renderEcharts4r({
          homelessness_stats_eng_plot(eng_table)  
        })
        
        # --- National fuel poverty ---
        output$fuelp_text <- renderUI({
          fuelp_stats_eng_text(eng_table)
        })
        
        output$fuelp <- renderEcharts4r({
          fuelp_stats_eng_plot(eng_table)
        })

        # ---- National unemployed ----
        output$unemployment_text <- renderUI({
          unemployment_stats_eng_text(eng_table)  
        })
        
        output$unemployment <- renderEcharts4r({
          unemployment_stats_eng_plot(eng_table)  
        })

        
        # ---- National Tactical cell ----    
        output$digital_text <- renderUI({
          digital_exclusion_eng_text(eng_table)
        })
        
        # clear plot nothing
        output$digital <- renderEcharts4r({
        })
        
        # --- National people shielding ---
        output$shielding_text <- renderUI({
          shielding_eng_text(eng_table)
        })
        
        output$shielding_f <- renderEcharts4r({
          shielding_eng_plot(eng_table)
        })
      }


      else {
        # summary for tactical cell
        if (input$lad_selected == 'All local authorities in region') {

          # --- population demographics ---
          # ---- Tactical Cell level BAME ----
          output$bame_population_text <- renderUI({
            ethnicity_stats_mac_text(curr_table, par_table_tc_avg)
          })
          
          output$bame_population <- renderEcharts4r({
            # # Plot population statistics
            ethnicity_stats_mac_plot(curr_table, par_table_tc_avg)
          })

          # --- Tactical cell level section 95 support ----
          output$section95_text <- renderUI({
            sec95_stats_mac_text(curr_table, par_table_tc_avg)
          })
          
          output$section95 <- renderEcharts4r({
            sec95_stats_mac_plot(curr_table, par_table_tc_avg)
          })
          
          # --- homeless ----
          output$homeless_text <- renderUI({
            homelessness_stats_mac_text(curr_table, par_table_tc_avg)
          })
          
          output$homeless <- renderEcharts4r({
            homelessness_stats_mac_plot(curr_table, par_table_tc_avg)
          })
          
           # --- fuel_poverty ---
          output$fuelp_text <- renderUI({
            fuelp_stats_mac_text(curr_table, par_table_tc_avg) 
          })
          
          output$fuelp <- renderEcharts4r({
            fuelp_stats_mac_plot(curr_table, par_table_tc_avg) 
          })

          # --- Unemployment ---
          output$unemployment_text <- renderUI({
            unemployment_stats_mac_text(curr_table, par_table_tc_avg)
          })
          
          output$unemployment <- renderEcharts4r({
            unemployment_stats_mac_plot(curr_table, par_table_tc_avg)
          })
          
          
          # --- Digital exclusion ---
          output$digital_text <- renderUI({
            digital_exclusion_mac_text(curr_table, par_table_tc_avg) 
          })
          
          output$digital <- renderEcharts4r({
            digital_exclusion_mac_plot(curr_table, par_table_tc_avg)
          })
          

          # --- Shielding ---
          output$shielding_text <- renderUI({
            shielding_mac_text(curr_table, par_table_tc_avg)
          })
          
          output$shielding_f <- renderEcharts4r({
            shielding_mac_plot(curr_table, par_table_tc_avg)
          })

         }
  
        # -------------------------- #
        # -- just local authority -- #
        # -------------------------- #
        else {
          lad_of_interest <- lad_uk2areas2vulnerability %>% 
            filter(Name == input$lad_selected) %>% 
            select('LAD19CD') %>% st_drop_geometry()
          
          # --- population demographics ---
          output$bame_population_text <- renderUI({
            ethnicity_stats_lad_text(curr_table, par_table_lad_avg, lad_of_interest)
          })
          
          output$bame_population <- renderEcharts4r({
            ethnicity_stats_lad_plot(curr_table, par_table_lad_avg, lad_of_interest)
          })
          
          # --- section 95 support ----
          output$section95_text <- renderUI({
            sec95_stats_lad_text(curr_table, par_table_lad_avg, lad_of_interest)
          })
          
          output$section95 <- renderEcharts4r({
            sec95_stats_lad_plot(curr_table, par_table_lad_avg, lad_of_interest)
          })
          
          # --- homeless ----
          output$homeless_text <- renderUI({
            homelessness_stats_lad_text(curr_table, par_table_lad_avg, lad_of_interest)
          })
          
          output$homeless <- renderEcharts4r({
            homelessness_stats_lad_plot(curr_table, par_table_lad_avg, lad_of_interest)
          })
          
          # --- fuel_poverty ---
          output$fuelp_text <- renderUI({
            fuelp_stats_lad_text(curr_table, par_table_lad_avg, lad_of_interest)
          })
          
          output$fuelp <- renderEcharts4r({
            fuelp_stats_lad_plot(curr_table, par_table_lad_avg, lad_of_interest)
          })

          #--- Unemployment ---
          output$unemployment_text <- renderUI({
            unemployment_stats_lad_text(curr_table, par_table_lad_avg, lad_of_interest)
          })
          
          output$unemployment <- renderEcharts4r({
            unemployment_stats_lad_plot(curr_table, par_table_lad_avg, lad_of_interest)
          })
          
          # --- Digital exclusion ---
          output$digital_text <- renderUI({
            digital_exclusion_lad_text(curr_table, par_table_lad_avg, lad_of_interest)
          })
          
          output$digital <- renderEcharts4r({
            digital_exclusion_lad_plot(curr_table, par_table_lad_avg, lad_of_interest)
          })
          
          # --- Shielding ---
          output$shielding_text <- renderUI({
            shielding_lad_text(curr_table, par_table_lad_avg, lad_of_interest)
          })
          
          output$shielding_f <- renderEcharts4r({
            shielding_lad_plot(curr_table, par_table_lad_avg, lad_of_interest)
          })
        }
      }
    }
})
  
#store what ordering was desired for areas to focus
store_rank_wanted <- reactiveValues(rank_wanted_covid ='cases per 100,000  ', rank_wanted_flooding = 'Flood warnings/alerts')

  
observe({
  if(input$theme == 'Covid-19') {
  # set up options for areas to focus lists
  output$top10options <- renderUI({
      div(
        selectInput(
          inputId = "top_cases_top_change",
          label = "", 
          choices = c("cases per 100,000  ", "% change in cases"),
          selected = "cases per 100,000  ",
          #inline = TRUE, 
          #checkbox = TRUE,
          width="100%"
        ),
        hr(style = "border-top: 1px solid #000000;margin-top:-15px; margin-bottom:10px;padding-bottom:10px;padding-top=-10px;margin-left:-4px"),
        tags$br())
    })
  }
  
  else{
    if (input$theme == 'Flooding') {
      # set up options for areas to focus lists
      output$top10options <- renderUI({
        div(
          selectInput(
            inputId = "top_cases_top_change",
            label = "", 
            choices = c("Flood warnings/alerts","Historical flood incidents per 10,000", "% of population living in flood risk areas"),
            selected = "Flood warnings/alerts",
            #inline = TRUE, 
            #checkbox = TRUE,
            width="100%"
          ),
          hr(style = "border-top: 1px solid #000000;margin-top:-15px; margin-bottom:10px;padding-bottom:10px;padding-top=-10px;margin-left:-4px"),
          tags$br())
          #hr(style = "border-top: 1px solid #000000;margin-top:-15px; margin-bottom:10px;padding-bottom:10px;padding-top=-10px;margin-left:-25px"))
      })
      
    }
  }
  
})

# make areas to focus list
  observe({
    
    if(input$theme == 'Covid-19') {
      
      # -- change title based on what's selected --- 
      if (store_rank_wanted$rank_wanted_covid == 'cases per 100,000  ') {
        title_wanted <- "- Top 10 areas with highest number of covid cases per 100,000"
      }
    
    
      else {
        title_wanted <- "- Top 10 areas with highest % change in covid cases"
      }
    

    
    top10 <- filtered_areas2focus_list()
    
    #print(input$lad_selected)
    
    ## if just showing a local authority: 
    # the null is required because i think the way i've set up the second selection - it's called after this so when this is first run it's not currently assigned.
    if (input$lad_selected == 'All local authorities in region' || is.null(input$lad_selected)) {
      
      # plot title 
      output$title_focus_list <- renderUI({
        div(
          p(tags$strong(input$tactical_cell), title_wanted),
          hr(style = "border-top: 1px solid #000000;"))
      })
      
      top102show <- head(top10, 10)
      
      # to colour number red or green 
      top102show <- top102show %>% mutate(colour = case_when(`% change in covid cases` >0 ~ 'red',
                                                             `% change in covid cases` == 0 ~ 'orange',
                                                             `% change in covid cases` < 0 ~ 'green')) %>%
        mutate(format_number = case_when(`% change in covid cases` > 0 ~ paste0('+',.$`% change in covid cases`,'%'),
                                         `% change in covid cases` == 0 ~ paste0(.$`% change in covid cases`,'%'),
                                         `% change in covid cases` < 0 ~ paste0(.$`% change in covid cases`,'%')))
      
      
   
      # format text 
      #glimpse(top102show)
      output$top_10_1 <- renderUI({
        #paste("1.", top102show[1,1], "-", top102show[1,3], "per 100k,", top102show[1,4], "cases,", top102show[1,7], sep=" ")
        #div(
          p(style='margin-top:10px;margin-bottom:10px', id='top_1', tags$strong('1.'), paste0(top102show[1,1], ":"), top102show[1,3], "per 100k,", top102show[1,4], "cases,", tags$strong(top102show[1,7], style = paste("color:", top102show[1,6])))
      })
      #style='margin-top:-10px;margin-bottom:-5px'
      #style='margin-top:-10px;margin-bottom:-5px;margin-right:-5px;margin-left:-5px;padding-right:-10px;'
      output$top_10_2 <- renderUI({
        p(style='margin-top:10px;margin-bottom:10px',id='top_2', tags$strong('2.'), paste0(top102show[2,1],":"), top102show[2,3], "per 100k,", top102show[2,4], "cases,", tags$strong(top102show[2,7], style = paste("color:", top102show[2,6])))
      })

      output$top_10_3 <- renderUI({
        p(style='margin-top:10px;margin-bottom:10px',id='top_3', tags$strong('3.'), paste0(top102show[3,1],":"), top102show[3,3], "per 100k,", top102show[3,4], "cases,", tags$strong(top102show[3,7], style = paste("color:", top102show[3,6])))
      })

      output$top_10_4 <- renderUI({
        p(style='margin-top:10px;margin-bottom:10px',id='top_4', tags$strong('4.'), paste0(top102show[4,1],":"), top102show[4,3], "per 100k,", top102show[4,4], "cases,", tags$strong(top102show[4,7], style = paste("color:", top102show[4,6])))
      })

      output$top_10_5 <- renderUI({
        p(style='margin-top:10px;margin-bottom:10px',id='top_5', tags$strong('5.'), paste0(top102show[5,1],":"), top102show[5,3], "per 100k,", top102show[5,4], "cases,", tags$strong(top102show[5,7], style = paste("color:", top102show[5,6])))
      })

      output$top_10_6 <- renderUI({
        p(style='margin-top:10px;margin-bottom:10px',id='top_6', tags$strong('6.'), paste0(top102show[6,1],":"), top102show[6,3], "per 100k,", top102show[6,4], "cases,", tags$strong(top102show[6,7], style = paste("color:", top102show[6,6])))
      })

      output$top_10_7 <- renderUI({
        p(style='margin-top:10px;margin-bottom:10px',id='top_7', tags$strong('7.'), paste0(top102show[7,1],":"),  top102show[7,3], "per 100k,", top102show[7,4], "cases,", tags$strong(top102show[7,7], style = paste("color:", top102show[7,6])))
      })

      output$top_10_8 <- renderUI({
        p(style='margin-top:10px;margin-bottom:10px',id='top_8', tags$strong('8.'), paste0(top102show[8,1],":"),  top102show[8,3], "per 100k,", top102show[8,4], "cases,", tags$strong(top102show[8,7], style = paste("color:", top102show[8,6])))
      })

      output$top_10_9 <- renderUI({
        p(style='margin-top:10px;margin-bottom:10px',id='top_9', tags$strong('9.'), paste0(top102show[9,1],":"), top102show[9,3], "per 100k,", top102show[9,4], "cases,", tags$strong(top102show[9,7], style = paste("color:", top102show[9,6])))
      })

      output$top_10_10 <- renderUI({
        p(style='margin-top:10px;margin-bottom:10px',id='top_10', tags$strong('10.'), paste0(top102show[10,1],":"), top102show[10,3], "per 100k,", top102show[10,4], "cases,", tags$strong(top102show[10,7], style = paste("color:", top102show[10,6])))
      })
      
    #   output$areas2focus_list <- renderUI({
    #       div( hr(),
    #        # top 
    #        p(style='margin-top:-10px;margin-bottom:-10px',tags$strong('1.'), top102show[1,1], paste0("(", top102show[1,3]), "per 100k,", top102show[1,4], "cases,", tags$strong(top102show[1,7], style = paste("color:", top102show[1,6])), ")"),
    #        hr(),
    #        p(style='margin-top:-10px;margin-bottom:-10px',tags$strong('2.'), top102show[2,1], paste0("(", top102show[2,3]), "per 100k,", top102show[2,4], "cases,", tags$strong(top102show[2,7], style = paste("color:", top102show[2,6])), ")"),
    #        hr(),
    #        p(style='margin-top:-10px;margin-bottom:-10px',tags$strong('3.'), top102show[3,1],paste0( "(", top102show[3,3]), "per 100k,", top102show[3,4], "cases,", tags$strong(top102show[3,7], style = paste("color:", top102show[3,6])), ")"),
    #        hr(),
    #        p(style='margin-top:-10px;margin-bottom:-10px',tags$strong('4.'), top102show[4,1], paste0("(", top102show[4,3]), "per 100k,", top102show[4,4], "cases,", tags$strong(top102show[4,7], style = paste("color:", top102show[4,6])), ")"),
    #        hr(),
    #        p(style='margin-top:-10px;margin-bottom:-10px',tags$strong('5.'), top102show[5,1], paste0("(", top102show[5,3]), "per 100k,", top102show[5,4], "cases,", tags$strong(top102show[5,7], style = paste("color:", top102show[5,6])), ")"),
    #        hr(),
    #        p(style='margin-top:-10px;margin-bottom:-10px',tags$strong('6.'), top102show[6,1], paste0("(", top102show[6,3]), "per 100k,", top102show[6,4], "cases,", tags$strong(top102show[6,7], style = paste("color:", top102show[6,6])), ")"),
    #        hr(),
    #        p(style='margin-top:-10px;margin-bottom:-10px',tags$strong('7.'), top102show[7,1], paste0("(", top102show[7,3]), "per 100k,", top102show[7,4], "cases,", tags$strong(top102show[7,7], style = paste("color:", top102show[7,6])), ")"),
    #        hr(),
    #        p(style='margin-top:-10px;margin-bottom:-10px',tags$strong('8.'), top102show[8,1], paste0("(", top102show[8,3]), "per 100k,", top102show[8,4], "cases,", tags$strong(top102show[8,7], style = paste("color:", top102show[8,6])), ")"),
    #        hr(),
    #        p(style='margin-top:-10px;margin-bottom:-10px',tags$strong('9.'), top102show[9,1], paste0("(", top102show[9,3]), "per 100k,", top102show[9,4], "cases,", tags$strong(top102show[9,7], style = paste("color:", top102show[9,6])), ")"),
    #        hr(),
    #        p(style='margin-top:-10px;margin-bottom:-10px',tags$strong('10.'), top102show[10,1], paste0("(", top102show[10,3]),"per 100k,", top102show[10,4], "cases,", tags$strong(top102show[10,7], style = paste("color:", top102show[10,6])), ")")
    #   )
    #   
    # })
    
    }
    
    else {
      top102show <- head(top10, 1)
      
      # plot title 
      output$title_focus_list <- renderUI({
        div(
          p(tags$strong(input$lad_selected), '- number of covid cases per 100,000, Total cases and % change in covid cases,'),
          hr(style = "border-top: 1px solid #000000;"))
      })
      
      # to colour number red or green 
      top102show <- top102show %>% mutate(colour = case_when(`% change in covid cases` >0 ~ 'red',
                                                             `% change in covid cases` == 0 ~ 'orange',
                                                             `% change in covid cases` < 0 ~ 'green')) %>%
        mutate(format_number = case_when(`% change in covid cases` > 0 ~ paste0('+',.$`% change in covid cases`,'%'),
                                         `% change in covid cases` == 0 ~ paste0(.$`% change in covid cases`,'%'),
                                         `% change in covid cases` < 0 ~ paste0(.$`% change in covid cases`,'%')))
      
      # -- selected UI -- 
      
      output$top_10_1 <- renderUI({
        #paste("1.", top102show[1,1], "-", top102show[1,3], "per 100k,", top102show[1,4], "cases,", top102show[1,7], sep=" ")
        p(id='selected_1', style='background-color:#f0f0f0;margin-top:10px;margin-bottom:10px;padding-bottom:10px;padding-top:10px',
           tags$strong(paste0(top102show[1,1], ":"), top102show[1,3], "per 100k,", top102show[1,4], "cases,", tags$strong(top102show[1,7], style = paste("color:", top102show[1,6]))))
      })
      
      output$top_10_2 <- renderUI({
        #p(id='top_2', tags$strong('2.'), top102show[2,1], paste0("(", top102show[2,3]), "per 100k,", top102show[2,4], "cases,", tags$strong(top102show[2,7], style = paste("color:", top102show[2,6])), ")")
      })
      
      output$top_10_3 <- renderUI({
        #p(id='top_3', tags$strong('3.'), top102show[3,1],paste0( "(", top102show[3,3]), "per 100k,", top102show[3,4], "cases,", tags$strong(top102show[3,7], style = paste("color:", top102show[3,6])), ")")
      })
      
      output$top_10_4 <- renderUI({
        #p(id='top_4', tags$strong('4.'), top102show[4,1], paste0("(", top102show[4,3]), "per 100k,", top102show[4,4], "cases,", tags$strong(top102show[4,7], style = paste("color:", top102show[4,6])), ")")
      })
      
      output$top_10_5 <- renderUI({
        #p(id='top_5', tags$strong('5.'), top102show[5,1], paste0("(", top102show[5,3]), "per 100k,", top102show[5,4], "cases,", tags$strong(top102show[5,7], style = paste("color:", top102show[5,6])), ")")
      })
      
      output$top_10_6 <- renderUI({
        #p(id='top_6', tags$strong('6.'), top102show[6,1], paste0("(", top102show[6,3]), "per 100k,", top102show[6,4], "cases,", tags$strong(top102show[6,7], style = paste("color:", top102show[6,6])), ")")
      })
      
      output$top_10_7 <- renderUI({
        #p(id='top_7', tags$strong('7.'), top102show[7,1], paste0("(", top102show[7,3]), "per 100k,", top102show[7,4], "cases,", tags$strong(top102show[7,7], style = paste("color:", top102show[7,6])), ")")
      })
      
      output$top_10_8 <- renderUI({
        #p(id='top_8', tags$strong('8.'), top102show[8,1], paste0("(", top102show[8,3]), "per 100k,", top102show[8,4], "cases,", tags$strong(top102show[8,7], style = paste("color:", top102show[8,6])), ")")
      })
      
      output$top_10_9 <- renderUI({
        #p(id='top_9', tags$strong('9.'), top102show[9,1], paste0("(", top102show[9,3]), "per 100k,", top102show[9,4], "cases,", tags$strong(top102show[9,7], style = paste("color:", top102show[9,6])), ")")
      })
      
      output$top_10_10 <- renderUI({
        #p(id='top_10', tags$strong('10.'), top102show[10,1], paste0("(", top102show[10,3]),"per 100k,", top102show[10,4], "cases,", tags$strong(top102show[10,7], style = paste("color:", top102show[10,6])), ")")
      })
      
      }
    } # end of covid 
    
    else {
      
      if(input$theme == 'Flooding') {
        
        # retrieve list that's been selected
        #flooding_focus_list <- dd_areas2focus$d
        
        flooding_focus_list <- filtered_areas2focus_list()
        #print(flooding_focus_list)
        
        # -- which list was wanted -- 
        if(store_rank_wanted$rank_wanted_flooding == 'Historical flood incidents per 10,000') {
          title_wanted <- "- Top 10 areas with highest number of historical flood incidents per 10,000 people"
        
          
          # the null is required because i think the way i've set up the second selection - it's called after this so when this is first run it's not currently assigned.
          if (input$lad_selected == 'All local authorities in region' || is.null(input$lad_selected)) {
          
            # plot title 
            output$title_focus_list <- renderUI({
              div(
                p(tags$strong(input$tactical_cell), title_wanted),
                hr(style = "border-top: 1px solid #000000;"))
              })
          
              top102show <- head(flooding_focus_list, 10)
              
              output$top_10_1 <- renderUI({
          
                p(style='margin-top:10px;margin-bottom:10px',id='top_1',tags$strong('1.'), paste0(top102show[1,1],":"), paste0(top102show[1,6], " incidents per 10K, ", top102show[1,5], " historical floods"))
                
                })
              #style='margin-top:-10px;margin-bottom:-5px'
              #style='margin-top:-10px;margin-bottom:-5px;margin-right:-5px;margin-left:-5px;padding-right:-10px;'
              output$top_10_2 <- renderUI({
                p(style='margin-top:10px;margin-bottom:10px',id='top_2',tags$strong('2.'), paste0(top102show[2,1],":"), paste0(top102show[2,6], " incidents per 10K, ", top102show[2,5], " historical floods"))
                
                
              })
              
              output$top_10_3 <- renderUI({
                p(style='margin-top:10px;margin-bottom:10px',id='top_3',tags$strong('3.'), paste0(top102show[3,1],":"), paste0(top102show[3,6], " incidents per 10K, ", top102show[3,5], " historical floods"))
              
                })
              
              output$top_10_4 <- renderUI({
                p(style='margin-top:10px;margin-bottom:10px',id='top_4',tags$strong('4.'), paste0(top102show[4,1],":"), paste0(top102show[4,6], " incidents per 10K, ", top102show[4,5], " historical floods"))
                
              })
              
              output$top_10_5 <- renderUI({
                p(style='margin-top:10px;margin-bottom:10px',id='top_5',tags$strong('5.'), paste0(top102show[5,1],":"), paste0(top102show[5,6], " incidents per 10K, ", top102show[5,5], " historical floods"))
                
              })
              
              output$top_10_6 <- renderUI({
                p(style='margin-top:10px;margin-bottom:10px',id='top_6',tags$strong('6.'), paste0(top102show[6,1],":"), paste0(top102show[6,6], " incidents per 10K, ", top102show[6,5], " historical floods"))
                
              })
              
              output$top_10_7 <- renderUI({
                p(style='margin-top:10px;margin-bottom:10px',id='top_7',tags$strong('7.'), paste0(top102show[7,1],":"), paste0(top102show[7,6], " incidents per 10K, ", top102show[7,5], " historical floods"))
                
              })
              
              output$top_10_8 <- renderUI({
                p(style='margin-top:10px;margin-bottom:10px',id='top_8',tags$strong('8.'), paste0(top102show[8,1],":"), paste0(top102show[8,6], " incidents per 10K, ", top102show[8,5], " historical floods"))
              })
              
              output$top_10_9 <- renderUI({
                p(style='margin-top:10px;margin-bottom:10px',id='top_9',tags$strong('9.'), paste0(top102show[9,1],":"), paste0(top102show[9,6], " incidents per 10K, ", top102show[9,5], " historical floods"))
                
              })
              
              output$top_10_10 <- renderUI({
                p(style='margin-top:10px;margin-bottom:10px',id='top_10',tags$strong('10.'), paste0(top102show[10,1],":"), paste0(top102show[10,6], " incidents per 10K, ", top102show[10,5], " historical floods"))
                
              })
          
        }
        
        # for focus list selected just show a local authority
        else {
          top102show <- head(flooding_focus_list, 1)
          
          # plot title 
          output$title_focus_list <- renderUI({
            div(
              p(tags$strong(input$lad_selected), '- number of historical flood incidents per 10,000 people'),
              hr(style = "border-top: 1px solid #000000;"))
          })
          
          
          output$top_10_1 <- renderUI({
            
            p(id='top_1', style='background-color:#f0f0f0;margin-top:10px;margin-bottom:10px;padding-bottom:10px;padding-top:10px', tags$strong(paste0(top102show[1,1],":"), paste0(top102show[1,6], " incidents per 10K, ", top102show[1,5], " historical floods")))
            
          })
          #style='margin-top:-10px;margin-bottom:-5px'
          #style='margin-top:-10px;margin-bottom:-5px;margin-right:-5px;margin-left:-5px;padding-right:-10px;'
          output$top_10_2 <- renderUI({
           
            
            
          })
          
          output$top_10_3 <- renderUI({
           
            
          })
          
          output$top_10_4 <- renderUI({
           
            
          })
          
          output$top_10_5 <- renderUI({
           
            
          })
          
          output$top_10_6 <- renderUI({
           
            
          })
          
          output$top_10_7 <- renderUI({
            
            
          })
          
          output$top_10_8 <- renderUI({
           
          })
          
          output$top_10_9 <- renderUI({
            
            
          })
          
          output$top_10_10 <- renderUI({
            
            
          })
          
        }
        }
        
          # other flooding list 
          else {
            
            # -- which list was wanted -- 
            if(store_rank_wanted$rank_wanted_flooding == 'Flood warnings/alerts') {
              title_wanted <- paste("- Top 10 areas with highest number of flood warnings and alerts as of", last_updated_time, last_updated_date)
              
              
              # the null is required because i think the way i've set up the second selection - it's called after this so when this is first run it's not currently assigned.
              if (input$lad_selected == 'All local authorities in region' || is.null(input$lad_selected)) {
                
                # plot title 
                output$title_focus_list <- renderUI({
                  div(
                    p(tags$strong(input$tactical_cell), title_wanted),
                    hr(style = "border-top: 1px solid #000000;"))
                })
                
                top102show <- head(flooding_focus_list, 10)
                
                output$top_10_1 <- renderUI({
                  p(style='margin-top:10px;margin-bottom:10px',id='top_1', tags$strong('1.'), paste0(top102show[1,1], ":"), tags$strong(top102show[1,7], "severe warnings,", top102show[1,8], "warnings,", style="color:red"), tags$strong(top102show[1,9],"alerts", style='color:orange'))
                })
                #style='margin-top:-10px;margin-bottom:-5px'
                #style='margin-top:-10px;margin-bottom:-5px;margin-right:-5px;margin-left:-5px;padding-right:-10px;'
                output$top_10_2 <- renderUI({
                  p(style='margin-top:10px;margin-bottom:10px',id='top_2', tags$strong('2.'), paste0(top102show[2,1],":"), tags$strong(top102show[2,7], "severe warnings,", top102show[2,8], "warnings,", style="color:red"), tags$strong(top102show[2,9],"alerts", style='color:orange'))
                  
                })
                
                output$top_10_3 <- renderUI({
                  p(style='margin-top:10px;margin-bottom:10px',id='top_3',tags$strong('3.'), paste0(top102show[3,1],":"), tags$strong(top102show[3,7], "severe warnings,", top102show[3,8], "warnings,", style="color:red"), tags$strong(top102show[3,9],"alerts", style='color:orange'))
                  
                })
                
                output$top_10_4 <- renderUI({
                 
                  p(style='margin-top:10px;margin-bottom:10px',id='top_4',tags$strong('4.'), paste0(top102show[4,1],":"), tags$strong(top102show[4,7], "severe warnings,", top102show[4,8], "warnings,", style="color:red"), tags$strong(top102show[4,9],"alerts", style='color:orange'))
                })
                
                output$top_10_5 <- renderUI({
                  p(style='margin-top:10px;margin-bottom:10px',id='top_5',tags$strong('5.'), paste0(top102show[5,1],":"), tags$strong(top102show[5,7], "severe warnings,", top102show[5,8], "warnings,", style="color:red"), tags$strong(top102show[5,9],"alerts", style='color:orange'))
                  
                })
                
                output$top_10_6 <- renderUI({
                  p(style='margin-top:10px;margin-bottom:10px',id='top_6',tags$strong('6.'), paste0(top102show[6,1],":"), tags$strong(top102show[6,7], "severe warnings,", top102show[6,8], "warnings,", style="color:red"), tags$strong(top102show[6,9],"alerts", style='color:orange'))
                  
                })
                
                output$top_10_7 <- renderUI({
                
                  p(style='margin-top:10px;margin-bottom:10px',id='top_7',tags$strong('7.'), paste0(top102show[7,1],":"), tags$strong(top102show[7,7], "severe warnings,", top102show[7,8], "warnings,", style="color:red"), tags$strong(top102show[7,9],"alerts", style='color:orange'))
                })
                
                output$top_10_8 <- renderUI({
                  p(style='margin-top:10px;margin-bottom:10px',id='top_8', tags$strong('8.'), paste0(top102show[8,1],":"), tags$strong(top102show[8,7], "severe warnings,", top102show[8,8], "warnings,", style="color:red"), tags$strong(top102show[8,9],"alerts", style='color:orange'))
                })
                
                output$top_10_9 <- renderUI({
                  p(style='margin-top:10px;margin-bottom:10px',id='top_9',tags$strong('9.'), paste0(top102show[9,1],":"), tags$strong(top102show[9,7], "severe warnings,", top102show[9,8], "warnings,", style="color:red"), tags$strong(top102show[9,9],"alerts", style='color:orange'))
                  
                })
                
                output$top_10_10 <- renderUI({
                  p(style='margin-top:10px;margin-bottom:10px',id='top_10',tags$strong('10.'), paste0(top102show[10,1],":"), tags$strong(top102show[10,7], "severe warnings,", top102show[10,8], "warnings,", style="color:red"), tags$strong(top102show[10,9],"alerts", style='color:orange'))
                  
                })
                
              }
              
              # for focus list selected just show a local authority
              else {
                top102show <- head(flooding_focus_list, 1)
                
                # plot title 
                output$title_focus_list <- renderUI({
                  div(
                    p(tags$strong(input$lad_selected), '- total flood warnings and alerts as of', last_updated_time, last_updated_date),
                    hr(style = "border-top: 1px solid #000000;"))
                })
                
                output$top_10_1 <- renderUI({
                  p(id='top_1', style='background-color:#f0f0f0;margin-top:10px;margin-bottom:10px;padding-bottom:10px;padding-top:10px', tags$strong(paste0(top102show[1,1], ":"), tags$strong(top102show[1,7], "severe warnings,", top102show[1,8], "warnings,", style="color:red"), tags$strong(top102show[1,9],"alerts", style='color:orange')))
                })
                #style='margin-top:-10px;margin-bottom:-5px'
                #style='margin-top:-10px;margin-bottom:-5px;margin-right:-5px;margin-left:-5px;padding-right:-10px;'
                output$top_10_2 <- renderUI({
                  
                  
                  
                })
                
                output$top_10_3 <- renderUI({
                 
                  
                })
                
                output$top_10_4 <- renderUI({
                  
                  
                })
                
                output$top_10_5 <- renderUI({
                 
                  
                })
                
                output$top_10_6 <- renderUI({
                  
                  
                })

                output$top_10_7 <- renderUI({
                  
                  
                })
                
                output$top_10_8 <- renderUI({
                  
                })
                
                output$top_10_9 <- renderUI({
                  
                  
                })
                
                output$top_10_10 <- renderUI({
                  
                })
                
              }
            }
            
            else {
              
              if(store_rank_wanted$rank_wanted_flooding == '% of population living in flood risk areas') {
                
                title_wanted <- paste("- Top 10 areas with highest proportion of population living in flood risk areas")
                
                
                # the null is required because i think the way i've set up the second selection - it's called after this so when this is first run it's not currently assigned.
                if (input$lad_selected == 'All local authorities in region' || is.null(input$lad_selected)) {
                  
                  # plot title 
                  output$title_focus_list <- renderUI({
                    div(
                      p(tags$strong(input$tactical_cell), title_wanted),
                      hr(style = "border-top: 1px solid #000000;"))
                  })
                  
                  top102show <- head(flooding_focus_list, 10)
                  
                  top102show <- top102show %>%
                    mutate(format_number = case_when(`% people in flood risk areas` > 0 ~ paste0(.$`% people in flood risk areas`,'%')))
                                                     
                  output$top_10_1 <- renderUI({
                    p(style='margin-top:10px;margin-bottom:10px',id='top_1', tags$strong('1.'), paste0(top102show[1,1],":"), paste0(top102show[1,10], ", ", format(as.numeric(top102show[1,3]), big.mark=",")," people"))
                    
                  })
                  #style='margin-top:-10px;margin-bottom:-5px'
                  #style='margin-top:-10px;margin-bottom:-5px;margin-right:-5px;margin-left:-5px;padding-right:-10px;'
                  output$top_10_2 <- renderUI({
                    p(style='margin-top:10px;margin-bottom:10px',id='top_2', tags$strong('2.'), paste0(top102show[2,1],":"), paste0(top102show[2,10], ", ", format(as.numeric(top102show[2,3]), big.mark=",")," people"))
                    
                    
                  })
                  
                  output$top_10_3 <- renderUI({
                    p(style='margin-top:10px;margin-bottom:10px',id='top_3', tags$strong('3.'), paste0(top102show[3,1],":"), paste0(top102show[3,10], ", ", format(as.numeric(top102show[3,3]), big.mark=",")," people"))
                    
                  })
                  
                  output$top_10_4 <- renderUI({
                    p(style='margin-top:10px;margin-bottom:10px',id='top_4', tags$strong('4.'), paste0(top102show[4,1],":"), paste0(top102show[4,10], ", ", format(as.numeric(top102show[4,3]), big.mark=",")," people"))
                    
                  })
                  
                  output$top_10_5 <- renderUI({
                    p(style='margin-top:10px;margin-bottom:10px',id='top_5', tags$strong('5.'), paste0(top102show[5,1],":"), paste0(top102show[5,10], ", ", format(as.numeric(top102show[5,3]), big.mark=",")," people"))
                    
                  })
                  
                  output$top_10_6 <- renderUI({
                    p(style='margin-top:10px;margin-bottom:10px',id='top_6', tags$strong('6.'), paste0(top102show[6,1],":"), paste0(top102show[6,10], ", ", format(as.numeric(top102show[6,3]), big.mark=",")," people"))
                    
                  })
                  
                  output$top_10_7 <- renderUI({
                    p(style='margin-top:10px;margin-bottom:10px',id='top_7', tags$strong('7.'), paste0(top102show[7,1],":"), paste0(top102show[7,10], ", ", format(as.numeric(top102show[7,3]), big.mark=",")," people"))
                    
                  })
                  
                  output$top_10_8 <- renderUI({
                    p(style='margin-top:10px;margin-bottom:10px',id='top_8', tags$strong('8.'), paste0(top102show[8,1],":"), paste0(top102show[8,10], ", ", format(as.numeric(top102show[8,3]), big.mark=",")," people"))
                  })
                  
                  output$top_10_9 <- renderUI({
                    
                    p(style='margin-top:10px;margin-bottom:10px',id='top_9', tags$strong('9.'), paste0(top102show[9,1],":"), paste0(top102show[9,10], ", ", format(as.numeric(top102show[9,3]), big.mark=",")," people"))
                  })
                  
                  output$top_10_10 <- renderUI({
                    
                    p(style='margin-top:10px;margin-bottom:10px',id='top_10', tags$strong('10.'), paste0(top102show[10,1],":"), paste0(top102show[10,10], ", ", format(as.numeric(top102show[10,3]), big.mark=",")," people"))
                  })
                  
                  }
                
                # for focus list selected just show a local authority
                else {
                  top102show <- head(flooding_focus_list, 1)
                  
                  top102show <- top102show %>%
                    mutate(format_number = case_when(`% people in flood risk areas` > 0 ~ paste0(.$`% people in flood risk areas`,'%')))
                  
                  
                  # plot title 
                  output$title_focus_list <- renderUI({
                    div(
                      p(tags$strong(input$lad_selected), '- Top 10 areas with highest proportion of population living in flood risk areas'),
                      hr(style = "border-top: 1px solid #000000;"))
                  })
                  
                  output$top_10_1 <- renderUI({
                    p(id='top_1', style='background-color:#f0f0f0;margin-top:10px;margin-bottom:10px;padding-bottom:10px;padding-top:10px', tags$strong(paste0(top102show[1,1],":"), paste0(top102show[1,10], ", ", format(as.numeric(top102show[1,3]), big.mark=",")," people")))
                  })
                  #style='margin-top:-10px;margin-bottom:-5px'
                  #style='margin-top:-10px;margin-bottom:-5px;margin-right:-5px;margin-left:-5px;padding-right:-10px;'
                  output$top_10_2 <- renderUI({
                    
                  })
                  
                  output$top_10_3 <- renderUI({

                    
                  })
                  
                  output$top_10_4 <- renderUI({
                    
                    
                  })
                  
                  output$top_10_5 <- renderUI({
                    
                    
                  })
                  
                  output$top_10_6 <- renderUI({
                    
                    
                  })
                  
                  output$top_10_7 <- renderUI({
                    
                    
                  })
                  
                  output$top_10_8 <- renderUI({
                    
                  })
                  
                  output$top_10_9 <- renderUI({
                    
                    
                  })
                  
                  output$top_10_10 <- renderUI({
                    
                    
                  })
                  
                  
                }
                
                
                
              }
              
            }
            
            
          }
        
      
      } # end of flooding if
      
    }
    
  })


  # observe if change in rank wanted has happened
  observeEvent(input$top_cases_top_change, {
    if (input$theme == 'Covid-19') {
    store_rank_wanted$rank_wanted_covid <- input$top_cases_top_change
    }
    else {
      if(input$theme == 'Flooding') {
        store_rank_wanted$rank_wanted_flooding <- input$top_cases_top_change
        
      }
    }
  })
  
 
# --- users click on areas to focus list ---  
onclick("top_1", {
          #print('testing'),
          row_wanted <- filtered_areas2focus_list()[1,]
          #print(row_wanted)
          
          lad_choices <- lad_uk2vuln_resilience %>%
            filter(TacticalCell == row_wanted$Region) %>%
             select('LAD19NM')
          
           lad_choices <- sort(as.vector(lad_choices$LAD19NM))
           lad_choices <- c('All local authorities in region', lad_choices)
          
          #
          # # --- update select input ---
           updateSelectInput(
             session, "tactical_cell",
             choices = tactical_cells,
             selected = row_wanted$Region
           )
          
           updateSelectInput(session, "lad_selected",
                             choices = lad_choices,
                             selected=row_wanted$`Local Authority`)

        }
       
  )
  
onclick("top_2", {
  #print('testing'),
  row_wanted <- filtered_areas2focus_list()[2,]
  #print(row_wanted)
  
  lad_choices <- lad_uk2vuln_resilience %>%
    filter(TacticalCell == row_wanted$Region) %>%
    select('LAD19NM')
  
  lad_choices <- sort(as.vector(lad_choices$LAD19NM))
  lad_choices <- c('All local authorities in region', lad_choices)
  
  #
  # # --- update select input ---
  updateSelectInput(
    session, "tactical_cell",
    choices = tactical_cells,
    selected = row_wanted$Region
  )
  
  updateSelectInput(session, "lad_selected",
                    choices = lad_choices,
                    selected=row_wanted$`Local Authority`)
  
  
}

)

onclick("top_3", {
  #print('testing'),
  row_wanted <- filtered_areas2focus_list()[3,]
  #print(row_wanted)
  
  lad_choices <- lad_uk2vuln_resilience %>%
    filter(TacticalCell == row_wanted$Region) %>%
    select('LAD19NM')
  
  lad_choices <- sort(as.vector(lad_choices$LAD19NM))
  lad_choices <- c('All local authorities in region', lad_choices)
  
  #
  # # --- update select input ---
  updateSelectInput(
    session, "tactical_cell",
    choices = tactical_cells,
    selected = row_wanted$Region
  )
  
  updateSelectInput(session, "lad_selected",
                    choices = lad_choices,
                    selected=row_wanted$`Local Authority`)
  
  
}

)

onclick("top_4", {
  #print('testing'),
  row_wanted <- filtered_areas2focus_list()[4,]
  #print(row_wanted)
  
  lad_choices <- lad_uk2vuln_resilience %>%
    filter(TacticalCell == row_wanted$Region) %>%
    select('LAD19NM')
  
  lad_choices <- sort(as.vector(lad_choices$LAD19NM))
  lad_choices <- c('All local authorities in region', lad_choices)
  
  #
  # # --- update select input ---
  updateSelectInput(
    session, "tactical_cell",
    choices = tactical_cells,
    selected = row_wanted$Region
  )
  
  updateSelectInput(session, "lad_selected",
                    choices = lad_choices,
                    selected=row_wanted$`Local Authority`)
  
  
}

)

onclick("top_5", {
  #print('testing'),
  row_wanted <- filtered_areas2focus_list()[5,]
  #print(row_wanted)
  
  lad_choices <- lad_uk2vuln_resilience %>%
    filter(TacticalCell == row_wanted$Region) %>%
    select('LAD19NM')
  
  lad_choices <- sort(as.vector(lad_choices$LAD19NM))
  lad_choices <- c('All local authorities in region', lad_choices)
  
  #
  # # --- update select input ---
  updateSelectInput(
    session, "tactical_cell",
    choices = tactical_cells,
    selected = row_wanted$Region
  )
  
  updateSelectInput(session, "lad_selected",
                    choices = lad_choices,
                    selected=row_wanted$`Local Authority`)
  
  
}

)

onclick("top_6", {
  #print('testing'),
  row_wanted <- filtered_areas2focus_list()[6,]
  #print(row_wanted)
  
  lad_choices <- lad_uk2vuln_resilience %>%
    filter(TacticalCell == row_wanted$Region) %>%
    select('LAD19NM')
  
  lad_choices <- sort(as.vector(lad_choices$LAD19NM))
  lad_choices <- c('All local authorities in region', lad_choices)
  
  #
  # # --- update select input ---
  updateSelectInput(
    session, "tactical_cell",
    choices = tactical_cells,
    selected = row_wanted$Region
  )
  
  updateSelectInput(session, "lad_selected",
                    choices = lad_choices,
                    selected=row_wanted$`Local Authority`)
  
  
}

)

onclick("top_7", {
  #print('testing'),
  row_wanted <- filtered_areas2focus_list()[7,]
  #print(row_wanted)
  
  lad_choices <- lad_uk2vuln_resilience %>%
    filter(TacticalCell == row_wanted$Region) %>%
    select('LAD19NM')
  
  lad_choices <- sort(as.vector(lad_choices$LAD19NM))
  lad_choices <- c('All local authorities in region', lad_choices)
  
  #
  # # --- update select input ---
  updateSelectInput(
    session, "tactical_cell",
    choices = tactical_cells,
    selected = row_wanted$Region
  )
  
  updateSelectInput(session, "lad_selected",
                    choices = lad_choices,
                    selected=row_wanted$`Local Authority`)
  
  
}

)

onclick("top_8", {
  #print('testing'),
  row_wanted <- filtered_areas2focus_list()[8,]
  #print(row_wanted)
  
  lad_choices <- lad_uk2vuln_resilience %>%
    filter(TacticalCell == row_wanted$Region) %>%
    select('LAD19NM')
  
  lad_choices <- sort(as.vector(lad_choices$LAD19NM))
  lad_choices <- c('All local authorities in region', lad_choices)
  
  #
  # # --- update select input ---
  updateSelectInput(
    session, "tactical_cell",
    choices = tactical_cells,
    selected = row_wanted$Region
  )
  
  updateSelectInput(session, "lad_selected",
                    choices = lad_choices,
                    selected=row_wanted$`Local Authority`)
  
  
}

)

onclick("top_9", {
  #print('testing'),
  row_wanted <- filtered_areas2focus_list()[9,]
  #print(row_wanted)
  
  lad_choices <- lad_uk2vuln_resilience %>%
    filter(TacticalCell == row_wanted$Region) %>%
    select('LAD19NM')
  
  lad_choices <- sort(as.vector(lad_choices$LAD19NM))
  lad_choices <- c('All local authorities in region', lad_choices)
  
  #
  # # --- update select input ---
  updateSelectInput(
    session, "tactical_cell",
    choices = tactical_cells,
    selected = row_wanted$Region
  )
  
  updateSelectInput(session, "lad_selected",
                    choices = lad_choices,
                    selected=row_wanted$`Local Authority`)
  
  
}

)

onclick("top_10", {
  #print('testing'),
  row_wanted <- filtered_areas2focus_list()[10,]
  #print(row_wanted)
  
  lad_choices <- lad_uk2vuln_resilience %>%
    filter(TacticalCell == row_wanted$Region) %>%
    select('LAD19NM')
  
  lad_choices <- sort(as.vector(lad_choices$LAD19NM))
  lad_choices <- c('All local authorities in region', lad_choices)
  
  #
  # # --- update select input ---
  updateSelectInput(
    session, "tactical_cell",
    choices = tactical_cells,
    selected = row_wanted$Region
  )
  
  updateSelectInput(session, "lad_selected",
                    choices = lad_choices,
                    selected=row_wanted$`Local Authority`)
  
  
}

)
  
  
  

  # store reactive table so can filter on table click
  # for if row in table selected:
  dd_areas2focus=reactiveValues(d=filtered_areas2focus, l='NULL', t='NULL')
  
  #filtered_areas2focus()
  # all lads in tcs wanted
  output$areas2focus <- DT::renderDataTable({
      #Sys.sleep(1.5)
      DT::datatable(dd_areas2focus$d, filter=list(position='top'),
            selection =c('single'),
            options = list(dom='tp', #should remove top search box the p includes paging
            paging = T,
            pageLength=10,
            lengthMenu = c(5, 10, 15, 20),
            scrollX=T,
            scrollY='350px',
            autoWidth = T,
            #columnDefs = list(list(width = '0px', targets = c(3,4))#,
            #                  ),
            initComplete = htmlwidgets::JS(
            "function(settings, json) {",
            paste0("$(this.api().table().container()).css({'font-size':'12px'});"),
                "}")
                )) %>%
      formatStyle('Local Authority',
                  target='row',
                  backgroundColor = styleEqual(c(input$lad_selected), c('yellow')))
            })
  
  
  #data table proxy
  proxy <- DT::dataTableProxy('areas2focus')
  clearSorting(proxy = dataTableProxy(outputId = "areas2focus"))
  
  # what are the user selections
  observe({
    req(input$sidebar_id)
    if (input$sidebar_id == 'unmetneed') {
      print('here')
      print(input$tactical_cell)
      print(input$lad_selected)
 
      # if user has tactical cell selected
      if (input$tactical_cell == '-- England --' & is.null(input$lad_selected)) {
        print("WHAT THE F IS THE PROBLEM")
          output$secondSelection <- renderUI({
              #lads2select <- unique(lad_uk2vuln_resilience$Name)
            #lads2select <- c('All local authorities in region',sort(lads2select))
            lads2select <- c('All local authorities in region')
          selectInput("lad_selected", "3. Local authority district", choices = lads2select, selected='All local authorities in region')
      })
      
      }
      
      else {
      
      #print(input$lad_selected)
      if (input$tactical_cell != '-- England --' & input$lad_selected == 'All local authorities in region') {
        # update reactive values 
        #clearSorting(proxy = dataTableProxy(outputId = "areas2focus"))
       
        # filter by tactical cell
        tc_filtered_areas2focus <- filtered_areas2focus() %>% filter(Region == input$tactical_cell)
        
        dd_areas2focus$l <- input$lad_selected
        dd_areas2focus$t <- input$tactical_cell
        dd_areas2focus$d <- tc_filtered_areas2focus
        
        DT::replaceData(proxy, tc_filtered_areas2focus)
        
        
        
      }
      
      else {
        
        if (input$tactical_cell != '-- England --' & input$lad_selected != 'All local authorities in region') {
          #clearSorting(proxy = dataTableProxy(outputId = "areas2focus"))
          
          lad_wanted_filtered_areas2focus <- filtered_areas2focus() %>% filter(`Local Authority` == input$lad_selected)
          
          lad_filtered_areas2focus <- filtered_areas2focus() %>% filter(Region == input$tactical_cell & `Local Authority` != input$lad_selected)
          
          lad_filtered_areas2focus <- rbind(lad_wanted_filtered_areas2focus, lad_filtered_areas2focus)
          
          dd_areas2focus$l <- input$lad_selected
          dd_areas2focus$t <- input$tactical_cell
          dd_areas2focus$d <- lad_filtered_areas2focus
        
          DT::replaceData(proxy, lad_filtered_areas2focus) 
          
        }
        
        else {
          
          #clearSorting(proxy = dataTableProxy(outputId = "areas2focus"))
          dd_areas2focus$l <- input$lad_selected
          dd_areas2focus$t <- input$tactical_cell
          dd_areas2focus$d <- filtered_areas2focus()
          DT::replaceData(proxy, filtered_areas2focus())
        }
        
      }
      
      }
    }
    
  })
  
  # IF someone clicks on table filter everything to that local authority 
  observeEvent(input$areas2focus_rows_selected, {
    #if (!is.null(input$areas2focus_rows_selected)) {
      #clearSearch(proxy)
      #clearSorting(proxy = dataTableProxy(outputId = "areas2focus"))
    
      # list of local authorities
      lad_choices <- sort(dd_areas2focus$d$`Local Authority`)
      lad_choices <- c('All local authorities in region', lad_choices)

      local_authority_selected <- dd_areas2focus$d[input$areas2focus_rows_selected,1]
      dd_areas2focus$l <- local_authority_selected
      #print(dd_areas2focus$l$`Local Authority`)

      # Tactical cell lad in
      tactical_cell_selected <- lad_uk2vuln_resilience %>%
      filter(LAD19NM == local_authority_selected$`Local Authority`) %>%
          select('TacticalCell') %>%
          st_drop_geometry()

        dd_areas2focus$t <- tactical_cell_selected

        updateSelectInput(
          session, "tactical_cell",
          choices = tactical_cells,
          selected = tactical_cell_selected
        )

        updateSelectInput(session, "lad_selected",
                        choices = lad_choices,
                        selected=dd_areas2focus$l$`Local Authority`)

      
    #}

  })
  
  
  # -- observe event local authority district click on map --
  observeEvent(input$map_shape_click, {
    
    # capture click 
    click <- input$map_shape_click
    
    if(!is.null(click$id)){
    
    # remove start
    lad_id <- str_split(click, '_')[[1]][2]
    
    # Tactical cell lad in
    tactical_cell_selected <- lad_uk2vuln_resilience %>%
      filter(LAD19CD == lad_id) %>%
      select('TacticalCell', 'LAD19NM') %>%
      st_drop_geometry()
    
    
    lad_choices <- lad_uk2vuln_resilience %>%
      filter(TacticalCell == tactical_cell_selected$TacticalCell) %>%
      select('LAD19NM') %>%
      st_drop_geometry()
    
    
    lad_choices <- sort(as.vector(lad_choices$LAD19NM))
    lad_choices <- c('All local authorities in region', lad_choices)
    
    
    # --- update select input ---
    updateSelectInput(
      session, "tactical_cell",
      choices = tactical_cells,
      selected = tactical_cell_selected$TacticalCell
    )
    
    updateSelectInput(session, "lad_selected",
                      choices = lad_choices,
                      selected=tactical_cell_selected$LAD19NM)
    
    }
    
  })
  
 
  
  # --- show on first look ---
  observe({
    
    req(input$sidebar_id)
    if(input$sidebar_id == 'unmetneed') {
      
      # sometimes this hasn't been initiated so causes error
      if(is.null(input$lad_selected)) {
        output$secondSelection <- renderUI({
          #lads2select <- unique(lad_uk2vuln_resilience$Name)
          #lads2select <- c('All local authorities in region',sort(lads2select))
          lads2select <- c('All local authorities in region')
          selectInput("lad_selected", "3. Local authority district", choices = lads2select, selected='All local authorities in region')
        })
        
        # search either whole tactical cell or all of engalnd
        bounding_wanted <- st_bbox(filtered_areas_at_risk_covid())
        # create search of charity database 
        output$search_needed <- renderUI({
          # search bar
          searchInput(inputId = "search_term", 
                      label = "Search CharityBase for charities with a particular purpose",
                      placeholder = 'i.e emergency response',
                      btnSearch = icon("search"), 
                      btnReset = icon("remove"), 
                      value='',
                      width = "100%")
        })
        
        output$expand_search_needed <- renderUI({
        })
      }
      
      else {
      
      # provide option for expanded search if lad selected
      if(input$lad_selected != 'All local authorities in region') {
        
        # initally only search for the selected lad
        lad_only <- lad_uk2vuln_resilience %>% filter(Name == input$lad_selected)
      
        bounding_wanted <- st_bbox(lad_only)
        
        # create search of charity database 
        output$search_needed <- renderUI({
          # search bar
          searchInput(inputId = "search_term", 
                      label = "Search CharityBase for charities with a particular purpose",
                      placeholder = 'i.e emergency response',
                      btnSearch = icon("search"), 
                      btnReset = icon("remove"), 
                      value='',
                      width = "100%")
        })  
      
      output$expand_search_needed <- renderUI({
        
        radioButtons(inputId="expand_search",
                       label="Include neighbouring local authorities in search",
                       #status='success',
                       width = "100%",
                       choices=c("Yes","No"),
                       selected="No",
                       inline=T
                       
                    )
        
        })
      }
        
        
        else {
            # search either whole tactical cell or all of engalnd
            bounding_wanted <- st_bbox(filtered_areas_at_risk_covid())
            # create search of charity database 
            output$search_needed <- renderUI({
              # search bar
              searchInput(inputId = "search_term", 
                          label = "Search CharityBase for charities with a particular purpose",
                          placeholder = 'i.e emergency response',
                          btnSearch = icon("search"), 
                          btnReset = icon("remove"), 
                          value='',
                          width = "100%")
            })
            
            output$expand_search_needed <- renderUI({
            })
          }
        } 
      
      
   
    #glimpse(bounding_wanted)
      
    charities_found <- NULL
    tryCatch({  
    # - does the call take too long
    charities_found <- withTimeout({
                findcharities(bounding_wanted, '')
      }, timeout = 0.5)
    },
    error = function(e) {
      charities_found <- NULL
      
    #TimeoutException = function(ex) {
    #  message("Timeout. Skipping.") 
    
    })
  
    #print(charities_found)
    
    if (is.null(charities_found)) {
      
      output$local_orgs_ui <- renderUI({
        div(p(tags$strong('Call to CharityBase database is running slowly'),
              tags$br(),
              'Pleas try again by searching for a cause in the search box'))
      })
      
    }
    
    # running properly
    else {
    # # create search of charity database 
    # output$search_needed <- renderUI({
    #   # search bar
    #   searchInput(inputId = "search_term", 
    #               label = "Search for charities with particular cause",
    #               placeholder = 'i.e emergency',
    #               btnSearch = icon("search"), 
    #               btnReset = icon("remove"), 
    #               value='',
    #               width = "50%")
    #   
    # })
    
    
    # UI for table... render table
    output$local_orgs <- DT::renderDataTable({
      #Sys.sleep(1.5)
      DT::datatable(charities_found, filter=list(position='top'), escape=F,
                    selection =c('single'),
                    options = list(dom='tp', #should remove top search box the p includes paging
                                   paging = T,
                                   pageLength=10,
                                   lengthMenu = c(5, 10, 15, 20),
                                   scrollX=T,
                                   scrollY='300px',
                                   autoWidth = T,
                                   columnDefs = list(list(width='400px',targets=c(3))),
                                   initComplete = htmlwidgets::JS(
                                     "function(settings, json) {",
                                     paste0("$(this.api().table().container()).css({'font-size':'12px'});"),
                                     "}")
                    )) }, escape=F)
    
    # now renderUI
    output$local_orgs_ui <- renderUI({
      DT::dataTableOutput('local_orgs')
    })    
    
    }
    
    }
  })
  
  
  # allow user to expand search area
  observeEvent(input$expand_search, {
    #print(input$expand_search)
    print(input$expand_search)
    if(input$expand_search == 'Yes') {
      
      # search charitybase for just lad bounding box
      if(input$theme == 'Covid-19') {
        
        #
        lad_of_interest <- lad_uk2vuln_resilience %>% filter(Name == input$lad_selected)
        neighbours_of_interest <- lad_uk2vuln_resilience %>% filter(lengths(st_intersects(., lad_of_interest)) > 0)
        
        #print(neighbours_of_interest$lad19nm)
        
        bounding_wanted <- st_bbox(neighbours_of_interest)
        
        charities_found <- NULL
        tryCatch({  
          # - does the call take too long
          charities_found <- withTimeout({
            findcharities(bounding_wanted, input$search_term)
          }, timeout = 0.5)
        },
        error = function(e) {
          charities_found <- NULL
          
          #TimeoutException = function(ex) {
          #  message("Timeout. Skipping.") 
          
        })
        
      }
      
      else {
        if(input$theme == 'Flooding') {
          
          # filter geometries to neighbouring locations and search bounding of theses.. 
          #using interesects so we don't loose findings from lad selected
          lad_of_interest <- lad_uk2vuln_resilience %>% filter(Name == input$lad_selected)
          neighbours_of_interest <- lad_uk2vuln_resilience %>% filter(lengths(st_intersects(., lad_of_interest)) > 0)
          
          #print(neighbours_of_interest$lad19nm)
          
          bounding_wanted <- st_bbox(neighbours_of_interest)
          charities_found <- NULL
          tryCatch({  
            # - does the call take too long
            charities_found <- withTimeout({
              findcharities(bounding_wanted, input$search_term)
            }, timeout = 0.5)
          },
          error = function(e) {
            charities_found <- NULL
            
            #TimeoutException = function(ex) {
            #  message("Timeout. Skipping.") 
            
          })
          #print(testing)
          
        }
      }
      
      
      if (is.null(charities_found)) {
        
        output$local_orgs_ui <- renderUI({
          div(p(tags$strong('Call to CharityBase database is running slowly'),
                tags$br(),
                'Pleas try again by searching for a cause in the search box'))
        })
        
        
        
      }
      
      else {
        
        if (dim(charities_found)[1] == 0) {
          
          output$local_orgs_ui <- renderUI({
            div(p(tags$strong('No charities with this purpose found within this area in CharityBase'),
                  tags$br(),
                  'Pleas try a different search term'))
          })
          
        }
        
        else{
          output$local_orgs <- DT::renderDataTable({
            #Sys.sleep(1.5)
            DT::datatable(charities_found, filter=list(position='top'), escape=F,
                          selection =c('single'),
                          options = list(dom='tp', #should remove top search box the p includes paging
                                         paging = T,
                                         pageLength=10,
                                         lengthMenu = c(5, 10, 15, 20),
                                         scrollX=T,
                                         scrollY='300px',
                                         autoWidth = T,
                                         columnDefs = list(list(width='400px',targets=c(3))),
                                         initComplete = htmlwidgets::JS(
                                           "function(settings, json) {",
                                           paste0("$(this.api().table().container()).css({'font-size':'12px'});"),
                                           "}")
                          )) }, escape=F)
          
          output$local_orgs_ui <- renderUI({
            DT::dataTableOutput('local_orgs')
          })
        }
      }
      
    } # end of expand search box
    
    else {
      
      # search charitybase for just lad bounding box
      if(input$theme == 'Covid-19') {
        
        lad_only <- lad_uk2vuln_resilience %>% filter(Name == input$lad_selected)
        bounding_wanted <- st_bbox(lad_only)
        
        charities_found <- NULL
        tryCatch({  
          # - does the call take too long
          charities_found <- withTimeout({
            findcharities(bounding_wanted, input$search_term)
          }, timeout = 0.5)
        },
        error = function(e) {
          charities_found <- NULL
          
          #TimeoutException = function(ex) {
          #  message("Timeout. Skipping.") 
          
        })
        
      }
      
      else {
        if(input$theme == 'Flooding') {
          
          lad_only <- lad_uk2vuln_resilience %>% filter(Name == input$lad_selected)
          bounding_wanted <- st_bbox(lad_only)
          
          charities_found <- NULL
          tryCatch({  
            # - does the call take too long
            charities_found <- withTimeout({
              findcharities(bounding_wanted, input$search_term)
            }, timeout = 0.5)
          },
          error = function(e) {
            charities_found <- NULL
            
            #TimeoutException = function(ex) {
            #  message("Timeout. Skipping.") 
            
          })
          #print(testing)
          
        }
      }
      
      
      if (is.null(charities_found)) {
        
        output$local_orgs_ui <- renderUI({
          div(p(tags$strong('Call to CharityBase database is running slowly'),
                tags$br(),
                'Pleas try again by searching for a cause in the search box'))
        })
        
        
        
      }
      
      else {
        
        if (dim(charities_found)[1] == 0) {
          
          output$local_orgs_ui <- renderUI({
            div(p(tags$strong('No charities with this purpose found within this area in CharityBase'),
                  tags$br(),
                  'Pleas try a different search term'))
          })
          
        }
        
        else{
          output$local_orgs <- DT::renderDataTable({
            #Sys.sleep(1.5)
            DT::datatable(charities_found, filter=list(position='top'), escape=F,
                          selection =c('single'),
                          options = list(dom='tp', #should remove top search box the p includes paging
                                         paging = T,
                                         pageLength=10,
                                         lengthMenu = c(5, 10, 15, 20),
                                         scrollX=T,
                                         scrollY='300px',
                                         autoWidth = T,
                                         columnDefs = list(list(width='400px',targets=c(3))),
                                         initComplete = htmlwidgets::JS(
                                           "function(settings, json) {",
                                           paste0("$(this.api().table().container()).css({'font-size':'12px'});"),
                                           "}")
                          )) }, escape=F)
          
          output$local_orgs_ui <- renderUI({
            DT::dataTableOutput('local_orgs')
          })
        }
      }
      
    }
    
  })
  
  
  
  # call api if user enters search term:
  observeEvent(input$search_term_search, {
    
    if(input$theme == 'Covid-19') {
      
      #print(input$expand_search)
      # if call times out before expand search option initiated it will be null
      if (is.null(input$expand_search)) {
        print("here")
        bounding_wanted <- st_bbox(filtered_areas_at_risk_covid())
      }
      
      else{
      
      if(input$expand_search == 'Yes') {
        
        lad_of_interest <- lad_uk2vuln_resilience %>% filter(Name == input$lad_selected)
        neighbours_of_interest <- lad_uk2vuln_resilience %>% filter(lengths(st_intersects(., lad_of_interest)) > 0)
        bounding_wanted <- st_bbox(neighbours_of_interest)
      }
      
      else {
        # just show local authority results
        if(input$expand_search == 'No') {
          
        lad_only <- lad_uk2vuln_resilience %>% filter(Name == input$lad_selected)
        bounding_wanted <- st_bbox(lad_only)
        
        }
        # if searching having not loaded issue after initiation
        else {
          print('no in here')
          bounding_wanted <- st_bbox(filtered_areas_at_risk_covid())
          }
        }
      }
      
      
      charities_found <- NULL
      tryCatch({  
        # - does the call take too long
        charities_found <- withTimeout({
          findcharities(bounding_wanted, input$search_term)
        }, timeout = 0.5)
      },
      error = function(e) {
        charities_found <- NULL
        
        #TimeoutException = function(ex) {
        #  message("Timeout. Skipping.") 
        
      })
      
    }
    
    else {
      if(input$theme == 'Flooding') {
        
        # Enable search when capture null expand search or when a local authroity is selected
        if (is.null(input$expand_search) | input$lad_selected == 'All local authorities in region') {
          bounding_wanted <- st_bbox(filtered_areas_at_risk_flooding_resilience())
        }
        
        else {
        # enable search if lad selected
        if (input$expand_search == 'Yes') {
          lad_of_interest <- lad_uk2vuln_resilience %>% filter(Name == input$lad_selected)
          neighbours_of_interest <- lad_uk2vuln_resilience %>% filter(lengths(st_intersects(., lad_of_interest)) > 0)
          bounding_wanted <- st_bbox(neighbours_of_interest)
        
        }
        else {
          lad_only <- lad_uk2vuln_resilience %>% filter(Name == input$lad_selected)
          bounding_wanted <- st_bbox(lad_only)
          #bounding_wanted <- st_bbox(filtered_areas_at_risk_flooding_resilience())
          }
        }
        
        # plot search result
        charities_found <- NULL
        tryCatch({  
          # - does the call take too long
          charities_found <- withTimeout({
            findcharities(bounding_wanted, input$search_term)
          }, timeout = 1)
        },
        error = function(e) {
          charities_found <- NULL
          
          #TimeoutException = function(ex) {
          #  message("Timeout. Skipping.") 
          
        })
        #print(testing)
        
      }
    }
    
   
    if (is.null(charities_found)) {
      
      output$local_orgs_ui <- renderUI({
        div(p(tags$strong('Call to CharityBase database is running slowly'),
              tags$br(),
              'Pleas try again by searching for a cause in the search box'))
      })
      
    }
    
    else {
      
      if (dim(charities_found)[1] == 0) {
        
        output$local_orgs_ui <- renderUI({
          div(p(tags$strong('No charities with this purpose found within this area in CharityBase'),
                tags$br(),
                'Pleas try a different search term'))
        })
        
      }
      
      else{
        output$local_orgs <- DT::renderDataTable({
        #Sys.sleep(1.5)
        DT::datatable(charities_found, filter=list(position='top'), escape=F,
                    selection =c('single'),
                    options = list(dom='tp', #should remove top search box the p includes paging
                                   paging = T,
                                   pageLength=10,
                                   lengthMenu = c(5, 10, 15, 20),
                                   scrollX=T,
                                   scrollY='300px',
                                   autoWidth = T,
                                   columnDefs = list(list(width='400px',targets=c(3))),
                                   initComplete = htmlwidgets::JS(
                                     "function(settings, json) {",
                                     paste0("$(this.api().table().container()).css({'font-size':'12px'});"),
                                     "}")
                    )) }, escape=F)
    
            output$local_orgs_ui <- renderUI({
            DT::dataTableOutput('local_orgs')
          })
      }
    }
  })
  
  
  


observeEvent(input$internal_reports, {
  internal_report_link()
})

observeEvent(input$internal_reports_from_box, {
  internal_report_link()
})

observeEvent(req(input$sidebar_id == 'internal_reports_from_sidebar'), {
  internal_report_link()
})
    

}


shinyApp(ui, server)
