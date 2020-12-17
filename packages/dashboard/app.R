library(shiny)
library(shinydashboard)
library(httr)
library(sf)
library(tidyverse)
library(jsonlite)
library(shinydashboardPlus)
library(leaflet)
library(viridis)
library(DT)
library(shiny)
library(echarts4r)
library(feather)
library(scales)
library(htmlwidgets)
library(magrittr)


# --- read in vulnerablity indices ---
# # --- local authority level ---
LA_vi <- read_csv('https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/output/vulnerability-LA.csv')
LA_vi <- LA_vi %>% rename('LAD19CD'=Code)
# --- Middle super output area level ---
msoa_vi <- read_csv('https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/output/vulnerability-MSOA-UK.csv')
msoa_vi <- msoa_vi %>% rename('MSOA11CD'=Code)
# -- Area lookup table ---
area_lookup <- read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/lookup%20mosa11%20to%20lad17%20to%20lad19%20to%20tactical%20cell.csv")
area_lookup_tc2lad <- area_lookup %>% select('LAD19CD', 'TacticalCell')


# ---- Read in the resilience index ----
#LA_res <- read_csv('https://github.com/britishredcrosssociety/resilience-index/raw/main/data/processed/resilience%20index.csv')
LA_res <- read_csv('data/resilience_index_bivar.csv')


# --- read in shape files with minimal metadata ---
# --- Local authorities ---
lad_uk <- read_sf('data/reduced_boundaries/lad19_eng_wales_sc_ni.geojson')
lad_uk <- lad_uk %>% rename('LAD19CD'=lad19cd)
# --- Middle admin level ---
# --- read in England and wales msoa ---
msoa_ew <- read_sf('data/reduced_boundaries/MSOA2011_EW.geojson')
# --- read in NI soa file ---
msoa_ni <- read_sf('data/reduced_boundaries/NI_soa.geojson')
# --- read in Scotland Intermediade zones ---
msoa_scot <- read_sf('data/reduced_boundaries/Scotland_intzones.geojson')
# --- join middle admin level data ---
all_msoas <- bind_rows(msoa_ew, msoa_ni, msoa_scot)
# -- rename to merge with lookup table ---
all_msoas <- all_msoas %>% rename('MSOA11CD'=Code)

# -- tactical cell boundaries --
#tc_shp <- st_read('./data/reduced_boundaries/vcsep_multiagencycells_wo-iom-ci_BFE.geojson') %>%
#  st_transform('+proj=longlat +datum=WGS84')
tc_shp <- read_sf('data/reduced_boundaries/vcsep_multiagencycells_wo-iom-ci_BFE.shp') %>%
  st_transform('+proj=longlat +datum=WGS84')

tc_shp <- tc_shp %>% mutate("TacticalCell"=case_when(lookup_loc == 'Midlands and the East' ~ 'Central',
                                                     TRUE ~ as.character(.$lookup_loc)))



# --- join lookup table ---
# --- to la shapefile  ---
lad_uk2areas <- left_join(lad_uk, area_lookup_tc2lad, by='LAD19CD', keep=F)
# --- to msoa shapefile ---
all_msoas2areas <- left_join(all_msoas, area_lookup, by='MSOA11CD', keep=F)

# --- join vulnerability index to shapefiles ---
# --- local authority level ---
lad_uk2areas2vulnerability_full <- left_join(unique(lad_uk2areas), LA_vi, by="LAD19CD", keep=F)
lad_uk2areas2vulnerability <- lad_uk2areas2vulnerability_full %>% select('LAD19CD', 'Name', 'Country', `Clinical Vulnerability quintile`, `Health/Wellbeing Vulnerability quintile`,`Economic Vulnerability quintile`,`Social Vulnerability quintile`,`Socioeconomic Vulnerability quintile`,`Vulnerability quintile`,'TacticalCell')

# --- msoa level ----
all_msoas2areas2vulnerability <- left_join(all_msoas2areas, msoa_vi, by='MSOA11CD', keep=F)

# res shapefile
lad_uk2vuln_resilience <- left_join(unique(lad_uk2areas), LA_res, by='LAD19CD', keep=F)
lad_uk2vuln_resilience <- lad_uk2vuln_resilience %>% filter(!is.na(fill))

# tactical cells
tactical_cells <- area_lookup_tc2lad %>% filter(TacticalCell != 'Wales' & TacticalCell != 'Northern Ireland and the Isle of Man' & TacticalCell != 'Scotland')
tactical_cells <- unique(tactical_cells$TacticalCell)
tactical_cells <- c('-- England --', tactical_cells)

#vuln_cols <- c("#77324C","#3F2949","#435786","#806A8A")

# --- filter to just areas most in need ---
#test <- LA_res %>% filter(fill %in% vuln_cols) %>% select(`LAD19NM`,`Vulnerability quintile`,`Capacity quintile`,`vuln_quantiles`,)

# --- Metadata ----
# --- people at risk data ---
par_table <- read_feather('data/people_at_risk/people-at-risk.feather')

# temp fix of typo 
par_table <- par_table %>% rename('lad_prop_recieving_section_95_support'=lad_prop_receving_section95_support,
                                  'lad_prop_unemployed_on_ucred' = 'lad_prop_upemployed_on_ucred') 

# just working with engalnd for now
england_regions = c("North", "Central", "London", "South and the Channel Islands", "South East")

# calculate averages for all local authorities across england - to me this is the avg across england
# need to select 
par_table_lad_avg <- par_table %>%
  select(
         'Proportion of neighbourhoods in 20% most digitally excluded',
         'percent_digitally_excluded',
         'People receiving Section 95 support',
         'lad_prop_recieving_section_95_support',
         `Percentage of population who are ethnic minority`,
         `week 49`,
         `Number of households in fuel poverty1`,
         `Proportion of households fuel poor (%)`,
         `Homelessness (rate per 1000)`,
         `lad_total_homeless`,
         'lad_prop_homeless',
         `Not in employment`,
         'lad_prop_unemployed_on_ucred',
         `Clinically extremely vulnerable`,
         `Proportion Clinically extremely vulnerable`
         ) %>%
  summarise_all(., list(mean), na.rm=T) %>%
  mutate('LAD19CD'='lad_avg', 'TacticalCell'='lad_avg') %>%
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
    'tc_total_homeless',
    `Homelessness per 1000 in tc`,
    'tc_prop_homeless',
    `tc_Not in employment`,
    `tc_prop_unemployed_on_universal_credit`,
    `tc_Clinically extremely vulnerable`,
    `tc_Clinically vulnerable proportion of population`
    ) %>%
  unique() %>%
  summarise_all(., list(mean), na.rm=T) %>%
  summarise_all(., list(round), 2) %>%
  mutate('LAD19CD'='tc_avg', 'TacticalCell'='tc_avg') %>%
  select('LAD19CD', 'TacticalCell', everything())




#par_table_lad_avg <- par_table %>% filter(TacticalCell %in% england_regions) %>%
#  mutate(`LAD_perc_Proportion of neighbourhoods in 20% most digitally excluded`=round(`LAD_perc_Proportion of neighbourhoods in 20% most digitally excluded`*100,1)) %>%
#  summarise(across(c(`LAD_int_Proportion of neighbourhoods in 20% most digitally excluded`:`LAD_perc_People receiving Section 95 support`), ~ mean(.x, na.rm=T))) %>%
  #rename columns
#  mutate('LAD19CD'='All LADs in England avg') %>%
#  mutate('TacticalCell'='All LADs in England avg') %>%
#  select('LAD19CD', 'TacticalCell', everything())


# calculate averages for local authorities within a region 
#par_table_avg_per_region <- area_avg %>% filter(TacticalCell %in% england_regions) %>%
#  select('LAD19CD', 'TacticalCell',`LAD_int_Proportion of neighbourhoods in 20% most digitally excluded`:`LAD_perc_People receiving Section 95 support`) %>%
#  mutate(`LAD_perc_Proportion of neighbourhoods in 20% most digitally excluded`=round(`LAD_perc_Proportion of neighbourhoods in 20% most digitally excluded`*100,1))

#par_table_avg_per_region <- aggregate(par_table_avg_per_region[,3:18], list(par_table_avg_per_region$TacticalCell), mean, na.rm=TRUE, na.action=NULL)

#par_table_avg_per_region <- par_table_avg_per_region %>% rename('TacticalCell'=Group.1)

# avg for tactical cells
#par_table_tc_avg <- par_table %>% filter(TacticalCell %in% england_regions) %>%
#  select(`tc_int_Proportion of neighbourhoods in 20% most digitally excluded`:`tc_perc_People receiving Section 95 support`) %>%
#  unique() %>%
#  mutate(`tc_perc_Proportion of neighbourhoods in 20% most digitally excluded`=round(`tc_perc_Proportion of neighbourhoods in 20% most digitally excluded`*100,1)) %>%
#  summarise(across(c(`tc_int_Proportion of neighbourhoods in 20% most digitally excluded`:`tc_perc_People receiving Section 95 support`), ~ mean(.x, na.rm=T))) %>%
#  #rename columns
#  mutate('LAD19CD'='TC avg') %>%
#  mutate('TacticalCell'='TC avg') %>%
#  select('LAD19CD', 'TacticalCell', everything())



# --- areas to focus
covid_area2focus <- read_csv('data/areas_to_focus/areas2focus_covid.csv')

# covid name for table
covid_week = tail(names(covid_area2focus),1)
covid_week = strsplit(covid_week, " ")
covid_week = paste('Week', covid_week[[1]][2],'\n')

covid_area2focus <- covid_area2focus %>%
  # for now removing (don't think this data has data for these areas anyway)
  filter(TacticalCell != "Northern Ireland and the Isle of Man",
             TacticalCell != "Scotland",
             TacticalCell != "Wales") %>%
  rename('covid cases per 100,000'=tail(names(.),1))
  



# -- vcs indicators
requests <- read_csv('data/vcs_indicators/requests_this_week_and_last.csv')
volunteers <- read_csv('data/vcs_indicators/volunteer-capacity-lad19CD-tc.csv')


# ---  dashboard --- #
# --- header --- #
header <- dashboardHeaderPlus(title = "VCSEP Insights", titleWidth = "300px",
                              dropdownMenu(
                                type = "notifications",
                                icon = icon("question-circle"),
                                badgeStatus = NULL,
                                headerText = "Help"))


# --- side bar --- #
sidebar <- dashboardSidebar(
  # width = "300px",
  #
  # # - always display region and vulnerability index to display -
  # br(),
  # p(style="text-align: justify;",
  #   "Identify areas in need in your region"),
  # br(),
  # selectInput("tactical_cell",
  #             label = "Choose Region",
  #             choices = sort(tactical_cells),
  #             selected = "--All UK--"
  # ),
  # br(),
  # uiOutput("secondSelection"),
  #
  # br(),
  # selectInput("theme",
  #             label="Select a Theme",
  #             choices = sort(c("Covid-19","Winter Pressures","Economic Hardship", "Mental Health","Flooding","Food Insecurity")),
  #             selected="Covid-19"),
  #
  #selectInput("vi",
  #            label = "Vulnerability Index",
  #            choices = c("Socioeconomic vulnerability", "Clinical vulnerability", "Overall vulnerability", 'Health/Wellbeing vulnerability', 'Economic vulnerability', 'Social vulnerability'),
  #            selected = "Overall vulnerability"
  #),

  # br(),
  # selectInput("res_index",
  #             label="Resilience Index",
  #             choices = c("Overall", "Floods", "Dwelling fires"),
  #             selected = "Overall"),
  #
  # p('--- MAP View ---'),
  # selectInput("maptype",
  #             label="View indicies together (bivariate) or seperately (single layer)",
  #             choices = c("Bivariate", "Single Layer")),


  #sidebarMenu( id = 'tabs',
    # -- overall unmet needs for your area -- #
    # menuItem("Unmet Needs", expandedName = "needs", icon = icon("dashboard"), selected=TRUE, startExpanded = TRUE,
    #          #expandedName = "unmetneeds",
    #          br(),
    #          p(style="text-align: justify;",
    #            "Select a region to focus in on"),
    #          # - select tactical cell
    #          selectInput("tactical_cell",
    #                      label = "Choose Region",
    #                      choices = sort(unique(vuln_index$TacticalCell)),
    #                      selected = "North"
    #          ),
    #          br(),
    #          # - select type of vulnerability index to display -
    #          p(style='text-align: justify;',
    #            "Select a vulnerability index to display"),
    #          selectInput("vi",
    #                      label = "Type of vulnerability",
    #                      choices = c("Socioeconomic vulnerability", "Clinical vulnerability", "Overall vulnerability")
    #          ),
    #
    #          br(),
    #          br()
    #     ),

  #   menuItem("Themes", icon = icon("th"), tabName = "themes", selected=F, newTab = TRUE, startExpanded = F,
  #            # - list themes -
  #            menuSubItem("Flooding", tabName = 'flooding'),
  #            menuSubItem("Mental Health", tabName='mh'),
  #            menuSubItem("Winter Pressures", tabName='winter_pressures'))
   #),

  width = "300px",
  tags$style(HTML(".sidebar-menu li a { font-size: 16px; }")),
  sidebarMenu(id="sidebar_id",
              # -- Home page ---
              menuItem('Home', tabName='home', icon=icon("home")),
              # -- Unmet need insight -- binoculars
              menuItem("Insight", icon = icon("binoculars"), tabName = "insight", startExpanded = T,
                       menuSubItem(HTML("Areas at risk in an emergency"), tabName="unmetneed", icon=icon("hands-helping"))),
              # -- trying conditional panel ---
              conditionalPanel(condition = "input.sidebar_id == 'unmetneed'",
                               div(style="text-align: justify;",
                                   br(),
                               p("This dashboard is to help answer the",tags$br(), "question of what",
                                 tags$strong("areas and people"), tags$br(), "would be/are at risk should the", tags$br(),
                                tags$strong("emergency"), "scenario selected occur")),
                               selectInput("tactical_cell",
                                           label = "Choose Region",
                                           choices = sort(tactical_cells),
                                            selected = "-- England --"

                               ),
                               uiOutput("secondSelection"),

                               selectInput("theme",
                                    label="Select an emergency",
                                    choices = sort(c("Covid-19","Winter Pressures","Economic Hardship", "Mental Health","Flooding","Food Insecurity")),
                                    selected="Covid-19")
              ),

              #menuItem(HTML("Emergencies Partnership<br/>Statistics"), tabName='vcs_usage', startExpanded = F, icon=icon('balance-scale-right'),
              #         menuSubItem("Requests", tabName='request_data'),
              #         menuSubItem("Pulse Check", tabName="pulse_check"),
              #         menuSubItem("Volunteer Capacity", tabName="vol_capacity")),

              menuItem("References", tabName='references', icon=icon('feather-alt')),


  # - display vcsep logo -
  div(p("Developed by"), img(src = "vcs-logo-text.png", width = 225),style="position:fixed; bottom:0; padding:15px; text-align: center;")

  )
)

body <- dashboardBody(

  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  tags$head(includeCSS("styles.css")),
  #tags$head(HTML("<title> VCSEP Unmet needs platform </title>")),

  tabItems(
    # --- Home page ---
    tabItem(tabName="home", selected=T,
            # - row 1 -
            fluidRow(style="padding-right:20px",
              # column 1
              column(width = 8,
                     box(width=NULL,
                       uiOutput('welcome'),
                       style = "height:620px; overflow-y: scroll;overflow-x: scroll;", footer=div(
                         p(tags$strong(tags$i("This platform is still in the early stages of development. 
                               Some features may not work properly, but are coming soon.")), 
                           style="color:blue")),
                       accordion(inputId='accordion1',
                         accordionItem(
                           id=1,
                           title='About',
                           collapse=T,
                           div(
                             p(tags$strong('Purpose:')),
                             p('In times of an emergency, this platform seeks to answer the key question of "where is the need greatest?"'),
                             p('It helps responders who want to target their limited resource in areas of greatest risk and least capacity to respond.
              The tools is also useful for those wanting estimates of people at risk and in need to support their
              influencing and advocacy efforts across a range of themes.'),
                             #br(),
                             br(),
                             p(tags$strong('Scope:')),
                             p('The platform attempts to provide a fuller picture of unmet need before,
              during and after an emergency. To do this, we highlight areas of high vulnerability
              and least resilience based on the British Red Cross’ and show this alongside service reach.'),
                             p('Our hope is that by combining data from across the sector, we get a fuller picture of where there
              is unmet need. For example, where requests for support have come through to our partners that haven’t
              been met. This could be through support line calls that have been signposted elsewhere, or requests
              for hardship support that have not been met.'),
                             br(),
                             p(tags$strong('About the data:')),
                             p('We use open source and private data from our contributing partners to answer key
            questions that inform emergency preparedness, response and recovery.
            This includes numbers and rates of people at risk, reach of services by activity or support
            type, and area ranks by vulnerability or capacity.'),
                             p('We prioritize data that can be either mapped geographically or shown over time to highlight
            areas at risk and changes in unmet need. Where the data allows, we aim to show this to as
            granular level as possible without including personally identifiable information.
            At present, we show data by region, local authority and middle super output area.')
                           )
                         ),
                         accordionItem(
                           id=2,
                           title='View insight',
                           collapsed=F,
                           div(
                             p("View the latest insight", tags$strong('underneath the insights tab'), 'in the sidebar')
                           )
                           #uiOutput('welcome_insight')
                           ),
                         accordionItem(
                           id=3,
                           title='Get involved',
                           collapsed=T,
                           div(p("Our", tags$strong("Data Working Group"), "meets fortnightly on a Thursday at 11am to help us prioritise
                             what data and analysis to focus on next."),
                               p(tags$strong("Join us"), "to lend your voice to the conversation.")
                           )
                           #uiOutput('welcome_get_involved')
                         ),
                         accordionItem(
                           id=4,
                           title='Share data',
                           collapsed=T,
                           div(
                             p("Use our", tags$a(href="https://ingest.vcsep.org.uk/", target="_blank","Data App"), "or get in touch with
             our Data Team at", tags$a(href='insight@vcsep.org.uk', target="_blank", "insight@vcsep.org.uk"))
                           )
                          ),
                    
                         accordionItem(
                           id=5,
                           title='Feedback or make a request',
                           collapsed=T,
                           div(
                           p("We welcome your thoughts on what data would be useful to help shape your support to those in need.
                                  To feedback, make a request, or if you have any questions please get in touch with us at", tags$a(href="insight@vcsep.org.uk", target="_blank", "insight@vcsep.org.uk")
                                 )
                         )
                        ),
                         accordionItem(
                           id=6,
                           title='Find out more',
                           collapsed=T,
                           div(p("To learn more about the work of the VCS Emergencies Partnership, visit us at", tags$a(href="https://vcsep.org.uk/", target="_blank", "vcsep.org.uk")
                                 )
                          
                              )
                          )
                       )
                      )
                     ),

              # column 2
              column(width = 4,
                # - Row 1 -
                fluidRow(
                  tags$head(tags$script('!function(d,s,id){var js,fjs=d.getElementsByTagName(s)    [0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");')),
                  box( width=NULL, #height='175px',
                      a(class="twitter-timeline", href="https://twitter.com/vcsep"),
                      style = "height:220px; overflow-y: scroll;overflow-x: scroll;")
                      #uiOutput('twitter'))
                ),
                # - Row 2 -
                fluidRow(
                  box(title='Data store', height='220px', width=NULL,
                      uiOutput('how_much_data'))
                ),
                # - Row 3 -
                fluidRow(
                  box(title='Data contributors', width=NULL, #height='220px
                      uiOutput('members'),
                      style = "height:175px; overflow-y: scroll;overflow-x: scroll;")
                )
              )
            )
    ),

    # -- areas in need --
    tabItem(tabName = "unmetneed",
  # - row 1 -
  fluidRow(
    infoBoxOutput("requests", width = 4),
    infoBoxOutput("pulse", width = 4),
    infoBoxOutput("vols", width = 4)
  ),
    # - row 2  -
      fluidRow(
      # - column 1 -
      column(width = 6,
              box(width = NULL, collapsible=T, collapsed=T,
                  title='About this dashboard', 
                  uiOutput('about_needs'),
                  style = "height:200px; overflow-y: scroll;overflow-x: scroll;"),
             
              # row  -
                  fluidRow(
                    # column 1
                    column(width = 12,
                  # - row 2 (action areas) -
                  box( width = NULL,  collapsible = T, collapsed=F,
                    title = "Areas to focus", #height='400px',
                      DT::dataTableOutput('areas2focus', height='325px'),
                      style = "height:400px; overflow-y: scroll;overflow-x: scroll;"
                      )
             )),

                      # - row 3 -
                      fluidRow(
                        # - column 1 -
                        column(
                          width = 12,
                          box(
                            width = NULL, collapsible = T, collapsed=F,#solidHeader = TRUE, status='primary',
                            title = "People at risk", align = "center", style = "height:300px; overflow-y: scroll;overflow-x: scroll;",
                            
                            # multi columned box
                            fluidRow(
                              column(
                                width = 6,
                                  uiOutput('bame_population_text'),
                                  echarts4rOutput('bame_population', height='40px'),
                                  rightBorder=T,
                                  marginBottom=T
                              ),
                              
                              column(
                                width = 6,
                                  uiOutput('section95_text'),
                                  echarts4rOutput('section95', height='40px'),
                                  rightBorder=T,
                                  marginBottom =T
                                )
                              ),
                            
                            fluidRow(
                              column(
                                width = 6,
                                uiOutput('homeless_text'),
                                echarts4rOutput('homeless', height='40px'),
                                rightBorder=T,
                                marginBottom=T
                              ),
                              
                              column(
                                width = 6,
                                uiOutput('fuelp_text'),
                                echarts4rOutput('fuelp',height='40px'),
                                rightBorder=T,
                                marginBottom =T
                              )
                            ),
                            fluidRow(
                              column(
                                width = 6,
                                uiOutput('unemployment_text'),
                                echarts4rOutput('unemployment',height='40px'),
                                rightBorder=T,
                                marginBottom=T
                              ),
                              
                              column(
                                width = 6,
                                uiOutput('digital_text'),
                                echarts4rOutput('digital',height='40px'),
                                rightBorder=T,
                                marginBottom =T
                              )
                            ),
                            fluidRow(
                              column(
                                width = 6,
                                uiOutput('shielding_text'),
                                echarts4rOutput('shielding_f',height='40px'),
                                rightBorder=T,
                                marginBottom=T
                              )
                            )
                          )
                        )
                      ),
                            
                            #echarts4rOutput('total_population',height='60px'),
                            #uiOutput('bame_population_text'),
                            #echarts4rOutput('bame_population', height='40px'),
                        #     uiOutput('section95_text'),
                        #     echarts4rOutput('section95', height='40px'),
                        #     uiOutput('homeless_text'),
                        #     echarts4rOutput('homeless', height='40px'),
                        #     uiOutput('fuelp_text'),
                        #     echarts4rOutput('fuelp',height='40px'),
                        #     uiOutput('unemployment_text'),
                        #     echarts4rOutput('unemployment',height='40px'),
                        #     uiOutput('digital_text'),
                        #     echarts4rOutput('digital',height='40px'),
                        #     uiOutput('shielding_text'),
                        #     echarts4rOutput('shielding_f',height='40px'),
                        #     style = "height:300px; overflow-y: scroll;overflow-x: scroll;"
                        #   )
                        # )
                        # ),

                        fluidRow(
                        column(width = 12,
                             box(
                                width = NULL, collapsible = T, collapsed=F,#solidHeader = TRUE, status='primary',
                                 title = "People in need", align = "center", #height = "600px"
                                 uiOutput('people_in_Need'),
                                 style = "height:300px; overflow-y: scroll;overflow-x: scroll;"
                               )
                            )

                      )
                    ),

              # column - 2
              column( width = 6,
                    # - row 1 -
                    box(
                      # bivariate
                      title = "Areas at risk",
                      width = NULL, height = "750px", #solidHeader = TRUE, status='primary',
                      leafletOutput("map", height = "650px"),

                    absolutePanel(
                      id = "legend", class = "panel panel-default",
                      top = "auto", bottom = 10, right = "auto", width = 250, fixed = FALSE,
                      draggable = FALSE, height = "auto",
                      img(src = "bivar-legend.png", width = 250)
                    )
                )
              )
        )
    ),

  # - Request
  # tabItem(tabName = 'request_data',
  #        h2("Request stats")),
  #tabItem(tabName = 'pulse_check',
  #        h2("Pulse stats")),
  #tabItem(tabName = 'vol_capacity',
  #        h2("volunteer capacity analysis")),
  tabItem(tabName = 'references',
          fluidRow(style="padding-right:120px;padding-left:120px;padding-top:40px;padding-bottom:40px",
                   # column 1
                   column(width = 12,
                          box(width=NULL,
                              uiOutput('refs'),
                              style = "height:650px; overflow-y: scroll;overflow-x: scroll;"
                              #style = 'overflow-y:scroll; height: calc(100vh - 200px) !important;'
                          )))
    )
  )
)



# --- build user interface --- #
ui <- function(request) {
  dashboardPagePlus(
  sidebar_fullCollapse = TRUE,
  skin='purple',
  header,
  sidebar,
  body,
)
}


# ---- server ---- #
server = function(input, output) {

  # obeserve if first tab is selected
  observe({

    req(input$sidebar_id)

    if (input$sidebar_id == 'home') {
      output$welcome <- renderUI({

        div(
          h2(tags$strong('Insights from the Emergencies Partnership')),
          hr(),
          h4('Bringing together data to', tags$strong('improve collaboration'), 'across the voluntary and community sector,',
             tags$strong('before,'), tags$strong('during,'), "and", tags$strong('after'), "an", tags$strong("emergency"), ""),
          br()
        )
      })
      
        
        # output$welcome_about <- renderUI({
        #   div(
        #     p(tags$strong('Purpose:')),
        #     p('In times of an emergency, this platform seeks to answer the key question of "where is the need greatest?"'),
        #     p('It helps responders who want to target their limited resource in areas of greatest risk and least capacity to respond. 
        #       The tools is also useful for those wanting estimates of people at risk and in need to support their 
        #       influencing and advocacy efforts across a range of themes.'),
        #     #br(),
        #     br(),
        #     p(tags$strong('Scope:')),
        #     p('The platform attempts to provide a fuller picture of unmet need before, 
        #       during and after an emergency. To do this, we highlight areas of high vulnerability 
        #       and least resilience based on the British Red Cross’ and show this alongside service reach.'),
        #     p('Our hope is that by combining data from across the sector, we get a fuller picture of where there 
        #       is unmet need. For example, where requests for support have come through to our partners that haven’t
        #       been met. This could be through support line calls that have been signposted elsewhere, or requests 
        #       for hardship support that have not been met.'),
        #     br(),
        #     p(tags$strong('About the data:')),
        #     p('We use open source and private data from our contributing partners to answer key 
        #     questions that inform emergency preparedness, response and recovery. 
        #     This includes numbers and rates of people at risk, reach of services by activity or support 
        #     type, and area ranks by vulnerability or capacity.'),
        #     p('We prioritize data that can be either mapped geographically or shown over time to highlight
        #     areas at risk and changes in unmet need. Where the data allows, we aim to show this to as 
        #     granular level as possible without including personally identifiable information. 
        #     At present, we show data by region, local authority and middle super output area.')
        #   )
        # })

        #   br(),
        #   h3(tags$strong('Get involved')),
        #   p("Our Data Working Group meets fortnightly on a Thursday at 11am to help us prioritise
        #     what data and analysis to focus on next. Join us to lend your voice to the conversation."),
        #   br(),
        #   h3(tags$strong('Share data')),
        #   p("Use our", tags$a(href="https://ingest.vcsep.org.uk/", target="_blank","Data App"), "or get in touch with
        #     our Data Team at", tags$a(href='insight@vcsep.org.uk', target="_blank", "insight@vcsep.org.uk")),
        #   br(),
        #   h3(tags$strong('Feedback or make a request')),
        #   p("We welcome your thoughts on what data would be useful to help shape your support to those in need.
        #     To feedback, make a request, or if you have any questions please get in touch with us at", tags$a(href="insight@vcsep.org.uk", target="_blank", "insight@vcsep.org.uk")),
        #   br(),
        #   h3(tags$strong('Find out more')),
        #   p("To learn more about the work of the VCS Emergencies Partnership, visit us at", tags$a(href="https://vcsep.org.uk/", target="_blank", "vcsep.org.uk")),
        # 
        #   br(),
        #   br(),
        #   p(tags$strong(tags$i("This platform is still in the early stages of development. Some features may not work properly, but are coming soon.")), style="color:blue")
        # 
        # )
      

      # --- plot list of contributors ---
      output$members <- renderUI({
        div(
          tags$ul(tags$li(tags$a(href="https://www.bitc.org.uk/", target="_blank","Business in the Community")),
                tags$li(tags$a(href="https://www.childrenscommissioner.gov.uk/", target="_blank","Children’s Commissioner for England")),
                tags$li(tags$a(href="https://www.citizensadvice.org.uk/", target="_blank","Citizens Advice")),
                tags$li(tags$a(href="https://www.cruse.org.uk/", target="_blank","Cruse")),
                tags$li(tags$a(href="https://fareshare.org.uk/", target="_blank","FareShare")),
                tags$li(tags$a(href="https://foodfoundation.org.uk/", target="_blank","Food Foundation")),
                tags$li(tags$a(href="https://www.foodaidnetwork.org.uk/", target="_blank","Independent Food Aid Network")),
                tags$li(tags$a(href="https://www.mind.org.uk/", target="_blank","Mind")),
                tags$li(tags$a(href="https://www.re-act.org.uk/", target="_blank","RE:ACT")),
                tags$li(tags$a(href="https://www.stepchange.org/", target="_blank","Stepchange")),
                tags$li(tags$a(href="https://www.themix.org.uk/", target="_blank","The Mix")),
                tags$li(tags$a(href="https://www.turn2us.org.uk/", target="_blank","Turn2Us")),
                tags$li(tags$a(href="https://www.victimsupport.org.uk/", target="_blank","Victim Support")),
                tags$li(tags$a(href="https://volunteeringmatters.org.uk/", target="_blank","Volunteering Matters")))
        )

      })

    }

  })


  # --- observe if references tab selected ---
  observe({

    req(input$sidebar_id)

    if (input$sidebar_id == 'references') {

      output$refs <- renderUI({
        div(
          h2(tags$strong("Data contributors")),
          hr(),
          h4("We make use of a range of data sources to bring you this insight,
            including", tags$strong("data that is open source"), "as well as", tags$strong("data from our contributing partners.")),
          br(),
          h4("The bulk of the data included in the platform comes from",
            tags$a(href="https://www.ons.gov.uk/", target="_blank", "Office of National Statistics"), "and", tags$a(href="https://digital.nhs.uk/", target="_blank", "NHS Digital"), "shared under an", tags$a(href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/", target="_blank", "Open Government Licence.")),
          br(),
          h4("Although not all data has been visualised in the platform yet,
            we would also like to thank the following organisation for their contributions:"),
          br(),
          tags$ul(tags$li(tags$a(href="https://www.bitc.org.uk/", target="_blank","Business in the Community")),
                  tags$li(tags$a(href="https://www.childrenscommissioner.gov.uk/", target="_blank","Children’s Commissioner for England")),
                  tags$li(tags$a(href="https://www.citizensadvice.org.uk/", target="_blank","Citizens Advice")),
                  tags$li(tags$a(href="https://www.cruse.org.uk/", target="_blank","Cruse")),
                  tags$li(tags$a(href="https://fareshare.org.uk/", target="_blank","FareShare")),
                  tags$li(tags$a(href="https://foodfoundation.org.uk/", target="_blank","Food Foundation")),
                  tags$li(tags$a(href="https://www.foodaidnetwork.org.uk/", target="_blank","Independent Food Aid Network")),
                  tags$li(tags$a(href="https://www.mind.org.uk/", target="_blank","Mind")),
                  tags$li(tags$a(href="https://www.re-act.org.uk/", target="_blank","RE:ACT")),
                  tags$li(tags$a(href="https://www.stepchange.org/", target="_blank","Stepchange")),
                  tags$li(tags$a(href="https://www.themix.org.uk/", target="_blank","The Mix")),
                  tags$li(tags$a(href="https://www.turn2us.org.uk/", target="_blank","Turn2Us")),
                  tags$li(tags$a(href="https://www.victimsupport.org.uk/", target="_blank","Victim Support")),
                  tags$li(tags$a(href="https://volunteeringmatters.org.uk/", target="_blank","Volunteering Matters")))

        )

      })

    }
  })
  
  
  # --- help for about needs dashboard ---
  observe({
    
    req(input$sidebar_id)
    if (input$sidebar_id == 'unmetneed') {
      
      output$about_needs <- renderUI({
        div(
          hr(),
          p("This dashboard helps responders who wish to", tags$strong("target their efforts in areas of highest
            risk and least capacity to cope with an emergency."), "It also provides estimates of “People at risk”
              based on different characteristics, to support influencing and advocacy efforts around emergencies."),
          tags$br(),
          p("Users are able to", tags$strong("select the type of emergency"), "they are responding to and 
            the",  tags$strong("“Areas at risk” map"), "and", tags$strong("“Areas to focus” list"), "will update", tags$strong("to show those 
            areas least resilient to the emergency."), "For information on what indicators 
            are used to make these assessments, see the", tags$strong(tags$a(href="https://github.com/britishredcrosssociety/covid-19-vulnerability", target="_blank", 'British Red Cross Vulnerability')), 
            "and", tags$strong(tags$a(href="https://github.com/britishredcrosssociety/resilience-index", target="_blank", "Resilience")), "Indices."),
          tags$br(),
          p("As more organisations contribute their data, over time", tags$strong("we will build a better 
            understanding of “People in need” during an emergency, and where such needs may be going unmet."))
          
        )
      })
      
    }
    
  })


  # --- preparing base map ---- #


  #set up polygons
  pal <- colorFactor("viridis", c(1:5), reverse = TRUE)

  # set up the static parts of the map (that don't change as user selects different options)
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, attributionControl = T)) %>%
      setView(lat = 54.00366, lng = -2.547855, zoom = 5) %>% # centre map on Whitendale Hanging Stones, the centre of GB: https://en.wikipedia.org/wiki/Centre_points_of_the_United_Kingdom
      addProviderTiles(providers$CartoDB.Positron) %>%
      addTiles(urlTemplate = "", attribution = '2020 (c) British Red Cross')
  })


  # ---- Respond to users tactical cell ----
  filteredLA <- reactive({
    lad_uk2areas2vulnerability %>% filter(TacticalCell == input$tactical_cell)

  })

  observe({
  # ---- Adjust LAD options based on tactical cell ---
  output$secondSelection <- renderUI({
    lads2select <- unique(filteredLA()$Name)
    lads2select <- c('All local authorities in region',lads2select)
    selectInput("lad_selected", "Local Authority", choices = sort(lads2select), selected='All local authorities in region')
  })
})



  # ---- Respond to users tactical generate filtered vi table ---
  filteredVI <- reactive({

    lad_uk2areas2vulnerability %>%
      filter(TacticalCell == input$tactical_cell) %>%

      mutate(quintile2plot = case_when(
        input$vi == "Socioeconomic vulnerability" ~ `Socioeconomic Vulnerability quintile`,
        input$vi == "Clinical vulnerability" ~ `Clinical Vulnerability quintile`,
        input$vi == "Overall vulnerability" ~ `Vulnerability quintile` ,
        input$vi == "Health/Wellbeing vulnerability" ~ `Health/Wellbeing Vulnerability quintile`,
        input$vi == "Economic vulnerability" ~ `Economic Vulnerability quintile`,
        input$vi == "Social vulnerability" ~ `Social Vulnerability quintile`
      ))
  })



  filterpar_tab <- reactive({
    
    for_tc <- par_table %>% filter(TacticalCell == input$tactical_cell)

    # --- tactical cells ---
    # -- integers
    #for_tc_int <- par_table %>% filter(TacticalCell == input$tactical_cell) %>% select(`tc_int_Proportion of neighbourhoods in 20% most digitally excluded`:`tc_int_People receiving Section 95 support`) %>% mutate(`tc_int_Fuel Poor Households`= round(`tc_int_Fuel Poor Households`, 1))
    # rename stuff again
    #names(for_tc_int) = gsub(pattern = "tc_int_", replacement = "int_", x = names(for_tc_int))
    # pivot
    #tc_int <- pivot_longer(for_tc_int, cols=c(`Proportion of neighbourhoods in 20% most digitally excluded`:`People receiving Section 95 support`), names_to='Indicator', values_to='Total') %>% unique()

    # -- percentages --
    #for_tc_perc <- par_table %>% filter(TacticalCell == input$tactical_cell) %>% select(`tc_perc_Proportion of neighbourhoods in 20% most digitally excluded`:`tc_perc_People receiving Section 95 support`) %>%
    #  mutate(`tc_perc_Proportion of neighbourhoods in 20% most digitally excluded`=round(`tc_perc_Proportion of neighbourhoods in 20% most digitally excluded`*100,1))

    #names(for_tc_perc) = gsub(pattern = "tc_perc_", replacement = "perc_", x = names(for_tc_perc))

    #tc_table <-  par_table %>% filter(TacticalCell == input$tactical_cell) %>%
    #  select('TacticalCell', `tc_int_Proportion of neighbourhoods in 20% most digitally excluded`:`tc_perc_People receiving Section 95 support`) %>%
    #  mutate(`tc_int_Fuel Poor Households`= round(`tc_int_Fuel Poor Households`, 1)) %>%
    #  mutate(`tc_perc_Proportion of neighbourhoods in 20% most digitally excluded`=round(`tc_perc_Proportion of neighbourhoods in 20% most digitally excluded`*100,1)) %>%
    #    unique()

    #names(tc_table) = gsub(pattern = "tc_int_", replacement = "int_", x = names(tc_table))
    #names(tc_table) = gsub(pattern = "tc_perc_", replacement = "perc_", x = names(tc_table))
    #print(tc_table)




    #tc_perc <- pivot_longer(for_tc_perc, cols=c(`Proportion of neighbourhoods in 20% most digitally excluded`:`People receiving Section 95 support`), names_to='Indicator', values_to='Percentage (%)') %>%
    #  unique()

    #tc_int_perc <- left_join(tc_int, tc_perc, by='Indicator', keep=F)

    #no_nas_table <- tc_int_perc[!with(tc_int_perc, is.na(`Total`) & is.na(`Percentage (%)`)),]
    #print(no_nas_table)

  })


  # --- Areas to focus ----
  # -- covid
  filtered_covid_areas <- reactive({
    if(input$tactical_cell == '-- England --') {
      covid_lads_in_tc <- covid_area2focus %>% arrange(-`Vulnerability quintile`, -`covid cases per 100,000`) %>%
        select('LAD19CD','Local Authority'= Name, 'Overall vulnerability' =`Vulnerability quintile`, `covid cases per 100,000`)
      #print(covid_lads_in_tc)
    }
    else {

    lads_in_tc <- covid_area2focus %>% filter(TacticalCell == input$tactical_cell)
    # order descending by quintile and covid cases
    covid_lads_in_tc <- lads_in_tc %>% arrange(-`Vulnerability quintile`, -`covid cases per 100,000`) %>%
      select('LAD19CD','Local Authority'= Name, 'Overall vulnerability' =`Vulnerability quintile`, `covid cases per 100,000`)
    }

  })



  # --- Requests ----
  filtered_requests <- reactive({
    requests_tc <- requests %>% filter(TacticalCell==input$tactical_cell)

  })

  # --- Volunteer capacity ---
  filtered_volunteers <- reactive({
    volunteers_tc <- volunteers %>% filter(TacticalCell==input$tactical_cell)

  })



  # --- Generate Map ----
  observe({

    # which tab is selected:
    req(input$sidebar_id)
    if (input$sidebar_id == 'unmetneed') {

      # -- if showing whole of UK --
      if(input$tactical_cell == '-- England --') {

        # vulnerable colurs
        # High income, High inequality --> #3F2949
        # High income, Medium inequality --> "#435786"
        # Medium income, medium inequality --> #806A8A
        # high inequality, medium income  --> "#77324C"
        # "#3F2949" -->
        vuln_cols <- c("#77324C","#3F2949","#435786","#806A8A")

        # --- filter to just areas most in need ---
        lad_uk_most_vuln <- lad_uk2vuln_resilience %>% filter(fill %in% vuln_cols)

        # -- zoom for uk ---
        curr_bbox <- st_bbox(tc_shp)

        leafletProxy("map") %>%
          clearShapes() %>%
          #
          addPolygons(data=tc_shp, layerId = ~TacticalCell,
                      group='tactical cell boundary',
                      stroke=T,
                      weight = 0.7,
                      opacity = 0.8,
                      color = "black",
                      dashArray = "0.1",
                      fill=F) %>%

          addPolygons(data=lad_uk_most_vuln, layerId = ~LAD19CD,
                  group="Vulnerability vs Capacity to cope", fillColor = ~fill,
                  weight = 0.7,
                  opacity = 0.8,
                  color = "black",
                  dashArray = "0.1",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE,
                  ),
                  label=~lad19nm,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                  )
                ) %>%
          flyToBounds(lng1 = as.numeric(curr_bbox["xmin"]),
                      lat1 = as.numeric(curr_bbox["ymin"]),
                      lng2 = as.numeric(curr_bbox["xmax"]),
                      lat2 = as.numeric(curr_bbox["ymax"]))

              }

        else{

          if (input$lad_selected == 'All local authorities in region') {

            # get all local authorities in tc
            curr_LA <- lad_uk2vuln_resilience %>% filter(TacticalCell == input$tactical_cell)

            # filter for just those most vulnerable and least resilient
            vuln_cols <- c("#77324C","#3F2949","#435786","#806A8A")
            # --- filter to just areas most in need ---
            curr_LA <- curr_LA %>% filter(fill %in% vuln_cols)

            # --- filter tactical cell boundary ---
            curr_TC <- tc_shp %>% filter(TacticalCell == input$tactical_cell)

            # -- filter local authorith boundaries --
            curr_LA_all_boundaries <- lad_uk2vuln_resilience %>% filter(TacticalCell == input$tactical_cell)


            #calculate centres to zoom to appropiate area https://stackoverflow.com/questions/52522872/r-sf-package-centroid-within-polygon
            # get_coords <- st_bbox(curr_TC)
            # coord_polygons <- st_as_sfc(get_coords)
            # centroids = data.frame(NAME='zoom')
            # st_geometry(centroids) = coord_polygons
            #
            # centroids <- centroids %>%
            # mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
            #     lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))
            #

            # zoom to tactical cell
            curr_bbox <- st_bbox(curr_TC)

            # show on map:
            leafletProxy("map") %>%
            clearShapes() %>%
              # TC boundary
              addPolygons(data=curr_TC, layerId = ~TacticalCell,
                          group='tactical cell boundary',
                          stroke=T,
                          weight = 0.7,
                          opacity = 0.8,
                          color = "black",
                          dashArray = "0.1",
                          fill=F) %>%
              # show lad boundaries
              addPolygons(data=curr_LA_all_boundaries, layerId = ~lad19nm,
                          group='lad_boundaries',
                          stroke=T,
                          weight = 0.7,
                          opacity = 0.8,
                          color = "grey",
                          dashArray = "0.1",
                          fill=F,
                          label=~lad19nm,
                          labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto"
                          )) %>%
              # most deprived
              addPolygons(data=curr_LA, layerId = ~LAD19CD,
                    group="Vulnerability vs Resilience", fillColor = ~fill,
                    weight = 0.7,
                    opacity = 0.8,
                    color = "black",
                    dashArray = "0.1",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE
                    ),
                    label=~lad19nm,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"
                    )
                ) %>%
              flyToBounds(lng1 = as.numeric(curr_bbox["xmin"]),
                          lat1 = as.numeric(curr_bbox["ymin"]),
                          lng2 = as.numeric(curr_bbox["xmax"]),
                          lat2 = as.numeric(curr_bbox["ymax"]))
          }

          else {

            # get just local authority selected
            curr_LA <- lad_uk2vuln_resilience %>% filter(lad19nm == input$lad_selected)

            # filter for just those most vulnerable and least resilient
            #vuln_cols <- c("#77324C","#3F2949","#435786","#806A8A")
            # --- filter to just areas most in need ---
            #curr_LA <- curr_LA %>% filter(fill %in% vuln_cols)

            # --- filter tactical cell boundary ---
            curr_TC <- tc_shp %>% filter(TacticalCell == input$tactical_cell)

            # --- all lads ---
            all_LAs <- lad_uk2vuln_resilience %>% filter(TacticalCell == input$tactical_cell)

            #calculate centres to zoom to appropiate area https://stackoverflow.com/questions/52522872/r-sf-package-centroid-within-polygon
            # get_coords <- st_bbox(curr_LA)
            # coord_polygons <- st_as_sfc(get_coords)
            # centroids = data.frame(NAME='zoom')
            # st_geometry(centroids) = coord_polygons
            #
            # centroids <- centroids %>%
            #   mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
            #          lat = map_dbl(geometry, ~st_centroid(.x)[[2]]))

            # -- get bounding box of la
            curr_bbox <- st_bbox(curr_LA)


            # show on map:
            leafletProxy("map") %>%
              clearShapes() %>%
              # TC boundary
              addPolygons(data=curr_TC, layerId = ~TacticalCell,
                          group='tactical cell boundary',
                          stroke=T,
                          weight = 0.7,
                          opacity = 0.8,
                          color = "black",
                          dashArray = "0.1",
                          fill=F) %>%
              # show lad boundaries
              addPolygons(data=all_LAs, layerId = ~lad19nm,
                          group='tactical cell boundary',
                          stroke=T,
                          weight = 0.5,
                          opacity = 0.8,
                          color = "grey",
                          dashArray = "0.1",
                          fill=F) %>%
              # most deprived
              addPolygons(data=curr_LA, layerId = ~LAD19CD,
                          group="Vulnerability vs Resilience", fillColor = ~fill,
                          weight = 0.7,
                          opacity = 0.8,
                          color = "black",
                          dashArray = "0.1",
                          fillOpacity = 0.7,
                          highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE
                          ),
                          label=~lad19nm,
                          labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto"
                          )
                       ) %>%
                      flyToBounds(lng1 = as.numeric(curr_bbox["xmin"]),
                          lat1 = as.numeric(curr_bbox["ymin"]),
                          lng2 = as.numeric(curr_bbox["xmax"]),
                          lat2 = as.numeric(curr_bbox["ymax"]))


          }


    }
  }
})



  #Use a separate observer to recreate the legend as needed.
  # observe({
  #   leafletProxy("map", data = lad_uk2areas2vulnerability) %>%
  #     clearControls() %>%
  #     addLegend(
  #       position = "topright",
  #       pal = pal,
  #       values = ~`Socioeconomic Vulnerability quintile`,
  #       title = paste0("<b>", 'vulnerability score', "</b></br>", '(5 = worst)', "<br/>"),
  #       opacity = 0.8
  #     )
  # })

  # ------- People at Risk table -------
  # --- Store click ---
  #data <- reactiveValues(clickedShape=NULL)

  observe({

    # --- Which tab is selected ---
    req(input$sidebar_id)
    if (input$sidebar_id == 'unmetneed') {
      # -- this will get tactical cell table --
      curr_table <- filterpar_tab()
      print(curr_table)

      # -- Don't have stats for the whole uk at the moment --
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
                 'eng_total_homeless',
                 'proprotion_homeless',
                 'eng_total_unemployed_on_ucred',
                 'prop_eng_pop_unemployed_on_ucred',
                 'total_shielding_eng',
                 'proportion_total_shielding_Eng') 
        
        # --- England BAME stats ---
        bame_to_plot  <- eng_table %>% select('england_proportion_bame') %>%
          pivot_longer(`england_proportion_bame`, names_to = "Indicator", values_to = "proportion")
        
        bame_to_plot <- bame_to_plot %>% filter(!is.na(proportion)) %>%
          unique() %>% mutate('proportion'=round(proportion,0))

        bame_to_show <- paste0(round(bame_to_plot$proportion,0), "%")
        
        output$bame_population_text <- renderUI({
          div(style= " text-align: center;",
            hr(),
            p(bame_to_show, tags$br(),
            "of the population are BAME"
          )
          )

        })
        
        output$bame_population <- renderEcharts4r({
        # # Plot population statistics
        bame <- bame_to_plot %>%
              e_charts(x = Indicator) %>%
              e_bar(proportion, bar_width=0.1, showBackground=T) %>%
              e_labels(position = "right", color='black') %>%
              e_color(c('purple')) %>%
              #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
              #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
              #e_mark_line(data=eng_avg_bame, symbol = "none", lineStyle = list(color = "black")) %>%
              e_hide_grid_lines() %>%
              e_flip_coords() %>%
              e_grid(containLabel = TRUE, left=30, right=30, top=5, bottom=0, height='60%') %>%

              #e_rm_axis(axis="x") %>%
              #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
              e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
              e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
              e_y_axis(show=F) %>%
              e_legend(FALSE)

                  })

        # --- Section 95 Support ---
        eng_sec95_to_write <- eng_table %>% select('eng_people_recieving_section_95_support','prop_eng_receiving_section_95_support') %>%
          unique()
        
        eng_sec95_to_plot <- eng_sec95_to_write %>% select('prop_eng_receiving_section_95_support') %>%
          pivot_longer('prop_eng_receiving_section_95_support', names_to = "Indicator", values_to = "proportion")
        
        eng_sec95_to_plot <- eng_sec95_to_plot %>% filter(!is.na(proportion)) %>%
          unique()
        
        eng_sec95_to_write <- eng_sec95_to_write %>% filter(!is.na(eng_people_recieving_section_95_support) & !is.na('prop_eng_receiving_section_95_support'))
        
        write_eng_sec95 <- paste0("(",eng_sec95_to_write$prop_eng_receiving_section_95_support,"% of the population)")
        
        output$section95_text <- renderUI({
          div(style= " text-align: center;",
              hr(),
              p(format(eng_sec95_to_write$eng_people_recieving_section_95_support, big.mark=',', scientific = F), tags$br(),
                "people receiving Section 95 support")
              #p(tags$strong('No. of people receiving Section 95 support:'), format(eng_sec95_to_write$eng_people_recieving_section_95_support, big.mark=',', scientific = F), "people", tags$br(), write_eng_sec95)
          )
        })
        
        output$section95 <- renderEcharts4r({
                      # # Plot population statistics
                      sec95 <- eng_sec95_to_plot %>%
                        e_charts(x = Indicator) %>%
                        e_bar(proportion, bar_width=0.1,showBackground=T) %>%
                        e_labels(position = "right", color='black') %>%
                        e_color(c('purple')) %>%
                        #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
                        #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
                        #e_mark_line(data=eng_avg_section95, symbol = "none", lineStyle = list(color = "black")) %>%
                        e_hide_grid_lines() %>%
                        e_flip_coords() %>%
                        e_grid(containLabel = TRUE, left=30, right=30, top=5, bottom=0, height='60%') %>%

                        #e_rm_axis(axis="x") %>%
                        #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
                        e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
                        e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
                        e_y_axis(show=F) %>%
                        e_legend(FALSE)

                    })
        
        # --- homelessness ---
        eng_homeless_to_write <- eng_table %>% select('eng_total_homeless','proprotion_homeless') %>%
          unique()
        
        eng_homeless_to_plot <- eng_homeless_to_write %>% select('proprotion_homeless') %>%
          pivot_longer('proprotion_homeless', names_to = "Indicator", values_to = "proportion")
        
        eng_homeless_to_plot <- eng_homeless_to_plot %>% filter(!is.na(proportion)) %>%
          unique()
        
        eng_homeless_to_write <- eng_homeless_to_write %>% filter(!is.na(eng_total_homeless) & !is.na('proprotion_homeless'))
        
        write_eng_homeless <- paste0("(",eng_homeless_to_write$proprotion_homeless,"% of the population)")
        

        output$homeless_text <- renderUI({
          div(style= " text-align: center;",
              hr(),
              p(format(eng_homeless_to_write$eng_total_homeless, big.mark=',', scientific = F), tags$br(),
                'homeless people')
              #p(tags$strong('No. of people homeless:'), format(eng_homeless_to_write$eng_total_homeless, big.mark=',', scientific = F), "people", tags$br(), write_eng_homeless),
          )
        })
        
        output$homeless <- renderEcharts4r({
          # # Plot population statistics
          sec95 <- eng_homeless_to_plot %>%
            e_charts(x = Indicator) %>%
            e_bar(proportion, bar_width=0.1,showBackground=T) %>%
            e_labels(position = "right", color='black') %>%
            e_color(c('purple')) %>%
            #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
            #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
            #e_mark_line(data=eng_avg_section95, symbol = "none", lineStyle = list(color = "black")) %>%
            e_hide_grid_lines() %>%
            e_flip_coords() %>%
            e_grid(containLabel = TRUE, left=30, right=30, top=5, bottom=0, height='60%') %>%
            
            #e_rm_axis(axis="x") %>%
            #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
            e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
            e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
            e_y_axis(show=F) %>%
            e_legend(FALSE)
          
        })
        
        
        # --- fuel poverty ---
        
        eng_fuelp_to_write <- eng_table %>% select('eng_total_fuel_poor_households','eng_prop_households_fuel_poor') %>%
          unique()
        
        eng_fuelp_to_plot <- eng_fuelp_to_write %>% select('eng_prop_households_fuel_poor') %>%
          pivot_longer('eng_prop_households_fuel_poor', names_to = "Indicator", values_to = "proportion")
        
        eng_fuelp_to_plot <- eng_fuelp_to_plot %>% filter(!is.na(proportion)) %>%
          unique() %>%
          mutate('proportion'=round(proportion,0))
        
        eng_fuelp_to_write <- eng_fuelp_to_write %>% filter(!is.na(eng_total_fuel_poor_households) & !is.na('eng_prop_households_fuel_poor'))
        
        write_eng_fuelp <- paste0("(",eng_fuelp_to_write$eng_prop_households_fuel_poor,"% of households)")
        

        output$fuelp_text <- renderUI({
          div(style= " text-align: center;",
              hr(),
              p(format(eng_fuelp_to_write$eng_total_fuel_poor_households, big.mark=',', scientific = F),
                tags$br(),
                "households in fuel poverty"
              )
              #p(tags$strong('No. of households in fuel poverty:'), format(eng_fuelp_to_write$eng_total_fuel_poor_households, big.mark=',', scientific = F), "households", tags$br(), write_eng_fuelp),
          )
        })
        
        output$fuelp <- renderEcharts4r({
          # # Plot population statistics
          fuelp_t <- eng_fuelp_to_plot %>%
            e_charts(x = Indicator) %>%
            e_bar(proportion, bar_width=0.1,showBackground=T) %>%
            e_labels(position = "right", color='black') %>%
            e_color(c('purple')) %>%
            #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
            #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
            #e_mark_line(data=eng_avg_section95, symbol = "none", lineStyle = list(color = "black")) %>%
            e_hide_grid_lines() %>%
            e_flip_coords() %>%
            e_grid(containLabel = TRUE, left=30, right=30, top=5, bottom=0, height='60%') %>%
            
            #e_rm_axis(axis="x") %>%
            #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
            e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
            e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
            e_y_axis(show=F) %>%
            e_legend(FALSE)
          
        })

        # ---- unemployed ---
        
        eng_unem_to_write <- eng_table %>% select('eng_total_unemployed_on_ucred','prop_eng_pop_unemployed_on_ucred') %>%
          unique()
        
        eng_unem_to_plot <- eng_unem_to_write %>% select('prop_eng_pop_unemployed_on_ucred') %>%
          pivot_longer('prop_eng_pop_unemployed_on_ucred', names_to = "Indicator", values_to = "proportion")
        
        eng_unem_to_plot <- eng_unem_to_plot %>% filter(!is.na(proportion)) %>%
          unique() %>%
          mutate('proportion'=round(proportion,0))
        
        eng_unem_to_write <- eng_unem_to_write %>% filter(!is.na(eng_total_unemployed_on_ucred) & !is.na('prop_eng_pop_unemployed_on_ucred'))
        
        write_eng_unem <- paste0("(",eng_unem_to_write$prop_eng_pop_unemployed_on_ucred,"% of people)")
        
        
        
        output$unemployment_text <- renderUI({
          div(style= " text-align: center;",
              hr(),
              p(format(eng_unem_to_write$eng_total_unemployed_on_ucred, big.mark=',', scientific = F),
                tags$br(),
                "people unemployed on universal credit"
              )
              #p(tags$strong('No. of people unemployed receiving universal credit:'), format(eng_unem_to_write$eng_total_unemployed_on_ucred, big.mark=',', scientific = F), "people", tags$br(), write_eng_unem)
          )
        })
        
        output$unemployment <- renderEcharts4r({
          # # Plot population statistics
          unem_t <- eng_unem_to_plot %>%
            e_charts(x = Indicator) %>%
            e_bar(proportion, bar_width=0.1,showBackground=T) %>%
            e_labels(position = "right", color='black') %>%
            e_color(c('purple')) %>%
            #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
            #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
            #e_mark_line(data=eng_avg_section95, symbol = "none", lineStyle = list(color = "black")) %>%
            e_hide_grid_lines() %>%
            e_flip_coords() %>%
            e_grid(containLabel = TRUE, left=30, right=30, top=5, bottom=0, height='60%') %>%
            
            #e_rm_axis(axis="x") %>%
            #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
            e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
            e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
            e_y_axis(show=F) %>%
            e_legend(FALSE)
          
        })

        output$digital_text <- renderUI({
          div(style= " text-align: center;",
              hr(),
              p(tags$strong('Data not currently available at national level for digital exclusion'))
          )
        })
        
        # clear plot nothing
        output$digital <- renderEcharts4r({
          
        })
        
        # --- people shielding ---
        
        eng_shielding_to_write <- eng_table %>% select('total_shielding_eng','proportion_total_shielding_Eng') %>%
          unique()
        
        eng_shielding_to_plot <- eng_shielding_to_write %>% select('proportion_total_shielding_Eng') %>%
          pivot_longer('proportion_total_shielding_Eng', names_to = "Indicator", values_to = "proportion")
        
        eng_shielding_to_plot <- eng_shielding_to_plot %>% filter(!is.na(proportion)) %>%
          unique()
        
        eng_shielding_to_write <- eng_shielding_to_write %>% filter(!is.na(total_shielding_eng) & !is.na('proportion_total_shielding_Eng'))
        
        write_eng_shielding <- paste0("(",eng_shielding_to_write$proportion_total_shielding_Eng,"% of people)")
        
        

        output$shielding_text <- renderUI({
          div(style= " text-align: center;",
              hr(),
              p(format(eng_shielding_to_write$total_shielding_eng, big.mark=',', scientific = F),
                tags$br(),
                "people clinically extremely vulnerable")
              #p(tags$strong('No. of people clinically extremely vulnerable:'), format(eng_shielding_to_write$total_shielding_eng, big.mark=',', scientific = F), "people", tags$br(), write_eng_shielding)
          )
        })
        
        output$shielding_f <- renderEcharts4r({
          # # Plot population statistics
          shielding_t <- eng_shielding_to_plot %>%
            e_charts(x = Indicator) %>%
            e_bar(proportion, bar_width=0.1,showBackground=T) %>%
            e_labels(position = "right", color='black') %>%
            e_color(c('purple')) %>%
            #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
            #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
            #e_mark_line(data=eng_avg_section95, symbol = "none", lineStyle = list(color = "black")) %>%
            e_hide_grid_lines() %>%
            e_flip_coords() %>%
            e_grid(containLabel = TRUE, left=30, right=30, top=5, bottom=0, height='60%') %>%
            
            #e_rm_axis(axis="x") %>%
            #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
            e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
            e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
            e_y_axis(show=F) %>%
            e_legend(FALSE)
          
        })


        #

        #output$people_at_risk <- DT::renderDataTable({
        #  DT::datatable(curr_table,
                      #caption = htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:black; font-size:125% ;',title_needed),
        #              options = list(
        #                paging=FALSE
        #              ))
        #    })
          }

      else {
        # summary for tactical cell
        if (input$lad_selected == 'All local authorities in region') {
          #Tactical cell
          tc = input$tactical_cell
          title_needed <- paste0('People at risk in Tactical Cell: ', tc)
          print('tactical cell test')

          # --- people at risk ----

          # --- population demographics ---
          #bame <- curr_table %>% select('TacticalCell',`int_Fuel Poor Households`,`perc_Fuel Poor Households`)
          bame_to_plot <- curr_table %>% select(`tc_proportion`)

          # transpose dataframe
          bame_to_plot  <- bame_to_plot %>% pivot_longer(`tc_proportion`, names_to = "Indicator", values_to = "proportion") %>%
            unique() %>% filter(!is.na(proportion)) %>% 
            mutate('proportion'=round(proportion,0))

          # for echarts
          tc_avg_bame <- par_table_tc_avg %>% select(`tc_proportion`) %>%
            mutate(`tc_proportion`=round(`tc_proportion`,0)) %>%
            select('xAxis' = `tc_proportion`) %>%
            as.list()
          
          # to format percentage
          bame_to_show <- paste0(round(bame_to_plot$proportion,0), "%")

          # label to sho
          label_to_show <- paste(round(par_table_tc_avg$`tc_proportion`,0), '%', '\n','(regional avg)')
          
          output$bame_population_text <- renderUI({
            div(style= " text-align: center;",
                hr(),
                p(bame_to_show,
                  tags$br(),
                  'of the population are BAME')
                #p(tags$strong('Proportion of population who are BAME:'), bame_to_show)
                
            )
          })
          
          output$bame_population <- renderEcharts4r({
            # # Plot population statistics
            bame <- bame_to_plot %>%
              e_charts(x = Indicator) %>%
              e_bar(proportion, bar_width=0.1, showBackground=T) %>%
              e_labels(position = "right", color='black') %>%
              e_color(c('purple')) %>%
              #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
              #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
              e_mark_line(data=tc_avg_bame, symbol = "none", lineStyle = list(color = "black"), title=label_to_show, label=list(formatter='label',fontSize=10)) %>%
              e_hide_grid_lines() %>%
              e_flip_coords() %>%
              e_grid(containLabel = TRUE, left=30, right=30, top=15, bottom=0, height='60%') %>%
              
              #e_rm_axis(axis="x") %>%
              #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
              e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
              e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
              e_y_axis(show=F) %>%
              e_legend(FALSE)
            
          })

          # --- section 95 support ----
          tc_sec95_to_write <- curr_table %>% select(`tc_People receiving Section 95 support`,'tc_prop_people_recieving_section_95_support') %>%
            unique()
          
          tc_sec95_to_plot <- tc_sec95_to_write %>% select('tc_prop_people_recieving_section_95_support') %>%
            pivot_longer('tc_prop_people_recieving_section_95_support', names_to = "Indicator", values_to = "proportion")
          
          tc_sec95_to_plot <- tc_sec95_to_plot %>% filter(!is.na(proportion)) %>%
            unique()
          
          tc_sec95_to_write <- tc_sec95_to_write %>% filter(!is.na(`tc_People receiving Section 95 support`) & !is.na('tc_prop_people_recieving_section_95_support'))
  
          write_tc_sec95 <- paste0("(",tc_sec95_to_write$tc_prop_people_recieving_section_95_support,"% of the population)")
  
          # for echarts
          tc_avg_section95 <- par_table_tc_avg %>% select(`tc_prop_people_recieving_section_95_support`) %>%
            select('xAxis' = `tc_prop_people_recieving_section_95_support`) %>%
            as.list()
          
          tc_sec95_for_avg = paste0(par_table_tc_avg$tc_prop_people_recieving_section_95_support, '%', '\n','(regional avg)')
                  
          output$section95_text <- renderUI({
            div(style= " text-align: center;",
                hr(),
                p(format(tc_sec95_to_write$`tc_People receiving Section 95 support`, big.mark=',', scientific = F),
                  tags$br(),
                  'people receiving Section 95 support')
                #p(tags$strong('No. of people receiving Section 95 support:'), format(tc_sec95_to_write$`tc_People receiving Section 95 support`, big.mark=',', scientific = F), "people", tags$br(), write_tc_sec95)
            )
          })
          
          output$section95 <- renderEcharts4r({
            # # Plot population statistics
            sec95 <- tc_sec95_to_plot %>%
              e_charts(x = Indicator) %>%
              e_bar(proportion, bar_width=0.1,showBackground=T) %>%
              e_labels(position = "right", color='black') %>%
              e_color(c('purple')) %>%
              #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
              #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
              e_mark_line(data=tc_avg_section95, symbol = "none", lineStyle = list(color = "black"), title=tc_sec95_for_avg, label=list(formatter='label',fontSize=10)) %>%
              e_hide_grid_lines() %>%
              e_flip_coords() %>%
              e_grid(containLabel = TRUE, left=30, right=30, top=15, bottom=0, height='60%') %>%
              
              #e_rm_axis(axis="x") %>%
              #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
              e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
              e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
              e_y_axis(show=F) %>%
              e_legend(FALSE)
            
          })
          
          
          # --- homeless ----
          tc_homeless_to_write <- curr_table %>% select(`tc_total_homeless`,'tc_prop_homeless') %>%
            unique()
          
          tc_homeless_to_plot <- tc_homeless_to_write %>% select('tc_prop_homeless') %>%
            pivot_longer('tc_prop_homeless', names_to = "Indicator", values_to = "proportion")
          
          tc_homeless_to_plot <- tc_homeless_to_plot %>% filter(!is.na(proportion)) %>%
            unique() %>% mutate('proportion'=round(proportion,1))
          
          tc_homeless_to_write <- tc_homeless_to_write %>% filter(!is.na(`tc_total_homeless`) & !is.na('tc_prop_homeless'))
          
          write_tc_homeless <- paste0("(",tc_homeless_to_write$tc_prop_homeless,"% of the population)")
          
          # for echarts
          tc_avg_homeless <- par_table_tc_avg %>% select(`tc_prop_homeless`) %>%
            select('xAxis' = `tc_prop_homeless`) %>%
            as.list()
          print(tc_avg_homeless)
          tc_homeless_for_avg = paste0(par_table_tc_avg$tc_prop_homeless, '%', '\n','(regional avg)')
          
          output$homeless_text <- renderUI({
            div(style= " text-align: center;",
                hr(),
                p(format(tc_homeless_to_write$tc_total_homeless, big.mark=',', scientific = F),
                  tags$br(),
                  "homeless people")
                #p(tags$strong('No. of homeless people:'), format(tc_homeless_to_write$tc_total_homeless, big.mark=',', scientific = F), "people", tags$br(), write_tc_homeless)
            )
          })
          
          output$homeless <- renderEcharts4r({
            # # Plot population statistics
            sec95 <- tc_homeless_to_plot %>%
              e_charts(x = Indicator) %>%
              e_bar(proportion, bar_width=0.1,showBackground=T) %>%
              e_labels(position = "right", color='black') %>%
              e_color(c('purple')) %>%
              #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
              #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
              e_mark_line(data=tc_avg_homeless, symbol = "none", lineStyle = list(color = "black"), title=tc_homeless_for_avg, label=list(formatter='label',fontSize=10)) %>%
              e_hide_grid_lines() %>%
              e_flip_coords() %>%
              e_grid(containLabel = TRUE, left=30, right=30, top=15, bottom=0, height='60%') %>%
              
              #e_rm_axis(axis="x") %>%
              #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
              e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
              e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
              e_y_axis(show=F) %>%
              e_legend(FALSE)
            
          })
          

#           # --- fuel_poverty ---
          
          tc_fuelp_to_write <- curr_table %>% select(`tc_Number of households in fuel poverty1`,'tc_prop_households_fuel_poor') %>%
            unique()
          
          tc_fuelp_to_plot <- tc_fuelp_to_write %>% select('tc_prop_households_fuel_poor') %>%
            pivot_longer('tc_prop_households_fuel_poor', names_to = "Indicator", values_to = "proportion") 
          
          tc_fuelp_to_plot <- tc_fuelp_to_plot %>% filter(!is.na(proportion)) %>%
            unique() %>%
            mutate('proportion'=round(proportion,0))
          
          tc_fuelp_to_write <- tc_fuelp_to_write %>% 
            filter(!is.na(`tc_Number of households in fuel poverty1`) & !is.na('tc_prop_households_fuel_poor')) %>%
            mutate(`tc_Number of households in fuel poverty1`=round(`tc_Number of households in fuel poverty1`,0))
          
          write_tc_fuelp <- paste0("(",tc_fuelp_to_write$tc_prop_households_fuel_poor,"% of households)")
          
          # for echarts
          tc_avg_fuelp <- par_table_tc_avg %>% select(`tc_prop_households_fuel_poor`) %>%
            mutate(`tc_prop_households_fuel_poor`=round(`tc_prop_households_fuel_poor`,0)) %>%
            select('xAxis' = `tc_prop_households_fuel_poor`) %>%
            as.list()
          
          tc_fuelp_for_avg = paste0(round(par_table_tc_avg$tc_prop_households_fuel_poor,0), '%', '\n','(regional avg)')
          
          output$fuelp_text <- renderUI({
            div(style= " text-align: center;",
                hr(),
                p(format(tc_fuelp_to_write$`tc_Number of households in fuel poverty1`, big.mark=',', scientific = F),
                  tags$br(),
                  'households in fuel poverty')
                #p(tags$strong('No. of households in fuel poverty:'), format(tc_fuelp_to_write$`tc_Number of households in fuel poverty1`, big.mark=',', scientific = F), "households", tags$br(), write_tc_fuelp)
            )
          })
          
          output$fuelp <- renderEcharts4r({
            # # Plot population statistics
            sec95 <- tc_fuelp_to_plot %>%
              e_charts(x = Indicator) %>%
              e_bar(proportion, bar_width=0.1,showBackground=T) %>%
              e_labels(position = "right", color='black') %>%
              e_color(c('purple')) %>%
              #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
              #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
              e_mark_line(data=tc_avg_fuelp, symbol = "none", lineStyle = list(color = "black"), title=tc_fuelp_for_avg, label=list(formatter='label',fontSize=10)) %>%
              e_hide_grid_lines() %>%
              e_flip_coords() %>%
              e_grid(containLabel = TRUE, left=30, right=30, top=15, bottom=0, height='60%') %>%
              
              #e_rm_axis(axis="x") %>%
              #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
              e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
              e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
              e_y_axis(show=F) %>%
              e_legend(FALSE)
            
          })

#           # --- Unemployment ---
          tc_unem_to_write <- curr_table %>% select(`tc_Not in employment`,'tc_prop_unemployed_on_universal_credit') %>%
            unique()
          
          tc_unem_to_plot <- tc_unem_to_write %>% select('tc_prop_unemployed_on_universal_credit') %>%
            pivot_longer('tc_prop_unemployed_on_universal_credit', names_to = "Indicator", values_to = "proportion") 
          
          tc_unem_to_plot <- tc_unem_to_plot %>% filter(!is.na(proportion)) %>%
            unique() %>%
            mutate('proportion'=round(proportion,0))
          
          tc_unem_to_write <- tc_unem_to_write %>% filter(!is.na(`tc_Not in employment`) & !is.na('tc_prop_unemployed_on_universal_credit'))
          
          write_tc_unem <- paste0("(",tc_unem_to_write$tc_prop_unemployed_on_universal_credit,"% of people)")
          
          # for echarts
          tc_avg_unem <- par_table_tc_avg %>% select(`tc_prop_unemployed_on_universal_credit`) %>%
            select('xAxis' = `tc_prop_unemployed_on_universal_credit`) %>%
            as.list()
          
          tc_unem_for_avg = paste0(round(par_table_tc_avg$tc_prop_unemployed_on_universal_credit,0), '%', '\n','(regional avg)')
          
          output$unemployment_text <- renderUI({
            div(style= " text-align: center;",
                hr(),
                p(format(tc_unem_to_write$`tc_Not in employment`, big.mark=',', scientific = F),
                  tags$br(),
                  "people unemployed on universal credit")
                #p(tags$strong('No. of people unemployed receiving universal credit:'), format(tc_unem_to_write$`tc_Not in employment`, big.mark=',', scientific = F), "people", tags$br(), write_tc_unem)
            )
          })
          
          output$unemployment <- renderEcharts4r({
            # # Plot population statistics
            sec95 <- tc_unem_to_plot %>%
              e_charts(x = Indicator) %>%
              e_bar(proportion, bar_width=0.1,showBackground=T) %>%
              e_labels(position = "right", color='black') %>%
              e_color(c('purple')) %>%
              #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
              #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
              e_mark_line(data=tc_avg_unem, symbol = "none", lineStyle = list(color = "black"), title=tc_unem_for_avg,label=list(formatter='label',fontSize=10)) %>%
              e_hide_grid_lines() %>%
              e_flip_coords() %>%
              e_grid(containLabel = TRUE, left=30, right=30, top=15, bottom=0, height='60%') %>%
              
              #e_rm_axis(axis="x") %>%
              #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
              e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
              e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
              e_y_axis(show=F) %>%
              e_legend(FALSE)
            
          })
          
          
          # --- Digital exclusion ---
          tc_de_to_write <- curr_table %>% select('tc_percent_digitally_excluded') %>%
            unique()
          
          tc_de_to_plot <- tc_de_to_write %>% select('tc_percent_digitally_excluded') %>%
            pivot_longer('tc_percent_digitally_excluded', names_to = "Indicator", values_to = "proportion")
          
          tc_de_to_plot <- tc_de_to_plot %>% filter(!is.na(proportion)) %>%
            unique() %>%
            mutate('proportion'=round(proportion,0))
          
          tc_de_to_write <- tc_de_to_write %>% filter(!is.na(tc_percent_digitally_excluded))
          
          write_tc_de <- paste0(round(tc_de_to_write$tc_percent_digitally_excluded,0), "%")
          
         
          # for echarts
          tc_avg_de <- par_table_tc_avg %>% select('tc_percent_digitally_excluded') %>%
            #mutate('tc_percent_digitally_excluded'=round(tc_percent_digitally_excluded,0))
            select('xAxis' = `tc_percent_digitally_excluded`) %>%
            mutate('xAxis' = round(xAxis,0)) %>%
            as.list()
          
          tc_de_for_avg = paste0(round(par_table_tc_avg$tc_percent_digitally_excluded,0), '%', '\n','(regional avg)')
          
          output$digital_text <- renderUI({
            div(style= " text-align: center;",
                hr(),
                p(write_tc_de,
                  tags$br(),
                  "neighbourhoods in 20% most digitally excluded")
                #p(tags$strong('Proportion of neighbourhoods in the 20% most digitally excluded:'), write_tc_de)
            )
          })
          
          output$digital <- renderEcharts4r({
            # # Plot population statistics
            sec95 <- tc_de_to_plot %>%
              e_charts(x = Indicator) %>%
              e_bar(proportion, bar_width=0.1,showBackground=T) %>%
              e_labels(position = "right", color='black') %>%
              e_color(c('purple')) %>%
              #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
              #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
              e_mark_line(data=tc_avg_de, symbol = "none", lineStyle = list(color = "black"), title=tc_de_for_avg, label=list(formatter='label',fontSize=10)) %>%
              e_hide_grid_lines() %>%
              e_flip_coords() %>%
              e_grid(containLabel = TRUE, left=30, right=30, top=15, bottom=0, height='60%') %>%
              
              #e_rm_axis(axis="x") %>%
              #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
              e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
              e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
              e_y_axis(show=F) %>%
              e_legend(FALSE)
            
          })
          

          # --- Shielding ---
          tc_shielding_to_write <- curr_table %>% select(`tc_Clinically extremely vulnerable`,`tc_Clinically vulnerable proportion of population`) %>%
            unique()
          
          tc_shielding_to_plot <- tc_shielding_to_write %>% select(`tc_Clinically vulnerable proportion of population`) %>%
            pivot_longer(`tc_Clinically vulnerable proportion of population`, names_to = "Indicator", values_to = "proportion")
          
          tc_shielding_to_plot <- tc_shielding_to_plot %>% filter(!is.na(proportion)) %>%
            unique() %>%
            mutate('proportion'=round(proportion,0))
          
          tc_shielding_to_write <- tc_shielding_to_write %>% filter(!is.na(`tc_Clinically extremely vulnerable`) & !is.na(`tc_Clinically vulnerable proportion of population`))
          
          write_tc_shielding <- paste0("(",tc_shielding_to_write$`tc_Clinically vulnerable proportion of population`,"% of the population)")
          
          # for echarts
          tc_avg_shielding <- par_table_tc_avg %>% 
            select(`tc_Clinically vulnerable proportion of population`) %>%
            select('xAxis' = `tc_Clinically vulnerable proportion of population`) %>%
            mutate('xAxis' = round(xAxis,0)) %>%
            as.list()
          
         
          tc_shielding_for_avg = paste0(round(par_table_tc_avg$`tc_Clinically vulnerable proportion of population`,0), '%', '\n','(regional avg)')
          
          output$shielding_text <- renderUI({
            div(style= " text-align: center;",
                hr(),
                p(format(tc_shielding_to_write$`tc_Clinically extremely vulnerable`, big.mark=',', scientific = F),
                  tags$br(),
                  'people clinically extremely vulnerable')
                #p(tags$strong('No. of people clinically extremely vulnerable:'), format(tc_shielding_to_write$`tc_Clinically extremely vulnerable`, big.mark=',', scientific = F), "people", tags$br(), write_tc_shielding)
            )
          })
          
          output$shielding_f <- renderEcharts4r({
            # # Plot population statistics
            sec95 <- tc_shielding_to_plot %>%
              e_charts(x = Indicator) %>%
              e_bar(proportion, bar_width=0.1,showBackground=T) %>%
              e_labels(position = "right", color='black') %>%
              e_color(c('purple')) %>%
              #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
              #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
              e_mark_line(data=tc_avg_shielding, symbol = "none", lineStyle = list(color = "black"), title=tc_shielding_for_avg, label=list(formatter='label',fontSize=10)) %>%
              e_hide_grid_lines() %>%
              e_flip_coords() %>%
              e_grid(containLabel = TRUE, left=30, right=30, top=15, bottom=0, height='60%') %>%
              
              #e_rm_axis(axis="x") %>%
              #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
              e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
              e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
              e_y_axis(show=F) %>%
              e_legend(FALSE)
            
          })
          

         }
  
        # -------------------------- #
        # -- just local authority -- #
        # -------------------------- #
        else {
          print('lad section')
          
          lad_of_interest <- lad_uk2areas2vulnerability %>% filter(Name == input$lad_selected) %>% select('LAD19CD') %>% st_drop_geometry()
          print(lad_of_interest$LAD19CD)
          
          # --- population demographics ---
          #bame <- curr_table %>% select('TacticalCell',`int_Fuel Poor Households`,`perc_Fuel Poor Households`)
          lad_bame_to_plot <- curr_table %>% select('LAD19CD',`Percentage of population who are ethnic minority`) %>%
            filter(LAD19CD == lad_of_interest$LAD19CD)
          
          
          # transpose dataframe
          lad_bame_to_plot  <- lad_bame_to_plot %>% pivot_longer(`Percentage of population who are ethnic minority`, names_to = "Indicator", values_to = "proportion") %>%
            unique() %>% filter(!is.na(proportion)) %>%
            mutate('proportion'=round(proportion,0))
          
          #print(lad_bame_to_plot)
          
          # for echarts
          lad_avg_bame <- par_table_lad_avg %>% select(`Percentage of population who are ethnic minority`) %>%
            select('xAxis' = `Percentage of population who are ethnic minority`) %>%
            mutate('xAxis'= round(xAxis,0)) %>%
            as.list()
          
          # to format percentage
          lad_bame_to_show <- paste0(round(lad_bame_to_plot$proportion,0), "%")
          
          # label to sho
          lad_label_to_show <- paste0(round(par_table_lad_avg$`Percentage of population who are ethnic minority`,0), '%', '\n','(eng avg)')
          
        
          
          if (dim(lad_bame_to_plot)[1] != 0) {
          
          output$bame_population_text <- renderUI({
            div(style= "text-align: center;",
                hr(),
                p(lad_bame_to_show,
                  tags$br(),
                  'of the population are BAME')
                #p(tags$strong('Proportion of population who are BAME:'), lad_bame_to_show)
                
            )
          })
          
          output$bame_population <- renderEcharts4r({
            # # Plot population statistics
            bame <- lad_bame_to_plot %>%
              e_charts(x = Indicator) %>%
              e_bar(proportion, bar_width=0.1, showBackground=T, label=list(show=T, color='black', position='right')) %>%
              #e_labels(position = "right", color='black') %>%
              e_color(c('purple')) %>%
              #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
              #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
              e_mark_line(data=lad_avg_bame, symbol = "none", lineStyle = list(color = "black"), title=lad_label_to_show, label=list(formatter='label',fontSize=10)) %>%
              e_hide_grid_lines() %>%
              e_flip_coords() %>%
              e_grid(containLabel = TRUE, left=30, right=30, top=15, bottom=0, height='60%') %>%
              
              #e_rm_axis(axis="x") %>%
              #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
              e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
              e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
              e_y_axis(show=F) %>%
              e_legend(FALSE)
            
          })
          
          }
          
          else {
            
            lad_bame_to_show <- "Data unavailable"
            
            # -- no data --
            output$bame_population_text <- renderUI({
              div(style= "text-align: center;",
                  hr(),
                  p(lad_bame_to_show,
                    tags$br(),
                    'for % of the population who are BAME')
                  #p(tags$strong('Proportion of population who are BAME:'), lad_bame_to_show)
                  
              )
            })
            
            output$bame_population <- renderEcharts4r({
              # remove other plot but don't plot anything
            })
            
          }
          
          # --- section 95 support ----
          lad_sec95_to_write <- curr_table %>% select('LAD19CD',`People receiving Section 95 support`,'lad_prop_recieving_section_95_support') %>%
            filter(LAD19CD == lad_of_interest$LAD19CD) %>%
            unique()
          
          lad_sec95_to_plot <- lad_sec95_to_write %>% select('lad_prop_recieving_section_95_support') %>%
            pivot_longer('lad_prop_recieving_section_95_support', names_to = "Indicator", values_to = "proportion")
          
          lad_sec95_to_plot <- lad_sec95_to_plot %>% filter(!is.na(proportion)) %>%
            unique() 
          
          lad_sec95_to_write <- lad_sec95_to_write %>% filter(!is.na(`People receiving Section 95 support`) & !is.na('lad_prop_recieving_section_95_support'))
          
          write_lad_sec95 <- paste0("(",lad_sec95_to_write$lad_prop_recieving_section_95_support,"% of the population)")
          
          # for echarts
          lad_avg_section95 <- par_table_lad_avg %>% select(`lad_prop_recieving_section_95_support`) %>%
            select('xAxis' = `lad_prop_recieving_section_95_support`) %>%
            as.list()
          
          lad_sec95_for_avg = paste0(round(par_table_lad_avg$lad_prop_recieving_section_95_support,2), '%', '\n','(eng avg)')
          
          if(dim(lad_sec95_to_plot)[1] != 0) {
          
          output$section95_text <- renderUI({
            div(style= " text-align: center;",
                hr(),
                p(format(lad_sec95_to_write$`People receiving Section 95 support`, big.mark=',', scientific = F),
                  tags$br(),
                  'people receiving Section 95 support')
                #p(tags$strong('No. of people receiving Section 95 support:'), format(lad_sec95_to_write$`People receiving Section 95 support`, big.mark=',', scientific = F), "people", tags$br(), write_lad_sec95)
            )
          })
          
          output$section95 <- renderEcharts4r({
            # # Plot population statistics
            sec95 <- lad_sec95_to_plot %>%
              e_charts(x = Indicator) %>%
              e_bar(proportion, bar_width=0.1,showBackground=T) %>%
              e_labels(position = "right", color='black') %>%
              e_color(c('purple')) %>%
              #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
              #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
              e_mark_line(data=lad_avg_section95, symbol = "none", lineStyle = list(color = "black"), title=lad_sec95_for_avg,label=list(formatter='label',fontSize=10)) %>%
              e_hide_grid_lines() %>%
              e_flip_coords() %>%
              e_grid(containLabel = TRUE, left=30, right=30, top=15, bottom=0, height='60%') %>%
              
              #e_rm_axis(axis="x") %>%
              #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
              e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
              e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
              e_y_axis(show=F) %>%
              e_legend(FALSE)
            
          })
          }
          
          else {
            output$section95_text <- renderUI({
              div(style= " text-align: center;",
                  hr(),
                  p("Data unavailable",
                    tags$br(),
                    'for people recieving Section 95 support')
                  #p(tags$strong('No. of people receiving Section 95 support:'), format(lad_sec95_to_write$`People receiving Section 95 support`, big.mark=',', scientific = F), "people", tags$br(), write_lad_sec95)
              )
            })
            
            output$section95 <- renderEcharts4r({
              
            })
            
          }
          
          
          # --- homeless ----
          lad_homeless_to_write <- curr_table %>% select('LAD19CD',`lad_total_homeless`,'lad_prop_homeless') %>%
            filter(LAD19CD == lad_of_interest$LAD19CD) %>%
            unique()
          
          lad_homeless_to_plot <- lad_homeless_to_write %>% select('lad_prop_homeless') %>%
            pivot_longer('lad_prop_homeless', names_to = "Indicator", values_to = "proportion")
          
          lad_homeless_to_plot <- lad_homeless_to_plot %>% filter(!is.na(proportion)) %>%
            unique()
          
          lad_homeless_to_write <- lad_homeless_to_write %>% filter(!is.na(`lad_total_homeless`) & !is.na('lad_prop_homeless'))
          
          write_lad_homeless <- paste0("(",lad_homeless_to_write$lad_prop_homeless,"% of the population)")
          
          # for echarts
          lad_avg_homeless <- par_table_lad_avg %>% select(`lad_prop_homeless`) %>%
            select('xAxis' = `lad_prop_homeless`) %>%
            as.list()
          
          lad_homeless_for_avg = paste0(round(par_table_lad_avg$lad_prop_homeless,2), '%', '\n','(eng avg)')
          
          
          if(dim(lad_homeless_to_plot)[1] != 0) {
          
          output$homeless_text <- renderUI({
            div(style= " text-align: center;",
                hr(),
                p(format(lad_homeless_to_write$lad_total_homeless, big.mark=',', scientific = F),
                  tags$br(),
                  'homeless people')
                #p(tags$strong('No. of homeless people:'), format(lad_homeless_to_write$lad_total_homeless, big.mark=',', scientific = F), "people", tags$br(), write_lad_homeless)
            )
          })
          
          output$homeless <- renderEcharts4r({
            # # Plot population statistics
            sec95 <- lad_homeless_to_plot %>%
              e_charts(x = Indicator) %>%
              e_bar(proportion, bar_width=0.1,showBackground=T) %>%
              e_labels(position = "right", color='black') %>%
              e_color(c('purple')) %>%
              #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
              #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
              e_mark_line(data=lad_avg_homeless, symbol = "none", lineStyle = list(color = "black"), title=lad_homeless_for_avg, label=list(formatter='label',fontSize=10)) %>%
              e_hide_grid_lines() %>%
              e_flip_coords() %>%
              e_grid(containLabel = TRUE, left=30, right=30, top=15, bottom=0, height='60%') %>%
              
              #e_rm_axis(axis="x") %>%
              #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
              e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
              e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
              e_y_axis(show=F) %>%
              e_legend(FALSE)
            
          })
          
          }
          
          else {
            
            output$homeless_text <- renderUI({
              div(style= " text-align: center;",
                  hr(),
                  p("Data unavailable",
                    tags$br(),
                    'for homeless people')
                  #p(tags$strong('No. of homeless people:'), format(lad_homeless_to_write$lad_total_homeless, big.mark=',', scientific = F), "people", tags$br(), write_lad_homeless)
              )
            })
            
            output$homeless <- renderEcharts4r({
          
            })
            
          }
          
          # --- fuel_poverty ---
          
          lad_fuelp_to_write <- curr_table %>% select('LAD19CD',`Number of households in fuel poverty1`,`Proportion of households fuel poor (%)`) %>%
            filter(LAD19CD == lad_of_interest$LAD19CD) %>%
            unique()
          
          lad_fuelp_to_plot <- lad_fuelp_to_write %>% select(`Proportion of households fuel poor (%)`) %>%
            pivot_longer(`Proportion of households fuel poor (%)`, names_to = "Indicator", values_to = "proportion")
          
          lad_fuelp_to_plot <- lad_fuelp_to_plot %>% filter(!is.na(proportion)) %>%
            unique() %>%
            mutate('proportion'=round(proportion,0))
          
          
          lad_fuelp_to_write <- lad_fuelp_to_write %>% filter(!is.na(`Number of households in fuel poverty1`) & !is.na(`Proportion of households fuel poor (%)`))
            
          
          write_lad_fuelp <- paste0("(",lad_fuelp_to_write$`Proportion of households fuel poor (%)`,"% of households)")
          
          # for echarts
          lad_avg_fuelp <- par_table_lad_avg %>% select(`Proportion of households fuel poor (%)`) %>%
            select('xAxis' = `Proportion of households fuel poor (%)`) %>%
            mutate('xAxis'=round(xAxis,0)) %>%
            as.list()
          
          lad_fuelp_for_avg = paste0(round(par_table_lad_avg$`Proportion of households fuel poor (%)`,0), '%', '\n','(eng avg)')
          
          if (dim(lad_fuelp_to_plot)[1] != 0) {
          output$fuelp_text <- renderUI({
            div(style= " text-align: center;",
                hr(),
                p(format(round(lad_fuelp_to_write$`Number of households in fuel poverty1`,0), big.mark=',', scientific = F),
                  tags$br(),
                  "households in fuel poverty")
                #p(tags$strong('No. of households in fuel poverty:'), format(lad_fuelp_to_write$`Number of households in fuel poverty1`, big.mark=',', scientific = F), "households", tags$br(), write_lad_fuelp)
            )
          })
          
          output$fuelp <- renderEcharts4r({
            # # Plot population statistics
            lad_fuelp <-  lad_fuelp_to_plot %>%
              e_charts(x = Indicator) %>%
              e_bar(proportion, bar_width=0.1,showBackground=T) %>%
              e_labels(position = "right", color='black') %>%
              e_color(c('purple')) %>%
              #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
              #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
              e_mark_line(data=lad_avg_fuelp, symbol = "none", lineStyle = list(color = "black"), title=lad_fuelp_for_avg, label=list(formatter='label',fontSize=10)) %>%
              e_hide_grid_lines() %>%
              e_flip_coords() %>%
              e_grid(containLabel = TRUE, left=30, right=30, top=15, bottom=0, height='60%') %>%
              
              #e_rm_axis(axis="x") %>%
              #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
              e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
              e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
              e_y_axis(show=F) %>%
              e_legend(FALSE)
            
          })
          }
          
          else {
            
            output$fuelp_text <- renderUI({
              div(style= " text-align: center;",
                  hr(),
                  p("Data unavailable",
                    tags$br(),
                    "for households in fuel poverty")
                  #p(tags$strong('No. of households in fuel poverty:'), format(lad_fuelp_to_write$`Number of households in fuel poverty1`, big.mark=',', scientific = F), "households", tags$br(), write_lad_fuelp)
              )
            })
            
            output$fuelp <- renderEcharts4r({
              
            })
            
          }
          
          #--- Unemployment ---
          lad_unem_to_write <- curr_table %>% select('LAD19CD',`Not in employment`,'lad_prop_unemployed_on_ucred') %>%
            filter(LAD19CD == lad_of_interest$LAD19CD) %>%
            unique()
          
          lad_unem_to_plot <- lad_unem_to_write %>% select('lad_prop_unemployed_on_ucred') %>%
            pivot_longer('lad_prop_unemployed_on_ucred', names_to = "Indicator", values_to = "proportion")
          
          lad_unem_to_plot <- lad_unem_to_plot %>% filter(!is.na(proportion)) %>%
            unique() %>%
            mutate('proportion'=round(proportion,0))
          
          lad_unem_to_write <- lad_unem_to_write %>% filter(!is.na(`Not in employment`) & !is.na('lad_prop_unemployed_on_ucred'))
          
          write_lad_unem <- paste0("(",lad_unem_to_write$lad_prop_unemployed_on_ucred,"% of people)")
          
          # for echarts
          lad_avg_unem <- par_table_lad_avg %>% select(`lad_prop_unemployed_on_ucred`) %>%
            select('xAxis' = `lad_prop_unemployed_on_ucred`) %>%
            mutate('xAxis'=round(xAxis,0)) %>%
            as.list()
          
          lad_unem_for_avg = paste0(round(par_table_lad_avg$lad_prop_unemployed_on_ucred,0), '%', '\n','(eng avg)')
          
          if (dim(lad_unem_to_plot)[1] != 0) {
          
          output$unemployment_text <- renderUI({
            div(style= " text-align: center;",
                hr(),
                p(format(lad_unem_to_write$`Not in employment`, big.mark=',', scientific = F),
                  tags$br(),
                  'people unemployed on universal credit')
                #p(tags$strong('No. of people unemployed receiving universal credit:'), format(lad_unem_to_write$`Not in employment`, big.mark=',', scientific = F), "people", tags$br(), write_lad_unem)
            )
          })
          
          output$unemployment <- renderEcharts4r({
            # # Plot population statistics
            lad_unem <- lad_unem_to_plot %>%
              e_charts(x = Indicator) %>%
              e_bar(proportion, bar_width=0.1,showBackground=T) %>%
              e_labels(position = "right", color='black') %>%
              e_color(c('purple')) %>%
              #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
              #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
              e_mark_line(data=lad_avg_unem, symbol = "none", lineStyle = list(color = "black"), title=lad_unem_for_avg, label=list(formatter='label',fontSize=10)) %>%
              e_hide_grid_lines() %>%
              e_flip_coords() %>%
              e_grid(containLabel = TRUE, left=30, right=30, top=15, bottom=0, height='60%') %>%
              
              #e_rm_axis(axis="x") %>%
              #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
              e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
              e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
              e_y_axis(show=F) %>%
              e_legend(FALSE)
            
          })
          
          }
          
          else {
            output$unemployment_text <- renderUI({
              div(style= " text-align: center;",
                  hr(),
                  p('Data unavailable',
                    tags$br(),
                    'for people unemployed on universal credit')
                  #p(tags$strong('No. of people unemployed receiving universal credit:'), format(lad_unem_to_write$`Not in employment`, big.mark=',', scientific = F), "people", tags$br(), write_lad_unem)
              )
            })
            
            output$unemployment <- renderEcharts4r({
              
            })
          }
          
          # --- Digital exclusion ---
          lad_de_to_write <- curr_table %>% select('LAD19CD','percent_digitally_excluded') %>%
            filter(LAD19CD == lad_of_interest$LAD19CD) %>%
            unique()
          
          lad_de_to_plot <- lad_de_to_write %>% select('percent_digitally_excluded') %>%
            pivot_longer('percent_digitally_excluded', names_to = "Indicator", values_to = "proportion")
          
          lad_de_to_plot <- lad_de_to_plot %>% filter(!is.na(proportion)) %>%
            unique() %>% 
            mutate('proportion'=round(proportion,0))
          
          lad_de_to_write <- lad_de_to_write %>% filter(!is.na('percent_digitally_excluded'))
          
          write_lad_de <- paste0(round(lad_de_to_write$percent_digitally_excluded,0), "%")
          
          # for echarts
          lad_avg_de <- par_table_lad_avg %>% select(`percent_digitally_excluded`) %>%
            select('xAxis' = `percent_digitally_excluded`) %>%
            mutate('xAxis'=round(xAxis,0)) %>%
            as.list()
          
          lad_de_for_avg = paste0(round(par_table_lad_avg$percent_digitally_excluded,0), '%', '\n','(eng avg)')
        
          if (dim(lad_de_to_plot)[1] != 0) {
          
          output$digital_text <- renderUI({
            div(style= " text-align: center;",
                hr(),
                p(write_lad_de,
                  tags$br(),
                  'neighbourhoods in the 20% most digitally excluded')
                #p(tags$strong('Proportion of neighbourhoods in the 20% most digitally excluded:'), write_lad_de)
            )
          })
          
          output$digital <- renderEcharts4r({
            # # Plot population statistics
            lad_de <- lad_de_to_plot %>%
              e_charts(x = Indicator) %>%
              e_bar(proportion, bar_width=0.1,showBackground=T) %>%
              e_labels(position = "right", color='black') %>%
              e_color(c('purple')) %>%
              #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
              #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
              e_mark_line(data=lad_avg_de, symbol = "none", lineStyle = list(color = "black"), title=lad_de_for_avg, label=list(formatter='label',fontSize=10)) %>%
              e_hide_grid_lines() %>%
              e_flip_coords() %>%
              e_grid(containLabel = TRUE, left=30, right=30, top=15, bottom=0, height='60%') %>%
              
              #e_rm_axis(axis="x") %>%
              #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
              e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
              e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
              e_y_axis(show=F) %>%
              e_legend(FALSE)
            
          })
          
          }
          
          else{
            output$digital_text <- renderUI({
              div(style= " text-align: center;",
                  hr(),
                  p("Data unavailable",
                    tags$br(),
                    'for neighbourhoods in the 20% most digitally excluded')
                  #p(tags$strong('Proportion of neighbourhoods in the 20% most digitally excluded:'), write_lad_de)
              )
            })
            
            output$digital <- renderEcharts4r({
              
            })
          }
          
          
          # --- Shielding ---
          lad_shielding_to_write <- curr_table %>% select('LAD19CD',`Clinically extremely vulnerable`,`Proportion Clinically extremely vulnerable`) %>%
            filter(LAD19CD == lad_of_interest$LAD19CD) %>%
            unique()
          
          lad_shielding_to_plot <- lad_shielding_to_write %>% select(`Proportion Clinically extremely vulnerable`) %>%
            pivot_longer(`Proportion Clinically extremely vulnerable`, names_to = "Indicator", values_to = "proportion")
          
          lad_shielding_to_plot <- lad_shielding_to_plot %>% filter(!is.na(proportion)) %>%
            unique() %>%
            mutate('proportion'=round(proportion,0))
          
          lad_shielding_to_write <- lad_shielding_to_write %>% filter(!is.na(`Clinically extremely vulnerable`) & !is.na(`Proportion Clinically extremely vulnerable`))
          
          write_lad_shielding <- paste0("(",lad_shielding_to_write$`Proportion Clinically extremely vulnerable`,"% of the population)")
          
          # for echarts
          lad_avg_shielding <- par_table_lad_avg %>% select(`Proportion Clinically extremely vulnerable`) %>%
            select('xAxis' = `Proportion Clinically extremely vulnerable`) %>%
            mutate('xAxis'=round(xAxis,0)) %>%
            as.list()
          
          lad_sheilding_for_avg = paste0(round(par_table_lad_avg$`Proportion Clinically extremely vulnerable`,0), '%', '\n','(eng avg)')
          
          if (dim(lad_shielding_to_plot)[1] != 0) {
          
          output$shielding_text <- renderUI({
            div(style= " text-align: center;",
                hr(),
                p(format(lad_shielding_to_write$`Clinically extremely vulnerable`, big.mark=',', scientific = F),
                  tags$br(),
                  'people clinically extremely vulnerable')
                #p(tags$strong('No. of people clinically extremely vulnerable:'), format(lad_shielding_to_write$`Clinically extremely vulnerable`, big.mark=',', scientific = F), "people", tags$br(), write_lad_shielding)
            )
          })
          
          output$shielding_f <- renderEcharts4r({
            # # Plot population statistics
            sec95 <- lad_shielding_to_plot %>%
              e_charts(x = Indicator) %>%
              e_bar(proportion, bar_width=0.1,showBackground=T) %>%
              e_labels(position = "right", color='black') %>%
              e_color(c('purple')) %>%
              #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
              #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
              e_mark_line(data=lad_avg_shielding, symbol = "none", lineStyle = list(color = "black"), title=lad_sheilding_for_avg, label=list(formatter='label',fontSize=10)) %>%
              e_hide_grid_lines() %>%
              e_flip_coords() %>%
              e_grid(containLabel = TRUE, left=30, right=30, top=15, bottom=0, height='60%') %>%
              
              #e_rm_axis(axis="x") %>%
              #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
              e_x_axis(position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=2),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
              e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
              e_y_axis(show=F) %>%
              e_legend(FALSE)
            
          })
          
          }
          
          else {
            output$shielding_text <- renderUI({
              div(style= " text-align: center;",
                  hr(),
                  p('Data unavailable',
                    tags$br(),
                    'for people clinically extremely vulnerable')
                  #p(tags$strong('No. of people clinically extremely vulnerable:'), format(lad_shielding_to_write$`Clinically extremely vulnerable`, big.mark=',', scientific = F), "people", tags$br(), write_lad_shielding)
              )
            })
            
            output$shielding_f <- renderEcharts4r({
            })
          }
      
        }
      }
    }
})

  # # --- Filter People at Risk table on map click ---
  # # --- Do something with click ----
  # # https://stackoverflow.com/questions/59342680/can-i-use-in-r-the-leaflet-map-shape-click-event-to-populate-a-box-with-a-da
  # observeEvent(input$map_shape_click, {
  #
  #   data$clickedShape <- input$map_shape_click
  #
  #   #capture the info of the clicked polygon
  #   click <- input$map_shape_click
  #
  #   if(!is.null(click$id)){
  #
  #   # ------ subset your table with the id of the clicked polygon ----
  #   # --- look up local authority code ---
  #   lad_of_interest <- lad_uk2areas2vulnerability %>% filter(Name == click$id) %>% select('LAD19CD') %>% st_drop_geometry()
  #
  #   for_lad_int <- par_table %>% filter(LAD19CD == lad_of_interest$LAD19CD) %>% select(`LAD_int_Proportion of neighbourhoods in 20% most digitally excluded`:`LAD_int_People receiving Section 95 support`)  %>% mutate(`LAD_int_Fuel Poor Households` = round(`LAD_int_Fuel Poor Households`, 1))
  #   print(for_lad_int)
  #   # rename stuff again
  #   names(for_lad_int) = gsub(pattern = "LAD_int_", replacement = "", x = names(for_lad_int))
  #   # pivot
  #   lad_int <- pivot_longer(for_lad_int, cols=c(`Proportion of neighbourhoods in 20% most digitally excluded`:`People receiving Section 95 support`), names_to='Indicator', values_to='Total') %>% unique()
  #   print(lad_int)
  #   for_lad_perc <- par_table %>% filter(LAD19CD == lad_of_interest$LAD19CD) %>% select(`LAD_perc_Proportion of neighbourhoods in 20% most digitally excluded`:`LAD_perc_People receiving Section 95 support`) %>% mutate(`LAD_perc_Proportion of neighbourhoods in 20% most digitally excluded` = round(`LAD_perc_Proportion of neighbourhoods in 20% most digitally excluded`, 3))
  #   names(for_lad_perc) = gsub(pattern = "LAD_perc_", replacement = "", x = names(for_lad_perc))
  #   lad_perc <- pivot_longer(for_lad_perc, cols=c(`Proportion of neighbourhoods in 20% most digitally excluded`:`People receiving Section 95 support`), names_to='Indicator', values_to='Percentage (%)') %>%
  #     unique()
  #
  #   lad_int_perc <- left_join(lad_int, lad_perc, by='Indicator', keep=F)
  #
  #   no_nas_lad_table <- lad_int_perc[!with(lad_int_perc, is.na(`Total`) & is.na(`Percentage (%)`)),]
  #   print(no_nas_lad_table)
  #
  #   # LAD title
  #   lad_name <- click$id
  #   title_needed <- paste0("People at risk in LAD:", lad_name)
  #
  #   output$people_at_risk = DT::renderDataTable({
  #       DT::datatable(no_nas_lad_table,
  #                     caption = htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:black; font-size:125% ;',title_needed),
  #                     options = list(
  #                       paging=FALSE))
  #     })
  #
  #   }
  #
  # })
  #
  # # - change back to tactical cell level if clicks off the shape
  # observeEvent(input$map_click,{
  #   data$clickedShape <- NULL
  #
  #   #Tactical cell
  #   tc = input$tactical_cell
  #   title_needed <- paste0('People at risk in Tactical Cell:',tc)
  #
  #   curr_table <- filterpar_tab()
  #   # --- set up data table output ----
  #   output$people_at_risk <- DT::renderDataTable({
  #     DT::datatable(curr_table,
  #                   caption = htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:black; font-size:125% ;',title_needed),
  #                   options = list(
  #                     paging=FALSE))
  #
  #   })
  #
  # })
  #

  # --- Areas to focus ----
  observe({

    # --- If tab selected ---
    req(input$sidebar_id)
    if (input$sidebar_id == 'unmetneed') {

      # what theme is being investigated
      if (input$theme == 'Covid-19') {
        # get current covid data for selected tactical cell - done in reactive -
        curr_covid_list <-filtered_covid_areas()
        
        # get volunteer data (not filtered by tactical cell)
        volunteers_available <- volunteers

        # join data to volunteers
        covid_cases2volunteers <- left_join(curr_covid_list, volunteers_available, by='LAD19CD', keep=F) %>%
          mutate('Volunteer capacity' = case_when(mean_score <= 1.5 ~ 'High',
                                                mean_score >= 2.5 ~ 'Low',
                                                (mean_score >1.5 & mean_score < 2.5) ~ 'Medium',
                                                is.na(mean_score) ~ 'Data unavailable')) %>%
        select('Local Authority', 'Overall vulnerability', 'Volunteer capacity', 'Score'=mean_score, `covid cases per 100,000`)

        print(covid_cases2volunteers)
        # - order
        covid_cases2volunteers <- covid_cases2volunteers %>% arrange(-`Overall vulnerability`, -`covid cases per 100,000`, -Score) %>%
          select(-Score) %>% rename(`Volunteer presence`=`Volunteer capacity`) %>%
          mutate_at(vars(`covid cases per 100,000`), replace_na, 'NA') %>%
          # renaming coivd cases to show week - hate format
          rename_at(vars(`covid cases per 100,000`), ~ paste0(covid_week, .))
         
        
        
        # append covid week to column name


       # -- if want to show whole of the UK --
        if ( input$tactical_cell == '-- England --') {
        # all lads in tcs wanted
        output$areas2focus <- DT::renderDataTable({
         DT::datatable(covid_cases2volunteers, filter=list(position='top'),
                   options = list(dom='tp', #should remove top search box the p includes paging
                     paging = T,
                     pageLength=5,
                     scrollX=T,
                     scrollY='200px',
                     autoWidth = TRUE,
                     initComplete = htmlwidgets::JS(
                       "function(settings, json) {",
                       paste0("$(this.api().table().container()).css({'font-size':'12px'});"),
                       "}")
                   ))
        
          #%>%#formatStyle(columns=colnames(covid_cases2volunteers), lineHeight='80%')
              })

            }

          else {
            # show just tactical cell
            if (input$theme == 'Covid-19' & input$lad_selected == 'All local authorities in region') {
              output$areas2focus <- DT::renderDataTable({
                DT::datatable(covid_cases2volunteers, filter=list(position='top'),
                              options = list(dom='tp', #should remove top search box the p includes paging
                                             paging = T,
                                             pageLength=5,
                                             scrollX=T,
                                             scrollY='200px',
                                             autoWidth = TRUE,
                                             initComplete = htmlwidgets::JS(
                                               "function(settings, json) {",
                                               paste0("$(this.api().table().container()).css({'font-size':'12px'});"),
                                               "}")
                              )) #%>%
                
                #formatStyle(columns=colnames(covid_cases2volunteers), lineHeight='80%')
              })
                }
              # move la to top
            else {
              show_at_top <- as.vector(input$lad_selected)
              print(covid_cases2volunteers)
              wanted <- covid_cases2volunteers$`Local Authority` %in% show_at_top
              lad_covid_cases2volunteers <- rbind(covid_cases2volunteers[wanted,], covid_cases2volunteers[!wanted,])

              output$areas2focus <- DT::renderDataTable({
                DT::datatable(lad_covid_cases2volunteers, filter=list(position='top'),
                          options = list(dom='tp', #should remove top search box the p includes paging
                                         paging = T,
                                         pageLength=5,
                                         scrollX=T,
                                         scrollY='200px',
                                         autoWidth = TRUE,
                                         initComplete = htmlwidgets::JS(
                                           "function(settings, json) {",
                                           paste0("$(this.api().table().container()).css({'font-size':'12px'});"),
                                           "}")
                          ))  %>%
                formatStyle('Local Authority',
                          target='row',
                          backgroundColor = styleEqual(c(input$lad_selected), c('yellow')))
                  })
              }
            }
        }
      }
  })


  # --- show selected LAD at the top ----



  # # ---- volunteer capacity areas to focus ----
  #
  # observe({
  #   volunteers_available <- filtered_volunteers()
  #   vulnerability <- filteredVI()
  #   vulnerability <- vulnerability %>% select('LAD19CD', 'Name',`Vulnerability quintile`)
  #
  #
  #   # - join to vulnerability
  #   volunteers2vulnerability <- left_join(vulnerability, volunteers_available, by='LAD19CD', keep=F) %>%
  #     st_drop_geometry() %>%
  #     rename('Overall Vulnerability' = `Vulnerability quintile`) %>%
  #     mutate('Volunteer capacity' = case_when(mean_score <= 1.5 ~ 'High',
  #               mean_score >= 2.5 ~ 'Low',
  #               (mean_score >1.5 & mean_score < 2.5) ~ 'Medium',
  #               is.na(mean_score) ~ 'Data unavailable')) %>%
  #     select('Name', 'Overall Vulnerability', 'Volunteer capacity', 'Score'=mean_score)
  #
  #   # - order
  #   volunteers2vulnerability <- volunteers2vulnerability %>% arrange(-`Overall Vulnerability`, -Score)
  #
  #   output$volunteers <- DT::renderDataTable({
  #     DT::datatable(volunteers2vulnerability,
  #                   options = list(
  #                  paging =FALSE
  #                 ))
  # })
  #
  # })



  # ---- VCS INDICATORS -----

  # --- observe requests ---
  observe({

    # --- which tab is selected ---
    req(input$sidebar_id)
    if (input$sidebar_id == 'unmetneed') {

      requests_status <- filtered_requests()

      if(is.null(input$lad_selected)) {
        output$requests <- renderInfoBox({
          infoBox(
            "Requests",
              color = "navy", fill = TRUE
             )
            })
          }

      else {

         if (input$tactical_cell == '-- England --') {

            # tactical cell total
            total_requests_this_week <- requests %>%
            mutate('last_week_requests_no_na' = replace_na(total_requests_last_week, 0)) %>%
            mutate(total_this_week = sum(last_week_requests_no_na)) %>%
            select('TacticalCell','total_this_week') %>%
            unique()

            total_requests_week_previous <- requests %>%
            mutate('previous_week_requests_no_na' = replace_na(total_requests_previous_week, 0)) %>%
            mutate(total_previous_week = sum(previous_week_requests_no_na, na.rm=T)) %>%
            select('TacticalCell','total_previous_week') %>%
            unique()


            difference <- as.numeric(total_requests_this_week$total_this_week) - as.numeric(total_requests_week_previous$total_previous_week)
            difference <- sprintf("%+3d", difference)

            to_print <- paste("Previous 7 days:", total_requests_this_week$total_this_week, "Difference to last week:",difference, sep="\n")

            # because it sums for each tactical cell need to do unique
            #print(to_print)

            output$requests <- renderInfoBox({
            infoBox(
              "Requests", unique(to_print),
                color = "olive", fill = TRUE
              )
            })
           }

        else {

          if (input$lad_selected == 'All local authorities in region') {

          # tactical cell total
          total_requests_this_week <- requests_status %>%
            mutate('last_week_requests_no_na' = replace_na(total_requests_last_week, 0)) %>%
            mutate(total_this_week = sum(last_week_requests_no_na)) %>%
            select('TacticalCell','total_this_week') %>%
            unique()

          total_requests_week_previous <- requests_status %>%
            mutate('previous_week_requests_no_na' = replace_na(total_requests_previous_week, 0)) %>%
            mutate(total_previous_week = sum(previous_week_requests_no_na, na.rm=T)) %>%
            select('TacticalCell','total_previous_week') %>%
            unique()


          difference <- as.numeric(total_requests_this_week$total_this_week) - as.numeric(total_requests_week_previous$total_previous_week)
          difference <- sprintf("%+3d", difference)

          to_print <- paste("Previous 7 days:", total_requests_this_week$total_this_week, "Difference to last week:",difference, sep="\n")

          output$requests <- renderInfoBox({
          infoBox(
            "Requests", to_print,
            color = "olive", fill = TRUE
                )
          })

          }

        else {
          # look up lad name
          lad_name <- lad_uk2areas2vulnerability %>% filter(Name == input$lad_selected) %>% select('LAD19CD') %>% unique()

          # plot lad level
          lad_requests <- requests_status %>% filter(LAD19CD==lad_name$LAD19CD)

          # tactical cell total
          total_requests_this_week <- lad_requests %>%
           mutate('last_week_requests_no_na' = replace_na(total_requests_last_week, 0)) %>%
            mutate(total_this_week = sum(last_week_requests_no_na)) %>%
            select('TacticalCell','total_this_week') %>%
            unique()

          total_requests_week_previous <- lad_requests %>%
            mutate('previous_week_requests_no_na' = replace_na(total_requests_previous_week, 0)) %>%
            mutate(total_previous_week = sum(previous_week_requests_no_na, na.rm=T)) %>%
            select('TacticalCell','total_previous_week') %>%
            unique()

          difference <- as.numeric(total_requests_this_week$total_this_week) - as.numeric(total_requests_week_previous$total_previous_week)
          difference <- sprintf("%+3d", difference)

          to_print <- paste("Previous 7 days:", total_requests_this_week$total_this_week, "Difference to last week:",difference, sep="\n")

          output$requests <- renderInfoBox({
            infoBox(
              "Requests", to_print,
              color = "olive", fill = TRUE
                )
            })
          }
       }
    }
  }

})

# --- Volunteer capacity ---
  observe({

    req(input$sidebar_id)
    if (input$sidebar_id == 'unmetneed') {


      volunteer_capacity <- filtered_volunteers()

      if(is.null(input$lad_selected)) {
        output$vols <- renderInfoBox({
         infoBox(
            "Volunteer Capacity",
            color = "navy", fill = TRUE
            )
          })
        }

      else {

        if (input$tactical_cell == '-- England --') {

          avg_score <- volunteers %>%
            mutate(avg_over_area = round(mean(mean_score, na.rm=TRUE),1)) %>% select('avg_over_area') %>%
            unique() %>% mutate(colour = case_when(avg_over_area <= 1.5 ~ 'green',
                                                 avg_over_area >= 2.5 ~ 'red',
                                                 (avg_over_area >1.5 & avg_over_area < 2.5) ~ 'orange',
                                                 is.na(avg_over_area) ~ 'navy')) %>%
            mutate(to_print = case_when(is.na(avg_over_area) ~ 'No data available',
                                      TRUE ~ as.character(.$avg_over_area))) %>%
            mutate('Volunteer capacity' = case_when(avg_over_area <= 1.5 ~ 'High',
                                                  avg_over_area >= 2.5 ~ 'Low',
                                                  (avg_over_area >1.5 & avg_over_area < 2.5) ~ 'Medium',
                                                  is.na(avg_over_area) ~ 'Data unavailable'))



          output$vols <- renderInfoBox({
            infoBox(
              "Volunteer presence", avg_score$`Volunteer capacity`,
              color = avg_score$colour, fill = TRUE
                )
              })
            }

        else {

        # -- Tactical cell level --
          if (input$lad_selected == 'All local authorities in region') {

            #print(volunteer_capacity)

            # -- calculate average of others
            avg_score <- volunteer_capacity %>%
            mutate(avg_over_area = round(mean(mean_score, na.rm=TRUE),1)) %>% select('avg_over_area') %>%
            unique() %>% mutate(colour = case_when(avg_over_area <= 1.5 ~ 'green',
                                                 avg_over_area >= 2.5 ~ 'red',
                                                 (avg_over_area >1.5 & avg_over_area < 2.5) ~ 'orange',
                                                 is.na(avg_over_area) ~ 'navy')) %>%
            mutate(to_print = case_when(is.na(avg_over_area) ~ 'No data available',
                                      TRUE ~ as.character(.$avg_over_area))) %>%
            mutate('Volunteer capacity' = case_when(avg_over_area <= 1.5 ~ 'High',
                                                  avg_over_area >= 2.5 ~ 'Low',
                                                  (avg_over_area >1.5 & avg_over_area < 2.5) ~ 'Medium',
                                                  is.na(avg_over_area) ~ 'Data unavailable'))



            output$vols <- renderInfoBox({
            infoBox(
            "Volunteer presence", avg_score$`Volunteer capacity`,
              color = avg_score$colour, fill = TRUE
                )
              })
             }

          else {
          # --- lad level ---

            # look up lad name
            lad_name <- lad_uk2areas2vulnerability %>% filter(Name == input$lad_selected) %>% select('LAD19CD') %>% unique()

            # plot lad level
            lad_volunteers <- volunteer_capacity %>% filter(LAD19CD==lad_name$LAD19CD)

            #print(lad_volunteers)

            # -- calculate average of others
            avg_score <- lad_volunteers %>%
             mutate(colour = case_when(mean_score <= 1.5 ~ 'green',
                                                 mean_score >= 2.5 ~ 'red',
                                                 (mean_score >1.5 & mean_score < 2.5) ~ 'orange',
                                    is.na(mean_score) ~ 'navy')) %>%
              mutate(to_print = case_when(is.na(mean_score) ~ 'No data available',
                                      TRUE ~ as.character(.$mean_score))) %>%
              mutate('Volunteer capacity' = case_when(mean_score <= 1.5 ~ 'High',
                                                  mean_score >= 2.5 ~ 'Low',
                                                  (mean_score >1.5 & mean_score < 2.5) ~ 'Medium',
                                                  is.na(mean_score) ~ 'Data unavailable'))


            #print(avg_score)

              output$vols <- renderInfoBox({
                infoBox(
                  "Volunteer presence", avg_score$`Volunteer capacity`,
                    color = avg_score$colour, fill = TRUE
                  )
                })
              }
            }
        }
    }
  })


  # --- pulse survey ---
  observe({
    req(input$sidebar_id)
    if (input$sidebar_id == 'unmetneed') {

      output$pulse <- renderInfoBox({
      infoBox(
        "Pulse", 'Coming Soon',
        color = "navy", fill = TRUE
        )
      })
    }
  })
}


shinyApp(ui, server)
