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
tactical_cells <- c('--All UK--', tactical_cells)

#vuln_cols <- c("#77324C","#3F2949","#435786","#806A8A")

# --- filter to just areas most in need ---
#test <- LA_res %>% filter(fill %in% vuln_cols) %>% select(`LAD19NM`,`Vulnerability quintile`,`Capacity quintile`,`vuln_quantiles`,)

# --- Metadata ----
# --- people at risk data ---
par_table <- read_csv('data/people_at_risk/people_at_risk_table.csv')

# calculate eng averages
par_table_lad_avg <- par_table %>%
  summarise(across(c(`LAD_int_Proportion of neighbourhoods in 20% most digitally excluded`:`LAD_perc_People receiving Section 95 support`), ~ mean(.x, na.rm=T))) %>%
  #rename columns
  mutate('LAD19CD'='England avg') %>%
  mutate('TacticalCell'='England') %>%
  select('LAD19CD', 'TacticalCell', everything())

# add england avg to par table as a row
par_table_tc_avg <- par_table %>% select(`tc_int_Proportion of neighbourhoods in 20% most digitally excluded`:`tc_perc_People receiving Section 95 support`) %>%
  unique() %>%
  mutate(`tc_perc_Proportion of neighbourhoods in 20% most digitally excluded`=round(`tc_perc_Proportion of neighbourhoods in 20% most digitally excluded`*100,1)) %>%
  summarise(across(c(`tc_int_Proportion of neighbourhoods in 20% most digitally excluded`:`tc_perc_People receiving Section 95 support`), ~ mean(.x, na.rm=T))) %>%
  #rename columns
  mutate('LAD19CD'='TC avg') %>%
  mutate('TacticalCell'='TC avg') %>%
  select('LAD19CD', 'TacticalCell', everything())

# --- areas to focus
covid_area2focus <- read_csv('data/areas_to_focus/areas2focus_covid.csv')
# -- vcs indicators
requests <- read_csv('data/vcs_indicators/requests_this_week_and_last.csv')
volunteers <- read_csv('data/vcs_indicators/volunteer-capacity-lad19CD-tc.csv')




# ---  dashboard --- #
# --- header --- #
header <- dashboardHeaderPlus(title = "VCSEP Insights Platform", titleWidth = "300px",
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
                               br(),
                               p(style="text-align: justify;",
                                 "Choose an Area and Theme", tags$br(), "to find places in need"),
                               br(),
                               selectInput("tactical_cell",
                                           label = "Choose Region",
                                           choices = sort(tactical_cells),
                                            selected = "--All UK--"

                               ),
                               br(),
                               uiOutput("secondSelection"),

                               br(),

                               selectInput("theme",
                                    label="Select a Theme",
                                    choices = sort(c("Covid-19","Winter Pressures","Economic Hardship", "Mental Health","Flooding","Food Insecurity")),
                                    selected="Covid-19")
              ),

              menuItem(HTML("Emergencies Partnership<br/>Statistics"), tabName='vcs_usage', startExpanded = F, icon=icon('balance-scale-right'),
                       menuSubItem("Requests", tabName='request_data'),
                       menuSubItem("Pulse Check", tabName="pulse_check"),
                       menuSubItem("Volunteer Capacity", tabName="vol_capacity")),

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
                       style = "height:700px; overflow-y: scroll;overflow-x: scroll;"
                       #style = 'overflow-y:scroll; height: calc(100vh - 200px) !important;'
                     )),

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
                  style = "height:100px; overflow-y: scroll;overflow-x: scroll;"),
              # row  -
                  fluidRow(
                    # column 1
                    column(width = 12,
                  # - row 2 (action areas) -
                  box( width = NULL,  collapsible = T, collapsed=F,
                    title = "Areas to focus",
                      DT::dataTableOutput('areas2focus'),
                      style = "height:300px; overflow-y: scroll;overflow-x: scroll;"
                      )
             )),

                      # - row 3 -
                      fluidRow(
                        # - column 1 -
                        column(
                          width = 12,
                          box(
                            width = NULL, collapsible = T, collapsed=F,#solidHeader = TRUE, status='primary',
                            title = "People at Risk", align = "center", #height = "600px"
                            #DT::dataTableOutput('people_at_risk'),
                            #uiOutput('Population'),
                            #echarts4rOutput('total_population',height='60px'),
                            uiOutput('bame_population_text'),
                            echarts4rOutput('bame_population', height='60px'),
                            uiOutput('section95_text'),
                            echarts4rOutput('section95', height='60px'),
                            uiOutput('homeless_text'),
                            echarts4rOutput('homeless', height='60px'),
                            uiOutput('fuelp_text'),
                            echarts4rOutput('fuelp',height='60px'),
                            uiOutput('unemployment_text'),
                            echarts4rOutput('unemployment',height='60px'),
                            uiOutput('digital_text'),
                            echarts4rOutput('digital',height='60px'),
                            uiOutput('shielding_text'),
                            echarts4rOutput('shielding_f',height='60px'),
                            style = "height:300px; overflow-y: scroll;overflow-x: scroll;"
                          )
                        )
                        ),
                        fluidRow(
                        column(width = 12,
                               box(
                                 width = NULL, collapsible = T, collapsed=F,#solidHeader = TRUE, status='primary',
                                 title = "People in Need", align = "center", #height = "600px"
                                 uiOutput('people_in_Need'),
                                 style = "height:300px; overflow-y: scroll;overflow-x: scroll;"
                               )
                            )

                      )
                    ),

              # column - 2
              column( width = 6,
                    # - row 1 -
                    tabBox(
                      title = "Map", side = "right",
                      id = 'tabset2', width= NULL,
                    # bivariate
                    tabPanel(title = "Vulnerability vs Capacity to cope",
                      width = NULL, height = "675px", #solidHeader = TRUE, status='primary',
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
        )
    ),

  # - Request
  tabItem(tabName = 'request_data',
          h2("Request stats")),
  tabItem(tabName = 'pulse_check',
          h2("Pulse stats")),
  tabItem(tabName = 'vol_capacity',
          h2("volunteer capacity analysis")),
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
          h2(tags$strong('The Emergencies Partnership Insights Platform')),
          hr(),
          h4('Bringing together data to', tags$strong('improve collaboration'), 'across the voluntary and community sector,',
             tags$strong('before,'), tags$strong('during,'), "and", tags$strong('after'), "an", tags$strong("emergency"), ""),

          br(),
          h3(tags$strong('Get involved')),
          p("Our Data Working Group meets fortnightly on a Thursday at 11am to help us prioritise
            what data and analysis to focus on next. Join us to lend your voice to the conversation."),
          br(),
          h3(tags$strong('Share data')),
          p("Use our", tags$a(href="https://ingest.vcsep.org.uk/", target="_blank","Data App"), "or get in touch with
            our Data Team at", tags$a(href='insight@vcsep.org.uk', target="_blank", "insight@vcsep.org.uk")),
          br(),
          h3(tags$strong('Feedback or make a request')),
          p("We welcome your thoughts on what data would be useful to help shape your support to those in need.
            To feedback, make a request, or if you have any questions please get in touch with us at", tags$a(href="insight@vcsep.org.uk", target="_blank", "insight@vcsep.org.uk")),
          br(),
          h3(tags$strong('Find out more')),
          p("To learn more about the work of the VCS Emergencies Partnership, visit us at", tags$a(href="https://vcsep.org.uk/", target="_blank", "vcsep.org.uk")),

          br(),
          br(),
          p(tags$strong(tags$i("This platform is still in the early stages of development. Some features may not work properly, but are coming soon.")), style="color:blue")

        )
      })

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
    lads2select <- c('-- all LADs in TC --',lads2select)
    selectInput("lad_selected", "Local Authority", choices = sort(lads2select), selected='-- all LADs in TC --')
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

    tc_table <-  par_table %>% filter(TacticalCell == input$tactical_cell) %>%
      select('TacticalCell', `tc_int_Proportion of neighbourhoods in 20% most digitally excluded`:`tc_perc_People receiving Section 95 support`) %>%
      mutate(`tc_int_Fuel Poor Households`= round(`tc_int_Fuel Poor Households`, 1)) %>%
      mutate(`tc_perc_Proportion of neighbourhoods in 20% most digitally excluded`=round(`tc_perc_Proportion of neighbourhoods in 20% most digitally excluded`*100,1)) %>%
        unique()

    names(tc_table) = gsub(pattern = "tc_int_", replacement = "int_", x = names(tc_table))
    names(tc_table) = gsub(pattern = "tc_perc_", replacement = "perc_", x = names(tc_table))
    print(tc_table)




    #tc_perc <- pivot_longer(for_tc_perc, cols=c(`Proportion of neighbourhoods in 20% most digitally excluded`:`People receiving Section 95 support`), names_to='Indicator', values_to='Percentage (%)') %>%
    #  unique()

    #tc_int_perc <- left_join(tc_int, tc_perc, by='Indicator', keep=F)

    #no_nas_table <- tc_int_perc[!with(tc_int_perc, is.na(`Total`) & is.na(`Percentage (%)`)),]
    #print(no_nas_table)

  })


  # --- Areas to focus ----
  # -- covid
  filtered_covid_areas <- reactive({
    if(input$tactical_cell == '--All UK--') {
      covid_lads_in_tc <- covid_area2focus %>% arrange(-`Vulnerability quintile`, -`week 46`) %>%
        select('Local Authority'= Name, 'Overall vulnerability' =`Vulnerability quintile`, 'Latest covid cases'=tail(names(.),1), 'LAD19CD')
      print(covid_lads_in_tc)
    }
    else {

    lads_in_tc <- covid_area2focus %>% filter(TacticalCell == input$tactical_cell)
    # order descending by quintile and covid cases
    covid_lads_in_tc <- lads_in_tc %>% arrange(-`Vulnerability quintile`, -`week 46`) %>%
      select('Local Authority'= Name, 'Overall vulnerability' =`Vulnerability quintile`, 'Latest covid cases'=tail(names(.),1), 'LAD19CD')
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
      if(input$tactical_cell == '--All UK--') {

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

          if (input$lad_selected == '-- all LADs in TC --') {

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
      if (input$tactical_cell == '--All UK--') {

        tc = input$tactical_cell
        title_needed <- paste0('People at risk in: ', tc)

        output$bame_population_text <- renderUI({
          div(style= " text-align: left;",
            hr(),
            h4(tags$strong('Proportion of population who are BAME:')),
            p('Data not currently available at UK level')
          )

        })

        output$section95_text <- renderUI({
          div(style= " text-align: left;",
              hr(),
              h4(tags$strong('No. of people receiving Section 95 support:')),
              p('Data not currently uavailable at UK level')
          )
        })

        output$homeless_text <- renderUI({
          div(style= " text-align: left;",
              hr(),
              h4(tags$strong('Homelessness (rate per 1000):')),
              p('Data not currently uavailable at UK level')
          )
        })

        output$fuelp_text <- renderUI({
          div(style= " text-align: left;",
              hr(),
              h4(tags$strong('Fuel Poverty:')),
              p('Data not currently uavailable at UK level')
          )
        })

        output$unemployment_text <- renderUI({
          div(style= " text-align: left;",
              hr(),
              h4(tags$strong('Unemployed receiving Universal Credit:')),
              p('Data not currently uavailable at UK level')
          )
        })

        output$digital_text <- renderUI({
          div(style= " text-align: left;",
              hr(),
              h4(tags$strong('Percentage of neighbourhoods in 20% most digitally excluded:')),
              p('Data not currently uavailable at UK level')
          )
        })

        output$shielding_text <- renderUI({
          div(style= " text-align: left;",
              hr(),
              h4(tags$strong('No. people clinically extremely vulnerable:')),
              p('Data not currently uavailable at UK level')
          )
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
        if (input$lad_selected == '-- all LADs in TC --') {
          #Tactical cell
          tc = input$tactical_cell
          title_needed <- paste0('People at risk in Tactical Cell: ', tc)


          # --- people at risk ----

          # --- population demographics ---
          #bame <- curr_table %>% select('TacticalCell',`int_Fuel Poor Households`,`perc_Fuel Poor Households`)
          bame_to_plot <- curr_table %>% select('TacticalCell',`perc_Percentage of population who are ethnic minority`)

          # transpose dataframe
          bame_to_plot  <- bame_to_plot %>% pivot_longer(`perc_Percentage of population who are ethnic minority`, names_to = "Indicator", values_to = "proportion")

          # add in england avg
          #bame_to_plot <- bame_to_plot %>% mutate(england_avg = par_table_tc_avg$`LAD_perc_Percentage of population who are ethnic minority`)

          # for echarts
          eng_avg_bame <- par_table_tc_avg %>% select(`tc_perc_Percentage of population who are ethnic minority`) %>%
            select('xAxis' = `tc_perc_Percentage of population who are ethnic minority`) %>%
            as.list()

          output$bame_population_text <- renderUI({
            div(style= " text-align: left;",
                hr(),
                h4(tags$strong('Proportion of population who are BAME:')),
                p(curr_table$`perc_Percentage of population who are ethnic minority`, '%', 'of the population are BAME in the', input$tactical_cell, 'region')
            )
          })
          #
          output$bame_population <- renderEcharts4r({
            # # Plot population statistics
            fuel_p <- bame_to_plot %>%
              e_charts(x = Indicator) %>%
              e_bar(proportion, bar_width=0.1) %>%
              e_labels(position = "right") %>%
              #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
              #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
              e_mark_line(data=eng_avg_bame, symbol = "none", lineStyle = list(color = "black")) %>%
              e_hide_grid_lines() %>%
              e_flip_coords() %>%
              e_grid(containLabel = TRUE, left=0, right=100, top=20, bottom=20, height='80%') %>%

              #e_rm_axis(axis="x") %>%
              #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
              e_x_axis(min=0, max=100, show=F) %>%
              e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
              e_y_axis(show=F) %>%
              e_legend(FALSE)

          })

          # --- section 95 support ----
          section95_to_plot <- curr_table %>% select('TacticalCell',`int_People receiving Section 95 support`)

          # transpose dataframe
          section95_to_plot  <- section95_to_plot %>% pivot_longer(`int_People receiving Section 95 support`, names_to = "Indicator", values_to = "proportion")

          # add in england avg
          #section95_to_plot <- section95_to_plot %>% mutate(england_avg = par_table_test$`LAD_perc_Percentage of population who are ethnic minority`)

          # for echarts
          eng_avg_section95 <- par_table_tc_avg %>% select(`tc_int_People receiving Section 95 support`) %>%
            mutate('xAxis' = round(`tc_int_People receiving Section 95 support`,0)) %>%
            select('xAxis') %>%
            as.list()

          output$section95_text <- renderUI({
            div(style= " text-align: left;",
                hr(),
                h4(tags$strong('No. of people receiving Section 95 support:')),
                p(format(section95_to_plot$`proportion`, big.mark=',', scientific = F), 'people in the', input$tactical_cell, 'region are receiving Section 95 support')
            )
          })
          #
          output$section95 <- renderEcharts4r({
            # # Plot population statistics
            fuel_p <- section95_to_plot %>%
              e_charts(x = Indicator) %>%
              e_bar(proportion, bar_width=0.1) %>%
              e_labels(position = "right") %>%
              #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
              #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
              e_mark_line(data=eng_avg_section95, symbol = "none", lineStyle = list(color = "black")) %>%
              e_hide_grid_lines() %>%
              e_flip_coords() %>%
              e_grid(containLabel = TRUE, left=0, right=100, top=20, bottom=20, height='80%') %>%

              #e_rm_axis(axis="x") %>%
              #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
              e_x_axis(min=0, max=22000, show=F) %>%
              e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
              e_y_axis(show=F) %>%
              e_legend(FALSE)

          })

          # --- Homelessness ----
          #section95_to_plot <- curr_table %>% select('TacticalCell',`int_People receiving Section 95 support`)

          # transpose dataframe
          #section95_to_plot  <- section95_to_plot %>% pivot_longer(`int_People receiving Section 95 support`, names_to = "Indicator", values_to = "proportion")

          # add in england avg
          #section95_to_plot <- section95_to_plot %>% mutate(england_avg = par_table_test$`LAD_perc_Percentage of population who are ethnic minority`)

          # for echarts
          #eng_avg_section95 <- par_table_tc_avg %>% select(`tc_int_People receiving Section 95 support`) %>%
          #  mutate('xAxis' = round(`tc_int_People receiving Section 95 support`,0)) %>%
          #  select('xAxis') %>%
          #  as.list()

          output$homeless_text <- renderUI({
            div(style= " text-align: left;",
                hr(),
                h4(tags$strong('Homelessness (rate per 1000):')),
                p('This is currently unavailable at tactical cell level'),
                p('Please choose a local authority')
            )
          })


          # --- fuel_poverty ---
          # -- select columns --
          fuelp <- curr_table %>% select('TacticalCell',`int_Fuel Poor Households`,`perc_Fuel Poor Households`)
          fuelp_to_plot <- fuelp %>% select('TacticalCell',`perc_Fuel Poor Households`)

          # transpose dataframe
          fuelp_to_plot <- fuelp_to_plot %>% pivot_longer(`perc_Fuel Poor Households`, names_to = "Indicator", values_to = "proportion")

          # add in england avg
          #fuelp_to_plot <- fuelp_to_plot %>% mutate(england_avg = par_table_tc_avg$`LAD_perc_Fuel Poor Households`)

          # for echarts
          eng_avg <- par_table_tc_avg %>% select(`tc_perc_Fuel Poor Households`) %>%
            select('xAxis' = `tc_perc_Fuel Poor Households`) %>%
            as.list()

          output$fuelp_text <- renderUI({
            div(style= " text-align: left;",
              hr(),
              h4(tags$strong('Fuel Poverty:')),
              p(fuelp$`perc_Fuel Poor Households`, '%', "(", format(fuelp$`int_Fuel Poor Households`, big.mark=',', scientific=F), ")", 'of households are classified as fuel poor in the', input$tactical_cell, 'region')
            )
          })
          #
          output$fuelp <- renderEcharts4r({
          # # Plot population statistics
           fuel_p <- fuelp_to_plot %>%
            e_charts(x = Indicator) %>%
            e_bar(proportion, bar_width=0.1) %>%
             e_labels(position = "right") %>%
             #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
             #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
             e_mark_line(data=eng_avg, symbol = "none", lineStyle = list(color = "black")) %>%
             e_hide_grid_lines() %>%
             e_flip_coords() %>%
             e_grid(containLabel = TRUE, left=0, right=100, top=20, bottom=20, height='80%') %>%

             #e_rm_axis(axis="x") %>%
             #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
             e_x_axis(min=0, max=100, show=F) %>%
             e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
             e_y_axis(show=F) %>%
             #e_tooltip() %>%
             e_legend(FALSE)

           })

          # --- Unemployment ---
          # -- select columns --
          unemployment <- curr_table %>% select('TacticalCell',`int_Unemployed receiving Universal Credit`,`perc_Unemployed receiving Universal Credit`)
          unemployment_to_plot <- unemployment %>% select('TacticalCell',`perc_Unemployed receiving Universal Credit`)

          # transpose dataframe
          unemployment_to_plot <- unemployment_to_plot %>% pivot_longer(`perc_Unemployed receiving Universal Credit`, names_to = "Indicator", values_to = "proportion")

          # add in england avg
          #fuelp_to_plot <- fuelp_to_plot %>% mutate(england_avg = par_table_tc_avg$`LAD_perc_Fuel Poor Households`)

          # for echarts
          eng_avg_unemployment <- par_table_tc_avg %>% select(`tc_perc_Unemployed receiving Universal Credit`) %>%
            select('xAxis' = `tc_perc_Unemployed receiving Universal Credit`) %>%
            as.list()

          output$unemployment_text <- renderUI({
            div(style= " text-align: left;",
                hr(),
                h4(tags$strong('Unemployed receiving Universal Credit:')),
                p(unemployment$`perc_Unemployed receiving Universal Credit`, '%', "(", format(unemployment$`int_Unemployed receiving Universal Credit`, big.mark=',', scientific=F), ")", 'of people are unemployed and receiving Universal Credit in the', input$tactical_cell, 'region')
            )
          })
          #
          output$unemployment <- renderEcharts4r({
            # # Plot population statistics
            fuel_p <- unemployment_to_plot %>%
              e_charts(x = Indicator) %>%
              e_bar(proportion, bar_width=0.1) %>%
              e_labels(position = "right") %>%
              #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
              #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
              e_mark_line(data=eng_avg_unemployment, symbol = "none", lineStyle = list(color = "black")) %>%
              e_hide_grid_lines() %>%
              e_flip_coords() %>%
              e_grid(containLabel = TRUE, left=0, right=100, top=20, bottom=20, height='80%') %>%

              #e_rm_axis(axis="x") %>%
              #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
              e_x_axis(min=0, max=100, show=F) %>%
              e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
              e_y_axis(show=F) %>%
              #e_tooltip() %>%
              e_legend(FALSE)

          })

          # --- Digital exclusion ---
          # -- select columns --
          #digital <- curr_table %>% select('TacticalCell',`int_Unemployed receiving Universal Credit`,`perc_Unemployed receiving Universal Credit`)
          digital_to_plot <- curr_table %>% select('TacticalCell',`perc_Proportion of neighbourhoods in 20% most digitally excluded`)

          # transpose dataframe
          digital_to_plot <- digital_to_plot %>% pivot_longer(`perc_Proportion of neighbourhoods in 20% most digitally excluded`, names_to = "Indicator", values_to = "proportion")

          # add in england avg
          #fuelp_to_plot <- fuelp_to_plot %>% mutate(england_avg = par_table_tc_avg$`LAD_perc_Fuel Poor Households`)

          # for echarts
          eng_avg_digital <- par_table_tc_avg %>% select(`tc_perc_Proportion of neighbourhoods in 20% most digitally excluded`) %>%
            select('xAxis' = `tc_perc_Proportion of neighbourhoods in 20% most digitally excluded`) %>%
            as.list()

          output$digital_text <- renderUI({
            div(style= " text-align: left;",
                hr(),
                h4(tags$strong('Percentage of neighbourhoods in 20% most digitally excluded:')),
                p(digital_to_plot$`proportion`, '%', 'of neighbourhoods in the', input$tactical_cell, 'region, are in the 20% most digitally excluded')
            )
          })
          #
          output$digital <- renderEcharts4r({
            # # Plot population statistics
            fuel_p <- digital_to_plot %>%
              e_charts(x = Indicator) %>%
              e_bar(proportion, bar_width=0.1) %>%
              e_labels(position = "right") %>%
              #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
              #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
              e_mark_line(data=eng_avg_digital, symbol = "none", lineStyle = list(color = "black")) %>%
              e_hide_grid_lines() %>%
              e_flip_coords() %>%
              e_grid(containLabel = TRUE, left=0, right=100, top=20, bottom=20, height='80%') %>%

              #e_rm_axis(axis="x") %>%
              #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
              e_x_axis(min=0, max=100, show=F) %>%
              e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
              e_y_axis(show=F) %>%
              #e_tooltip() %>%
              e_legend(FALSE)

          })

              # --- Shielding ---
          # -- select columns --
          shielding <- curr_table %>% select('TacticalCell',`int_Clinically extremely vulnerable`,`perc_Clinically extremely vulnerable`)
          shielding_to_plot <- shielding %>% select('TacticalCell',`perc_Clinically extremely vulnerable`)

          # transpose dataframe
          shielding_to_plot <- shielding_to_plot %>% pivot_longer(`perc_Clinically extremely vulnerable`, names_to = "Indicator", values_to = "proportion")

          # add in england avg
          #fuelp_to_plot <- fuelp_to_plot %>% mutate(england_avg = par_table_tc_avg$`LAD_perc_Fuel Poor Households`)

          # for echarts
          eng_avg_shielding <- par_table_tc_avg %>% select(`tc_perc_Clinically extremely vulnerable`) %>%
            select('xAxis' = `tc_perc_Clinically extremely vulnerable`) %>%
            as.list()

          output$shielding_text <- renderUI({
            div(style= " text-align: left;",
                hr(),
                h4(tags$strong('No. people clinically extremely vulnerable:')),
                p(shielding$`perc_Clinically extremely vulnerable`, '%', "(", format(shielding$`int_Clinically extremely vulnerable`, big.mark=',', scientific=F), ")", 'of people are clinically extremely vulnerable in the', input$tactical_cell, 'region')
            )
          })
          #
          output$shielding_f <- renderEcharts4r({
            # # Plot population statistics
            shielding_to <- shielding_to_plot %>%
              e_charts(x = Indicator) %>%
              e_bar(proportion, bar_width=0.1) %>%
              e_labels(position = "right") %>%
              #e_scatter(england_avg, name = "National avg", symbolSize = 8) %>%
              #e_mark_line(data = list(xAxis=eng_avg), title='National Avg') %>%
              e_mark_line(data=eng_avg_shielding, symbol = "none", lineStyle = list(color = "black")) %>%
              e_hide_grid_lines() %>%
              e_flip_coords() %>%
              e_grid(containLabel = TRUE, left=0, right=100, top=20, bottom=20, height='80%') %>%

              #e_rm_axis(axis="x") %>%
              #e_x_axis(axisLabel = list(interval = 0, rotate = 45, formatter = "{value}%", show=F), name = "Percentage of population (%)", nameLocation = "middle", nameGap = 35) %>%
              e_x_axis(min=0, max=100, show=F) %>%
              e_y_axis(axisLabel = list(interval = 0, show = F)) %>%
              e_y_axis(show=F) %>%
              #e_tooltip() %>%
              e_legend(FALSE)

          })




        }


          # --- set up data table output ----
          # output$people_at_risk <- DT::renderDataTable({
          #   DT::datatable(curr_table,
          #               #caption = htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:black; font-size:125% ;',title_needed),
          #               options = list(
          #                 paging=FALSE
          #               ))
          #           })
          #         }
        # -- just local authority --
        else {
          lad_of_interest <- lad_uk2areas2vulnerability %>% filter(Name == input$lad_selected) %>% select('LAD19CD') %>% st_drop_geometry()

          for_lad_int <- par_table %>% filter(LAD19CD == lad_of_interest$LAD19CD) %>% select(`LAD_int_Proportion of neighbourhoods in 20% most digitally excluded`:`LAD_int_People receiving Section 95 support`)  %>% mutate(`LAD_int_Fuel Poor Households` = round(`LAD_int_Fuel Poor Households`, 1))
            #print(for_lad_int)
          # rename stuff again
            names(for_lad_int) = gsub(pattern = "LAD_int_", replacement = "", x = names(for_lad_int))
          # pivot
          lad_int <- pivot_longer(for_lad_int, cols=c(`Proportion of neighbourhoods in 20% most digitally excluded`:`People receiving Section 95 support`), names_to='Indicator', values_to='Total') %>% unique()

          for_lad_perc <- par_table %>% filter(LAD19CD == lad_of_interest$LAD19CD) %>% select(`LAD_perc_Proportion of neighbourhoods in 20% most digitally excluded`:`LAD_perc_People receiving Section 95 support`) %>%
                        mutate(`LAD_perc_Proportion of neighbourhoods in 20% most digitally excluded` = round(`LAD_perc_Proportion of neighbourhoods in 20% most digitally excluded`*100, 2))

          names(for_lad_perc) = gsub(pattern = "LAD_perc_", replacement = "", x = names(for_lad_perc))

          lad_perc <- pivot_longer(for_lad_perc, cols=c(`Proportion of neighbourhoods in 20% most digitally excluded`:`People receiving Section 95 support`), names_to='Indicator', values_to='Percentage (%)') %>%
          unique()

          lad_int_perc <- left_join(lad_int, lad_perc, by='Indicator', keep=F)

          no_nas_lad_table <- lad_int_perc[!with(lad_int_perc, is.na(`Total`) & is.na(`Percentage (%)`)),]
          #print(no_nas_lad_table)

          # LAD title
          lad_name <- input$lad_selected
          title_needed <- paste0("People at risk in LAD: ", lad_name)

          # --- set up data table output ----
          # output$people_at_risk <- DT::renderDataTable({
          #   DT::datatable(no_nas_lad_table,
          #               #caption = htmltools::tags$caption( style = 'caption-side: top; text-align: left; color:black; font-size:125% ;',title_needed),
          #               options = list(
          #                 paging=FALSE
          #               ))
          #
          #  })
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
        select('Local Authority', 'Overall vulnerability', 'Volunteer capacity', 'Score'=mean_score, `Latest covid cases`)

        # - order
        covid_cases2volunteers <- covid_cases2volunteers %>% arrange(-`Overall vulnerability`, -`Latest covid cases`, -Score) %>%
          select(-Score)


       # -- if want to show whole of the UK --
        if ( input$tactical_cell == '--All UK--') {
        # all lads in tcs wanted
        output$areas2focus <- DT::renderDataTable({
         DT::datatable(covid_cases2volunteers,
                   options = list(
                     paging =FALSE
                   ))
              })

            }

          else {
            # show just tactical cell
            if (input$theme == 'Covid-19' & input$lad_selected == '-- all LADs in TC --') {
              output$areas2focus <- DT::renderDataTable({
              DT::datatable(covid_cases2volunteers,
                          options = list(
                            paging =FALSE
                          ))
                        })
                }
              # move la to top
            else {
              show_at_top <- as.vector(input$lad_selected)
              print(covid_cases2volunteers)
              wanted <- covid_cases2volunteers$`Local Authority` %in% show_at_top
              lad_covid_cases2volunteers <- rbind(covid_cases2volunteers[wanted,], covid_cases2volunteers[!wanted,])

              output$areas2focus <- DT::renderDataTable({
                DT::datatable(lad_covid_cases2volunteers,
                          options = list(
                            paging =FALSE
                          )) %>%
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

         if (input$tactical_cell == '--All UK--') {

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

          if (input$lad_selected == '-- all LADs in TC --') {

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

        if (input$tactical_cell == '--All UK--') {

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
              "Volunteer capacity", avg_score$`Volunteer capacity`,
              color = avg_score$colour, fill = TRUE
                )
              })
            }

        else {

        # -- Tactical cell level --
          if (input$lad_selected == '-- all LADs in TC --') {

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
            "Volunteer capacity", avg_score$`Volunteer capacity`,
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
                  "Volunteer capacity", avg_score$`Volunteer capacity`,
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
