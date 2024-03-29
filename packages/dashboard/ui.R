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
source("./functions.R")
source("./global.R")


# ---  dashboard --- #
# --- header --- #
# header <- dashboardHeader(title = "", titleWidth = "300px",
#                           
#                           tags$li(class = "dropdown", 
#                                   tags$li(class="dropdown",
#                                           actionLink("home", "Home", icon=icon("home"))),
#                                   tags$li(class="dropdown",
#                                           actionLink("RI_tool","Risk Indicator Tool", icon=icon("fas fa-map-signs"))
#                                   ),
#                                   tags$li(class="dropdown",
#                                           actionLink("e_catalogue", "Public resources", icon=icon("fas fa-book-open"))),
#                                   tags$li(class="dropdown",
#                                           actionLink("vcs_reports_header", "Partner reports", icon=icon("fas fa-file"))),
#                                                      
#                                   tags$li(class="dropdown",
#                                           actionLink("internal_reports", "Internal Dashboards", icon=icon("fas fa-lock"))),
#                                   tags$li(class="dropdown",
#                                           actionLink("community_assets_header", "Community assets map", icon=icon("fas fa-map-marked"))),
#                                   tags$li(class="dropdown",
#                                           actionLink("help", "Help", icon=icon("far fa-question-circle"))),
#                                   tags$li(class="dropdown",
#                                           actionLink("references", "References", icon=icon('fas fa-feather-alt')))
#                           ))

header <- dashboardHeader(title = "", titleWidth = "300px",
                          tags$li(class="dropdown",
                                  tags$li(class="dropdown",
                                          p(style='padding-top:15px; padding-right:15px;color:white;font-size=12px', "Last updated at", last_updated_time_header, "(BST) on", last_updated_date), 
                                          #icon=icon("fas fa-clock")
                                  )))

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
              
              menuItem(HTML("Public Resources"), tabName="resource_catalogue", icon=icon("fas fa-book-open")),
              menuItem(HTML("Partner Reports"), tabName="vcs_report_sidebar", icon=icon("fas fa-file")),
              menuItem(HTML("Community Assets Map"), tabName="community_assets_sidebar", icon=icon("fas fa-map-marked")),
              menuItem(HTML("Latest News"), tabName="latest_news_tab", icon=icon("fas fa-newspaper")),
              menuItem("Help", tabName="Help", icon=icon("far fa-question-circle")),
              menuItem("References", tabName='references', icon=icon('fas fa-feather-alt'))
              
              
              # - display vcsep logo -
              #div(p("Developed by"), img(src = "vcs-logo-text.png", width = 225),style="position:fixed; bottom:0; padding:15px; text-align: center;")
              
  )
)

body <- dashboardBody(
  useShinyjs(),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}",),
  tags$head(
    includeCSS("styles.css"), includeHTML("cookie_consent_v2.html"),
    includeHTML("google-analytics.html"),
    tags$link(rel="icon", sizes="32X32", href="img/favicon-32x32.png"),
    tags$link(rel="icon",  sizes="16X16", href="img/favicon-16x16.png"),
    tags$link(rel="apple-touch-icon", sizes="180x180", href="img/img/apple-touch-icon.png"),
    tags$link(rel="manifest", href="img/site.webmanifest")),
  tabItems(
    # --- Home page ---
    tabItem(tabName="home", selected=T,
            
            # - row 1 -
            fluidRow(style="padding-right:30px; padding-left:30px; padding-bottom:20px;padding-top:20px;",
                     panel(
                       #fluidRow(style="padding-right:20px; padding-left:20px; padding-bottom:20px",
                       # column 1
                       column(width = 9,
                              h1("Welcome to the Emergencies Partnership Toolkit"),
                              h4("Sharing insight and resources amongst the Voluntary and Community Sector to help before, during and after an emergency.", style='margin-right:150px'),
                       ),
                       column(width = 3,
                              div(img(src = "vcsep_logo_line.jpg", width = 250, 
                                      style="align:center;margin-left:-40px;"))))),
            #style="position:fixed; padding-right:20px; padding-left:20px;padding-top:0px;padding-bottom:50px"))),
            # row two
            fluidRow(style="padding-right:20px; padding-left:20px; padding-bottom:0px",  
                     
                     column(width=4,
                            box(title=actionLink("RI_tool_box","Risk Indicator Tool"), width=NULL,
                                collapsible = T, collapsed=T,
                                icon = icon("fas fa-map-signs"),
                                p("The Risk Indicator Tool shows you key statistics about an
                                emergency along with the BRC Vulnerability and Resilience
                                indices and key area demographics to aide the planning and
                                response to an emergency.",tags$br(),
                                  tags$br(),
                                  tags$em(tags$strong("click on the title to go to the tool"))))),
                     
                     column(width=4,
                            box(title=actionLink("e_catalogue_box","Public Resources"),width=NULL, 
                                collapsible = T, collapsed=T,
                                icon = icon("fas fa-book-open"),
                                p("A collection of useful
                                  publicly available insight resources that could be of use to
                                  the VCS community.", tags$br(),
                                  tags$br(),
                                  tags$strong(tags$em("click on the title to go to the tool"))))),
                     
                     column(width=4,
                            box(title=actionLink("vcs_reports_box","Partner Reports"),
                                width=NULL, 
                                collapsible = T, collapsed=T,
                                icon = icon("fas fa-file"),
                                p("Insightful and thought-provoking reports that we and
                                  our parnters have written for the voluntary and community sector.", tags$br(),
                                  tags$br(),
                                  tags$strong(tags$em("click on the title to go to the reports")))))),
            fluidRow(width=NULL, style="padding-right:20px; padding-left:20px; padding-bottom:20px; padding-top:0px",
                     column(width=4,
                            box(title=actionLink("community_assets_box", "Community Assets Map"),
                                width=NULL,
                                collapsible = T, collapsed=T,
                                icon=icon("fas fa-map-marked"),
                                p("Community Assets Map allows you to explore and understand 
                                    assets in your area.", tags$br(),
                                  tags$br(),
                                  tags$strong(tags$em("click on the title to go to the tool"))))),
                     column(width=4,
                            box(title=actionLink("latest_news_box", "Latest News"),
                                width=NULL,
                                collapsible = T, collapsed=T,
                                icon=icon("fas fa-newspaper"),
                                p("Latest News from the partnership.", tags$br(),
                                  tags$br(),
                                  tags$strong(tags$em("click on the title to find out more")))))
            ),
            
            # row three 
            fluidRow(
                     column(12, align="center",
                      p("This toolkit is currently under development. Please use the links above for the current resources and insights available."))
                     ),
            fluidRow(
                    column(12, align="center",
                     p("For data on COVID-19 cases and vaccination rates please visit the UK government COVID-19 dashboard ",
                       tags$a(href="https://coronavirus.data.gov.uk/", "here.")))
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
                                       div(h4(tags$strong("Risk Indicator Tool")),
                                           p("This tool is to help answer the question of what",
                                             tags$strong("areas and people"), "would be/are at risk should the",
                                             tags$strong("emergency"), "scenario selected occur.", tags$br(), 
                                             "Please visit the", tags$strong("Help"), "page for information on how to use the tool."))),
                                column(width=3, style='padding-top:20px',
                                       selectInput("theme",
                                                   label="1. Select an emergency",
                                                   #choices = sort(c("Covid-19","Winter Pressures","Economic Hardship", "Mental Health","Flooding","Food Insecurity")),
                                                   choices = sort(c("Covid-19","Flooding")),
                                                   selected="Covid-19")),
                                
                                column(width=3, style='padding-top:20px',
                                       selectInput("tactical_cell",
                                                   label = "2. Choose a region",
                                                   choices = sort(tactical_cells),
                                                   selected = "-- England --")),
                                column(width=3,style='padding-top:20px',
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
                                       tabPanel('Areas to Focus',
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
                                                                uiOutput('top10list')
                                                         ))
                                                
                                       ),
                                       tabPanel("Area Demographics", 
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
                               tabPanel('Areas at Risk', 
                                        withSpinner(leafletOutput("map", height = "550px"))),
                               tabPanel("Find Local Resources",
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
                               tabPanel('Areas To Focus Table', 
                                        withSpinner(DT::dataTableOutput('areas2focus', height='350px')),
                                        style = "height:550px; overflow-y: scroll;overflow-x: scroll;")
                             )
                     )
            )
    ),
    
    tabItem(tabName='resource_catalogue',
            fluidRow(width=NULL, 
                     #style="padding-right:30px; padding-left:30px; padding-bottom:20px;padding-top:10px;",
                            panel(style="padding-right:30px; padding-left:30px; padding-bottom:20px;padding-top:10px;",
                                 fluidRow(column(width=12, p(h2("Public Resources"), style="font-size:1.9",
                                                    "This is a collection of useful publicly available resources that could be useful to people working in the Voluntary and Community Sector.", "Please",
                                                    tags$a(href="https://vcsep.org.uk/", target="_blank", "contact us"), 
                                                    "if you would like to share any further resources that we could signpost to.",
                                                    tags$br(), 
                                                    tags$em("Please note: The Emergencies Partnership does not have responsibility for these resources.", style='font-size:1.5vh')))),
                     fluidRow(
                       column(
                         3,
                         pickerInput(
                           inputId = "chosen_geography",
                           label = "Geography:",
                           choices = sort(unique(resources_info_colour$Geography)),
                           selected = sort(unique(resources_info_colour$Geography)),
                           options = list(`actions-box` = TRUE, title = "Please select geography"),
                           multiple = T
                         )
                       ),
                       column(
                         3,
                         pickerInput(
                           inputId = "chosen_theme",
                           label = "Theme:",
                           choices = sort(distinct_themes),
                           selected = sort(distinct_themes),
                           options = list(`actions-box` = TRUE, title = "Please select theme"),
                           multiple = T
                         )
                       ),
                       column(
                         3,
                         pickerInput(
                           inputId = "chosen_type",
                           label = "Type:",
                           choices = sort(distinct_types),
                           selected = sort(distinct_types),
                           options = list(`actions-box` = TRUE, title = "Please select type"),
                           multiple = T
                         )
                       )
                       )
                     )),
                     fluidRow(width=NULL, style="padding-right:30px; padding-left:30px; padding-bottom:20px;",
                       DT::dataTableOutput("public_resource_table")
                     )
                     
            
    ),
    tabItem(tabName="latest_news_tab", 
            fluidRow(width=NULL,style="padding-right:30px; padding-left:30px; padding-bottom:20px;padding-top:20px;",
                     column(width=12,
                            panel(h2("Latest News From the Emergencies Partnership")))),
            fluidRow(width=NULL,style="padding-right:30px; padding-left:30px;",
                     column(width=4, tags$head(tags$script('!function(d,s,id){var js,fjs=d.getElementsByTagName(s)    [0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");')),
                            box(width=NULL, height='750px',
                                headerBorder=F,
                                a(class="twitter-timeline", href="https://twitter.com/vcsep"),
                                style = "height:100vh; overflow-y: scroll;overflow-x: scroll;margin-top:-40px;padding-top:-40px")),
                     
                     column(width=4, 
                            box(title="In The Press/Success Stories", width=NULL,
                                uiOutput('press_highlights', height='100vh'))),
                     column(width=4, 
                            box(title="Coming Up", width=NULL, #height='750px',
                                uiOutput("coming_up", height='100vh'))))),
    
    tabItem(tabName='Help',
            column(width = 12, style="padding-right:30px; padding-left:30px; padding-bottom:20px;padding-top:20px;",
                   tabBox(id='information about dashboards', width=NULL,
                          tabPanel("About Us",
                                   uiOutput('about_us'),
                                   style = "height:600px; overflow-y: scroll;overflow-x: scroll;"),
                          tabPanel("Risk Indicator Tool",
                                   tabBox(id='help_areas_at_risk', width=NULL,
                                          tabPanel("About",
                                                   uiOutput('about_needs'),
                                                   style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
                                          ),
                                          tabPanel("Understanding the Map",
                                                   uiOutput("understand_map_top"),
                                                   uiOutput("understand_map_middle"),
                                                   uiOutput("understand_map_bottom"),
                                                   style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
                                          ),
                                          tabPanel("Emergency: Coronavirus",
                                                   uiOutput("emergency_covid"),
                                                   style = "height:600px; overflow-y: scroll;overflow-x: scroll;"),
                                          tabPanel("Emergency: Flooding",
                                                   uiOutput("emergency_flooding"),
                                                   style = "height:600px; overflow-y: scroll;overflow-x: scroll;")
                                   ))))),
    tabItem(tabName = 'references',
            fluidRow(style="padding-right:30px; padding-left:30px; padding-bottom:20px;padding-top:20px;",
                     # column 1
                     column(width = 12,
                            box(width=NULL, headerBorder=F,
                                uiOutput('refs_header', style="height:50px;margin-top:-50px;padding-top:-50px"),
                                uiOutput('refs',
                                         style = "height:700px; overflow-y: scroll;overflow-x: scroll;")
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