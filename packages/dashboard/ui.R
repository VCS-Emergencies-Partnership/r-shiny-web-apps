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
#                                           actionLink("internal_reports", "Internal dashboards", icon=icon("fas fa-lock"))),
#                                   tags$li(class="dropdown",
#                                           actionLink("community_assets_header", "Community assets map", icon=icon("fas fa-map-marked"))),
#                                   tags$li(class="dropdown",
#                                           actionLink("help", "Help", icon=icon("far fa-question-circle"))),
#                                   tags$li(class="dropdown",
#                                           actionLink("references", "References", icon=icon('fas fa-feather-alt')))
#                           ))

header <- dashboardHeader(title = "", titleWidth = "300px")

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
              
              menuItem(HTML("Public resources"), tabName="resource_catalogue", icon=icon("fas fa-book-open")),
              menuItem(HTML("Partner reports"), tabName="vcs_report_sidebar", icon=icon("fas fa-file")),
              menuItem(HTML("Internal dashboards"), tabName="internal_reports_from_sidebar", icon=icon("fas fa-lock")),
              menuItem(HTML("Community assets web map"), tabName="community_assets_sidebar", icon=icon("fas fa-map-marked")),
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
                              h4("Sharing insight and resources amongst the VCS to help before, during and after an emergency."),
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
                                p("The Risk indicator tool shows you key statistics about an
                                emergency along with the BRC Vulnerability and Resilience
                                indices and key area demographics to aide the planning and
                                response to an emergency.",tags$br(),
                                  tags$br(),
                                  tags$em(tags$strong("click on the title to go to the tool"))))),
                     
                     column(width=4,
                            box(title=actionLink("e_catalogue_box","Public resources"),width=NULL, 
                                collapsible = T, collapsed=T,
                                icon = icon("fas fa-book-open"),
                                p("A collection of useful
                                  publicly available insight resources that could be of use to
                                  the VCS community.", tags$br(),
                                  tags$br(),
                                  tags$strong(tags$em("click on the title to go to the tool"))))),
                     
                     column(width=4,
                            box(title=actionLink("vcs_reports_box","Partner reports"),
                                width=NULL, 
                                collapsible = T, collapsed=T,
                                icon = icon("fas fa-file"),
                                p("Insightful and thought-provoking reports that we and
                                  our parnters have written for the voluntary and community sector.", tags$br(),
                                  tags$br(),
                                  tags$strong(tags$em("click on the title to go to the reports")))))),
                     fluidRow(width=NULL, style="padding-right:20px; padding-left:20px; padding-bottom:20px; padding-top:0px",
                              
                        column(width=4,
                            box(title=actionLink("internal_reports_from_box", "Internal dashboards"),
                                width=NULL,
                                collapsible = T, 
                                collapsed=T,
                                icon = icon("fas fa-lock"),
                                p("These are internal reports and insight for VCS EP members.
                          Current reports include:",
                                  tags$li("The Request for support dashboard"),
                                  tags$li("The pulse survey dashboard"),
                                  tags$li("Vaccine uptake dashobard"),
                                  tags$br(),
                                  tags$strong(tags$em("click on the title to go to the tool"))))),
                     
                     column(width=4,
                            box(title=actionLink("community_assets_box", "Community assets map"),
                                width=NULL,
                                collapsible = T, collapsed=T,
                                icon=icon("fas fa-map-marked"),
                                p("Community assets map allows you to explore and understand 
                                    assets in your area.", tags$br(),
                                  tags$br(),
                                  tags$strong(tags$em("click on the title to go to the tool"))))),
                     column(width=4,
                            box(title=actionLink("latest_news_box", "Latest news"),
                                width=NULL,
                                collapsible = T, collapsed=T,
                                icon=icon("fas fa-newspaper"),
                                p("Latest News from the partnership.", tags$br(),
                                  tags$br(),
                                  tags$strong(tags$em("click on the title to find out more")))))
                     ),
            
            # row three 
            fluidRow(style="padding-right:20px; padding-left:20px; padding-bottom:40px",
                     column(width=4,
                            box(title="Where we're working", width=NULL, #height='80vh',
                                style='overflow-y: scroll;',
                                uiOutput("home_map_headlines", height='40vh'),
                                leafletOutput('home_map', height = "60vh"))),
                     column(width=4,
                            box(title="What our network is telling us", width=NULL,
                                uiOutput("latest_concerns_headline", height='40vh', width=NULL),
                                
                                echarts4rOutput('concerns', height="60vh"))),
                     
                     column(width = 4,
                            box(title="Latest insight", width=NULL,
                                uiOutput("latest_insight_headline", height='40vh'),
                                echarts4rOutput("latest_insight", height="60vh"))
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
                                                                uiOutput('top10list')
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
    
    tabItem(tabName='resource_catalogue',
            fluidRow(width=NULL, style="padding-right:30px; padding-left:30px; padding-bottom:20px;padding-top:10px;",
                     column(width=12,
                            panel(style="padding-right:30px; padding-left:30px; padding-bottom:20px;padding-top:10px;",
                                  column(width=6, p(h2("Public resources"), "This is a collection of useful publicly available 
                                            resources that could be useful to the Voluntary Community Sector.", 
                                                    tags$a(href="https://vcsep.org.uk/", target="_blank", "Contact us"), 
                                                    "if you know of any further resources that may be of use.",
                                            tags$br(), 
                                            "The VCS EP does not have responsibility 
                                            for these resources.")),
                                  column(width=6, style='padding-top:30px', searchInput(
                                    inputId = "resource_search",
                                    label = "Search for resources with a particular theme:", 
                                    placeholder = "i.e covid",
                                    btnSearch = icon("search"), 
                                    btnReset = icon("remove"),
                                    width = "100%"
                                  ))))),
            fluidRow(width=NULL, style="padding-right:30px; padding-left:30px; padding-bottom:20px;",
                     column(width=12,
                            uiOutput("dynamic_boxes")
                            #bubblesOutput('dynamic_bubbles')
                     #)
                )
            )
            
    ),
    tabItem(tabName="latest_news_tab", 
            fluidRow(width=NULL,style="padding-right:30px; padding-left:30px; padding-bottom:20px;padding-top:20px;",
                     panel(h2("Latest news from the Emergencies Partnership")),
                     fluidRow(width=NULL,style="padding-right:30px; padding-left:30px;",
                              column(width=4, tags$head(tags$script('!function(d,s,id){var js,fjs=d.getElementsByTagName(s)    [0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");')),
                                    box(width=NULL, height='750px',
                                        headerBorder=F,
                                        a(class="twitter-timeline", href="https://twitter.com/vcsep"),
                                        style = "height:80vh; overflow-y: scroll;overflow-x: scroll;margin-top:-40px;padding-top:-40px")),
                              column(width=4, 
                                     box(title="Coming up", width=NULL,
                                         div(
                                           tags$li(p(tags$strong("CKAN")))
                                         ))),
                              column(width=4, 
                                     box(title="In the press", width=NULL,
                                         div(tags$li(tags$a(href="https://www.computerweekly.com/news/252500063/How-the-British-Red-Cross-harnessed-digital-mapping-honed-abroad-for-the-domestic-Covid-19-crisis", target="_blank", "Computer Weekly")),
                                             tags$li(tags$a(href="https://emergencyservicestimes.com/british-red-cross-turns-to-digital-mapping-to-help-meet-increased-demand-for-support-due-to-covid/", target="_blank", "Emergency Services Times")),
                                             tags$li(tags$a(href="https://www.geoconnexion.com/news/british-red-cross-turns-to-digital-mapping-to-help-meet-increased-demand-for-support-due-to-covid-19", target="_blank", "Geoconnexion")),
                                             tags$li(tags$a(href="https://www.charitytimes.com/ct/Digital-mapping-transforming-UK-charities-response-to-emergencies.php", target="_blank", "Charity Times (behind paywall)"))
                                             )))))),
    
    tabItem(tabName='Help',
            column(width = 12, style="padding-right:30px; padding-left:30px; padding-bottom:20px;padding-top:20px;",
                   tabBox(id='information about dashboards', width=NULL,
                          tabPanel("About us",
                                   uiOutput('about_us'),
                                   style = "height:600px; overflow-y: scroll;overflow-x: scroll;"),
                          tabPanel("Areas at risk in an emergency dashboard",
                                   tabBox(id='help_areas_at_risk', width=NULL,
                                          tabPanel("About",
                                                   uiOutput('about_needs'),
                                                   style = "height:600px; overflow-y: scroll;overflow-x: scroll;"
                                          ),
                                          tabPanel("Understanding the map",
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
