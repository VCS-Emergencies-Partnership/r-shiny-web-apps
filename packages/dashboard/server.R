# ---- server ---- #
server = function(input, output, session) {
  
  observeEvent(input$home, {
    newtab <- switch(input$sidebar_id, "home")
    updateTabItems(session, "sidebar_id", newtab)
    
  })
  
  observeEvent(input$e_catalogue_box, {
    newtab <- switch(input$sidebar_id, "resource_catalogue")
    updateTabItems(session, "sidebar_id", newtab)
  })
  
  
  observeEvent(input$references, {
    newtab <- switch(input$sidebar_id, "references")
    updateTabItems(session, "sidebar_id", newtab)
    
    
  })
  
   observeEvent(input$latest_news_box, {
     newtab <- switch(input$sidebar_id, "latest_news_tab")
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
  observe({
    
    req(input$sidebar_id)
    
    if (input$sidebar_id == 'references') {
      
      output$refs_header <- renderUI({
        div(h3(tags$strong("Data, licenses and privacy policy")),
            hr())  
      })
      
      output$refs <- renderUI({
        create_data_license_help()
        
      })
    }
    
  })
  
  
  # --- help for about needs dashboard ---
  observe({
    
    req(input$sidebar_id)
    
    if (input$sidebar_id == 'Help') {
      
      # About us
      output$about_us <- renderUI({
        create_about_us(time, last_updated_date)
      })
      
      # About areas at risk dashboard
      output$about_needs <- renderUI({
        create_about_areas_at_risk(time, last_updated_date)
      })
      
      output$understand_map_top <- renderUI({
        create_map_help_top()
      })
      
      output$understand_map_middle <- renderUI({
        create_map_help_middle()
      })
      
      output$understand_map_bottom <- renderUI({
        create_map_help_bottom()
      })
      
      output$emergency_covid <- renderUI({
        create_emergency_covid(covid_data_date)   
      })
      
      output$emergency_flooding <- renderUI({
        create_flooding_help()
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
  
  
  output$home_map_headlines <- renderUI({
    rfs_highlights(requests_home, 1)
    
  })
  
  output$home_map <- renderLeaflet ({
    
    requests_home_no_na <- requests_home %>%
      filter(!is.na(admin_district_code))
    
    labels <- paste0(
      "<strong>Status: </strong>",  requests_home_no_na$request_status, "</br>",
      "<strong>Date of request: </strong>",  requests_home_no_na$formatted_date, "</br>",
      "<strong>Request for: </strong>",  "coming soon", "</br>",
      "<strong>Lead broker organisation: </strong>", requests_home_no_na$lead_broker_organisation
    ) %>%
      lapply(htmltools::HTML)
    
    #glimpse(requests_home_no_na)
    
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
                       stroke=F,
                       label=labels)
    #clusterOptions = markerClusterOptions())
    
  })
  
  output$source_home_map <- renderUI({
    div(p(tags$br(),
      tags$em(paste0("Source: Request for support service; see internal dashboards")
              
        ),
      tags$br()
      )
    )
  })
  
  output$latest_concerns_headline <- renderUI({
   
    max_in_need <- pulse %>% arrange(-desc(proportion_respondents))
    max_in_need <- tail(max_in_need, 1) %>%
      rename(`Proportion of respondents` = proportion_respondents)
    
    max_increase <- pulse %>% arrange(-desc(greatest_diff)) 
    max_increase <- tail(max_increase, 1) %>%
      rename(`Greatest increase in concern` = greatest_diff)
    
    #div(
    #  p(style='font-size:2.25vh',
    #    tags$strong(paste0(max_in_need$`Proportion of respondents`,"%")), paste0("(",max_in_need$group_total, ")"), "of respondents
    #              reported", tags$strong(max_in_need$clean_names), "as a concern
    #              in the next 14 days."))
    
    div(
      p(style='font-size:2.25vh',
        "The greatest", tags$strong("increase"), "in concern,", tags$strong(paste0(max_increase$`Greatest increase in concern`,"%")), paste0("(",max_increase$group_total, "),"), "was reported for", tags$strong(max_increase$clean_names)
    ))
    
    
    
  })
  
  output$source_concerns <- renderUI({
    div(p(tags$br(),
      tags$em(paste0("Source: Latest pulse check survey; see internal dashboards")
                  ),
      tags$br()
          )
    )
  })
  
  # pulse major concerns
  output$concerns <- renderEcharts4r({
    
    pulse <- pulse %>%
      rename(`Proportion of respondents reporting concern` = proportion_respondents, `% change in respondents reporting concern`=`greatest_diff`) %>%
      arrange(-desc(`Proportion of respondents reporting concern`))
    
    concerns_pulse <- pulse %>%
      e_charts(x = clean_names) %>%
      e_bar(`Proportion of respondents reporting concern`, bar_width=1, showBackground=T) %>%
      e_line(`% change in respondents reporting concern`) %>%
      e_hide_grid_lines() %>%
      e_flip_coords() %>%
      e_grid(containLabel = TRUE, left=20, right=30, top=60, bottom=5, height='85%') %>%
      e_x_axis( position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=T, fontWeight='bold', margin=2),min=-20, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=20) %>%
      e_y_axis(axisLabel = list(interval = 0, show = T), splitLine = list(show = FALSE)) %>%
      #e_axis_labels(x="% respondents") %>%
      e_legend() %>%
      e_tooltip()
    #name='% respondents', nameLocation="middle",
  })
  
  
  output$latest_insight_headline <- renderUI({
    vac_second_dose_highest <- vac_data %>%
      filter(dose=='Second dose')
    
    
    
    vac_second_dose_highest <- vac_second_dose_highest %>%
      arrange(-desc(prop_of_population)) %>%
      tail(n=1)
    
    #glimpse(vac_second_dose_highest)
    
    div(
      p(tags$strong(paste0(vac_second_dose_highest$prop_of_population, "%")),
        "of those aged", tags$strong(vac_second_dose_highest$age_range),
        "have received their", tags$strong("second dose", style='color:#91cc75'), "of vaccine against COVID-19",
        style='font-size:2.25vh')
      )
    
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
      e_grid(containLabel = TRUE, left=30, right=30, top=40, bottom=5, height='85%') %>%
      e_x_axis(name='% population', nameLocation="middle", position='top', axisLabel=list(formatter = "{value}%", show=T, fontSize=12, showMinLabel=F, fontWeight='bold', margin=10),min=0, max=100, axisLine=list(show=F), axisTick=list(show=F, length=0), minInterval=100) %>%
      e_y_axis(axisLabel = list(interval = 0, show = T)) %>%
      e_y_axis(show=T) %>%
      e_legend(show=T) %>%
      e_tooltip()
    
  })
  
  output$source_insight_headline <- renderUI({
    vac_meta <- vac_data %>%
      tail(n=1)

    
    div(p(tags$em("See vaccine uptake dashboard in internal dashboards."),
    tags$br(),
    tags$em("Source:",vac_meta$source, style="font-size:10px"),
    tags$br(), tags$em("Time-period:", vac_meta$time_span,
            "published:", vac_meta$published,
            style="font-size:10px")))
    
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
        
        if (dim(flood_warning_points)[1]==0) {
          flood_warnings2show <- data.frame()
        }
        else {
        
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
    }
  })
  
  # --- flood warning map polygons --- 
  filteredFlood_warnings_polygons <- reactive({
    
    req(input$sidebar_id)
    if (input$sidebar_id == 'unmetneed') {
      
      if (input$theme == 'Flooding') {
        
        if (dim(flood_warning_polygons)[1]==0) {
          flood_warnings2show <- data.frame()
        }
        else {
        
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
    
    if (dim(flood_warning_points)[1]==0) {
      flood_warning_labels <- paste0('no flood warnings')
    }
    else {
    
    flood_warning_labels <- paste0(
      sprintf("<strong>%s</strong><br/>",  filteredFlood_warnings_points()$description),
      filteredFlood_warnings_points()$severity, ": ", filteredFlood_warnings_points()$alertlevelmeaning, "<br/>",
      "last updated (at time dashboard refreshed): ",  filteredFlood_warnings_points()$lastupdateday, " ", filteredFlood_warnings_points()$lastupdatetime) %>%
      lapply(htmltools::HTML)
    }
    
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
            select('LAD19CD', 'Local Authority'= to_show, 'Region'='TacticalCell', `covid cases per 100,000`, `Total cases`, `% change in covid cases`)
          
          covid_cases4list <- covid_lads_in_tc %>% arrange(-`covid cases per 100,000`) %>%
            select(-'LAD19CD') #%>% 
          #rename_at(vars(`covid cases per 100,000`), ~ paste0(covid_week, .)) 
        }
        else {
          if (input$tactical_cell != '-- England --' & input$lad_selected == 'All local authorities in region') {
            
            covid_lads_in_tc <- covid_area2focus %>% filter(TacticalCell == input$tactical_cell) %>%
              arrange(-`covid cases per 100,000`) %>%
              select('LAD19CD', 'Local Authority'= to_show, 'Region'='TacticalCell', `covid cases per 100,000`, `Total cases`, `% change in covid cases`)
            
            covid_cases4list <- covid_lads_in_tc %>% arrange(-`covid cases per 100,000`) %>%
              select(-'LAD19CD') #%>% 
            #rename_at(vars(`covid cases per 100,000`), ~ paste0(covid_week, .)) 
          }
          
          else {
            covid_lads_in_tc <- covid_area2focus %>% filter(Name == input$lad_selected) %>%
              arrange(-`covid cases per 100,000`) %>%
              select('LAD19CD', 'Local Authority'= to_show, 'Region'='TacticalCell', `covid cases per 100,000`, `Total cases`, `% change in covid cases`)
            
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
            select('LAD19CD', 'Local Authority'= to_show, 'Region'='TacticalCell', `covid cases per 100,000`, `Total cases`, `% change in covid cases`)
          
          covid_cases4list <- covid_lads_in_tc %>% arrange(-`% change in covid cases`) %>%
            select(-'LAD19CD') # %>% 
          #rename_at(vars(`covid cases per 100,000`), ~ paste0(covid_week, .)) 
        }
        else {
          if (input$tactical_cell != '-- England --' & input$lad_selected == 'All local authorities in region') {
            
            covid_lads_in_tc <- covid_area2focus %>% filter(TacticalCell == input$tactical_cell) %>%
              arrange(-`% change in covid cases`) %>%
              select('LAD19CD', 'Local Authority'= to_show, 'Region'='TacticalCell', `covid cases per 100,000`, `Total cases`,`% change in covid cases`)
            
            covid_cases4list <- covid_lads_in_tc %>% arrange(-`% change in covid cases`) %>%
              select(-'LAD19CD') #%>% 
            #rename_at(vars(`covid cases per 100,000`), ~ paste0(covid_week, .)) 
          }
          
          else {
            covid_lads_in_tc <- covid_area2focus %>% filter(Name == input$lad_selected) %>%
              arrange(-`% change in covid cases`) %>%
              select('LAD19CD', 'Local Authority'= to_show, 'Region'='TacticalCell', `covid cases per 100,000`, `Total cases`, `% change in covid cases`)
            
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
          
          flood_all <- filtered_areas_at_risk_flooding_resilience()
          
          plot_flood_warning_polygon <- tryCatch(
            {
              st_as_sf(filteredFlood_warnings_polygons())
            },
            error = function(x){
              data.frame()
            }
          )
          
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
  
  # Create top 10 list options 
  observe({
    # top 10 list to show options
    output$top10options <- renderUI({
      top10_options(input$theme)
    })
  })
  
  # make areas to focus list
  observe({
    
    # plot list title
    output$title_focus_list <- renderUI({
    # function to plot title
    top_10_list_title(theme=input$theme, 
                      rank=store_rank_wanted, 
                      tc=input$tactical_cell, 
                      lad=input$lad_selected,
                      date_of_data=covid_data_date,
                      flood_points=flood_warning_points)
    })
  
    # plot list
    output$top10list <- renderUI({
      glimpse(filtered_areas2focus_list())
      # function to plot list
      top_10_list(top10list=filtered_areas2focus_list(),
      theme=input$theme, 
      rank=store_rank_wanted, 
      tc=input$tactical_cell, 
      lad=input$lad_selected)
      
    })

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
          DT::datatable(charities_found, filter=list(position='top'),
                        selection =c('single'),
                        options = list(dom='tp', #should remove top search box the p includes paging
                                       paging = T,
                                       pageLength=10,
                                       lengthMenu = c(5, 10, 15, 20),
                                       scrollX=T,
                                       scrollY='300px',
                                       autoWidth = T,
                                       columnDefs = list(list(width='400px',targets=c(3))),
                                       escape=FALSE,
                                       initComplete = htmlwidgets::JS(
                                         "function(settings, json) {",
                                         paste0("$(this.api().table().container()).css({'font-size':'12px'});"),
                                         "}")
                        )) })
        
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
            DT::datatable(charities_found, filter=list(position='top'),
                          selection =c('single'),
                          options = list(dom='tp', #should remove top search box the p includes paging
                                         paging = T,
                                         pageLength=10,
                                         lengthMenu = c(5, 10, 15, 20),
                                         scrollX=T,
                                         scrollY='300px',
                                         autoWidth = T,
                                         escape=FALSE,
                                         columnDefs = list(list(width='400px',targets=c(3))),
                                         initComplete = htmlwidgets::JS(
                                           "function(settings, json) {",
                                           paste0("$(this.api().table().container()).css({'font-size':'12px'});"),
                                           "}")
                          )) })
          
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
            DT::datatable(charities_found, filter=list(position='top'),
                          selection =c('single'),
                          options = list(dom='tp', #should remove top search box the p includes paging
                                         paging = T,
                                         pageLength=10,
                                         lengthMenu = c(5, 10, 15, 20),
                                         scrollX=T,
                                         scrollY='300px',
                                         autoWidth = T,
                                         escape=FALSE,
                                         columnDefs = list(list(width='400px',targets=c(3))),
                                         initComplete = htmlwidgets::JS(
                                           "function(settings, json) {",
                                           paste0("$(this.api().table().container()).css({'font-size':'12px'});"),
                                           "}")
                          )) })
          
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
          DT::datatable(charities_found, filter=list(position='top'),
                        selection =c('single'),
                        options = list(dom='tp', #should remove top search box the p includes paging
                                       paging = T,
                                       pageLength=10,
                                       lengthMenu = c(5, 10, 15, 20),
                                       scrollX=T,
                                       scrollY='300px',
                                       autoWidth = T,
                                       escape=FALSE,
                                       columnDefs = list(list(width='400px',targets=c(3))),
                                       initComplete = htmlwidgets::JS(
                                         "function(settings, json) {",
                                         paste0("$(this.api().table().container()).css({'font-size':'12px'});"),
                                         "}")
                        )) })
        
        output$local_orgs_ui <- renderUI({
          DT::dataTableOutput('local_orgs')
        })
      }
    }
  })
  
  
  
  
  # going to external sites
  # Dashboards
  observeEvent(input$internal_reports_from_box, {
    internal_report_link()
  })
  
  observeEvent(req(input$sidebar_id == 'internal_reports_from_sidebar'), {
    internal_report_link()
  })
  
  
  
  # Reports from the sector
  observeEvent(input$vcs_reports_box, {
    reports_from_the_sector_link()
  })
  
  observeEvent(req(input$sidebar_id == 'vcs_report_sidebar'), {
    reports_from_the_sector_link()
    })
  
  
  
  observeEvent(req(input$sidebar_id == 'community_assets_sidebar'),{
    webmap_link()
  })
  
  observeEvent(input$community_assets_box,{
    webmap_link()
  })
  
  # --- resource bank ---
  
  
  # plot original 
  observeEvent(req(input$sidebar_id == 'resource_catalogue') ,{
    
    output$dynamic_boxes <- renderUI({
      plot_resource_cat(resources_info)
    })
    
  })
  
 
  observeEvent(input$resource_search, {
    
    # to stop it being called in other windows - it shouldn't be but is
    if(req(input$sidebar_id == 'resource_catalogue')) {
      #print(input$resource_search)
      if(input$resource_search == '') {
        
        output$dynamic_boxes <- renderUI({
          plot_resource_cat(resources_info)
        })
      }
      
      else {
        output$dynamic_boxes <- renderUI({
          search_resources(resources_info, input$resource_search)
        })
        
      }
    }
    
  })
  
  #latest news press releases
  observeEvent(req(input$sidebar_id == 'latest_news_tab'), {
    output$press_highlights <- renderUI({
      in_the_press()
    })
  })
  
  observeEvent(req(input$sidebar_id == 'latest_news_tab'), {
    
    output$coming_up <- renderUI({
      coming_up_text()
    })
  })
  
  
}

