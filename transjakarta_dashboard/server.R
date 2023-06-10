function(input, output, session){
  output$busiest_month <- renderValueBox({
    valueBox(
      value = month.name[pasenger_volume_data[[1]][1]],
      subtitle = "Busiest Month",
      icon = icon("calendar-day"),
      color = "fuchsia"
    )
  })
  
  output$busiest_route <- renderValueBox({
    valueBox(
      value = top10_busy_routes[[1]][1],
      subtitle = "Busiest Routes",
      icon = icon("route"),
      color = "maroon"
    )
  })
  
  output$total_passengers <- renderValueBox({
    valueBox(
      value = format(
        sum(pasenger_volume_data$total_penumpang),
        big.mark = ",",
        trim = T
        ),
      subtitle = "Total Passengers",
      icon = icon("people-arrows"),
      color = "teal"
    )
  })
  
  output$num_of_routes <- renderValueBox({
    valueBox(
      value = length(levels(data$trayek)),
      subtitle = "Number of Routes",
      icon = icon("location-dot", class = "text-light"),
      color = "purple"
    )
  })
  
  output$num_of_transportation_type <- renderValueBox({
    valueBox(
      value = length(levels(data$jenis)),
      subtitle = "Number of Transportation Type",
      icon = icon("bus-simple"),
      color = "olive"
    )
  })
  
  
  output$num_of_routes_with_no_passenger <- renderValueBox({
    routes_with_no_passenger_data <- data %>% 
      filter(jumlah_penumpang == 0)
    
    valueBox(
      value = length(unique(routes_with_no_passenger_data$trayek)),
      subtitle = "Number of Routes with No Passengers on any Month",
      color = "black"
    )
  })
  
  dashboard_route_filter <- reactiveValues(route = NULL)
  
  observeEvent(input$dashboard_route_filter, {
    
    dashboard_route_filter$route <- input$dashboard_route_filter
    
  })
  
  observeEvent(input$btn_reset_dashboard_route_filter, {
    dashboard_route_filter$route = NULL
    
    updateSelectizeInput(inputId = "dashboard_route_filter",
                        selected = NULL,
                        choices = levels(data$trayek)
                        )
  })
  
  
  
  output$table_routes_list <- renderDataTable({
    tblData <- data %>% 
      select(trayek)
    
    tblData
      
  }, options = list(pageLength = 10))
  
  
  
  dashboard_month_filter <- reactiveValues(month = "All")
  observeEvent(input$dashboard_month_filter, {
    dashboard_month_filter$month = input$dashboard_month_filter
  })
  
  observeEvent(input$btn_reset_dashboard_month_filter, {
    dashboard_month_filter$month = "All"
    
    updateSelectInput(
      inputId = "dashboard_month_filter",
      selected = "All",
      choices = get_month_list()
      )
  })
  
  output$table_num_of_routes_with_no_passenger <- renderDataTable({
    if(dashboard_month_filter$month == "All"){
      filtered_data <- data
    }else{
      filtered_data <- data %>% 
        filter(bulan == match(dashboard_month_filter$month, month.name))
    }
      
    tblData <- filtered_data %>% 
      filter(jumlah_penumpang == 0) %>% 
      select(trayek)
    
    unique(tblData)
    
  }, options = list(pageLength = 6))
  
  
  output$passenger_volume_chart <-  renderPlotly({
    
    filter_data <- NULL
    
    if(is.null(dashboard_route_filter$route)){
      
      filter_data <- pasenger_volume_data
    }else{
      filter_data <- data %>% 
        filter(trayek %in% dashboard_route_filter$route) %>% 
        select(bulan, jumlah_penumpang) %>% 
        group_by(bulan) %>% 
        summarise(total_penumpang = sum(jumlah_penumpang)) %>% 
        arrange(desc(total_penumpang))
    }
    
    passenger_volume_chart <- filter_data %>% 
      ggplot(mapping = aes(x = total_penumpang, 
                           y = reorder(month.name[bulan], total_penumpang),
                           text = glue("Passenger Volume: {format(total_penumpang, big.mark = ',', trim = T)}")
      )
      )+
      geom_col(aes(fill = total_penumpang))+
      scale_fill_gradient(high = "#48cae4" , low = "#264653")+
      scale_x_continuous(labels = number_format(big.mark = ","))+
      labs(
        title = "Passenger Volume trend by Month",
        x = "Passenger Volume",
        y = NULL,
      )+
      guides(fill = "none")+
      theme_classic()
    
    ggplotly(passenger_volume_chart, tooltip = "text")
  })
  
  
  routes_month_filter <- reactiveValues(month = "All")
  
  observeEvent(input$routes_month_filter, {
    routes_month_filter$month = input$routes_month_filter
  })
  
  observeEvent(input$btn_reset_transport_type_month_filter, {
    routes_month_filter$month = "All"
    
    updateSelectInput(
      inputId = "routes_month_filter",
      selected = "All",
      choices = get_month_list()
    )
  })
  
  output$top10_busiest_routes_chart <- renderPlotly({
    
    if(routes_month_filter$month == "All"){
      filtered_data <- top10_busy_routes
    }else{
      filtered_data <- data %>% 
        filter(bulan == match(routes_month_filter$month, month.name)) %>% 
        group_by(trayek) %>% 
        summarise(total_penumpang = sum(jumlah_penumpang)) %>% 
        arrange(desc(total_penumpang)) %>% 
        top_n(10)
    }
    
    top10_busiest_routes_chart <- filtered_data %>% 
      ggplot(mapping = aes(x = total_penumpang,
                           y = reorder(trayek, total_penumpang),
                           text = glue("Passenger Volume: {format(total_penumpang, big.mark = ',', trim = T)}")
      )
      )+
      geom_col(aes(fill = total_penumpang))+
      scale_fill_gradient(low = "#9b2226" , high = "#ff758f")+
      labs(
        title = "Top 10 Busiest Routes by Passenger Volume",
        x = "Passenger Volume",
        y = NULL
      )+
      guides(fill = "none")+
      scale_x_continuous(labels = number_format(big.mark = ","))+
      scale_y_discrete(labels = wrap_format(width = 18))+
      theme_classic()
    
    ggplotly(top10_busiest_routes_chart, tooltip = "text")
  })
  
  output$top5_least_busy_routes_chart <- renderPlotly({
    
    if(routes_month_filter$month == "All"){
      filtered_data <- data
    }else{
      filtered_data <- data %>% 
        filter(bulan == match(routes_month_filter$month, month.name))
    }
    
    filtered_data <- filtered_data %>% 
      group_by(trayek) %>% 
      summarise(total_penumpang = sum(jumlah_penumpang)) %>% 
      arrange(total_penumpang) %>% 
      head(5)
    
    top5_least_busy_routes_chart <- filtered_data %>% 
      ggplot(mapping = aes(x = total_penumpang,
                           y = reorder(trayek, -total_penumpang),
                           text = glue("Passenger Volume: {format(total_penumpang, big.mark = ',', trim = T)}")
      )
      )+
      geom_col(aes(fill = total_penumpang))+
      scale_fill_gradient(high = "#212529" , low = "#adb5bd")+
      labs(
        title = "Top 5 Least Busy Routes by Passenger Volume",
        x = "Passenger Volume",
        y = NULL
      )+
      guides(fill = "none")+
      scale_x_continuous(labels = number_format(big.mark = ","))+
      scale_y_discrete(labels = wrap_format(width = 18))+
      theme_classic()
    
    ggplotly(top5_least_busy_routes_chart, tooltip = "text")
  })
  
  output$table_list_of_transportation_type <- renderDataTable({
    
    data <- as.data.frame(levels(data$jenis))
    colnames(data) <- NULL

    data
    
  }, options = list(dom = "t"))
  
  output$transportation_type_proportion_chart <- renderPlotly({
    if(routes_month_filter$month == "All"){
      filtered_data <- data
    }else{
      filtered_data <- data %>% 
        filter(bulan == match(routes_month_filter$month, month.name))
    }
    passenger_num_by_tansportation_type <- filtered_data %>% 
      group_by(jenis) %>% 
      summarise(total_penumpang = sum(jumlah_penumpang)) %>% 
      mutate(proportion = (total_penumpang/sum(total_penumpang)*100))
    
    transportation_type_proportion_chart <- passenger_num_by_tansportation_type %>% 
      ggplot(mapping = aes(x = 1,
                           y = proportion,
                           fill = jenis,
                           text = glue("Passenger Volume: {format(total_penumpang, big.mark = ',', trim = T)}")
      )
      )+
      geom_bar(stat = "identity")+
      geom_text(
        aes(label = paste0(round(proportion, 2), "%")),
        position = position_stack(vjust = 0.5)
      )+
      labs(
        title = "Transportation Type Proportion",
        fill = "Transportation Type",
        x = NULL,
        y = NULL
      )+
      theme_void()
    
    ggplotly(transportation_type_proportion_chart, tooltip = "text")
  })
  
  output$database <- renderDataTable(data, 
                                     options = list(pageLength = 50)
                                     )
}