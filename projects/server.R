shinyServer(function(input, output, session) {

  output$map <- renderLeaflet({
    leaflet(
      data = prj_sites, width = "100%") %>% 
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      addMarkers(
        label        = ~label_html, 
        popup        = ~popup_html) })
  
  output$p <- renderPlotly({
    
    fig <- plot_ly(colors = scale2, symbols = shp2, height = 700) #height=700, width = 1000
    
    fig <- fig %>% 
      add_segments(
        data = d_times,
        x = ~date_beg,
        xend = ~date_end,
        y = ~project_name,
        yend = ~project_name,
        color = ~project_status,
        line = list(width = 10))
    #fig
    #plotly_json(p = fig)
    
    fig <- fig %>% add_markers(
      data = d_permits,
      x = ~license_date, 
      y = ~project_name,
      symbol = ~permit_type,
      symbols = shp,
      color = ~permit_type,
      colors = scale, 
      size = 10,
      hoverinfo = "text",
      hovertext = paste('License Date: ', d_permits$license_date, 
                        '<br> Project Name: ', d_permits$project_name, 
                        '<br>Permit Type: ', d_permits$permit_type))
    
    #fig
    #plotly_json(p = fig)
    
    fig <- fig %>% layout(
      xaxis = list(
        title = 'Date',
        showline = FALSE,
        showgrid = FALSE
      ),
      yaxis = list(
        title = '',
        autorange = "reversed",
        domain = c(0,1),
        range = c(0, length(unique(d_times$project_name))),
        showline = FALSE,
        showgrid = FALSE,
        type = "category"
      ),
      # margin = list(
      #   r = 10, 
      #   t = 25, 
      #   b = 40, 
      #   l = 100
      # ),
      legend = list(
        x = 1.01, 
        y = 0.5
      ), 
      shapes = list(
        list(
          line = list(
            color = "black", 
            width = 0.8
          ), 
          type = "line", 
          x0 = 0, 
          x1 = 1, 
          xref = "paper", 
          y0 = -0.5 + riv.n, #Defines horizontal line separating riverine projects from tidal projects
          y1 = -0.5 + riv.n, 
          yref = "y"
        ), 
        list(
          line = list(
            color = "black", 
            width = 0.8
          ), 
          type = "line", 
          x0 = 0, 
          x1 = 1, 
          xref = "paper", 
          y0 = -0.5 + riv.n + tid.n, #Defines horizontal line separating tidal projects from wave projects
          y1 = -0.5 + riv.n + tid.n, 
          yref = "y"
        ), 
        list(
          line = list(
            color = "black", 
            width = 0.8
          ), 
          type = "line", 
          x0 = 0, 
          x1 = 1, 
          xref = "paper", 
          y0 = 0.025, 
          y1 = 0.025, 
          yref = "paper"
        ),
        list(
          line = list(
            color = "black", 
            width = 0.8
          ), 
          type = "line", 
          x0 = 0, 
          x1 = 1, 
          xref = "paper", 
          y0 = 0.975, 
          y1 = 0.975, 
          yref = "paper"
        ),
        list(
          line = list(
            color = "black", 
            width = 0.8
          ), 
          type = "line", 
          x0 = 0, 
          x1 = 0, 
          xref = "paper", 
          y0 = 0.975, 
          y1 = 0.025, 
          yref = "paper"
        )
      ),
        annotations = list(
          list(
            x = 1,
            y = (-1 + riv.n)/2,
            showarrow = FALSE,
            text = "<b>Riverine</b>",
            xref = "paper",
            yref = "y",
            align = "center",
            font = list(size = 14),
            textangle = "90",
            yshift = 4
          ),
          list(
            x = 1,
            y = (-1 + riv.n + (tid.n)/2),
            showarrow = FALSE,
            text = "<b>Tidal</b>",
            xref = "paper",
            yref = "y",
            align = "center",
            font = list(size = 14),
            textangle = "90"
          ),
          list(
            x = 1,
            y = (-1 + riv.n + tid.n + (wav.n)/2),
            showarrow = FALSE,
            text = "<b>Wave</b>",
            xref = "paper",
            yref = "y",
            align = "center",
            font = list(size = 14),
            textangle = "90"
          )
        )
      )
    
    #fig
  
   })
  
   output$click <- renderPrint({
     d <- event_data("plotly_click")
     if (is.null(d)) "Click events appear here (double-click to clear)" else d
   })
  
  # Use a separate observer to zoom to point
  observe({
    d <- event_data("plotly_click")
    req(d)
    
    proxy <- leafletProxy("map")
    
    s <- prj_sites %>% 
      filter(project_name == d$y)
      
    proxy %>% 
      flyTo(s$longitude, s$latitude, 8)
    
  })
  
})
