shinyServer(function(input, output, session) {

  output$map <- renderLeaflet({
    leaflet(
      data = prj_sites, width = "100%") %>% 
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      addMarkers(
        label        = ~label_html, 
        popup        = ~popup_html) })
  
  output$p_old <- renderPlotly({
    
    #pick the color and shape scale
    scale <- RColorBrewer::brewer.pal(n=10, name = 'PiYG') #For permit type
    scale <- scale[c(1:5, 5, 7, 7:10)] #Create discrete values for permit type
    scale2 <- c("#30A4E1", "#999999", scale) #Concatenate with color scale for Active/Inactive Projects
    scale2 <- setNames( #Create named color scale
      scale2, 
      c("Active Project",
        "Inactive Project",
        "Notice of Intent/Preliminary Permit Application",
        "Draft Pilot License App",
        "Final Pilot License App",
        "Pilot License Issued",
        "Draft License App",
        "Draft Re-License App",
        "Final License App",
        "Final Re-License App",
        "Environmental Assessment",
        "Settlement Agreement",
        "Permit Issued"))
    
    shp   <- c(rep('triangle-up', 3), 'triangle-down', 'triangle-up', 'triangle-down', 'triangle-up', 'triangle-down', rep('triangle-up', 3)) #Create shape for permit type symbols
    shp2 <- c(rep(NA, 2), shp) #Concatenate with shapes (NA) for Active/Inactive Projects
    shp2 <- setNames( #Create named shape scale
      shp2, 
      c("Active Project",
        "Inactive Project",
        "Notice of Intent/Preliminary Permit Application",
        "Draft Pilot License App",
        "Final Pilot License App",
        "Pilot License Issued",
        "Draft License App",
        "Draft Re-License App",
        "Final License App",
        "Final Re-License App",
        "Environmental Assessment",
        "Settlement Agreement",
        "Permit Issued"))
    
    
    
    
    #the input to ggplot is what determines the tooltip label
    g <- ggplot(
      d_permits, 
      aes(text = paste('License Date: ', license_date, '\nProject Name: ', project_name, '\nPermit Type: ', permit_type))) +
      #the segment is a gray bar that covers the time period of the permits
      geom_segment(
        data = d_times, 
        aes(x = date_beg, y = project_name, xend = date_end, yend = project_name, color = project_status), size = 4) +
      #the points have colors and shapes indicating different permit types
      geom_point(data = d_permits, 
                 aes(x = license_date, y = project_name, fill = permit_type, shape = permit_type), size = 4, stroke = 0) +
      scale_color_manual(name = "", values = c("#30A4E1", "#999999"), breaks = c("Active Project", "Inactive Project"))+
      scale_fill_manual(name = "", values = scale) + 
      scale_shape_manual(name = "", values = shp) +
      #label the plot
      labs(title = "MHK Project Timeline", x = "Year of Project", y = "") +
      facet_grid(rows = vars(technology_type), scales='free_y', space = 'free') +
      #choose a theme
      theme(panel.grid.minor = element_blank(), 
            panel.grid.major = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            #legend.margin=margin(100,100,100,100),
            #legend.box.margin=margin(20,20,20,20),
            #legend.position = c(0.9, 0.84),
            #legend.background = element_rect(fill = "transparent", colour = NA),
            #axis.text.y = axis.groups(unique(d_times$technology_type)),
            axis.text.x = element_text(color="black", size=12, angle=45, vjust=1, hjust = 1),
            axis.text.y = element_text(color="black", size=12, vjust = -1),
            axis.title.y=element_text(face="bold", size=13),
            axis.title.x=element_text(face="bold", size=13),
            #plot.margin = margin(.15, .2, 0, 0, "cm"),
            plot.background = element_rect(fill = "transparent", colour = NA))
    
    # interactive plot with tooltip
    #specify the tooltip in the ggplotly function to get custom text
    ggplotly(g, tooltip = 'text', height = 700, width = 1000)
  
  })
  
  output$p <- renderPlotly({
    
    fig <- plot_ly(colors = scale2, symbols = shp2, height=800)
    
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
      size = 10)
    
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
        showline = TRUE,
        showgrid = FALSE,
        type = "category"
      ),
      margin = list(
        r = 10, 
        t = 25, 
        b = 40, 
        l = 100
      ),
      legend = list(
        x = 1, 
        y = 0.5
      ), 
      shapes = list(
        list(
          line = list(
            color = "black", 
            width = 0.8
          ), 
          type = "line", 
          x0 = -0.3, 
          x1 = 1, 
          xref = "paper", 
          y0 = 1-0.125, 
          y1 = 1-0.125, 
          yref = "paper"
        ), 
        list(
          line = list(
            color = "black", 
            width = 0.8
          ), 
          type = "line", 
          x0 = -0.3, 
          x1 = 1, 
          xref = "paper", 
          y0 = 1-0.45, 
          y1 = 1-0.45, 
          yref = "paper"
        ),
        list(
          line = list(
            color = "black", 
            width = 0.8
          ), 
          type = "line", 
          x0 = -0.3, 
          x1 = 1, 
          xref = "paper", 
          y0 = 1, 
          y1 = 1, 
          yref = "paper"
        ),
        list(
          line = list(
            color = "black", 
            width = 0.8
          ), 
          type = "line", 
          x0 = -0.3, 
          x1 = 1, 
          xref = "paper", 
          y0 = 0, 
          y1 = 0, 
          yref = "paper"
        )
      ),
        annotations = list(
          list(
            x = -0.0951769406393, 
            y = 1.03, 
            showarrow = FALSE, 
            text = "Project Name", 
            xref = "paper", 
            yref = "paper",
            font = list(size = 14)
          ), 
          list(
            x = -0.3, 
            y = 1.03, 
            showarrow = FALSE, 
            text = "Technology", 
            xref = "paper", 
            yref = "paper",
            font = list(size = 14)
          ), 
          list(
            x = -0.3, 
            y = 0.98, 
            showarrow = FALSE, 
            text = "Riverine", 
            xref = "paper", 
            yref = "paper",
            font = list(size = 14)
          ), 
          list(
            x = -0.3, 
            y = 1-0.125, 
            showarrow = FALSE, 
            text = "Tidal", 
            xref = "paper", 
            yref = "paper",
            font = list(size = 14)
          ),
          list(
            x = -0.3, 
            y = 1-0.4625, 
            showarrow = FALSE, 
            text = "Wave", 
            xref = "paper", 
            yref = "paper",
            font = list(size = 14)
          )
        )
      )
    
    #fig
  
   })
  
  
  #TEST
  # fig <- plot_ly(orientation='h', line=list(color='gray'), height=400, width=600)
  # fig <- fig %>% add_boxplot(x=c(2,3,1,5), y=c('A','A','A','A'), name='A')
  # fig <- fig %>% add_boxplot(x=c(8,3,6,5), y=c('B','B','B','B'), name='B')
  # fig <- fig %>% add_boxplot(x=c(2,3,2,5), y=c('C','C','C','C'), name='C')
  # fig <- fig %>% add_boxplot(x=c(7.5,3,6,4), y=c('D','D','D','D'), name='D')
  # fig <- fig %>% layout(
  #   title = '',
  #   yaxis = list(
  #     autorange = TRUE, 
  #     categoryorder = "category descending", 
  #     domain = c(0, 1), 
  #     range = c(-0.5, 3.5), 
  #     showline = TRUE, 
  #     title = "", 
  #     type = "category"
  #   ),
  #   margin = list(
  #     r = 10, 
  #     t = 25, 
  #     b = 40, 
  #     l = 110
  #   ),
  #   legend = list(
  #     x = 0.986145833333, 
  #     y = 0.936263886049
  #   ), 
  #   shapes = list(
  #     list(
  #       line = list(
  #         color = "rgba(68, 68, 68, 0.5)", 
  #         width = 1
  #       ), 
  #       type = "line", 
  #       x0 = -0.3, 
  #       x1 = 1.2, 
  #       xref = "paper", 
  #       y0 = 0.5, 
  #       y1 = 0.5, 
  #       yref = "paper"
  #     ), 
  #     list(
  #       line = list(
  #         color = "rgba(68, 68, 68, 0.63)", 
  #         width = 1
  #       ), 
  #       type = "line", 
  #       x0 = -0.3, 
  #       x1 = 1.2, 
  #       xref = "paper", 
  #       y0 = 1, 
  #       y1 = 1, 
  #       yref = "paper"
  #     )
  #   ),
  #   annotations = list(
  #     list(
  #       x = -0.0951769406393, 
  #       y = 1.06972670892, 
  #       showarrow = FALSE, 
  #       text = "Subgroup", 
  #       xref = "paper", 
  #       yref = "paper"
  #     ), 
  #     list(
  #       x = -0.235516552511, 
  #       y = 1.07060587474, 
  #       showarrow = FALSE, 
  #       text = "Group", 
  #       xref = "paper", 
  #       yref = "paper"
  #     ), 
  #     list(
  #       x = -0.235516552511, 
  #       y = 0.922906017856, 
  #       showarrow = FALSE, 
  #       text = "One", 
  #       xref = "paper", 
  #       yref = "paper"
  #     ), 
  #     list(
  #       x = -0.235516552511, 
  #       y = 0.375, 
  #       showarrow = FALSE, 
  #       text = "Two", 
  #       xref = "paper", 
  #       yref = "paper"
  #     )
  #   )
  # )
  
  
  
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
