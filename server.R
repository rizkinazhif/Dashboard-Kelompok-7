server <- function(input, output){
  base_crime <- reactive({
    if (input$year == "all_year") {
      if (input$crime == "all_crime") {
        res <- jmlkejahatan %>%
          select(-tahun) %>%
          group_by(provinsi) %>%
          summarise(y = nyawa + fisik + kesusilaan + kemerdekaan +
                      hakkekerasan + hakmilik + narkotika +
                      penipuan + ketertiban) %>%
          summarise(y = sum(y)) %>%
          rename(
            x = provinsi
          )
      } else {
        res <- jmlkejahatan %>%
          select(provinsi, input$crime) %>%
          group_by(provinsi) %>%
          rename(
            x = provinsi,
            y = input$crime
          ) %>%
          summarise(y = sum(y))
      }
    } else if (input$crime == "all_crime") {
      res <- jmlkejahatan %>%
        filter(tahun == input$year) %>%
        group_by(provinsi) %>%
        summarise(y = nyawa + fisik + kesusilaan + kemerdekaan +
                    hakkekerasan + hakmilik + narkotika +
                    penipuan + ketertiban) %>%
        rename(
          x = provinsi
        )
    } else {
      res <- jmlkejahatan %>%
        filter(tahun == input$year) %>%
        select(provinsi, input$crime) %>%
        rename(
          x = provinsi,
          y = input$crime
        )
    }
    
    res
  })
  
  base_year <- reactive ({
    if (input$crime == "all_crime") {
      res <- jmlkejahatan %>%
        select(-provinsi) %>%
        group_by(tahun) %>%
        summarise(y = nyawa + fisik + kesusilaan + kemerdekaan +
                    hakkekerasan + hakmilik + narkotika +
                    penipuan + ketertiban) %>%
        summarise(y = sum(y)) %>%
        rename(
          x = tahun
        )
    } else {
      res <- jmlkejahatan %>%
        select(tahun, input$crime) %>%
        group_by(tahun) %>%
        rename(
          x = tahun,
          y = input$crime
        ) %>%
        summarise(y = sum(y))
    }
    
    res
  })
  
  base_crime2 <- reactive({
    if (input$year == "all_year") {
      res <- jmlkejahatan %>%
        select(-c(provinsi, tahun)) %>%
        summarise(across(everything(), sum))
    } else {
      res <- jmlkejahatan %>%
        select(-provinsi) %>%
        filter(tahun == input$year) %>%
        group_by(tahun) %>%
        summarise(across(everything(), sum)) %>%
        select(-tahun)
    }
    colnames(res) <- c("Kejahatan terhadap Nyawa",
                       "Kejahatan terhadap Fisik/ Badan",
                       "Kejahatan terhadap Kesusilaan",
                       "Kejahatan terhadap Kemerdekaan Orang",
                       "Kejahatan terhadap Hak Milik/ Barang dengan Penggunaan Kekerasan",
                       "Kejahatan terhadap Hak Milik/ Barang",
                       "Kejahatan Terkait Narkotika",
                       "Kejahatan Terkait Penipuan, Penggelapan, dan Korupsi",
                       "Kejahatan terhadap Ketertiban Umum")
    res <- res[which.max(res)]
    res
  })
  
  base_table <- reactive({
    if (input$year == "all_year") {
      if (input$crime == "all_crime") {
        table.res <- jmlkejahatan.res
        
      } else {
        table.res <- jmlkejahatan %>%
          select(provinsi, input$crime) %>%
          group_by(provinsi) %>%
          rename(
            total = input$crime
          ) %>%
          summarise(total = sum(total))
      }
    } else if (input$crime == "all_crime") {
      table.res <- jmlkejahatan %>%
        filter(tahun == input$year) %>%
        group_by(provinsi) %>%
        summarise(total = nyawa + fisik + kesusilaan + kemerdekaan +
                    hakkekerasan + hakmilik + narkotika +
                    penipuan + ketertiban) 
    } else {
      table.res <- jmlkejahatan %>%
        filter(tahun == input$year) %>%
        select(provinsi, input$crime)
    }
    
    colnames(table.res) <- toupper(colnames(table.res))
    table.res
  })
  
  output$total_crime <- renderValueBox({
    # The following code runs inside the database.
    # pull() bring the results into R, which then
    # it's piped directly to a valueBox()
    base_crime() %>%
      summarise(sumcol = sum(y)) %>%
      pull(sumcol) %>%
      as.integer() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(icon = icon("skull-crossbones"), color = "purple",
               subtitle = "Jumlah Kasus Kejahatan")
  })
  
  output$per_prov <- renderValueBox({
    # The following code runs inside the database
    prov <- base_crime() %>%
      top_n(1, y) %>%
      pull(x)
    base_crime() %>%
      top_n(1, y) %>%
      pull(y) %>%
      prettyNum(big.mark = ",") %>%
      valueBox(icon = icon("map-marked-alt"), color = "light-blue",
               subtitle = paste0("Provinsi dengan Kasus Kejahatan Tertinggi (",
                                 prov, ")")) 
  })
  
  output$most_crime <- renderValueBox({
    kjhtn <- colnames(base_crime2())
    base_crime2()[1, 1] %>%
      pull() %>%
      prettyNum(big.mark = ",") %>%
      valueBox(icon = icon("sort-amount-up"), color = "teal",
               subtitle = paste0("Jenis Kejahatan yang Sering Terjadi (",
                                 kjhtn, ")"))
  })
  
  output$mymap <- renderLeaflet({
    bins <- c(0, 2500, 5000, 7500, 10000, Inf)
    pal <- colorBin("BuGn", domain = data2018@data$total, bins = bins)


    leaflet(data2018) %>%
        addProviderTiles("CartoDB", options = providerTileOptions(
          id = "mapbox.light",
          accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
        addPolygons(
          fillColor = ~pal(total),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 3,
            color = "yellow",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>%
        addLegend(pal = pal, values = ~total, opacity = 0.7, title = NULL,
                  position = "bottomright")
  })
  
  # Top 10 Provinsi
  output$group_totals2 <- renderD3({
    res <- base_crime() %>%
      mutate(label = x) %>%
      top_n(10, y) %>% 
      arrange(desc(y)) %>% 
      r2d3("bar_plot.js")
  })
  
  # Kejahatan tiap tahun
  output$group_totals3 <- renderD3({
    res <- base_year() %>%
      mutate(label = x) %>%
      r2d3("col_plot.js")
  })
  
  #tindak pidana
  base_bubble <- reactive({
    if (input$year == "all_year") {
      res <- dt %>%
        select(-Tahun) %>%
        group_by(Provinsi) %>%
        summarise(across(everything(), sum))
    } else {
      res <- dt %>%
        filter(Tahun == input$year) %>%
        select(-Tahun)
    }
    res
  })
  
  output$tindak_pidana <- renderPlotly({
    p <- base_bubble() %>%
      mutate(pidana_selesai=round(pidana_selesai,0)) %>%
      mutate(proporsi_korban=round(proporsi_korban*100,2)) %>%
      
      # Reorder countries to having big bubbles on top
      arrange(desc(proporsi_korban)) %>%
      mutate(Provinsi = factor(Provinsi, Provinsi)) %>%
      
      # prepare text for tooltip
      mutate(text = paste("Provinsi: ", Provinsi, 
                          "\nProporsi korban: ", proporsi_korban, "%",
                          "\nJumlah Tindak Pidana: ", jml_tindak_pidana, 
                          "\nJumlah Tindak Pidana selesai: ", pidana_selesai, sep="")) %>%
      
      # Classic ggplot
      ggplot( aes(x=pidana_selesai, 
                  y=jml_tindak_pidana, 
                  size = proporsi_korban, 
                  color = Provinsi, 
                  text=text)) +
      xlab("Jumlah Pidana Selesai") +
      ylab("Jumlah Tindak Pidana") +
      geom_point(alpha=0.7) +
      scale_size(range = c(1.4, 19), name="Proporsi Korban") +
      scale_color_viridis(discrete=TRUE, guide=FALSE) +
      theme_classic() +
      theme(legend.position="none",
            axis.title = element_text(size = rel(0.75))) + 
      #ubah axis 0 x dan y ketemu
      scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
    pp <- ggplotly(p, tooltip="text")
    pp
  })
  
  output$table_crime <- renderDT({
    base_table()
  })

  
}