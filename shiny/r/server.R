server <- function(input, output) {
  janzen_data <- reactive({
    data <- filter(
      shiny_janzen,
      singleton < input$singleton,
      sampling_range >= input$sampling_range
    ) %>% 
      left_join(shiny_bioclim, by = c("location", "elev_band")) %>% 
      left_join(shiny_paleoclim, by = "location") %>% 
      drop_na(starts_with("bio"))
    
    if (input$bioclim == "delta_mat") {
      data <- drop_na(data, starts_with("delat"))
    }
    
    if ("standardise_sampling_range" %in% input$options) {
      data <- data %>% 
        mutate(
          sampling_min = sampling_max - input$sampling_range,
          sampling_range = sampling_max - sampling_min
        ) %>% 
        filter(elev_max > sampling_min) %>%
        mutate(
          elev_min = if_else(
            elev_min < sampling_min,
            sampling_min,
            elev_min
          ),
          elev_range = elev_max - elev_min,
          elev_range = if_else(elev_range == 0, 10, elev_range)
        )
    } else {
      data <- filter(data, sampling_min <= input$sampling_min)
    }
    
    if (input$species_by == "sigma") {
      data <- mutate(data, sigma = input$sigma)
    } else if (input$species_by == "mid_point") {
      data <- group_by(data, location) %>% 
        mutate(sigma = (sampling_max + sampling_min) / 2) %>% 
        ungroup()
    }
    
    if (any(c("sigma", "mid_point") %in% input$species_by)) {
      data <- data %>% 
        mutate(
          max_buffer = sigma + input$buffer,
          min_buffer = sigma - input$buffer
        ) %>%
        rowwise() %>%
        filter(
          any(between(c(elev_min, elev_max), min_buffer, max_buffer)) | elev_min < min_buffer & elev_max > max_buffer
        ) %>% 
        ungroup()
    }
    
    data <- group_by(data, location)
    
    if (input$species_by == "none") {
      data <- mutate(data, across(starts_with("bio"), mean))
    }
    
    data <- data %>% 
      mutate(
        elev_range = mean(elev_range),
        n_sp = n()
      ) %>% 
      ungroup() %>% 
      distinct(location, .keep_all = TRUE)
    
    if ("log" %in% input$options) {
      data <- mutate(data, elev_range = log(elev_range))
    }
    
    if (all(c("display_locations", "display_sampling_range") %in% input$options)) {
      data <- mutate(data, label = paste0(location, " (", sampling_range, " m)"))
    } else if (all(c("display_locations", "display_nb_sp") %in% input$options)) {
      data <- mutate(data, label = paste0(location, " (", n_sp, " sp.)"))
    } else if ("display_locations" %in% input$options) {
      data <- mutate(data, label = location)
    } else if ("display_sampling_range" %in% input$options) {
      data <- mutate(data, label = paste(sampling_range, "m"))
    } else if ("display_nb_sp" %in% input$options) {
      data <- mutate(data, label = paste(n_sp, "sp."))
    }
    
    if (input$type == "islands") {
      data <- filter(data, type == "island")
    } else if (input$type == "continents") {
      data <- filter(data, type == "continent")
    }
    
    data
  })
  
  output$janzen_plot <- renderPlot({
    bioclim_ls <- list(
      "Diurnal temperature range (°C)" = "bio2",
      "Temperature seasonality (°C)" = "bio4",
      "Temperature variability (°C) [21000-0 BP]" = "delta_mat",
      "Mean annual temperature (°C)" = "bio1",
      "Mean annual precipitation (mm)" = "bio12",
      "Mean temp. of warmest quarter (°C)" = "bio10",
      "Mean temp. of coldest quarter (°C)" = "bio11"
    )
    
    plot <- janzen_data() %>% 
      ggplot(aes(
        x = .data[[input$bioclim]],
        y = elev_range,
        shape = if (input$type == "all_type_shapes") {
          type
        } else {
          NULL
        },
        color = if (any(c("display_locations", "display_sampling_range", "display_nb_sp") %in% input$options)) {
          zone
        } else {
          NULL
        }
      )) +
      geom_point(size = 2) +
      theme_classic() +
      labs(
        y = "Mean elevational range size (m)",
        x = names(bioclim_ls[bioclim_ls == input$bioclim])
      ) +
      theme(
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 17)
      ) +
      scale_color_manual(values = c("blue", "red"))
    
    if (any(c("display_locations", "display_sampling_range", "display_nb_sp") %in% input$options)) {
      plot <- plot + geom_text_repel(aes(label = label), size = 4.5, force = 5, show.legend = FALSE)
    }
    if ("log" %in% input$options) {
      plot <- plot + ylab("log(elevational range size)")
    }
    if ("lm" %in% input$options) {
      plot <- plot + geom_smooth(method = "lm")
    }
    
    plot + theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 14)
    )
  })
}