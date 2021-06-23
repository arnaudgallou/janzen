####  Init  ####
  {
    # · Libraries ----
      {
        library(rnaturalearth)
      }
    
    # · Settings ----
      {
        save_map <- FALSE
        colors <- c("#CC3E61", "#3D61CC") #c("#FF4D79", "#4d79ff")
      }
  }

####  Map  ####
  {
    janzen %>% 
      filter(
        sampling_range >= 1500,
        singleton < 25
      ) %>% 
      distinct(id_ref, .keep_all = TRUE) %>% 
      summarise(
        lon,
        lat,
        n_sp,
        transect_length = case_when(
          sampling_range < 2500 ~ "0",
          sampling_range >= 2500 ~ "1",
          TRUE ~ NA_character_
        )
      ) %>% 
      drop_na() %>% 
      ggplot() +
      geom_sf(
        data = ne_countries(returnclass = "sf"),
        size = 0.02,
        alpha = .3
      ) +
      coord_sf(expand = FALSE) +
      geom_point(
        aes(
          x = lon,
          y = lat,
          size = n_sp,
          color = transect_length,
          fill = transect_length
        ),
        stroke = .3,
        shape = 21
      ) +
      scale_size_continuous(range = c(.5, 7)) +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = alpha(colors, .3)) +
      theme_void() +
      theme(legend.position = "none")
    
    if (save_map) {
      save_map("map_transects.svg", width = 9)
    }
  }
