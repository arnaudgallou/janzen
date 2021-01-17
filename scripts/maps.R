####  Init  ####
  {
    # · Libraries ----
      {
        library(tmap)
        library(sf)
      }
    
    # · Settings ----
      {
        save_map <- FALSE
        sampling_range_thr <- 3000
        singleton_thr <- 30
        col_bubble <- "#0099ff"
        proj <- 4326
        tmap_mode("plot")
        data(World)
      }
  }

####  Map  ####
  {
    janzen_map <- janzen %>% 
      filter(
        sampling_range >= sampling_range_thr,
        singleton < singleton_thr
      ) %>%
      distinct(id_ref, .keep_all = TRUE) %>%
      summarise(
        id_ref,
        location,
        lon,
        lat,
        n_sp = as.numeric(n_sp)
      ) %>% 
      drop_na() %>% 
      st_as_sf(
        coords = c("lon", "lat"),
        crs = proj
      ) %>% {
        tm_shape(World, projection = proj) +
          tm_format("World", inner.margins = 0) +
          tm_fill(alpha = .3) +
          tm_borders(
            lwd = .3,
            alpha = .3
          ) +
          tm_shape(.) +
          tm_bubbles(
            size = "n_sp",
            size.lim = c(0, 18000),
            scale = 2,
            col = col_bubble,
            alpha = .3,
            border.col = col_bubble
          ) +
          tm_legend(show = FALSE)
      }
    
    if (save_map) {
      tmap_save(janzen_map, "figures/janzen_map_2000m_30perc_singleton.svg", outer.margins = 0, asp = 0)
    }
  }
