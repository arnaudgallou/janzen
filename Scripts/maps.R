####  Init  ####
  {
    # · Parameters ----
      {
        save_map <- FALSE
      }
    
    # · Libraries ----
      {
        library(tmap)
        library(sf)
      }
  }

####  Sites  ####
  {
    tmap_mode("plot")
    col_bubble <- "#0099ff"
    proj <- 4326
    data(World)
    
    janzen %>%
      distinct(id_ref, .keep_all = TRUE) %>% 
      summarise(
        lon,
        lat,
        sp_per_site,
        across("sp_per_site", as.numeric)
      )
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
            size = "sp_per_site",
            size.lim = c(0, 18000),
            scale = 2,
            col = col_bubble,
            alpha = .3,
            border.col = col_bubble
          ) +
          tm_legend(show = FALSE)
      }
    
    if (save_map) {
      tmap_save(mapOverview, "Figures/map_sites.svg", outer.margins = 0, asp = 0)
    }
  }