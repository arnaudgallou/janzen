####  Init  ####
  {
    # · Libraries ----
      {
        library(tidybayes)
      }
    
    # · Settings ----
      {
        settings <- c(
          sampling_range = 2000,
          singleton = 30
        )
      }
  }

####  Data  ####
  {
    # · Model data ----
      {
        mdl_data <- janzen %>% 
          filter(sampling_range >= settings["sampling_range"], singleton < settings["singleton"]) %>%
          drop_na(starts_with("bio")) %>% 
          mutate(
            elev_range = if_else(elev_range == 0, 10, elev_range) %>% log()
            # across(starts_with("bio"), standardize)
          ) %>% 
          rename(
            mat = bio1,
            dtr = bio2,
            ts = bio4,
            mtwq = bio10,
            mtcq = bio11,
            map = bio12
          ) %>% 
          select(location, sampling_range, elev_range, dtr, ts) %>% 
          compose_data()
      }
  }
