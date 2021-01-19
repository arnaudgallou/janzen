####  Init  ####
  {
    # · Libraries ----
      {
        library(tidybayes)
      }
    
    # · Settings ----
      {
        sampling_range_thr <- 3000
        singleton_thr <- 30
        buffer <- 500
      }
  }

####  Data  ####
  {
    # · Standardised sampling ranges ----
      {
        mdl_data_std <- janzen %>% 
          filter(sampling_range >= sampling_range_thr, singleton < singleton_thr) %>%
          mutate(
            sampling_min = sampling_max - sampling_range_thr,
            sampling_range = sampling_range_thr,
            sampling_mid = (sampling_min + sampling_max) / 2
          ) %>% 
          filter(elev_max > sampling_min) %>% 
          mutate(
            elev_min = if_else(elev_min < sampling_min, sampling_min, elev_min),
            elev_mean = (elev_max + elev_min) / 2,
            elev_range = elev_max - elev_min
          ) %>% 
          drop_na(starts_with("bio"))
      }
    
    # · Sampled species ----
      {
        mdl_data_buffered <- mdl_data_std %>% 
          mutate(elev_range = if_else(elev_range == 0, 10, elev_range)) %>% # to add 10 m to singletons for log transformation
          group_by(location) %>% 
          mutate(
            max_buffer = sampling_mid + buffer,
            min_buffer = sampling_mid - buffer
          ) %>% 
          rowwise() %>% 
          filter(any(between(c(elev_min, elev_max), min_buffer, max_buffer)) | elev_min < min_buffer & elev_max > max_buffer) %>% 
          ungroup()
      }
    
    # · Model data ----
      {
        mdl_data <- mdl_data_buffered %>% 
          group_by(location) %>% 
          mutate(
            elev_range = log(elev_range),
            across(matches("^(?:elev_range|bio)"), mean)
          ) %>% 
          ungroup() %>% 
          distinct(location, .keep_all = TRUE) %>% 
          rename(
            mat = bio1,
            dtr = bio2,
            ts = bio4,
            mtwq = bio10,
            mtcq = bio11,
            map = bio12
          ) %>%
          # left_join(diff_past_mat, by = "location") %>%
          select(location, elev_range, ts, dtr, mat, map) %>% 
          compose_data() %>% 
          c(list(
            n_obs = nrow(mdl_data_buffered),
            location_obs = as.factor(mdl_data_buffered$location),
            elev_range_obs = log(mdl_data_buffered$elev_range)
          ))
      }
  }
