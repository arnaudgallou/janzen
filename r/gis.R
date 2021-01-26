####  Init  ####
  {
    # · Notes ----
      {
        # - DEM
        # SRTM 1 Arc-Second Global
        # https://earthexplorer.usgs.gov
        # 
        # - Clim data
        # Layer order:
        # 1. bio_1  -> annual mean temp. (°C/10)
        # 2. bio_2  -> mean diurnal range [air temp.] (°C/10)
        # 3. bio_4  -> temp. seasonality (°C/1000)
        # 4. bio_10 -> mean temp. of warmest quarter (°C/10)
        # 5. bio_11 -> mean temp. of coldest quarter (°C/10)
        # 6. bio_12 -> annual prec. (kg.m-2)
        # 
        # More info at https://chelsa-climate.org
        # 
        # Data repository:
        # https://envicloud.wsl.ch/#/?prefix=chelsa%2Fchelsa_V1
      }
    
    # · Settings ----
      {
        extract_clim <- FALSE
        reclass_biome <- FALSE
      }
    
    # · Functions ----
      {
        source("scripts/gis_funs.R")
      }
  }

####  Bioclim extraction  ####
  {
    if (extract_clim) {
      dem_folders <- list.files("GIS/elev/dem", full.names = TRUE)
      bioclim <- "GIS/bioclim/bioclim-1979_2013.tif" %>% 
        rs_read_stk(c("bio1", "bio2", "bio4", "bio10", "bio11", "bio12"))
      dem_folders <- dem_folders[53]
      for (i in seq_along(dem_folders)) {
        dem <- dem_folders[i]
        loc <- dem %>% 
          basename() %>% 
          to_snake_case()
        
        dem %>% 
          list.files(pattern = "\\.tif$", full.names = TRUE) %>% 
          set_names(str_extract(dirname(.), "[^/]+$")) %>% 
          map(
            ~ .x %>% 
              rs_read() %>% 
              rs_set_range() %>% 
              rs_filter(. > 0) %>% 
              rs_reclass_dem(by = 100)
          ) %>% 
          map_df(
            ~ bioclim %>% 
              rs_crop(.x, snap = "out") %>% 
              rs_reproject(to = .x) %>% 
              rs_zonal(.x, fun = "mean") %>% 
              as_tibble(),
            .id = "location"
          ) %>% 
          rename(elev_band = zone) %>% 
          group_by(elev_band) %>%
          mutate(across(
            starts_with("bio"),
            mean
          )) %>% 
          ungroup() %>% 
          distinct(elev_band, .keep_all = TRUE) %>% 
          write_csv(str_c("GIS/Clim/Extracted/", loc, "-bioclim.csv"))
        
        if (i %% 2 == 0) {
          gc() # to free up RAM
        }
      }
    }
  }