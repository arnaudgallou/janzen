####  Init  ####
  {
    # · Parameters ----
      {
        extract_clim <- FALSE
        reclass_biome <- FALSE
      }
    
    # · Libraries ----
      {
        library(tmap)
        library(sf)
      }
  }

####  Spatial points  ####
  {
    geo_pts <- janzen %>% 
      distinct(id_ref, .keep_all = TRUE) %>%
      summarise(
        id_ref,
        location,
        lon,
        lat,
        across("sp_per_site", as.numeric)
      ) %>% 
      drop_na() %>% 
      st_as_sf(
        coords = c("lon", "lat"),
        crs = 4326
      )
  }

####  Bioclim  ####
  {
    # · Notes ----
      {
        # Layer order:
        # 1. bio_1  -> annual mean temp. (°C/10)
        # 2. bio_2  -> mean diurnal range [air temp.] (°C/10)
        # 3. bio_4  -> temp. seasonality (°C/1000)
        # 4. bio_10 -> mean temp. of warmest quarter (°C/10)
        # 5. bio_11 -> mean temp. of coldest quarter (°C/10)
        # 6. bio_12 -> annual prec. (kg.m-2)
        # 
        # More info at http://chelsa-climate.org
        # 
        # Data repository:
        # https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/bioclim/float/
      }
    
    # · Dem mosaic ----
      {
        # res: 1 arc-second
        # dem_ls <- list.files("GIS/Elev/dem", full.names = TRUE)
        # 
        # for (i in dem_ls) {
        #   file_name <- i %>% 
        #     basename() %>% 
        #     str_to_lower() %>% 
        #     str_replace_all(" ", "_")
        #   
        #   i %>% 
        #     list.files(
        #       pattern = "\\.tif$",
        #       full.names = TRUE
        #     ) %>% 
        #     gdal_utils(
        #       util = "buildvrt",
        #       source = .,
        #       destination = str_c("GIS/Elev/vrt/test/", file_name, ".vrt")
        #     )
        # }
      }
    
    # · Extraction ----
      {
        if (extract_clim) {
          dem_folders <- list.files("GIS/Elev/dem", full.names = TRUE)
          bioclim <- "GIS/Clim/BioClim/bioclim-1979_2013.tif" %>% 
            rs_read_stk(c("bio1", "bio2", "bio4", "bio10", "bio11", "bio12"))
          
          for (i in seq_along(dem_folders)) {
            dem <- dem_folders[i]
            loc <- dem %>% 
              basename() %>% 
              str_to_lower() %>% 
              str_replace_all(" ", "_")
            
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
              summarise(across(
                starts_with("bio"),
                mean
              )) %>% 
              write_csv(str_c("GIS/Clim/Extracted/", loc, "-bioclim.csv"))
            
            if (i %% 4 == 0) {
              gc() # to free up RAM
            }
          }
        }
      }
  }