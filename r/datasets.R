####  Init  ####
  {
    # · Libraries ----
      {
        library(gsheet)
        library(magrittr)
        library(tidyverse)
        library(toolkit)
        library(lubridate)
        # library(tidylog)
      }
    
    # · Settings ----
      {
        dropbox_save <- FALSE
        # options(readr.num_columns = 0)
      }
  }

####  Data  ####
  {
    # · Source ref ----
      {
        data_source <- "docs.google.com/spreadsheets/d/1sF2quSOTyqljF9Hle64J9UPwOdbFg8Qj0FJJaS-s3Uk" %>%
          gsheet2tbl() %>%
          mutate(received_date = mdy(received_date))
      }
    
    # · Normalized taxa ----
      {
        normalized <- "data/normalized" %>% 
          list.files(
            pattern = "normalized(?:_\\d)?\\.csv$",
            full.names = TRUE
          ) %>% 
          set_names(basename(.) %>% str_extract("[^-]+")) %>% 
          map_df(read_auto, .id = "dataset") %>% 
          rename(
            id_sp = occurrenceId,
            normalized_name = scientificName,
            accepted_name = species,
            name_status = status
          ) %>% 
          mutate(name_status = str_to_lower(name_status))
      }
    
    # · Datasets ----
      {
        # unsuitable data (too large or unknown locations, no min/max elevations, doubtful data, etc.)
        omit_ref <- c(10027, 10060, 10111, 10250, 10256, 20002, 20007, 20015, 20017, 20013, 20095, 30001, 30002, 30059, 30063, 20013, 20028, 20039, 20057, 20059, 20066, 20095, 20079)
        
        dfs <- "data/datasets" %>% 
          list.files(pattern = "\\.csv$", full.names = TRUE) %>% 
          set_names(basename(.) %>% str_extract("[^.]+")) %>% 
          map_df(~ {
            cols_re <- "^(?:id_(?!sp)|ref|(?:tax[aon]+|genus|genera|species|nomenclator|.*(?:min|max|low|high)(?:.?(?:alt|elev).*)?)$|auth|subsp|var|infra\\s*sp)"
            .x %>% 
              read_auto(guess_max = 2000) %>% 
              rename_with(str_to_lower) %>% 
              rename_with(
                str_replace_all,
                pattern = c(
                  "^ge.+" = "genus",
                  "^su.+" = "subspecies",
                  "^va.+" = "variety",
                  "^in.+" = "infraspecies",
                  "^(?:au|no).+" = "authority",
                  "^(?:sp|ta).+" = "original_name",
                  "^(?:id|re).+" = "id_ref",
                  "^(?:mi|lo).+|.+in$" = "min",
                  "^(?:ma|hi).+|.+ax$" = "max"
                ),
                str_which(colnames(.), cols_re)
              ) %>% 
              mutate(
                id_sp = row_number(),
                across(starts_with("id_"), as.character)
              )
            },
            .id = "dataset"
          ) %>% 
          mutate(
            id_ref = if_else(
              str_detect(dataset, "\\d"),
              str_extract(dataset, "\\d+"),
              id_ref
            ),
            across(starts_with("id_"), as.numeric)
          ) %>% 
          filter(!(id_ref %in% omit_ref)) %>% 
          group_by(id_ref) %>% 
          mutate(
            subspecies = if_else(
              !is.na(subspecies) & mean(original_name %>% str_detect(coll(subspecies))) == 1,
              NA_character_,
              subspecies
            ) %>% 
              str_remove("^(?:ssp|subsp)\\.\\s*"),
            variety = str_remove(variety, "^var\\.\\s*"),
            infraspecies = case_when(
              !is.na(infraspecies) & str_detect(infraspecies, coll(original_name)) ~ NA_character_,
              !is.na(subspecies) ~ str_c("subsp.", subspecies, sep = " "),
              !is.na(variety) ~ str_c("var.", variety, sep = " "),
              TRUE ~ infraspecies
            ) %>% 
              gsub2("^\\w+\\.(?!\\s)\\K", " "),
            original_name = str_c(
              genus %>% if_else(!is.na(.) & mean(. != first_word(original_name)) > .5, ., ""),
              original_name,
              infraspecies %>% if_else(is.na(.), "", .),
              authority %>% if_else(is.na(.), "", .),
              sep = " "
            ) %>% 
              clean_str() %>% 
              uc_first()
          ) %>% 
          ungroup() %>% 
          select(dataset, id_ref, id_sp, original_name, min, max) %>% 
          left_join(data_source, by = "id_ref") %>% 
          select(dataset, id_ref, location, region, continent, land_type, authority_code, id_sp:max, lat:lon, data_reliability, method)
      }
    
    # · Normalized dataset ----
      {
        norm_df <- dfs %>% 
          left_join(normalized, by = c("dataset", "id_sp")) %>% 
          filter(!is.na(accepted_name) & kingdom == "Plantae") %>% 
          rename(gbif_sp_key = key) %>% 
          select(id_ref:id_sp, gbif_sp_key, kingdom, class, family, original_name, normalized_name, name_status, accepted_name, min:data_reliability, method)
      }
      
    # · Main dataset ----
      {
        regions <- c("Hawaii", "Cape Verde", "Canary", "Socotra", "Azores", "Reunion", "Taiwan", "Nepal")
        
        mdf <- norm_df %>% 
          # filter(!(id_ref %in% c(30063, 20066) & min < 1000)) %>% 
          filter(min <= max & max <= 6500 & min > -50) %>% 
          drop_na(matches("^(?:min|max)")) %>% 
          mutate(location = case_when(
            region %in% regions ~ region,
            # id_ref %in% c(20095, 20013) ~ "Utah",
            id_ref %in% c(20062, 20082) ~ "South-Eastern Pyrenees",
            id_ref %in% c(20001, 20051, 30046) ~ "Kenya", # 30059 aberdare
            TRUE ~ location
          )) %>% 
          arrange(desc(id_ref)) %>% 
          group_by(location) %>% 
          mutate(
            id_ref = first(id_ref),
            lat = mean(lat),
            lon = mean(lon)
          ) %>% 
          group_by(accepted_name, .add = TRUE) %>% 
          mutate(
            min = min(min),
            max = max(max)
          ) %>% 
          distinct(accepted_name, .keep_all = TRUE) %>% 
          ungroup() %>% 
          mutate(
            min = if_else(location == "Canary", 0, floor_nearest(min)),
            max = floor_nearest(max),
            elev_mean = (min + max) / 2,
            elev_range = max - min,
            elev_band = floor_nearest(elev_mean, 100),
            land_type = if_else(is.na(land_type), "continent", land_type),
            zone = if_else(between(lat, -23.3, 23.3), "tropical", "temperate")
          ) %>% 
          group_by(id_ref) %>% 
          mutate(
            sampling_min = min(min),
            sampling_max = max(max),
            sampling_range = sampling_max - sampling_min,
            n_sp = n(),
            singleton = proportion(elev_range == 0)
          ) %>% 
          ungroup() %>% 
          rename(elev_min = min, elev_max = max) %>% 
          arrange(location) %>% 
          select(id_ref:region, land_type, zone, lat:lon, gbif_sp_key:elev_max, elev_mean:elev_band, sampling_min:n_sp, singleton, data_reliability, authority_code, method)
        
        if (dropbox_save) {
          write_delim(mdf, "~/Dropbox/janzen/dataset_janzen.csv", delim = ";")
        }
      }
    
    # · Bioclim ----
      {
        bioclim <- "gis/clim/extracted/present" %>% 
          list.files(pattern = "\\.csv$", full.names = TRUE) %>% 
          map_df(read_auto) %>% 
          mutate(
            across(matches("bio\\d[01]?$"), ~ .x / 10),
            bio4 = bio4 / 100
          )
      }
    
    # · Paleoclim ----
      {
        paleoclim <- read_auto("gis/clim/extracted/palaeo/palaeo_clim_variability.csv")
      }
    
    # · Janzen ----
      {
        janzen <- mdf %>% 
          left_join(bioclim, by = c("location", "elev_band")) %>% 
          left_join(paleoclim, by = "location")
      }
  }
