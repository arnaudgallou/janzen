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
    
    # · Normalized data ----
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
          mutate(
            across(starts_with("id_"), as.character),
            across(name_status, str_to_lower)
          )
      }
    
    # · Datasets ----
      {
        omit_ref <- c(10027, 10060, 10111, 10250, 10256, 20007, 20017, 30001, 30002)
        dfs <- "data/datasets" %>% 
          list.files(
            pattern = "\\.csv$",
            full.names = TRUE
          ) %>% 
          set_names(basename(.) %>% str_extract("[^.]+")) %>% 
          map_df(~ {
            cols_re <- "^(?:id_(?!sp)|ref|(?:tax[aon]+|genus|genera|species|nomenclator|.*(?:min|max|low|high)(?:.?(?:alt|elev).*)?)$|auth|subsp|var|infra\\s*sp)"
            .x %>% 
              read_auto(guess_max = 2000) %>% 
              rename_with(str_to_lower) %>% 
              rename_with(
                ~ .x %>% 
                  str_replace_all(
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
                    )
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
          mutate(id_ref = if_else(
            str_detect(dataset, "\\d"),
            str_extract(dataset, "\\d+"),
            id_ref
          )) %>% 
          filter(!(id_ref %in% omit_ref)) %>% 
          group_by(id_ref) %>% 
          mutate(
            subspecies = subspecies %>% {
              if_else(
                !is.na(.) & mean(original_name %>% str_detect(coll(.))) == 1,
                NA_character_,
                .
              )} %>% 
              str_remove("^(?:ssp|subsp)\\.\\s*"),
            variety = variety %>% str_remove("^var\\.\\s*"),
            infraspecies = infraspecies %>% {
              case_when(
                !is.na(.) & str_detect(., coll(original_name)) ~ NA_character_,
                !is.na(subspecies) ~ str_c("subsp.", subspecies, sep = " "),
                !is.na(variety) ~ str_c("var.", variety, sep = " "),
                TRUE ~ .
              )} %>% 
              gsub2("^\\w+\\.(?!\\s)\\K", " "),
            authority = authority %>% gsub2("(?:[).,](?=\\pL)|\\pL(?=\\())\\K", " "),
            original_name = str_c(
              genus %>% if_else(!is.na(.) & mean(. != first_word(original_name)) > .5, ., ""),
              original_name,
              infraspecies %>% if_else(!is.na(.), ., ""),
              authority %>% if_else(!is.na(.), ., ""),
              sep = " "
            ) %>% 
              clean_str() %>% 
              uc_first()
          ) %>% 
          ungroup() %>% 
          select(dataset, id_ref, id_sp, original_name, min, max) %>% 
          left_join(
            data_source %>% mutate(across(id_ref, as.character)),
            by = "id_ref"
          ) %>% 
          select(dataset, id_ref, location, region, continent, type, authority_code, id_sp:max, lat:lon, data_reliability)
      }
    
    # · Normalized dataset ----
      {
        norm_df <- dfs %>% 
          left_join(
            normalized,
            by = c("dataset", "id_sp")
          ) %>% 
          filter(!is.na(accepted_name) & kingdom == "Plantae") %>% 
          arrange(id_ref) %>% 
          rename(gbif_sp_key = key) %>% 
          select(id_ref:id_sp, gbif_sp_key, kingdom, class, family, original_name, normalized_name, name_status, accepted_name, min:data_reliability)
      }
      
    # · Master dataset ----
      {
        group <- list(
          regions = c("Hawaii", "Cape Verde", "Canary", "Socotra", "Azores", "Reunion", "Taiwan", "Nepal"),
          id = c(10081, 30000, 20095, 20013, 20091, 20001, 30059, 20062, 20082)
        )
        mdf <- norm_df %>% 
          filter(!(location %in% c("Maquipucuna", "Rocky Mountains") & min < 1000)) %>% 
          mutate_subset(
            region %in% group$regions | id_ref %in% group$id,
            fun = function(x) {
              x %>% 
                arrange(desc(id_ref)) %>% 
                mutate(location = case_when(
                  region %in% group$regions ~ region,
                  id_ref %in% c(20095, 20013) ~ "Utah",
                  id_ref %in% c(20062, 20082) ~ "South-Eastern Pyrenees",
                  TRUE ~ location
                )) %>% 
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
                ungroup()
            }
          ) %>% 
          filter(min <= max & max < 6500 & min > -50) %>% 
          group_by(id_ref, accepted_name) %>% 
          mutate(
            min = min(min),
            max = max(max)
          ) %>% 
          distinct(accepted_name, .keep_all = TRUE) %>% 
          ungroup() %>%
          mutate(
            id_sp = row_number(),
            min = floor_nearest(min),
            max = floor_nearest(max),
            elev_mean = (min + max) / 2,
            elev_range = max - min,
            elev_band = floor_nearest(elev_mean, 100),
            type = if_else(!is.na(type), type, "continent"),
            zone = if_else(between(lat, -23.3, 23.3), "tropical", "temperate")
          ) %>% 
          group_by(id_ref) %>% 
          mutate(
            sampling_min = min(min),
            sampling_max = max(max),
            sampling_mid = (sampling_max + sampling_min) / 2,
            sampling_range = sampling_max - sampling_min,
            n_sp = n(),
            singleton = proportion(elev_range == 0)
          ) %>% 
          ungroup() %>% 
          rename(elev_min = min, elev_max = max) %>% 
          select(id_ref:region, type, zone, lat:lon, id_sp:elev_max, elev_mean:elev_band, sampling_min:n_sp, singleton, data_reliability, authority_code)
        
        if (dropbox_save) {
          mdf %>% write_delim("~/Dropbox/janzen/dataset_janzen.csv", delim = ";")
        }
      }
    
    # · Bioclim ----
      {
        bioclim <- "GIS/Clim/Extracted" %>% 
          list.files(pattern = "\\.csv$", full.names = TRUE) %>% 
          map_df(read_auto) %>% 
          mutate(
            across(matches("bio\\d[01]?$"), ~ .x / 10),
            across("bio4", ~ .x / 100)
          )
      }
    
    # · Janzen ----
      {
        janzen <- left_join(mdf, bioclim, by = c("location", "elev_band"))
      }
  }