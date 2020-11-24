####  Init  ####
  {
    # · Librairies ----
      {
        library(shiny)
        library(tidyverse)
        library(ggrepel)
      }
    
    # · Data ----
      {
        shiny_bioclim <- read_csv("shiny/data/shiny_bioclim.csv")
        shiny_janzen <- read_csv("shiny/data/shiny_janzen.csv")
      }
    
    # · Function ----
      {
        floor_nearest <- function(x, nearest = 1000) {
          (x %/% nearest) * nearest
        }
      }
  }

####  App  ####
  {
    source("scripts/ui.R")
    source("scripts/server.R")
    
    shinyApp(ui = ui, server = server)
  }