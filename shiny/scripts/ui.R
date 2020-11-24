####  Interface  ####
  {
    # · Parameters ----
      {
        sampling_size_min <- min(shiny_janzen$sampling_size)
        sampling_size_max <- floor_nearest(max(shiny_janzen$sampling_size))
        
        default <- c(
          singleton = 30,
          sampling_size = 3000,
          sampling_min = 500,
          sigma = 2000,
          buffer = 0,
          species_by = "mid_point",
          bioclim = "bio2",
          options = "same_sampling_size"
        )
        
        bioclim_ls <- list(
          "Diurnal temperature range" = "bio2",
          "Temperature seasonality" = "bio4",
          "Mean annual temperature" = "bio1",
          "Mean annual precipitation" = "bio12",
          "Mean temp. of warmest quarter" = "bio10",
          "Mean temp. of coldest quarter" = "bio11"
        )
      }
    
    # · Viewer ----
      {
        ui <- fluidPage(
          includeCSS("shiny_janzen.css"),
          sidebarLayout(
            sidebarPanel(
              width = 4,
              
              # filtering locations
              sliderInput(
                inputId = "singleton",
                label = "Singleton threshold:",
                min = 0,
                max = 100,
                value = default["singleton"],
                step = 2
              ),
              sliderInput(
                inputId = "sampling_size",
                label = "Minimum sampling size:",
                min = sampling_size_min,
                max = sampling_size_max,
                value = default["sampling_size"],
                step = 100
              ),
              sliderInput(
                inputId = "sampling_min",
                label = "Maximum lowest elevation:",
                min = 0,
                max = 1000,
                value = default["sampling_min"],
                step = 100
              ),
              
              # filtering species
              sliderInput(
                inputId = "sigma",
                label = "Sigma:",
                min = 0,
                max = sampling_size_max,
                value = default["sigma"],
                step = 500
              ),
              sliderInput(
                inputId = "buffer",
                label = "Buffer:",
                min = 0,
                max = 500,
                value = default["buffer"],
                step = 10
              ),
              selectInput(
                inputId = "species_by",
                label = "Filter species by:",
                choices = list(
                  "sigma" = "sigma",
                  "location mid-point" = "mid_point",
                  "none" = "none"
                ),
                selected = default["species_by"]
              ),
              
              # climate data
              selectInput(
                inputId = "bioclim",
                label = "Bioclim variable:",
                choices = bioclim_ls,
                selected = default["bioclim"]
              ),
              
              # options
              checkboxGroupInput(
                inputId = "options",
                label = "Options:",
                choices = list(
                  "Standardise sampling size" = "standardise_sampling_size",
                  "Location names" = "show_locations",
                  "Sampling size" = "show_sampling_size",
                  "Regression line" = "lm"
                ),
                selected = default["options"]
              )
            ),
            mainPanel(
              plotOutput(
                outputId = "janzen_plot",
                height = "500px"
              )
            )
          )
        )
      }
  }
