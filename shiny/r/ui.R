####  Settings  ####
  {
    sampling_range_min <- min(shiny_janzen$sampling_range)
    sampling_range_max <- floor_nearest(max(shiny_janzen$sampling_range))
    
    default <- c(
      singleton = 30,
      sampling_range = 3000,
      sampling_min = 500,
      sigma = 2000,
      buffer = 0,
      species_by = "mid_point",
      bioclim = "bio2",
      options = "same_sampling_range",
      land_type = "all"
    )
    
    bioclim_ls <- list(
      "Diurnal temperature range" = "bio2",
      "Temperature seasonality" = "bio4",
      "Temperature variability (21k-0 BP)" = "delta_mat",
      "Mean annual temperature" = "bio1",
      "Mean annual precipitation" = "bio12",
      "Mean temp. of warmest quarter" = "bio10",
      "Mean temp. of coldest quarter" = "bio11"
    )
  }

####  Viewer  ####
  {
    ui <- fluidPage(
      includeCSS("../style_janzen.css"),
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
            inputId = "sampling_range",
            label = "Minimum sampling range:",
            min = sampling_range_min,
            max = sampling_range_max,
            value = default["sampling_range"],
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
            max = sampling_range_max,
            value = default["sigma"],
            step = 500
          ),
          sliderInput(
            inputId = "buffer",
            label = "Buffer:",
            min = 0,
            max = 1500,
            value = default["buffer"],
            step = 50
          ),
          selectInput(
            inputId = "species_by",
            label = "Filtering mode:",
            choices = list(
              "sigma" = "sigma",
              "mid-point" = "mid_point",
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
          
          # land type
          selectInput(
            inputId = "type",
            label = "Land type:",
            choices = list(
              "all" = "all_type",
              "islands" = "islands",
              "continents" = "continents"
            ),
            selected = default["land_type"]
          ),
          
          # options
          checkboxGroupInput(
            inputId = "options",
            label = "Options:",
            choices = list(
              "Standardise sampling range" = "standardise_sampling_range",
              "Display location names" = "display_locations",
              "Display sampling range" = "display_sampling_range",
              "Display number of species" = "display_nb_sp",
              "Log transform data" = "log",
              "Regression line" = "lm"
            ),
            selected = default["options"]
          ),
          helpText(
            "Help:", tags$br(),
            tags$strong("Singleton threshold"), ' - filter locations by percentage of singletons (single observations). A value of 30 (the default) will only select locations that have less than 30 percent of singletons.', tags$br(), tags$br(),
            tags$strong("Maximum lowest elevation"), ' - filter locations by maximum lowest elevations. A value of 500 (the default) will only select locations whose the lowest elevational value is between 0 and 500 m.', tags$br(), tags$br(),
            tags$strong("Sigma"), ' - a point on the elevational gradients used as a reference point for the buffer. "Filtering mode" must be set on "sigma" to enable this parameter.', tags$br(), tags$br(),
            tags$strong("Buffer"), ' - produces a buffer area that filter any species with at least one theoritical observation (any elevation between the mininium and maximum observations of a species) falling in the buffer. The buffer is generated around "Sigma", meaning that a buffer of 500 m produces a total buffer area of 1000 m (500 m above and 500 m below "Sigma"). If "Filtering mode" is set to "mid-point", will produce a buffer around the mid-point of each elevational gradient.', tags$br(), tags$br(),
            tags$strong("Filtering mode"), ' - "sigma": uses the elevation as defined by "Sigma" to select species and produce the buffer.', tags$br(), '"mid-point": uses the mid-point of each elevational gradient to select species and produce the buffer.', tags$br(), '"none": uses all data.'
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
