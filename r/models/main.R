####  Init  ####
  {
    library(drake)
    library(R2jags)
    library(janzenUtilities)
  }

####  Compile model  ####
  {
    # · Load model ----
      {
        comp <- get_mdl("mdl1")
      }
    
    # · Data ----
      {
        if (comp$name == "mdl1") {
          mdl_data <- make_mdl_data(
            .data = janzen,
            min_gradient_size = 2500,
            singleton_thr = 25,
            excluding_zone = 200,
            std_gradients = TRUE,
            average = TRUE,
            cols = c("location", "elev_range", "land_type", "dtr", "ts", "past_delta_mat", "mat", "map"),
            matrix_formula = ~ dtr
          )
        } else {
          mdl_data <- make_mdl_data(
            .data = janzen,
            min_gradient_size = 2500,
            singleton_thr = 25,
            excluding_zone = 200,
            cols = c("location", "elev_range", "dtr", "ts")
          )
        }
      }
  }

####  Run model  ####
  {
    plan_rs_fit <- drake_plan(
      rs_fit = jags(
        data = mdl_data,
        inits = NULL,
        parameters.to.save = mdl_param,
        model.file = mdl,
        n.iter = 40000,
        n.thin = 5,
        n.chains = 3,
        n.burnin = 20000
      )
    )

    make(plan_rs_fit, lock_cache = FALSE)
  }

####  Model selection  ####
  {
    mdl_selection <- readd(rs_fit) %>% eval_mdl(method = "both")
    mdl_selection
  }