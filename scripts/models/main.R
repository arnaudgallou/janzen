####  Init  ####
  {
    # · Libraries ----
      {
        library(R2jags)
        library(drake)
        library(janzenUtilities)
      }
    
    # · Compile model ----
      {
        comp <- get_mdl("mdl1")
        source(comp$file)
      }
    
    # · Data ----
      {
        if (comp$name == "mdl3") {
          mdl_data <- make_mdl_data(
            .data = janzen,
            min_gradient_size = 3000,
            singleton_thr = 30,
            buffer_size = 500,
            std_gradients = TRUE,
            average = TRUE,
            cols = c("location", "elev_range", "singleton", "ts", "dtr")
          )
        } else {
          mdl_data <- make_mdl_data(
            .data = janzen,
            min_gradient_size = 3000,
            singleton_thr = 30,
            cols = c("location", "elev_range", "sampling_range", "ts", "dtr")
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
        n.iter = 10000,
        n.thin = 5,
        n.chains = 3,
        n.burnin = 5000
      )
    )
    
    make(plan_rs_fit, lock_cache = FALSE)
  }

