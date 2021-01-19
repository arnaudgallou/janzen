####  Init  ####
  {
    # · Libraries ----
      {
        library(R2jags)
        library(drake)
      }
    
    # · Compile model ----
      {
        comp <- compile_mdl(
          "scripts/models/mdl3/mdl3.R",
          "scripts/models/data/composed_std_data.R"
        )
      }
  }

####  Run model  ####
  {
    plan_rs_fit <- drake_plan(
      rs_fit = jags(
        data = comp$mdl_data,
        inits = NULL,
        parameters.to.save = comp$mdl_param,
        model.file = comp$mdl,
        n.iter = 10000,
        n.thin = 5,
        n.chains = 3,
        n.burnin = 5000
      )
    )
    
    make(plan_rs_fit, lock_cache = FALSE)
  }

