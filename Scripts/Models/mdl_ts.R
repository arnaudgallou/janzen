####  Init  ####
  {
    # · Notes ----
      {
        # Model examining the relationship between
        # temperature seasonility and elevational range size
        # within each location
      }
    
    # · Librairies ----
      {
        library(R2jags)
        library(drake)
        library(tidybayes)
      }
  }

####  Jags  ####
  {
    # · Data ----
      {
        mdl_data <- jags_data %>% 
          select(location, elev_range, ts) %>% 
          compose_data()
      }
    
    # · Model ----
      {
        mdl_ts <- function() {
          # priors
          for (i in 1:n_location) {
            alpha_rs[i]~ dnorm(0, 1E-6) # intercepts for each location
            beta_rs[i]~ dnorm(0, 1E-6)  # slopes for each location
          }
          tau_rs ~ dgamma(.001, .001)
          
          # likelihood
          for (i in 1:n) {
            elev_range[i] ~ dnorm(mu_rs[i], tau_rs)
            mu_rs[i] <- alpha_rs[location[i]] + beta_rs[location[i]] * ts[i]
            
            # simulated data for posterior predictive check
            elev_range_rep[i] ~ dnorm(mu_rs[i], tau_rs)
            
            # log-likelihood for model selection
            loglik[i] <- logdensity.norm(elev_range[i], mu_rs[i], tau_rs)
          }
        }
      }
  }

####  Plan  ####
  {
    plan_rs_jags <- drake_plan(
      rs_jags_data = mdl_data,
      rs_fit_ts = jags(
        data = mdl_data,
        inits = NULL,
        parameters.to.save = c(
          "alpha_rs",
          "beta_rs",
          "tau_rs",
          "elev_range_rep",
          "loglik"
        ),
        model.file = mdl_ts,
        n.iter = 5000,
        n.thin = 5,
        n.chains = 3,
        n.burnin = 2500
      )
    )
    
    make(plan_rs_jags, lock_cache = FALSE)
  }