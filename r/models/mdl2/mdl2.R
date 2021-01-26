####  Init  ####
  {
    # · Note ----
      {
        # model to examine the relationship between the elevational range size of species and temperature variability in each location
      }
    
    # · Parameters ----
      {
        mdl_param <- c(
          "alpha_rs",
          "beta_rs",
          "tau_rs",
          "elev_range_rep",
          "loglik"
        )
      }
  }

####  Model  ####
  {
    mdl <- function() {
      # priors
      for (i in 1:n_location) {
        alpha_rs[i]~ dnorm(0, 1E-6)
        beta_rs[i]~ dnorm(0, 1E-6)
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
