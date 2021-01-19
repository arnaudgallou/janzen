####  Init  ####
  {
    # · Note ----
      {
        # model examining the relationship between the elevational range size of species and temperature variability with the effect of sampling range and location as a random effect
      }
    
    # · Parameters ----
      {
        mdl_param <- c(
          "alpha_rs",
          "beta_rs1",
          "beta_rs2",
          "tau_rs",
          "tau_site",
          "elev_range_rep",
          "site",
          "loglik"
        )
      }
  }

####  Model  ####
  {
    mdl <- function() {
      # priors
      alpha_rs ~ dnorm(0, 1E-6)
      beta_rs1 ~ dnorm(0, 1E-6)
      beta_rs2 ~ dnorm(0, 1E-6)
      tau_rs ~ dgamma(.001, .001)
      tau_site ~ dgamma(.001, .001)
      
      # group effect
      for (i in 1:n_location) {
        site[i] ~ dnorm(0, tau_site)
      }
      
      # likelihood
      for (i in 1:n) {
        # hierarchical model
        elev_range[i] ~ dnorm(mu_rs[i], tau_rs)
        mu_rs[i] <- alpha_rs + beta_rs1 * dtr[i] + beta_rs2 * sampling_range[i] + site[location[i]]
        
        # simulated data for posterior predictive check
        elev_range_rep[i] ~ dnorm(mu_rs[i], tau_rs)
        
        # log-likelihood for model selection
        loglik[i] <- logdensity.norm(elev_range[i], mu_rs[i], tau_rs)
      }
    }
  }
