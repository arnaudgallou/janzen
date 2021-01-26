####  Note  ####
  {
    # model to examine the relationship between the elevational range size of species and temperature variability.
    # fixed effect:
    #   - sampling range
    # random effect groups:
    #   - location
    #   - method
  }

####  Model  ####
  {
    mdl <- function() {
      # likelihood ------------
      for (i in 1:n) {
        elev_range[i] ~ dnorm(mu_rs[i], tau_rs)
        mu_rs[i] <- alpha_rs + beta_rs * dtr[i] + beta_rs2 * sampling_range[i] + L[location[i]] #+ M[method[i]]
        
        # simulated data for posterior predictive check
        # elev_range_rep[i] ~ dnorm(mu_rs[i], tau_rs)
        
        # log-likelihood for model selection
        # loglik[i] <- logdensity.norm(elev_range[i], mu_rs[i], tau_rs)
      }
      
      # random effects ------------
      # loop through each values of the groups to create a vector of random deviations
      # group effect of locations
      for (j in 1:n_location) {
        L[j] ~ dnorm(0, tau_location)
      }
      
      # group effect of sampling methods
      # for (j in 1:n_method) {
      #   M[j] ~ dnorm(0, tau_method)
      # }
      
      # priors ------------
      alpha_rs ~ dnorm(0, 1E-6)
      beta_rs ~ dnorm(0, 1E-6)
      beta_rs2 ~ dnorm(0, 1E-6)
      tau_rs ~ dgamma(.001, .001)
      tau_location ~ dgamma(.001, .001)
    }
  }

####  Parameters  ####
  {
    mdl_param <- c(
      "alpha_rs",
      "beta_rs",
      "beta_rs2",
      "tau_rs",
      "tau_location",
      "elev_range_rep",
      "L",
      "loglik"
    )
  }