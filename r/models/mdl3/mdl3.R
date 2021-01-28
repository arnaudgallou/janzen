####  Note  ####
  {
    # model to examine the relationship between the average elevational range size of species and temperature variability across site
    # mean elevational range size simulated in each location
  }

####  Model  ####
  {
    mdl <- function() {
      # priors ------------
      alpha_rs ~ dnorm(0, 1E-6)
      beta_rs ~ dnorm(0, 1E-6)
      tau_rs ~ dgamma(.001, .001)
      tau_site ~ dgamma(.001, .001)

      # likelihood ------------
      # to estimate the mean range size in each location
      for (i in 1:n_obs) {
        elev_range_obs[i] ~ dnorm(mu_site[i], tau_site)
        mu_site[i] <- alpha_site[location_obs[i]]

        # simulated data for posterior predictive check
        elev_range_rep[i] ~ dnorm(mu_site[i], tau_site)
      }

      for (i in 1:n) {
        alpha_site[i] ~ dnorm(mu_rs[i], tau_rs)
        mu_rs[i] <- alpha_rs + beta_rs * dtr[i]

        # simulated data for posterior predictive check
        alpha_site_rep[i] ~ dnorm(mu_rs[i], tau_rs)
        
        # for model selection
        loglik[i] <- logdensity.norm(alpha_site[i], mu_rs[i], tau_rs)
      }
    }
  }

####  Parameters  ####
  {
    mdl_param <- c(
      "alpha_rs",
      "beta_rs",
      "tau_rs",
      "alpha_site",
      "alpha_site_rep",
      "elev_range_rep",
      "loglik"
    )
  }