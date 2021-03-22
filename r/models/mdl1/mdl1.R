####  Note  ####
  {
    # model to examine the relationship between the mean elevational range size of species and temperature variability across sites
  }

####  Model  ####
  {
    mdl <- function() {
      # priors ----------------
      for (j in 1:n_param) {
        beta[j] ~ dnorm(0, 1E-6)
      }
      tau_rs ~ dgamma(.001, .001)
      tau_site ~ dgamma(.001, .001)
      # tau_method ~ dgamma(.001, .001)
      
      # random effect ---------
      # group effect of sampling methods
      # for (j in 1:n_method) {
      #   M[j] ~ dnorm(0, tau_method)
      # }
      
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
        mu_rs[i] <- inprod(beta, model_matrix[i, ]) #+ M[method[i]]

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
      "beta",
      "tau_rs",
      "alpha_site",
      "alpha_site_rep",
      "elev_range_rep",
      "loglik"
    )
  }