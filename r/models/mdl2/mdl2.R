####  Note  ####
  {
    # model to examine the relationship between the elevational range size of species and temperature variability within each site
  }

####  Model  ####
  {
    mdl <- function() {
      # priors ----------------
      for (i in 1:n_location) {
        alpha[i]~ dnorm(0, 1E-6)
        beta[i]~ dnorm(0, 1E-6)
      }
      tau ~ dgamma(.001, .001)
      
      # likelihood ------------
      for (i in 1:n) {
        elev_range[i] ~ dnorm(mu[i], tau)
        mu[i] <- alpha[location[i]] + beta[location[i]] * dtr[i]
        
        # simulated data for posterior predictive check
        elev_range_rep[i] ~ dnorm(mu[i], tau)
        
        # log-likelihood for model selection
        loglik[i] <- logdensity.norm(elev_range[i], mu[i], tau)
      }
    }
  }

####  Parameters  ####
  {
    mdl_param <- c(
      "alpha",
      "beta",
      "tau",
      "elev_range_rep",
      "loglik"
    )
  }
