####  Init  ####
  {
    # · Libraries ----
      {
        library(R2jags)
        # library(drake)
      }
    
    # · Data ----
      {
        source("scripts/models/data/composed_std_data.R")
      }
  }

####  Model  ####
  {
    mdl <- function() {
      # priors
      alpha_rs ~ dnorm(0, 1E-6)
      beta_rs ~ dnorm(0, 1E-6)
      tau_rs ~ dgamma(.001, .001)
      tau_site ~ dgamma(.001, .001)

      # to simulate a probabilistic distribution of the mean
      for (i in 1:n_obs) {
        elev_range_obs[i] ~ dnorm(mu_site[i], tau_site)
        mu_site[i] <- alpha_site[location_obs[i]]

        # simulated data for posterior predictive check
        elev_range_rep[i] ~ dnorm(mu_site[i], tau_site)
      }

      # to simulate regressions
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

####  Jags  ####
  {
    rs_fit <- jags(
      data = mdl_data,
      inits = NULL,
      parameters.to.save = c(
        "alpha_rs",
        "beta_rs",
        "tau_rs",
        "alpha_site",
        "alpha_site_rep",
        "elev_range_rep",
        "loglik"
      ),
      model.file = mdl,
      n.iter = 10000,
      n.thin = 5,
      n.chains = 3,
      n.burnin = 5000
    )
  }
