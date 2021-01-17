####  Init  ####
  {
    library(tidybayes)
    library(bayesplot)
    library(broom.mixed)
    library(loo)
    # library(drake)
  }

####  Diagnostics  ####
  {
    # · Fit parameters ----
      {
        fit_sims <- rs_fit$BUGSoutput$sims.matrix %>% 
          as_tibble() %>% 
          select(matches("^(?:alpha|beta|tau|dev)"))
      }
    
    # · Summary ----
      {
        fit_summary <- rs_fit$BUGSoutput$summary %>% 
          as_tibble(rownames = "parameter") %>% 
          filter(parameter %>% str_detect("^(?:alpha|beta|tau)"))
        
        fit_summary
      }
    
    # · Chains convergence ----
      {
        rs_fit %>%
          as.mcmc() %>%
          mcmc_trace(
            regex_pars = "^(?:beta|alpha|tau)_rs",
            facet_args = lst(ncol = 1, strip.position = "left")
          )
      }
    
    # · Rhat ----
      {
        fit_summary %>%
          filter(str_detect(parameter, "^(?:beta|alpha)")) %>%
          pull(Rhat) %>%
          mcmc_rhat()
      }
    
    # · Autocorrelation ----
      {
        fit_sims %>% mcmc_acf_bar(
          regex_pars = "^(?:beta|alpha|tau)_rs",
          lags = 20
        )
      }
    
    # · Posterior distribution ----
      {
        fit_sims %>% 
          mcmc_areas(
            regex_pars = "^beta_rs",
            prob = .95,
            prob_outer = 1,
            point_est = "median"
          ) +
          xlab(expression(β[dtr])) +
          coord_cartesian(expand = FALSE) +
          theme(
            axis.line.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
          )
      }
    
    # · Posterior predictive check ----
      {
        y <- mdl_data$elev_range_obs
        yrep <- rs_fit$BUGSoutput$sims.list$elev_range_rep
        
        ppc(
          y = y,
          yrep = yrep,
          type = "overlaid",
          n_draws = 50,
          adjust = 3
        ) +
          geom_vline(
            xintercept = mean(y),
            color = "#001f4b",
            linetype = "dashed"
          ) +
          theme(axis.line.y = element_blank()) +
          xlab(label = expression(italic("log(")*species~range~size*italic(")")))
      }
    
    # · Regressions ----
      {
        fitted_draws <- slice_draws(fit_sims, beta_rs, n = 400)
        
        tibble(
          location = unique(mdl_data$location_obs),
          elev_range = mdl_data$elev_range,
          exp_var = mdl_data$dtr
        ) %>% 
          ggplot(aes(
            x = exp_var,
            y = elev_range,
          )) +
          geom_abline(
            slope = fitted_draws$beta_rs,
            intercept = fitted_draws$alpha_rs,
            color = "#08519C", # 3182bd
            alpha = 1/20
          ) +
          geom_abline(
            slope = mean(fitted_draws$beta_rs),
            intercept = mean(fitted_draws$alpha_rs),
            color = "#001f4b",
            size = 1
          ) +
          geom_point() +
          labs(
            x = "Diurnal temperature range (°C)",
            y = expression(italic("log(")*species~range~size*italic(")"))
          ) +
          theme_classic()
      }
  }

####  Model selection  ####
  {
    # · Log-likelihood ----
      {
        loglik <- rs_fit$BUGSoutput$sims.list$loglik
      }
    
    # · WAIC ----
      {
        rs_waic <- waic(loglik)
        rs_waic
      }
    
    # · LOO ----
      {
        rs_eff <- relative_eff(
          exp(loglik),
          chain_id = rep(1:1, each = 3000)
        )
        rs_loo <- loo(loglik, r_eff = rs_eff)
        rs_loo
      }
  }
