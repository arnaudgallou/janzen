####  Init  ####
  {
    # · Libraries ----
      {
        library(tidybayes)
        library(bayesplot)
        library(broom.mixed)
        library(loo)
        # library(drake)
      }
    
    # · Fit parameters ----
      {
        fit_sims <- rs_fit$BUGSoutput$sims.matrix %>% 
          as_tibble() %>% 
          select(matches("^(?:alpha|beta|tau|dev)")) %>% 
          rename_with(~ str_remove_all(.x, "[\\[\\]]"))
      }
    
    # · Summary ----
      {
        fit_summary <- rs_fit$BUGSoutput$summary %>% 
          as_tibble(rownames = "parameter") %>% 
          filter(parameter %>% str_detect("^(?:alpha|beta|tau)"))
        
        fit_summary
      }
  }

####  Diagnostics  ####
  {
    # · Chains convergence ----
      {
        rs_fit %>% 
          as.mcmc() %>% 
          mcmc_trace(
            regex_pars = "^(?:alpha|beta)",
            facet_args = lst(ncol = 1, strip.position = "left")
          )
      }
    
    # · Rhat ----
      {
        fit_summary %>%
          filter(str_detect(parameter, "^(?:beta|alpha|tau)")) %>%
          pull(Rhat) %>%
          mcmc_rhat()
      }
    
    # · Autocorrelation ----
      {
        fit_sims %>% mcmc_acf_bar(
          regex_pars = "^(?:beta|alpha|tau)",
          lags = 20
        )
      }
    
    # · Posterior distribution ----
      {
        fit_sims %>% 
          mcmc_areas(
            regex_pars = "^beta_rs1",
            prob = .9,
            prob_outer = 1,
            point_est = "median"
          ) +
          xlab(expression(β[1])) +
          coord_cartesian(expand = FALSE) +
          theme(
            axis.line.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
          )
      }
    
    # · Posterior predictive check ----
      {
        y <- mdl_data$elev_range
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
        
        mdl_data %>% 
          as_tibble() %>% 
          ggplot(aes(
            x = dtr,
            y = elev_range - mean(fitted_draws$beta_rs2) * sampling_range
          )) +
          geom_jitter(
            size = .1,
            width = .05,
            height = .1,
            color = "#cdcee3",
            alpha = .2
          ) +
          geom_abline(
            slope = fitted_draws$beta_rs,
            intercept = fitted_draws$alpha_rs,
            color = "#81b3ec",
            alpha = .1
          ) +
          geom_abline(
            slope = mean(fitted_draws$beta_rs),
            intercept = mean(fitted_draws$alpha_rs),
            color = "#1e22a9",
            size = 1
          ) +
          theme_bw() +
          theme(
            axis.ticks = element_line(size = .25),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          ) +
          labs(
            x = "Temperature seasonality (°C)",
            y = expression(italic("log(")*range~size*italic(")")~"["*residuals*"]")
          )
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
          chain_id = rep(1:1, each = 1500)
        )
        rs_loo <- loo(loglik, r_eff = rs_eff)
      }
  }
