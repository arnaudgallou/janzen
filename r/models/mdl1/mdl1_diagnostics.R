####  Init  ####
  {
    library(tidybayes)
    library(bayesplot)
    library(broom.mixed)
    # library(drake)
  }

####  Diagnostics  ####
  {
    # · Fit parameters ----
      {
        fit_sims <- rs_fit$BUGSoutput$sims.matrix %>% 
          as_tibble() %>% 
          select(matches("^(?:alpha|beta|tau)"))
      }
    
    # · Summary ----
      {
        fit_summary <- rs_fit$BUGSoutput$summary %>% 
          as_tibble(rownames = "parameter") %>% 
          filter(parameter %>% str_detect("^(?:alpha|beta|tau)"))
        
        fit_summary
      }
    
    # · Traceplots ----
      {
        rs_fit %>%
          as.mcmc() %>%
          mcmc_trace(
            regex_pars = "^(?:beta|tau)",
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
        fit_sims %>% 
          mcmc_acf_bar(
            regex_pars = "^(?:beta|tau)",
            lags = 20
          )
      }
    
    # · Posterior predictive check ----
      {
        obs <- FALSE
        if (obs) {
          y <- mdl_data$elev_range_obs
          yrep <- rs_fit$BUGSoutput$sims.list$elev_range_rep
        } else {
          y <- mdl_data$elev_range
          yrep <- rs_fit$BUGSoutput$sims.list$alpha_site_rep
        }
        
        # density overlay
        ppc(y, yrep, adjust = 2) +
          theme(axis.line.y = element_blank())
        
        # statistics skew
        ppc_stat(y, yrep, stat = "median") +
          theme(axis.line.y = element_blank())
      }
    
    # · Posterior distribution ----
      {
        fit_sims %>%
          mcmc_areas(
            regex_pars = "beta\\[2\\]",
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
    
    # · Regressions ----
      {
        fitted_draws <- slice_draws(fit_sims, `beta[2]`, n = 400)
        
        fit_summary %>% 
          filter(str_detect(parameter, "^alpha_site\\[")) %>%
          bind_cols(tibble(
            land_type = mdl_data$land_type,
            dtr = mdl_data$dtr
          )) %>% 
          ggplot(aes(
            x = dtr,
            y = mean,
            shape = land_type
          )) +
          geom_abline(
            slope = fitted_draws$`beta[2]`,
            intercept = fitted_draws$`beta[1]`,
            color = "#b3cddf",
            alpha = .4,
            size = .25
          ) +
          geom_abline(
            slope = mean(fitted_draws$`beta[2]`),
            intercept = mean(fitted_draws$`beta[1]`),
            color = "#011f4b",
            size = 1
          ) +
          geom_point(alpha = .5) +
          geom_errorbar(
            aes(ymin = mean - sd, ymax = mean + sd),
            alpha = .5
          ) +
          labs(
            x = "Diurnal temperature range (°C)",
            y = expression(italic("log(")*mean~species~range~size*italic(")"))
          ) +
          theme_classic() +
          scale_shape_manual(
            name = "Land type",
            values = c(16, 17),
            labels = c("continent", "island")
          )
      }
  }
