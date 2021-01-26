####  Init  ####
  {
    library(tidybayes)
    library(bayesplot)
    library(ggridges)
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
        # voir avec fonction rhat() https://cran.r-project.org/web/packages/bayesplot/vignettes/visual-mcmc-diagnostics.html
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
          pivot_longer(
            cols = matches("^(?:alpha|beta)"),
            names_to = "parameter",
            values_to = "posterior"
          ) %>% 
          filter(str_detect(parameter, "^beta")) %>% 
          ggplot(aes(
            x = posterior,
            y = parameter
          )) +
          geom_vline(
            xintercept = 0,
            color = "#1a4d94",
            alpha = .7,
            linetype = "dashed"
          ) +
          geom_density_ridges(
            size = .2,
            fill = "#d0e5fd",
            color = "#1a4d94"
          ) +
          theme_classic() +
          theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
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
          xlab(label = expression(italic("log(")*range~size*italic(")")))
      }
    
    # · Regressions ----
      {
        
        for (i in 1:38) {
          draws[[i]] <- slice_draws(fit_sims, str_c("beta_rs", i))
        }
        
        fitted_draw <- fit_sims %>% 
          summarise(across(
            matches("^(?:alpha|beta)"),
            mean
          ))
        
        tibble(
          fit_sims
        )
        
        coef <- fit_sims %>% 
          summarise(across(.fns = mean)) %>% 
          select(matches("^(?:alpha|beta)")) %>% 
          pivot_longer(
            cols = everything(),
            names_to = "variables",
            values_to = "values"
          ) %>% 
          mutate(
            location = str_remove(variables, "\\D+") %>% as.numeric(),
            variables = str_remove(variables, "_.+$")
          ) %>% 
          pivot_wider(
            names_from = variables,
            values_from = values
          )
        
        dat <- mdl_data %>% 
          as_tibble() %>% 
          left_join(coef, by = "location") %>% 
          left_join(
            janzen %>% 
              filter(
                sampling_range >= settings["sampling_range"],
                singleton < settings["singleton"]
              ) %>% 
              arrange(location) %>% 
              distinct(location) %>% 
              rename(location_name = location) %>% 
              mutate(id = row_number()),
            by = c("location" = "id")
          ) %>% 
          mutate(location = location_name) %>% 
          select(-location_name)
          
        dat %>% 
          ggplot(aes(
            x = ts,
            y = elev_range
          )) +
          geom_point(
            size = .1,
            color = "#cdcee3",
            alpha = .5
          ) +
          geom_abline(aes(intercept = alpha, slope = beta)) +
          facet_wrap(~ location, scales = "free_x") +
          theme_bw() +
          labs(
            x = "Temperature seasonality (°C)",
            y = expression(italic("log(")*range~size*italic(")"))
          )
        
          geom_jitter(
            size = .1,
            width = .05,
            height = .1,
            color = "#DBEAF6",
            alpha = .2
          ) +
          # geom_line(aes(x = x, y = y, colour = factor(model)), show.legend = FALSE)
          geom_line(aes(x = x, y = y), show.legend = FALSE)
          labs(
            x = "TS (°C)",
            y = "log(range size)"
          ) +
          theme_classic() +
          theme(legend.position = "none")
        
        
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
          lab(
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
