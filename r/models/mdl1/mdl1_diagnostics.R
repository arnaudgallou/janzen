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
          select(matches("^(?:alpha|beta|tau)"))
      }
    
    # · Summary ----
      {
        fit_summary <- rs_fit$BUGSoutput$summary %>% 
          as_tibble(rownames = "parameter") %>% 
          filter(parameter %>% str_detect("^(?:alpha|beta|tau|L)"))
        
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
        
        mcmc_scatter(as.mcmc(rs_fit), regex_pars = "^beta")
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
        
        
        ppc(y, yrep, adjust = 2) +
        # ppc(y, yrep, type = "overlaid", adjust = 2) +
          theme(axis.line.y = element_blank())
        
        # to check statistics skew
        ppc_stat(y, yrep, stat = "median") +
          theme(axis.line.y = element_blank())
        
        # ppc_stat_grouped(y, yrep, stat = "median", group = mdl_data$location_obs)
      }
    
    # · Posterior distribution ----
      {
        # fit_sims_dtr <- fit_sims
        # fit_sims_ts <- fit_sims
        # fit_sims_paleo <- fit_sims
        
        # fit_sims %>% 
        bind_cols(
          select(fit_sims_dtr, matches("beta\\[[2]\\]")) %>% rename(`β DTR` = `beta[2]`),
          select(fit_sims_ts, matches("beta\\[[2]\\]")) %>% rename(`β TS` = `beta[2]`),
          select(fit_sims_paleo, matches("beta\\[[2]\\]")) %>% rename(`β ∆MAT` = `beta[2]`)
        ) %>% 
          mcmc_areas(
            # regex_pars = "beta\\[[2]\\]",
            prob = .95,
            prob_outer = 1,
            point_est = "median"
          ) 
          xlab(expression(β[dtr])) +
          coord_cartesian(expand = FALSE) +
          theme(
            axis.line.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
          )
          # xlim(-0.1, 0.01)
      }
    
    # · Regressions ----
      {
        fitted_draws <- slice_draws(fit_sims, `beta[2]`, n = 400)
        
        fit_summary %>% 
          filter(str_detect(parameter, "^alpha_site\\[")) %>% 
          bind_cols(tibble(
            location = unique(mdl_data$location_obs),
            land_type = mdl_data$land_type,
            dtr = mdl_data$dtr,
            sr = mdl_data$sampling_range
          )) %>% 
          ggplot(aes(
            x = dtr,
            y = mean,
            shape = land_type
          )) +
          geom_abline(
            slope = fitted_draws$`beta[2]`,
            intercept = fitted_draws$`beta[1]`,
            color = "#b3cddf", # 81b3ec c2d6e6 08519c
            alpha = .4,
            size = .25
          ) +
          geom_abline(
            slope = mean(fitted_draws$`beta[2]`),
            intercept = mean(fitted_draws$`beta[1]`),
            color = "#011f4b", # 1e22a9 001f4b
            size = 1
          ) +
          geom_point(alpha = .5) +
          geom_errorbar(aes(ymin = mean-sd, ymax = mean + sd), alpha = .5) +
          labs(
            x = "Diurnal temperature range (°C)",
            y = expression(italic("log(")*mean~species~range~size*italic(")"))
          ) +
          theme_classic()
        
        # fitted_draws <- slice_draws(fit_sims, `beta[2]`, n = 400)
        # 
        # fit_summary %>%
        #   filter(str_detect(parameter, "^alpha_site\\[")) %>%
        #   bind_cols(tibble(
        #     location = unique(mdl_data$location_obs),
        #     sr = mdl_data$sampling_range,
        #     elev_range = mdl_data$elev_range,
        #     exp_var = mdl_data$dtr
        #   )) %>%
        #   ggplot(aes(
        #     x = exp_var,
        #     y = elev_range #- mean(fitted_draws$`beta[3]`) * sr
        #   )) +
        #   geom_abline(
        #     slope = fitted_draws$`beta[2]`,
        #     intercept = fitted_draws$`beta[1]`,
        #     color = "#08519c", # 3182bd
        #     alpha = 1/20
        #   ) +
        #   geom_abline(
        #     slope = mean(fitted_draws$`beta[2]`),
        #     intercept = mean(fitted_draws$`beta[1]`),
        #     color = "#001f4b",
        #     size = 1
        #   ) +
        #   geom_point() +
        #   labs(
        #     x = "Diurnal temperature range (°C)",
        #     y = expression(italic("log(")*mean~species~range~size*italic(")"))
        #   ) +
        #   theme_classic()
        #   ggrepel::geom_text_repel(aes(label = location))
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
        n_sims <- rs_fit$BUGSoutput$n.sims
        rs_eff <- relative_eff(
          exp(loglik),
          chain_id = rep(1:1, each = n_sims)
        )
        rs_loo <- loo(loglik, r_eff = rs_eff)
        rs_loo
      }
    
    # · Comparison matrix ----
      {
        tribble(
          ~ mdl, ~ waic, ~ waic_se, ~ loo, ~ loo_se,
          "dtr", 7.5, 9.0, 7.8, 9.1,
          "ts", 15.4, 7.3, 15.5, 7.3,
          "dtr:mat", 10.4, 7.9, 10.9, 8.1,
          "dtr:map", 10.5, 8.3, 10.9, 8.4,
          "ts:mat", 15.2, 7.3, 15.5, 7.4,
          "ts:map", 17.8, 7.0, 18.0, 7.0,
          "∆mat paleo", 18.7, 8.2, 18.8, 8.2
        ) %>% 
          arrange(waic, loo) %>% 
          mutate(
            delta_waic = delta(waic),
            delta_loo = delta(loo)
          ) %>% 
          relocate(delta_waic, .after = waic_se)
      }
  }

# p1 <- pdtr +
#   theme(legend.position = "none") +
#   xlab("Diurnal temperature range (°C)")
# p2 <- pts + theme(
#   legend.position = "bottom",
#   axis.title.y = element_blank(),
#   axis.ticks.y = element_blank(),
#   axis.text.y = element_blank()
# ) +
#   xlab("Temperature seasonality (°C)")
# p3 <- pdmat + theme(
#   legend.position = "none",
#   axis.title.y = element_blank(),
#   axis.ticks.y = element_blank(),
#   axis.text.y = element_blank()
# ) +
#   xlab(expression("∆"*MAT[paleo]~"(°"*C*")"))
# p1 + p2 + p3

fitted_draws <- slice_draws(fit_sims, `beta[2]`, n = 400)
fit_summary %>% 
  filter(str_detect(parameter, "^alpha_site\\[")) %>% 
  bind_cols(tibble(
    location = mdl_data$location,
    land_type = mdl_data$land_type,
    dtr = mdl_data$dtr,
    sr = mdl_data$sampling_range
  )) %>% 
  ggplot(aes(
    x = dtr,
    y = mean,
    shape = land_type
  )) +
  geom_abline(
    slope = fitted_draws$`beta[2]`,
    intercept = fitted_draws$`beta[1]`,
    color = "#b3cddf", # 81b3ec c2d6e6 08519c
    alpha = .4,
    size = .25
  ) +
  geom_abline(
    slope = mean(fitted_draws$`beta[2]`),
    intercept = mean(fitted_draws$`beta[1]`),
    color = "#011f4b", # 1e22a9 001f4b
    size = 1
  ) +
  geom_point(alpha = .5) +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean + sd), alpha = .5) +
  labs(
    x = "Diurnal temperature range (°C)",
    # x = expression("∆"*MAT[paleo]~"(°"*C*")"),
    y = expression(italic("log(")*mean~species~range~size*italic(")"))
  ) +
  theme_classic() +
  scale_shape_manual(
    name = "Land type",
    values = c(16, 17),
    labels = c("continent", "island")
  )

  ggrepel::geom_text_repel(aes(label = location))
  theme(legend.title = element_blank())
  ggrepel::geom_text_repel(
    data = tibble(dtr = 4.19, mean = 6.69, type = "continent"),
    label = "La Amistad",
    show.legend = FALSE,
    color = "black",
    box.padding = 1,
    nudge_y = 1,
    nudge_x = 4
  )
  
  
janzen %>% 
  drop_na(starts_with("bio")) %>% 
  ggplot(aes(
    x = bio12,
    y = bio2
  )) +
  geom_point() +
  geom_smooth(method = "lm")