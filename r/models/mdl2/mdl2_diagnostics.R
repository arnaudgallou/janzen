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
            regex_pars = "^(?:alpha\\[[1-9]\\])",
            facet_args = lst(ncol = 1, strip.position = "left")
          )
      }
    
    # · Rhat ----
      {
        fit_summary %>%
          filter(str_detect(parameter, "^(?:alpha|beta|tau)")) %>%
          pull(Rhat) %>%
          mcmc_rhat()
      }
    
    # · Autocorrelation ----
      {
        fit_sims %>% mcmc_acf_bar(
          regex_pars = "^(?:alpha|beta|tau)",
          lags = 20
        )
      }
    
    # · Posterior distribution ----
      {
        fit_sims %>% 
          select(starts_with("beta")) %>% 
          pivot_longer(
            cols = starts_with("beta"),
            names_to = "parameter",
            values_to = "posterior"
          ) %>% 
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
        
        ppc(y, yrep, adjust = 2) +
          theme(axis.line.y = element_blank())
      }
    
    # · Histogram of slopes ----
      {
        summary_slopes <- fit_summary %>% 
          filter(str_detect(parameter, "beta")) %>% 
          mutate(
            location = str_remove_all(parameter, "\\D+"),
            parameter = str_remove_all(parameter, "[^a-z]")
          ) %>% 
          mutate(
            beta_std = mean / sd, # to standardise the means of the slope to the variances
            `95_ci` = if_else(`2.5%` > 0 & mean > 0 | `97.5%` < 0 & mean < 0, 0, 1) %>% 
              as.factor()
          )
        
        summary_slopes %>% 
          ggplot(aes(x = beta_std, fill = `95_ci`)) +
          geom_histogram(binwidth = 3, alpha = .8) +
          geom_vline(
            xintercept = 0,
            linetype = "dashed",
            alpha = .3
          ) +
          theme_classic() +
          scale_fill_manual(
            name = "95% CI without 0",
            labels = c("yes", "no"),
            values = c("#005b96", "#d1e1ec")
          ) +
          coord_cartesian(expand = FALSE) +
          theme(legend.position = c(.85, .9)) +
          labs(
            x = "Mean slopes / SD slopes",
            y = "Count"
          )
      }
    
    # · Histogram of slopes summary ----
      {
        summary_slopes %>%
          summarise(
            prop_pos = proportion(beta_std > 0),
            prop_neg = proportion(beta_std < 0),
            prop_pos_lc = proportion(beta_std > 0 & `95_ci` == 1),
            prop_pos_hc = proportion(beta_std > 0 & `95_ci` == 0),
            prop_neg_lc = proportion(beta_std < 0 & `95_ci` == 1),
            prop_neg_hc = proportion(beta_std < 0 & `95_ci` == 0)
          )
      }
  }
