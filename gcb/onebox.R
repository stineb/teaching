---
title: "Plots for intro lecture to Global Change Biology"
output: html_document
author: "Beni Stocker"
date: "`r Sys.Date()`"
# output:
#   html_document:
#     toc: true
#     toc_float: true
#     toc_depth: 4
#     number_sections: true
output:
  pdf_document:
  toc: true
toc_depth: 3
number_sections: true
---


## //////////////////////////////////////////////////
## 1-BOX MODEL
## --------------------------------------------------
onebox <- function( c_influx, tau, cpool0 ) {
  ## ------------------------------------------------
  ## c_influx:  flux into pool (GtC a-1)
  ## cpool0:    initial pool size (GtC)
  ## tau:       turnover (residence) time (a)
  ## ------------------------------------------------
  tauisvec <- ifelse(length(tau)==length(c_influx), TRUE, FALSE)

  ## determine integration length (number of time steps) from length of 'c_influx'
  len <- length(c_influx)

  ## initialise output variable (time series of pool size)
  out_cpool <- rep( NA, len )

  ## copy initial pool size to first element in output time series
  cpool <- cpool0

  ## integrate over each time step (this is an implementation of the differential equation)
  for (yr in seq(len) ) {

    ## copy current pool size to output time series
    out_cpool[yr] <- cpool

    ## update pool size with input and decay
    cpool <- cpool + c_influx[yr] - 1/ifelse(tauisvec, tau[yr], tau) * cpool

  }

  ## function return value is a vector containing the output time series
  return( out_cpool )

}

library(ggplot2)
library(tidyr)
library(gganimate)
library(gifski)

##//////////////////////////////////////////////////
## Pulse - decay
##--------------------------------------------------
nt       <- 400
cpool0    <- 3000
c_influx  <- rep( 0, nt )
c_influx[50] <- 100
tau       <- 50

df <- tibble(year = seq(nt), c_influx = c_influx) %>%
  dplyr::mutate(c_pool = onebox(c_influx = c_influx, tau = 50, cpool0 = 0))

gganim <- df %>%
  pivot_longer(starts_with("c_"), names_to = "var", values_to = "c_") %>%
  ggplot(aes(x = year, y = c_)) +
  geom_line(size = 1, color = "tomato") +
  labs(y = "Carbon (Gt C)", x = "Year") +
  facet_wrap( ~var, nrow = 2, labeller = labeller(var = c("c_influx" = "C influx", "c_pool" = "C pool")) ) +
  theme(strip.text = element_text(face = "bold")) +
  theme_grey(base_size = 20) +
  transition_reveal(year)

animate(gganim, nframes = 100, width = 700, height = 500, fps = 10, renderer = gifski_renderer("~/teaching/gcb/gganim_1box_pulse.gif", loop = TRUE))


##//////////////////////////////////////////////////
## Spin up with two turnover rates
##--------------------------------------------------
c_influx  <- rep( 60, nt )

df <- tibble(year = seq(nt), c_influx_tau50 = c_influx, c_influx_tau60 = c_influx) %>%
  dplyr::mutate(c_pool_tau50 = onebox(c_influx = c_influx_tau50, tau = 50, cpool0 = 0),
                c_pool_tau60 = onebox(c_influx = c_influx_tau60, tau = 60, cpool0 = 0))
library(purrr)
library(stringr)

gganim <- df %>%
  pivot_longer(starts_with("c_"), names_to = "var", values_to = "c_") %>%
  mutate(tau = purrr::map_chr(var, ~ifelse(str_detect(., pattern = "_tau50$"), "50", "60"))) %>%
  mutate(var = purrr::map_chr(var, ~str_remove(., "_tau50"))) %>%
  mutate(var = purrr::map_chr(var, ~str_remove(., "_tau60"))) %>%
  ggplot(aes(x = year, y = c_, color = tau)) +
  geom_line(size = 1) +
  facet_wrap(~var, nrow = 2, labeller = labeller(var = c("c_influx" = "C influx", "c_pool" = "C pool")), scales = "free") +
  labs(y = "Carbon (Gt C)", x = "Year") +
  theme(strip.text = element_text(face = "bold")) +
  theme_grey(base_size = 20) +
  transition_reveal(year)

animate(gganim, nframes = 100, width = 700, height = 500, fps = 10, renderer = gifski_renderer("~/teaching/gcb/gganim_1box_spinup.gif", loop = TRUE))


##//////////////////////////////////////////////////
## Equilibrium?
##--------------------------------------------------
c_influx  <- rep( 60, nt ) + rnorm(n = nt, mean = 0, sd = 5)

df <- tibble(year = seq(nt), c_influx = c_influx) %>%
  dplyr::mutate(c_pool_tau50 = onebox(c_influx = c_influx, tau = 50, cpool0 = 0))

gganim <- df %>%
  pivot_longer(starts_with("c_"), names_to = "var", values_to = "c_") %>%
  ggplot(aes(x = year, y = c_)) +
  geom_line(size = 1, color = "tomato") +
  facet_wrap(~var, nrow = 2, labeller = labeller(var = c("c_influx" = "C influx", "c_pool" = "C pool")), scales = "free") +
  labs(y = "Carbon (Gt C)", x = "Year") +
  theme(strip.text = element_text(face = "bold")) +
  theme_grey(base_size = 20) +
  transition_reveal(year)

animate(gganim, nframes = 100, width = 700, height = 500, fps = 10, renderer = gifski_renderer("~/teaching/gcb/gganim_1box_steadystate.gif", loop = TRUE))


##//////////////////////////////////////////////////
## Step change in input flux
##--------------------------------------------------
c_influx  <- c(rep( 60, 100 ), rep( 80, (nt-100) ))

df <- tibble(year = seq(nt), c_influx_tau50 = c_influx, c_influx_tau60 = c_influx) %>%
  dplyr::mutate(c_pool_tau50 = onebox(c_influx = c_influx_tau50, tau = 50, cpool0 = 3000),
                c_pool_tau60 = onebox(c_influx = c_influx_tau60, tau = 60, cpool0 = 3000*60/50))

gganim <- df %>%
  mutate(c_pool_tau50 = c_pool_tau50 - c_pool_tau50[1], c_pool_tau60 = c_pool_tau60 - c_pool_tau60[1]) %>%
  pivot_longer(starts_with("c_"), names_to = "var", values_to = "c_") %>%
  mutate(tau = purrr::map_chr(var, ~ifelse(str_detect(., pattern = "_tau50$"), "50", "60"))) %>%
  mutate(var = purrr::map_chr(var, ~str_remove(., "_tau50"))) %>%
  mutate(var = purrr::map_chr(var, ~str_remove(., "_tau60"))) %>%
  ggplot(aes(x = year, y = c_, color = tau)) +
  geom_line(size = 1) +
  facet_wrap(~var, nrow = 2, labeller = labeller(var = c("c_influx" = "C influx", "c_pool" = "Increase in C pool")), scales = "free") +
  labs(y = "Carbon (Gt C)", x = "Year") +
  theme(strip.text = element_text(face = "bold")) +
  theme_grey(base_size = 20) +
  transition_reveal(year)

animate(gganim, nframes = 100, width = 700, height = 500, fps = 10, renderer = gifski_renderer("~/teaching/gcb/gganim_1box_step.gif", loop = TRUE))


##//////////////////////////////////////////////////
## Step change in turnover time
##--------------------------------------------------
c_influx  <- rep( 60, nt )
vec_tau <-  c(rep( 50, 100 ), rep( 60, (nt-100) ))

df <- tibble(year = seq(nt),
             c_influx = c_influx,
             tau = vec_tau) %>%
  dplyr::mutate(c_pool = onebox(c_influx = c_influx, tau = vec_tau, cpool0 = 3000)) %>%
  dplyr::mutate(c_uptake = c_pool - lag(c_pool, 1))

gganim <- df %>%
  pivot_longer(c(tau, c_pool, c_uptake), names_to = "var", values_to = "c_") %>%
  mutate(var = factor(var, levels = c("tau", "c_pool", "c_uptake"))) %>%
  ggplot(aes(x = year, y = c_)) +
  geom_line(size = 1, color = "tomato") +
  facet_wrap(~var, nrow = 3, labeller = labeller(var = c("tau" = "tau (yr)", "c_pool" = "C pool (Gt C)", "c_uptake" = "C uptake (Gt C/yr)")), scales = "free") +
  labs(y = "", x = "Year") +
  theme(strip.text = element_text(face = "bold")) +
  theme_grey(base_size = 20) +
  transition_reveal(year)

animate(gganim, nframes = 100, width = 700, height = 500, fps = 10, renderer = gifski_renderer("~/teaching/gcb/gganim_1box_tauincrease.gif", loop = TRUE))



## execute function 'onebox' with above determined arguments
## and store output time series (function return value) to 'out1'
out1 <- onebox( c_influx, cpool0, tau )

## do the same but now with increased turnover time, store
## function return values to 'out2'
tau  <- 60
out2 <- onebox( c_influx, cpool0, tau )
