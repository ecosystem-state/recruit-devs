library(bayesdfa)
library(dplyr)
library(tidyr)

enviro <- read.csv("data/roms.july_june.mean.csv")
enviro = enviro[,1:7]
enviro = dplyr::rename(enviro, time = yr)
enviro = tidyr::pivot_longer(enviro,cols=2:7) %>%
  dplyr::rename(obs = value)

dat <- enviro
dat$ts <- as.numeric(as.factor(dat$name))

dat = dplyr::filter(dat,
                    time >= 1980, time <= 2016)
dat$year = dat$time
dat$time = dat$time - min(dat$time) + 1

# further cut out infinite vals
dat <- dplyr::filter(dat, is.finite(obs))

options(mc.cores = parallel::detectCores())
# Running these models is not informative. Data alone are too noisy
# and don't show a pattern
#
fit = list()
for(i in 1:1) {
  fit[[i]] <- fit_dfa(dat, num_trends = i,
                      trend_model = "rw",
                      #n_knots = 30,
                      data_shape="long",
                      chains = 3,
                      iter=4000)
}

# fit_dfa(dat, num_trends = 1,trend_model = "rw") 2379.4 48.8
# fit_dfa(dat, num_trends = 1,trend_model = "ps",knots=30) 2421.1 47.5
# fit_dfa(dat, num_trends = 1,trend_model = "rw",estimate_trend_ar = TRUE) 2380.6 48.7
# fit_dfa(dat, num_trends = 1,trend_model = "rw",estimate_nu = TRUE) 2378.3 48.8
# fit_dfa(dat, num_trends = 1,trend_model = "rw",estimate_trend_ma = TRUE) 2379.3 48.8

saveRDS(fit, "fitted_models/roms.rds")
