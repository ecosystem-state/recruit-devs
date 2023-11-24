# This constructs the DFA index for ROMS data
library(bayesdfa)
library(dplyr)
library(tidyr)
library(ggplot2)
d <- readRDS("data/clean_rec_devs.rds")
d = d[,c("Yr","dev","species")]
d = dplyr::rename(d, time=Yr,obs=dev,name=species)

enviro <- read.csv("data/roms.july_june.mean.csv")
enviro = enviro[,1:7]
enviro = dplyr::rename(enviro, time = yr)
enviro = tidyr::pivot_longer(enviro,cols=2:7) %>%
  dplyr::rename(obs = value)

dat <- rbind(d, enviro)
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

saveRDS(fit, "fitted_models/roms_dfa.rds")

spp_names = levels(as.factor(fit[[1]]$orig_data$name))
jpeg("plots/roms_dfa_trends.jpeg", width = 480*2, height = 480*2)
plot_trends(rotate_trends(fit[[1]])) + theme_bw()
dev.off()

jpeg("plots/roms_dfa_loadings.jpeg", width = 480*2, height = 480*2)
plot_loadings(rotate_trends(fit[[1]]), names = spp_names) + theme_bw()
dev.off()

jpeg("plots/roms_dfa_fitted.jpeg", width = 480*2, height = 480*2)
g = plot_fitted(fit[[1]], names = spp_names) +
  theme_bw()
g
dev.off()
