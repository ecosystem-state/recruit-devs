library(bayesdfa)
library(dplyr)
library(tidyr)

rreas <- readRDS("data/predicted_indices rreas.rds")
rreas <- dplyr::rename(rreas, time=year,obs = index, name = species)
rreas$name <- paste0(rreas$name, "-rs")

dat <- (rreas)
dat$year <- dat$time
dat$time <- dat$year - min(dat$year) + 1
dat$ts <- as.numeric(as.factor(dat$name))

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

saveRDS(fit, "fitted_models/rreas.rds")
