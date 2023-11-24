# This constructs the DFA index for RREAS data
library(bayesdfa)
library(dplyr)
library(tidyr)
library(ggplot2)
d <- readRDS("data/clean_rec_devs.rds")
d = d[,c("Yr","dev","species")]
d = dplyr::rename(d, time=Yr,obs=dev,name=species)
d = dplyr::filter(d, time >= 1990, time <= 2015)

rreas <- readRDS("data/predicted_indices rreas.rds")
rreas <- dplyr::rename(rreas, time=year,obs = index, name = species)
rreas$name <- paste0(rreas$name, "-rs")

dat <- rbind(d, rreas)
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
for(i in 1:3) {
  fit[[i]] <- fit_dfa(dat, num_trends = i,
                      trend_model = "rw",
                      #n_knots = 30,
                      data_shape="long",
                      chains = 3,
                      iter=4000)
}

saveRDS(fit[[1]], "rreas_dfa_trend_1.rds")
spp_names = levels(as.factor(fit[[1]]$orig_data$name))
jpeg("plots/rreas_dfa_trends.jpeg", width = 480*2, height = 480*2)
plot_trends(rotate_trends(fit[[1]])) + theme_bw()
dev.off()

jpeg("plots/rreas_dfa_loadings.jpeg", width = 480*2, height = 480*2)
plot_loadings(rotate_trends(fit[[1]]), names = spp_names) + theme_bw()
dev.off()

jpeg("plots/rreas_dfa_fitted.jpeg", width = 480*2, height = 480*2)
g = plot_fitted(fit[[1]], names = spp_names) + theme_bw()
g
dev.off()
