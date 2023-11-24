library(bayesdfa)
library(dplyr)

d <- readRDS("data/clean_rec_devs.rds")

# add obs / time / ts indicators
d$ts = as.numeric(as.factor(d$species))
d$time = d$Yr - min(d$Yr) + 1
d$obs = d$dev
d = dplyr::filter(d, Yr <= 2015)

options(mc.cores = parallel::detectCores())
# Running these models is not informative. Data alone are too noisy
# and don't show a pattern
#
fit = list()
for(i in 1:3) {
fit[[i]] <- fit_dfa(d, num_trends = i,
                  trend_model = "rw",
                  #n_knots = 30,
                  data_shape="long",
                  chains = 3,
                  iter=4000)
}

jpeg("plots/recdev_trends.jpeg", width = 480*2, height = 480*2)
plot_trends(rotate_trends(fit[[1]])) + theme_bw()
dev.off()

jpeg("plots/recdev_loadings.jpeg", width = 480*2, height = 480*2)
plot_loadings(rotate_trends(fit[[1]]), names = unique(d$species)) + theme_bw()
dev.off()

jpeg("plots/recdev_fitted.jpeg", width = 480*2, height = 480*2)
g = plot_fitted(fit[[1]], names = unique(d$species)) +
  theme_bw()
g
dev.off()

