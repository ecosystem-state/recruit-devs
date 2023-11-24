# This constructs the DFA index for Calcofi data
library(bayesdfa)
library(dplyr)
library(tidyr)
library(ggplot2)
d <- readRDS("data/clean_rec_devs.rds")
d = d[,c("Yr","dev","species")]
d = dplyr::rename(d, time=Yr,obs=dev,name=species)
d = dplyr::filter(d, time >= 1985, time <= 2015)
calcofi <- readRDS("data/predicted_indices.rds")
calcofi <- dplyr::filter(calcofi, year >= 1985)
calcofi <- calcofi[-which(duplicated(calcofi)==TRUE),]
calcofi <- dplyr::filter(calcofi,
                         species %in% c("Idiacanthus antrostomus",
                                        "Danaphos oculatus",
                                        "Disintegrated fish larvae",
                                        "Lestidiops ringens",
                                        "Argyropelecus sladeni") == FALSE)
calcofi <- dplyr::rename(calcofi, time=year, obs = index, name = species)

calcofi <- dplyr::group_by(calcofi, name) %>%
  dplyr::mutate(nz = length(which(n_pos_cpue!=0))) %>%
  dplyr::filter(nz >= 20) %>%
  dplyr::select(-nz)

calcofi$name <- paste0(calcofi$name, "-cc")
dat <- rbind(d, calcofi)
dat$year <- dat$time
dat$time <- dat$time - min(dat$time) + 1
dat$ts <- as.numeric(as.factor(dat$name))

# further cut out infinite vals
dat <- dplyr::filter(dat, is.finite(obs))

options(mc.cores = parallel::detectCores())
# Running these models is not informative. Data alone are too noisy
# and don't show a pattern
#
fit = list()
for(i in 1:2) {
  fit[[i]] <- fit_dfa(dat, num_trends = i,
                      trend_model = "rw",
                      #n_knots = 30,
                      estimate_nu=TRUE,
                      data_shape="long",
                      chains = 3,
                      iter=4000)
}
# fit_dfa(dat, num_trends = 1, trend_model = "rw") 4261.1 65.2
# fit_dfa(dat, num_trends = 2, trend_model = "rw") 4177.0 66.7
# fit_dfa(dat, num_trends = 1, trend_model = "rw", estimate_trend_ar = TRUE) 4262.1 65.1
# fit_dfa(dat, num_trends = 2, trend_model = "rw",estimate_trend_ar = TRUE) 4178.8 66.7
# fit_dfa(dat, num_trends = 1, trend_model = "rw", estimate_trend_ma = TRUE) 4261.2 65.2
# fit_dfa(dat, num_trends = 2, trend_model = "rw",estimate_trend_ma = TRUE) 4177.7 66.8
# fit_dfa(dat, num_trends = 1, trend_model = "rw", estimate_trend_ma = TRUE) 4261.2 65.2
# fit_dfa(dat, num_trends = 2, trend_model = "rw",estimate_trend_ma = TRUE) 4177.7 66.8

saveRDS(fit, "fitted_models/calcofi_dfa_trend_2.rds")

spp_names = levels(as.factor(fit[[2]]$orig_data$name))
jpeg("plots/calcofi_dfa_trends.jpeg", width = 480*2, height = 480*2)
plot_trends(rotate_trends(fit[[2]])) + theme_bw()
dev.off()

jpeg("plots/calcofi_dfa_loadings.jpeg", width = 480*2, height = 480*2)
plot_loadings(rotate_trends(fit[[2]]), names = spp_names) + theme_bw()
dev.off()

jpeg("plots/calcofi_dfa_fitted.jpeg", width = 480*2, height = 480*2)
g = plot_fitted(fit[[2]], names = spp_names) +
  theme_bw()
g
dev.off()
