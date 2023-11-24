library(bayesdfa)
library(dplyr)
library(tidyr)

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

calcofi$name <- paste0(calcofi$name, "-cc")
dat <- (calcofi)
dat$year <- dat$time
dat$time <- dat$time - min(dat$time) + 1
dat$ts <- as.numeric(as.factor(dat$name))

# dat <- dplyr::filter(calcofi, name %in% c("Engraulis mordax",
#                                           "Sardinops sagax",
#                                           "Merluccius productus",
#                                           "Vinciguerria poweriae",
#                                           "Stenobrachius leucopsarus",
#                                           "Sebastes",
#                                           "Lipolagus ochotensis",
#                                           "Diogenichthys laternatus",
#                                           "Triphoturus mexicanus",
#                                           "Citharichthys",
#                                           "Citharichthys sordidus",
#                                           "Protomyctophum crockeri",
#                                           "Bathylagoides wesethi",
#                                           "Chauliodus macouni",
#                                           "Cyclothone",
#                                           "Nannobrachium",
#                                           "Nannobrachium ritteri",
#                                           "Genyonemus lineatus",
#                                           "Scomber japonicus",
#                                           "Idiacanthus antrostomus",
#                                           "Parophrys vetulus"))

# further cut out infinite vals
dat <- dplyr::filter(dat, is.finite(obs))
# filter out spp with < 20 observations
dat <- dplyr::group_by(dat, name) %>%
  dplyr::mutate(nz = length(which(n_pos_cpue!=0))) %>%
  dplyr::filter(nz >= 20) %>%
  dplyr::select(-nz)

options(mc.cores = parallel::detectCores())
# Running these models is not informative. Data alone are too noisy
# and don't show a pattern
#
fit = list()
for(i in 1:2) {
  fit[[i]] <- fit_dfa(dat, num_trends = i,
                      trend_model = "rw",
                      #n_knots = 30,
                      data_shape="long",
                      chains = 3,
                      iter=4000)
}

saveRDS(fit, "fitted_models/calcofi.rds")
