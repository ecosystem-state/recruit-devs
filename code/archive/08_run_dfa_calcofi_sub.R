library(bayesdfa)
library(dplyr)
library(tidyr)

# Do regressions on calcofi-index -- both trends
mod = readRDS("calcofi_rec_dev_trend_2.rds")
fit <- dfa_fitted(mod, names = levels(as.factor(mod$orig_data$name)))
fits <- dplyr::group_by(fit, ID) %>%
  dplyr::summarise(rho = cor(estimate,y)) %>%
  dplyr::arrange(-rho) %>%
  as.data.frame()
loadings <- dfa_loadings(rotate_trends(mod),names = levels(as.factor(mod$orig_data$name)))
tdf = dplyr::filter(loadings, trend=="Trend 1")
tdf$median2 = dplyr::filter(loadings, trend=="Trend 2")$median
loadings = tdf[,c("name","median","median2")]
dist = dist(loadings[,-1], diag=TRUE, upper=TRUE)^2
fits <- loadings$name[c(2,9,52,54, 23, 33, 47, 4, 6, 7,8, 51,29,10,29,31,38,50,3)]

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

# further cut out infinite vals
dat <- dplyr::filter(dat, is.finite(obs))

# cut out spp that have higher loadings than anything we care about
#dat <- dplyr::filter(dat, name%in%c(fits$ID[1:8])==FALSE)

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

saveRDS(fit[[2]], "calcofi_trend_2_sub.rds")
