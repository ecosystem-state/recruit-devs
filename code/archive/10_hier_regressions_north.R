library(dplyr)
library(bayesdfa)
library(tidyr)

# Load response data
rec_devs = readRDS("data/clean_rec_devs.rds")
rec_devs = dplyr::rename(rec_devs, time = Yr)
# Standardize
rec_devs = dplyr::group_by(rec_devs, species) %>%
  dplyr::mutate(z = (dev-mean(dev))/sd(dev)) %>%
  dplyr::filter(time<=2016)

rec_devs = dplyr::filter(rec_devs, short_name%in%c("Black_rockfish_CA",
          "Blue/Deacon_rockfish_CA","Cabezon_SCA","California_scorpionfish",
          "Chilipepper_rockfish","Gopher/black-and-yellow_rockfish",
          "Yellowtail_rockfish_South") ==FALSE)

n_forecast = 15 # years to forecast for predictive modeling below

# Do regressions on CalCOFI variables -- 2 spp
rreas <- readRDS("data/predicted_indices rreas.rds")
rreas = dplyr::rename(rreas, time = year, name=species,cov=index)
rreas <- dplyr::group_by(rreas, name) %>%
  dplyr::mutate(nz = length(which(mean_catch!=0))) %>%
  dplyr::filter(nz >= 20) %>%
  dplyr::select(-nz) %>%
  dplyr::filter(time <= max(rec_devs$time))

spp_combo = expand.grid("species_1" = unique(rreas$name),
                        "species_2" = unique(rreas$name))
spp_combo = dplyr::filter(spp_combo, species_1 != species_2)
spp_combo$mean_r2 = 0
spp_combo$rmse = 0
spp_combo$aic = 0
spp_combo$log_dens = 0
# first pivot wider
rreas = tidyr::pivot_wider(rreas[,1:3], names_from="name",values_from = "cov")

# standardize SSB by spp
rec_devs = dplyr::group_by(rec_devs, species) %>%
  dplyr::mutate(SpawnBio = as.numeric(scale(log(SpawnBio))))

best_rmse <- 100
for(i in 1:nrow(spp_combo)) {
  tmp = rreas[,c(1,which(names(rreas) %in% c(spp_combo[i,1],spp_combo[i,2])))]
  tmp[,2] = as.numeric(scale(tmp[,2]))
  tmp[,3] = as.numeric(scale(tmp[,3]))

  sub = dplyr::left_join(as.data.frame(rec_devs), tmp, by="time")
  names(sub)[7:8] = c("cov_1","cov_2")

  sub =
    dplyr::filter(sub, !is.na(cov_1+cov_2), !is.infinite(cov_1+cov_2))
  # ggplot(sub, aes(cov,z)) + geom_point() + facet_wrap(~species)
  sub$est <- NA
  sub$log_dens <- NA
  for(yr in (max(sub$time)-n_forecast):max(sub$time)) {
    fit <- lm(z ~ -1 + cov_1:species + cov_2:species, data=sub[which(sub$time<yr),])
    sub$est[which(sub$time==yr)] <- predict(fit, newdata = sub[which(sub$time==yr),])
    se = predict(fit, newdata = sub[which(sub$time==yr),], se.fit=TRUE)$se.fit
    sub$log_dens[which(sub$time==yr)] = dnorm(sub$z[which(sub$time==yr)],
                                              mean = sub$est[which(sub$time==yr)],
                                              sd = se, log=TRUE)
  }

  fit_full <- lm(z ~ -1 + cov_1:species + cov_2:species, data=sub)
  spp_combo$aic[i] = AIC(fit_full)

  # calculate R2 by species
  r2 = dplyr::group_by(dplyr::filter(sub,!is.na(est)), species) %>%
    dplyr::summarise(r2 = round(cor(est,z)^2, 3))

  spp_combo$mean_r2[i] = mean(r2$r2)
  spp_combo$rmse[i] = sqrt(mean((sub$est - sub$z)^2,na.rm=T))
  if(spp_combo$rmse[i] < best_rmse) {
    best_rmse = spp_combo$rmse[i]
    best_dat = sub
  }
  spp_combo$log_dens[i] = sum(sub$log_dens,na.rm=T)
  #results_table = left_join(results_table, r2)
  #colnames(results_table)[ncol(results_table)] = unique(calcofi$name)[i]
}

results_rreas_2spp = list(all_results = spp_combo,
                            best_dat = best_dat)
saveRDS(results_rreas_2spp, "fitted_models/results_rreas_2spp.rds")

# Do regressions on RREAS variables -- 3 spp
rreas <- readRDS("data/predicted_indices rreas.rds")
rreas = dplyr::rename(rreas, time = year, name=species,cov=index)
rreas <- dplyr::group_by(rreas, name) %>%
  dplyr::mutate(nz = length(which(mean_catch!=0))) %>%
  dplyr::filter(nz >= 20) %>%
  dplyr::select(-nz) %>%
  dplyr::filter(time <= max(rec_devs$time))

spp_combo = expand.grid("species_1" = unique(rreas$name),
                        "species_2" = unique(rreas$name),
                        "species_3" = unique(rreas$name))
spp_combo = dplyr::filter(spp_combo, species_1 != species_2,
                          species_2 != species_3,
                          species_1 != species_3)
spp_combo$mean_r2 = 0
spp_combo$rmse = 0
spp_combo$aic = 0
spp_combo$log_dens = 0
# first pivot wider
rreas = tidyr::pivot_wider(rreas[,1:3], names_from="name",values_from = "cov")

# standardize SSB by spp
#rec_devs = dplyr::group_by(rec_devs, species) %>%
#  dplyr::mutate(SpawnBio = as.numeric(scale(log(SpawnBio))))

best_rmse <- 100
for(i in 1:nrow(spp_combo)) {
  tmp = rreas[,c(1,which(names(rreas) %in% c(spp_combo[i,1],spp_combo[i,2],spp_combo[i,3])))]
  tmp[,2] = as.numeric(scale(tmp[,2]))
  tmp[,3] = as.numeric(scale(tmp[,3]))
  tmp[,4] = as.numeric(scale(tmp[,4]))
  sub = dplyr::left_join(as.data.frame(rec_devs), tmp, by="time")
  names(sub)[7:9] = c("cov_1","cov_2","cov_3")

  sub =
    dplyr::filter(sub, !is.na(cov_1+cov_2+cov_3), !is.infinite(cov_1+cov_2+cov_3))

  # ggplot(sub, aes(cov,z)) + geom_point() + facet_wrap(~species)
  sub$est <- NA
  sub$log_dens <- NA
  for(yr in (max(sub$time)-n_forecast):max(sub$time)) {
    fit <- lm(z ~ -1 + cov_1:species + cov_2:species+cov_3:species, data=sub[which(sub$time<yr),])
    sub$est[which(sub$time==yr)] <- predict(fit, newdata = sub[which(sub$time==yr),])
    se = predict(fit, newdata = sub[which(sub$time==yr),], se.fit=TRUE)$se.fit
    sub$log_dens[which(sub$time==yr)] = dnorm(sub$z[which(sub$time==yr)],
                                              mean = sub$est[which(sub$time==yr)],
                                              sd = se, log=TRUE)
  }
  fit_full <- lm(z ~ -1 + cov_1:species + cov_2:species, data=sub)
  spp_combo$aic[i] = AIC(fit_full)

  # calculate R2 by species
  r2 = dplyr::group_by(dplyr::filter(sub,!is.na(est)), species) %>%
    dplyr::summarise(r2 = round(cor(est,z)^2, 3))

  spp_combo$mean_r2[i] = mean(r2$r2)
  spp_combo$rmse[i] = sqrt(mean((sub$est - sub$z)^2,na.rm=T))
  spp_combo$log_dens[i] = sum(sub$log_dens,na.rm=T)

  if(spp_combo$rmse[i] < best_rmse) {
    best_rmse = spp_combo$rmse[i]
    best_dat = sub
  }
  #results_table = left_join(results_table, r2)
  #colnames(results_table)[ncol(results_table)] = unique(calcofi$name)[i]
}
results_rreas_3spp = list(all_results = spp_combo,
                            best_dat = best_dat)
saveRDS(results_rreas_3spp, "fitted_models/results_rreas_3spp.rds")


# Do regressions on CalCOFI variables -- 2 spp
enviro <- read.csv("data/roms.july_june.mean.csv")
enviro = dplyr::rename(enviro, time = yr)
enviro = enviro[,c(1:7)]

spp_combo = expand.grid("species_1" = names(enviro)[-1],
                        "species_2" = names(enviro)[-1])
spp_combo = dplyr::filter(spp_combo, species_1 != species_2)
spp_combo$mean_r2 = 0
spp_combo$rmse = 0
spp_combo$aic = 0
spp_combo$log_dens = 0

best_rmse <- 100
for(i in 1:nrow(spp_combo)) {
  tmp = enviro[,c(1,which(names(enviro) %in% c(spp_combo[i,1],spp_combo[i,2])))]
  sub = dplyr::left_join(as.data.frame(rec_devs), tmp, by="time")
  sub$cov_1 <- NA
  sub$cov_2 <- NA
  names(sub)[7:8] = c("cov1","cov2")
  for(j in 1:nrow(sub)) {
    if(!is.null(sub$cov1[[j]])) sub$cov_1[j] = unlist(sub$cov1[[j]])
    if(!is.null(sub$cov2[[j]])) sub$cov_2[j] = unlist(sub$cov2[[j]])
  }
  sub =
    dplyr::filter(sub, !is.na(cov_1+cov_2), !is.infinite(cov_1+cov_2)) %>%
    dplyr::mutate(cov_1 = (cov_1-mean(cov_1))/sd(cov_1),
                  cov_2 = (cov_2-mean(cov_2))/sd(cov_2))
  # ggplot(sub, aes(cov,z)) + geom_point() + facet_wrap(~species)
  sub$est <- NA
  sub$log_dens <- NA
  for(yr in (max(sub$time)-n_forecast):max(sub$time)) {
    fit <- lm(z ~ -1 + cov_1:species + cov_2:species, data=sub[which(sub$time<yr),])
    sub$est[which(sub$time==yr)] <- predict(fit, newdata = sub[which(sub$time==yr),])
    se = predict(fit, newdata = sub[which(sub$time==yr),], se.fit=TRUE)$se.fit
    sub$log_dens[which(sub$time==yr)] = dnorm(sub$z[which(sub$time==yr)],
                                              mean = sub$est[which(sub$time==yr)],
                                              sd = se, log=TRUE)
  }

  fit_full <- lm(z ~ -1 + cov_1:species + cov_2:species, data=sub)
  spp_combo$aic[i] = AIC(fit_full)

  # calculate R2 by species
  r2 = dplyr::group_by(dplyr::filter(sub,!is.na(est)), species) %>%
    dplyr::summarise(r2 = round(cor(est,z)^2, 3))

  spp_combo$mean_r2[i] = mean(r2$r2)
  spp_combo$rmse[i] = sqrt(mean((sub$est - sub$z)^2,na.rm=T))
  if(spp_combo$rmse[i] < best_rmse) {
    best_rmse = spp_combo$rmse[i]
    best_dat = sub
  }
  spp_combo$log_dens[i] = sum(sub$log_dens,na.rm=T)
  #results_table = left_join(results_table, r2)
  #colnames(results_table)[ncol(results_table)] = unique(calcofi$name)[i]
}
results_enviro_2spp = list(all_results = spp_combo,
                           best_dat = best_dat)
saveRDS(results_enviro_2spp, "fitted_models/results_enviro-north_2spp.rds")


enviro <- read.csv("data/roms.july_june.mean.csv")
enviro = dplyr::rename(enviro, time = yr)
enviro = enviro[,c(1:7)]

spp_combo = expand.grid("species_1" = names(enviro)[-1],
                        "species_2" = names(enviro)[-1],
                        "species_3" = names(enviro)[-1])
spp_combo = dplyr::filter(spp_combo, species_1 != species_2,
                          species_1 != species_3,
                          species_2 != species_3)
spp_combo$mean_r2 = 0
spp_combo$aic = 0
spp_combo$rmse = 0
spp_combo$log_dens = 0

best_rmse <- 100
for(i in 1:nrow(spp_combo)) {
  tmp = enviro[,c(1,which(names(enviro) %in% c(spp_combo[i,1],spp_combo[i,2],spp_combo[i,3])))]
  sub = dplyr::left_join(as.data.frame(rec_devs), tmp, by="time")
  sub$cov_1 <- NA
  sub$cov_2 <- NA
  sub$cov_3 <- NA
  names(sub)[7:9] = c("cov1","cov2","cov3")
  for(j in 1:nrow(sub)) {
    if(!is.null(sub$cov1[[j]])) sub$cov_1[j] = unlist(sub$cov1[[j]])
    if(!is.null(sub$cov2[[j]])) sub$cov_2[j] = unlist(sub$cov2[[j]])
    if(!is.null(sub$cov3[[j]])) sub$cov_3[j] = unlist(sub$cov3[[j]])
  }
  sub =
    dplyr::filter(sub, !is.na(cov_1+cov_2+cov_3), !is.infinite(cov_1+cov_2+cov_3)) %>%
    dplyr::mutate(cov_1 = (cov_1-mean(cov_1))/sd(cov_1),
                  cov_2 = (cov_2-mean(cov_2))/sd(cov_2),
                  cov_3 = (cov_3-mean(cov_3))/sd(cov_3))
  # ggplot(sub, aes(cov,z)) + geom_point() + facet_wrap(~species)
  sub$est <- NA
  sub$log_dens <- NA
  for(yr in (max(sub$time)-n_forecast):max(sub$time)) {
    fit <- lm(z ~ -1 + cov_1:species + cov_2:species + cov_3:species, data=sub[which(sub$time<yr),])
    sub$est[which(sub$time==yr)] <- predict(fit, newdata = sub[which(sub$time==yr),])
    se = predict(fit, newdata = sub[which(sub$time==yr),], se.fit=TRUE)$se.fit
    sub$log_dens[which(sub$time==yr)] = dnorm(sub$z[which(sub$time==yr)],
                                              mean = sub$est[which(sub$time==yr)],
                                              sd = se, log=TRUE)
  }
  fit_full <- lm(z ~ -1 + cov_1:species + cov_2:species + cov_3:species, data=sub)
  spp_combo$aic[i] = AIC(fit_full)

  # calculate R2 by species
  r2 = dplyr::group_by(dplyr::filter(sub,!is.na(est)), species) %>%
    dplyr::summarise(r2 = round(cor(est,z)^2, 3))

  spp_combo$mean_r2[i] = mean(r2$r2)
  spp_combo$rmse[i] = sqrt(mean((sub$est - sub$z)^2,na.rm=T))
  spp_combo$log_dens[i] = sum(sub$log_dens,na.rm=T)

  if(spp_combo$rmse[i] < best_rmse) {
    best_rmse = spp_combo$rmse[i]
    best_dat = sub
  }
  #results_table = left_join(results_table, r2)
  #colnames(results_table)[ncol(results_table)] = unique(calcofi$name)[i]
}
results_enviro_3spp = list(all_results = spp_combo,
                           best_dat = best_dat)
saveRDS(results_enviro_3spp, "fitted_models/results_enviro-north_3spp.rds")


load("data/ewidata.rda")
dat <- ewidata[grep("NLCOPE",ewidata$code),c("year","code","value")]

dat = dplyr::filter(dat,year >= 1985) %>%
  dplyr::group_by(code) %>%
  dplyr::mutate(n = length(which(!is.na(value)))) %>%
  dplyr::filter(n >= n_forecast) %>%
  dplyr::select(-n)

dat = tidyr::pivot_wider(dat[,1:3], names_from="code",values_from = "value") %>%
  dplyr::rename(time = year)
for(i in 2:ncol(dat)) {
  dat[,i] = as.numeric(scale(dat[,i]))
}
spp_combo = expand.grid("species_1" = names(dat)[-1],
                        "species_2" = names(dat)[-1])
spp_combo = dplyr::filter(spp_combo, species_1 != species_2)
spp_combo$mean_r2 = 0
spp_combo$rmse = 0
spp_combo$aic = 0
spp_combo$log_dens = 0

best_rmse <- 100
for(i in 1:nrow(spp_combo)) {
  tmp = dat[,c(1,which(names(dat) %in% c(spp_combo[i,1],spp_combo[i,2])))]
  sub = dplyr::left_join(as.data.frame(rec_devs), tmp, by="time")
  names(sub)[7:8] = c("cov_1","cov_2")

  sub =
    dplyr::filter(sub, !is.na(cov_1+cov_2), !is.infinite(cov_1+cov_2))
  # ggplot(sub, aes(cov,z)) + geom_point() + facet_wrap(~species)
  sub$est <- NA
  sub$log_dens <- NA
  for(yr in (max(sub$time)-n_forecast):max(sub$time)) {
    fit <- lm(z ~ -1 + cov_1:species + cov_2:species, data=sub[which(sub$time<yr),])
    sub$est[which(sub$time==yr)] <- predict(fit, newdata = sub[which(sub$time==yr),])
    se = predict(fit, newdata = sub[which(sub$time==yr),], se.fit=TRUE)$se.fit
    sub$log_dens[which(sub$time==yr)] = dnorm(sub$z[which(sub$time==yr)],
                                              mean = sub$est[which(sub$time==yr)],
                                              sd = se, log=TRUE)
  }

  fit_full <- lm(z ~ -1 + cov_1:species + cov_2:species, data=sub)
  spp_combo$aic[i] = AIC(fit_full)

  # calculate R2 by species
  r2 = dplyr::group_by(dplyr::filter(sub,!is.na(est)), species) %>%
    dplyr::summarise(r2 = round(cor(est,z)^2, 3))

  spp_combo$mean_r2[i] = mean(r2$r2)
  spp_combo$rmse[i] = sqrt(mean((sub$est - sub$z)^2,na.rm=T))
  if(spp_combo$rmse[i] < best_rmse) {
    best_rmse = spp_combo$rmse[i]
    best_dat = sub
  }
  spp_combo$log_dens[i] = sum(sub$log_dens,na.rm=T)
  #results_table = left_join(results_table, r2)
  #colnames(results_table)[ncol(results_table)] = unique(calcofi$name)[i]
}

saveRDS(list(all_results = spp_combo,
             best_dat = best_dat), "fitted_models/results_nlcope_2spp.rds")

dat <- ewidata[grep("NLCOPE",ewidata$code),c("year","code","value")]

dat = dplyr::filter(dat,year >= 1985) %>%
  dplyr::group_by(code) %>%
  dplyr::mutate(n = length(which(!is.na(value)))) %>%
  dplyr::filter(n >= n_forecast) %>%
  dplyr::select(-n)

dat = tidyr::pivot_wider(dat[,1:3], names_from="code",values_from = "value") %>%
  dplyr::rename(time = year)
for(i in 2:ncol(dat)) {
  dat[,i] = as.numeric(scale(dat[,i]))
}
spp_combo = expand.grid("species_1" = names(dat)[-1],
                        "species_2" = names(dat)[-1],
                        "species_3" = names(dat)[-1])
spp_combo = dplyr::filter(spp_combo, species_1 != species_2,
                          species_1 != species_3,
                          species_2 != species_3)
spp_combo$mean_r2 = 0
spp_combo$rmse = 0
spp_combo$aic = 0
spp_combo$log_dens = 0

best_rmse <- 100
for(i in 1:nrow(spp_combo)) {
  tmp = dat[,c(1,which(names(dat) %in% c(spp_combo[i,1],spp_combo[i,2],spp_combo[i,3])))]
  sub = dplyr::left_join(as.data.frame(rec_devs), tmp, by="time")
  names(sub)[7:9] = c("cov_1","cov_2","cov_3")

  sub =
    dplyr::filter(sub, !is.na(cov_1+cov_2+cov_3), !is.infinite(cov_1+cov_2+cov_2))
  # ggplot(sub, aes(cov,z)) + geom_point() + facet_wrap(~species)
  sub$est <- NA
  sub$log_dens <- NA
  for(yr in (max(sub$time)-n_forecast):max(sub$time)) {
    fit <- lm(z ~ -1 + cov_1:species + cov_2:species + cov_3:species, data=sub[which(sub$time<yr),])
    sub$est[which(sub$time==yr)] <- predict(fit, newdata = sub[which(sub$time==yr),])
    se = predict(fit, newdata = sub[which(sub$time==yr),], se.fit=TRUE)$se.fit
    sub$log_dens[which(sub$time==yr)] = dnorm(sub$z[which(sub$time==yr)],
                                              mean = sub$est[which(sub$time==yr)],
                                              sd = se, log=TRUE)
  }

  fit_full <- lm(z ~ -1 + cov_1:species + cov_2:species + cov_3:species, data=sub)
  spp_combo$aic[i] = AIC(fit_full)

  # calculate R2 by species
  r2 = dplyr::group_by(dplyr::filter(sub,!is.na(est)), species) %>%
    dplyr::summarise(r2 = round(cor(est,z)^2, 3))

  spp_combo$mean_r2[i] = mean(r2$r2)
  spp_combo$rmse[i] = sqrt(mean((sub$est - sub$z)^2,na.rm=T))
  if(spp_combo$rmse[i] < best_rmse) {
    best_rmse = spp_combo$rmse[i]
    best_dat = sub
  }
  spp_combo$log_dens[i] = sum(sub$log_dens,na.rm=T)
  #results_table = left_join(results_table, r2)
  #colnames(results_table)[ncol(results_table)] = unique(calcofi$name)[i]
}

saveRDS(list(all_results = spp_combo,
             best_dat = best_dat), "fitted_models/results_nlcope_3spp.rds")


load("data/ewidata.rda")
dat <- ewidata[grep("WinIch",ewidata$code),c("year","code","value")]

dat = dplyr::filter(dat,year >= 1985) %>%
  dplyr::group_by(code) %>%
  dplyr::mutate(n = length(which(!is.na(value)))) %>%
  dplyr::filter(n >= n_forecast) %>%
  dplyr::select(-n)

dat = tidyr::pivot_wider(dat[,1:3], names_from="code",values_from = "value") %>%
  dplyr::rename(time = year)
for(i in 2:ncol(dat)) {
  dat[,i] = as.numeric(scale(dat[,i]))
}
spp_combo = expand.grid("species_1" = names(dat)[-1],
                        "species_2" = names(dat)[-1])
spp_combo = dplyr::filter(spp_combo, species_1 != species_2)
spp_combo$mean_r2 = 0
spp_combo$rmse = 0
spp_combo$aic = 0
spp_combo$log_dens = 0

best_rmse <- 100
for(i in 1:nrow(spp_combo)) {
  tmp = dat[,c(1,which(names(dat) %in% c(spp_combo[i,1],spp_combo[i,2])))]
  sub = dplyr::left_join(as.data.frame(rec_devs), tmp, by="time")
  names(sub)[7:8] = c("cov_1","cov_2")

  sub =
    dplyr::filter(sub, !is.na(cov_1+cov_2), !is.infinite(cov_1+cov_2))
  # ggplot(sub, aes(cov,z)) + geom_point() + facet_wrap(~species)
  sub$est <- NA
  sub$log_dens <- NA
  if(nrow(sub)>=n_forecast) {
  for(yr in (max(sub$time)-n_forecast):max(sub$time)) {
    fit <- lm(z ~ -1 + cov_1:species + cov_2:species, data=sub[which(sub$time<yr),])
    sub$est[which(sub$time==yr)] <- predict(fit, newdata = sub[which(sub$time==yr),])
    se = predict(fit, newdata = sub[which(sub$time==yr),], se.fit=TRUE)$se.fit
    sub$log_dens[which(sub$time==yr)] = dnorm(sub$z[which(sub$time==yr)],
                                              mean = sub$est[which(sub$time==yr)],
                                              sd = se, log=TRUE)
  }
  }
  fit_full <- lm(z ~ -1 + cov_1:species + cov_2:species, data=sub)
  spp_combo$aic[i] = AIC(fit_full)

  # calculate R2 by species
  r2 = dplyr::group_by(dplyr::filter(sub,!is.na(est)), species) %>%
    dplyr::summarise(r2 = round(cor(est,z)^2, 3))

  spp_combo$mean_r2[i] = mean(r2$r2)
  spp_combo$rmse[i] = sqrt(mean((sub$est - sub$z)^2,na.rm=T))
  if(!is.na(spp_combo$rmse[i]) & spp_combo$rmse[i] < best_rmse) {
    best_rmse = spp_combo$rmse[i]
    best_dat = sub
  }
  spp_combo$log_dens[i] = sum(sub$log_dens,na.rm=T)
  #results_table = left_join(results_table, r2)
  #colnames(results_table)[ncol(results_table)] = unique(calcofi$name)[i]
}

saveRDS(list(all_results = spp_combo,
             best_dat = best_dat), "fitted_models/results_winich_2spp.rds")

dat <- ewidata[grep("WinIch",ewidata$code),c("year","code","value")]

dat = dplyr::filter(dat,year >= 1985) %>%
  dplyr::group_by(code) %>%
  dplyr::mutate(n = length(which(!is.na(value)))) %>%
  dplyr::filter(n >= n_forecast) %>%
  dplyr::select(-n)

dat = tidyr::pivot_wider(dat[,1:3], names_from="code",values_from = "value") %>%
  dplyr::rename(time = year)
for(i in 2:ncol(dat)) {
  dat[,i] = as.numeric(scale(dat[,i]))
}
spp_combo = expand.grid("species_1" = names(dat)[-1],
                        "species_2" = names(dat)[-1],
                        "species_3" = names(dat)[-1])
spp_combo = dplyr::filter(spp_combo, species_1 != species_2,
                          species_1 != species_3,
                          species_2 != species_3)
spp_combo$mean_r2 = 0
spp_combo$rmse = 0
spp_combo$aic = 0
spp_combo$log_dens = 0

best_rmse <- 100
for(i in 1:nrow(spp_combo)) {
  tmp = dat[,c(1,which(names(dat) %in% c(spp_combo[i,1],spp_combo[i,2],spp_combo[i,3])))]
  sub = dplyr::left_join(as.data.frame(rec_devs), tmp, by="time")
  names(sub)[7:9] = c("cov_1","cov_2","cov_3")

  sub =
    dplyr::filter(sub, !is.na(cov_1+cov_2+cov_3), !is.infinite(cov_1+cov_2+cov_2))
  # ggplot(sub, aes(cov,z)) + geom_point() + facet_wrap(~species)
  sub$est <- NA
  sub$log_dens <- NA
  for(yr in (max(sub$time)-n_forecast):max(sub$time)) {
    fit <- lm(z ~ -1 + cov_1:species + cov_2:species + cov_3:species, data=sub[which(sub$time<yr),])
    sub$est[which(sub$time==yr)] <- predict(fit, newdata = sub[which(sub$time==yr),])
    se = predict(fit, newdata = sub[which(sub$time==yr),], se.fit=TRUE)$se.fit
    sub$log_dens[which(sub$time==yr)] = dnorm(sub$z[which(sub$time==yr)],
                                              mean = sub$est[which(sub$time==yr)],
                                              sd = se, log=TRUE)
  }

  fit_full <- lm(z ~ -1 + cov_1:species + cov_2:species + cov_3:species, data=sub)
  spp_combo$aic[i] = AIC(fit_full)

  # calculate R2 by species
  r2 = dplyr::group_by(dplyr::filter(sub,!is.na(est)), species) %>%
    dplyr::summarise(r2 = round(cor(est,z)^2, 3))

  spp_combo$mean_r2[i] = mean(r2$r2)
  spp_combo$rmse[i] = sqrt(mean((sub$est - sub$z)^2,na.rm=T))
  if(spp_combo$rmse[i] < best_rmse) {
    best_rmse = spp_combo$rmse[i]
    best_dat = sub
  }
  spp_combo$log_dens[i] = sum(sub$log_dens,na.rm=T)
  #results_table = left_join(results_table, r2)
  #colnames(results_table)[ncol(results_table)] = unique(calcofi$name)[i]
}

saveRDS(list(all_results = spp_combo,
             best_dat = best_dat), "fitted_models/results_winich_3spp.rds")






enviro <- read.csv("data/roms.july_june.mean.csv")
enviro = dplyr::rename(enviro, time = yr)
enviro = enviro[,c(1:7)]

spp_combo = expand.grid("species_1" = names(enviro)[-1],
                        "species_2" = names(enviro)[-1],
                        "species_3" = names(enviro)[-1])
spp_combo = dplyr::filter(spp_combo, species_1 != species_2,
                          species_1 != species_3,
                          species_2 != species_3)
spp_combo$mean_r2 = 0
spp_combo$aic = 0
spp_combo$rmse = 0
spp_combo$log_dens = 0

best_rmse <- 100
for(i in 1:nrow(spp_combo)) {
  tmp = enviro[,c(1,which(names(enviro) %in% c(spp_combo[i,1],spp_combo[i,2],spp_combo[i,3])))]
  sub = dplyr::left_join(as.data.frame(rec_devs), tmp, by="time")
  sub$cov_1 <- NA
  sub$cov_2 <- NA
  sub$cov_3 <- NA
  names(sub)[7:9] = c("cov1","cov2","cov3")
  for(j in 1:nrow(sub)) {
    if(!is.null(sub$cov1[[j]])) sub$cov_1[j] = unlist(sub$cov1[[j]])
    if(!is.null(sub$cov2[[j]])) sub$cov_2[j] = unlist(sub$cov2[[j]])
    if(!is.null(sub$cov3[[j]])) sub$cov_3[j] = unlist(sub$cov3[[j]])
  }
  sub =
    dplyr::filter(sub, !is.na(cov_1+cov_2+cov_3), !is.infinite(cov_1+cov_2+cov_3)) %>%
    dplyr::mutate(cov_1 = (cov_1-mean(cov_1))/sd(cov_1),
                  cov_2 = (cov_2-mean(cov_2))/sd(cov_2),
                  cov_3 = (cov_3-mean(cov_3))/sd(cov_3))
  # ggplot(sub, aes(cov,z)) + geom_point() + facet_wrap(~species)
  sub$est <- NA
  sub$log_dens <- NA
  for(yr in (max(sub$time)-n_forecast):max(sub$time)) {
    fit <- lm(z ~ -1 + cov_1:species + cov_2:species + cov_3:species, data=sub[which(sub$time<yr),])
    sub$est[which(sub$time==yr)] <- predict(fit, newdata = sub[which(sub$time==yr),])
    se = predict(fit, newdata = sub[which(sub$time==yr),], se.fit=TRUE)$se.fit
    sub$log_dens[which(sub$time==yr)] = dnorm(sub$z[which(sub$time==yr)],
                                              mean = sub$est[which(sub$time==yr)],
                                              sd = se, log=TRUE)
  }
  fit_full <- lm(z ~ -1 + cov_1:species + cov_2:species + cov_3:species, data=sub)
  spp_combo$aic[i] = AIC(fit_full)

  # calculate R2 by species
  r2 = dplyr::group_by(dplyr::filter(sub,!is.na(est)), species) %>%
    dplyr::summarise(r2 = round(cor(est,z)^2, 3))

  spp_combo$mean_r2[i] = mean(r2$r2)
  spp_combo$rmse[i] = sqrt(mean((sub$est - sub$z)^2,na.rm=T))
  spp_combo$log_dens[i] = sum(sub$log_dens,na.rm=T)

  if(spp_combo$rmse[i] < best_rmse) {
    best_rmse = spp_combo$rmse[i]
    best_dat = sub
  }
  #results_table = left_join(results_table, r2)
  #colnames(results_table)[ncol(results_table)] = unique(calcofi$name)[i]
}
results_enviro_3spp = list(all_results = spp_combo,
                           best_dat = best_dat)
saveRDS(results_enviro_3spp, "fitted_models/results_enviro-south_3spp.rds")






# Do regressions on CalCOFI variables -- 2 spp
enviro <- read.csv("data/roms.july_june.mean.csv")
enviro = dplyr::rename(enviro, time = yr)
enviro = enviro[,c(1,8:13)]

spp_combo = expand.grid("species_1" = names(enviro)[-1],
                        "species_2" = names(enviro)[-1])
spp_combo = dplyr::filter(spp_combo, species_1 != species_2)
spp_combo$mean_r2 = 0
spp_combo$rmse = 0
spp_combo$aic = 0
spp_combo$log_dens = 0

best_rmse <- 100
for(i in 1:nrow(spp_combo)) {
  tmp = enviro[,c(1,which(names(enviro) %in% c(spp_combo[i,1],spp_combo[i,2])))]
  sub = dplyr::left_join(as.data.frame(rec_devs), tmp, by="time")
  sub$cov_1 <- NA
  sub$cov_2 <- NA
  names(sub)[7:8] = c("cov1","cov2")
  for(j in 1:nrow(sub)) {
    if(!is.null(sub$cov1[[j]])) sub$cov_1[j] = unlist(sub$cov1[[j]])
    if(!is.null(sub$cov2[[j]])) sub$cov_2[j] = unlist(sub$cov2[[j]])
  }
  sub =
    dplyr::filter(sub, !is.na(cov_1+cov_2), !is.infinite(cov_1+cov_2)) %>%
    dplyr::mutate(cov_1 = (cov_1-mean(cov_1))/sd(cov_1),
                  cov_2 = (cov_2-mean(cov_2))/sd(cov_2))
  # ggplot(sub, aes(cov,z)) + geom_point() + facet_wrap(~species)
  sub$est <- NA
  sub$log_dens <- NA
  for(yr in (max(sub$time)-n_forecast):max(sub$time)) {
    fit <- lm(z ~ -1 + cov_1:species + cov_2:species, data=sub[which(sub$time<yr),])
    sub$est[which(sub$time==yr)] <- predict(fit, newdata = sub[which(sub$time==yr),])
    se = predict(fit, newdata = sub[which(sub$time==yr),], se.fit=TRUE)$se.fit
    sub$log_dens[which(sub$time==yr)] = dnorm(sub$z[which(sub$time==yr)],
                                              mean = sub$est[which(sub$time==yr)],
                                              sd = se, log=TRUE)
  }

  fit_full <- lm(z ~ -1 + cov_1:species + cov_2:species, data=sub)
  spp_combo$aic[i] = AIC(fit_full)

  # calculate R2 by species
  r2 = dplyr::group_by(dplyr::filter(sub,!is.na(est)), species) %>%
    dplyr::summarise(r2 = round(cor(est,z)^2, 3))

  spp_combo$mean_r2[i] = mean(r2$r2)
  spp_combo$rmse[i] = sqrt(mean((sub$est - sub$z)^2,na.rm=T))
  if(spp_combo$rmse[i] < best_rmse) {
    best_rmse = spp_combo$rmse[i]
    best_dat = sub
  }
  spp_combo$log_dens[i] = sum(sub$log_dens,na.rm=T)
  #results_table = left_join(results_table, r2)
  #colnames(results_table)[ncol(results_table)] = unique(calcofi$name)[i]
}
results_enviro_2spp = list(all_results = spp_combo,
                            best_dat = best_dat)
saveRDS(results_enviro_2spp, "fitted_models/results_enviro_2spp.rds")


enviro <- read.csv("data/roms.july_june.mean.csv")
enviro = dplyr::rename(enviro, time = yr)
enviro = enviro[,c(1,8:13)]

spp_combo = expand.grid("species_1" = names(enviro)[-1],
                        "species_2" = names(enviro)[-1],
                        "species_3" = names(enviro)[-1])
spp_combo = dplyr::filter(spp_combo, species_1 != species_2,
                          species_1 != species_3,
                          species_2 != species_3)
spp_combo$mean_r2 = 0
spp_combo$aic = 0
spp_combo$rmse = 0
spp_combo$log_dens = 0

best_rmse <- 100
for(i in 1:nrow(spp_combo)) {
  tmp = calcofi[,c(1,which(names(enviro) %in% c(spp_combo[i,1],spp_combo[i,2],spp_combo[i,3])))]
  sub = dplyr::left_join(as.data.frame(rec_devs), tmp, by="time")
  sub$cov_1 <- NA
  sub$cov_2 <- NA
  sub$cov_3 <- NA
  names(sub)[7:9] = c("cov1","cov2","cov3")
  for(j in 1:nrow(sub)) {
    if(!is.null(sub$cov1[[j]])) sub$cov_1[j] = unlist(sub$cov1[[j]])
    if(!is.null(sub$cov2[[j]])) sub$cov_2[j] = unlist(sub$cov2[[j]])
    if(!is.null(sub$cov3[[j]])) sub$cov_3[j] = unlist(sub$cov3[[j]])
  }
  sub =
    dplyr::filter(sub, !is.na(cov_1+cov_2+cov_3), !is.infinite(cov_1+cov_2+cov_3)) %>%
    dplyr::mutate(cov_1 = (cov_1-mean(cov_1))/sd(cov_1),
                  cov_2 = (cov_2-mean(cov_2))/sd(cov_2),
                  cov_3 = (cov_3-mean(cov_3))/sd(cov_3))
  # ggplot(sub, aes(cov,z)) + geom_point() + facet_wrap(~species)
  sub$est <- NA
  sub$log_dens <- NA
  for(yr in (max(sub$time)-n_forecast):max(sub$time)) {
    fit <- lm(z ~ -1 + cov_1:species + cov_2:species, data=sub[which(sub$time<yr),])
    sub$est[which(sub$time==yr)] <- predict(fit, newdata = sub[which(sub$time==yr),])
    se = predict(fit, newdata = sub[which(sub$time==yr),], se.fit=TRUE)$se.fit
    sub$log_dens[which(sub$time==yr)] = dnorm(sub$z[which(sub$time==yr)],
                                              mean = sub$est[which(sub$time==yr)],
                                              sd = se, log=TRUE)
  }
  fit_full <- lm(z ~ -1 + cov_1:species + cov_2:species, data=sub)
  spp_combo$aic[i] = AIC(fit_full)

  # calculate R2 by species
  r2 = dplyr::group_by(dplyr::filter(sub,!is.na(est)), species) %>%
    dplyr::summarise(r2 = round(cor(est,z)^2, 3))

  spp_combo$mean_r2[i] = mean(r2$r2)
  spp_combo$rmse[i] = sqrt(mean((sub$est - sub$z)^2,na.rm=T))
  spp_combo$log_dens[i] = sum(sub$log_dens,na.rm=T)

  if(spp_combo$rmse[i] < best_rmse) {
    best_rmse = spp_combo$rmse[i]
    best_dat = sub
  }
  #results_table = left_join(results_table, r2)
  #colnames(results_table)[ncol(results_table)] = unique(calcofi$name)[i]
}
results_enviro_3spp = list(all_results = spp_combo,
                            best_dat = best_dat)
saveRDS(results_enviro_3spp, "fitted_models/results_enviro_3spp.rds")

# regression on calcofi DFA
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

dat <- (calcofi)
dat$year <- dat$time
dat$time <- dat$time - min(dat$time) + 1
dat$ts <- as.numeric(as.factor(dat$name))
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
dfas = list()
library(bayesdfa)
for(yr in (max(dat$year)-n_forecast):max(dat$year)) {
  # fit dfa to data up to these years - 1
  dfas[[yr]] <- fit_dfa(dat[which(dat$year < yr),], num_trends = 2,
                      trend_model = "rw",
                      data_shape="long",
                      chains = 1,
                      iter=4000)
}
# do predictions
for(yr in (max(dat$year)-n_forecast):max(dat$year)) {
  trends = dfa_trends(rotate_trends(dfas[[yr]]))
  trends_1 = trends[which(trends$trend_number=="Trend 1"),]
  trends_2 = trends[which(trends$trend_number=="Trend 2"),]
  trend_df = data.frame(time=trends_1$time, "cov1" = trends_1$estimate,
                        "cov2"=trends_2$estimate)

  sub = as.data.frame(rec_devs)

  sub$year = sub$time
  sub$time = sub$time - min(sub$time) + 1

  sub = dplyr::left_join(sub, trend_df)
  fit <- lm(z ~ -1 + cov1:species + cov2:species, data=sub[which(sub$year<yr),])

  # update dfa model to include this year
  trends = dfa_trends(rotate_trends(dfas[[yr+1]]))
  trends_1 = trends[which(trends$trend_number=="Trend 1"),]
  trends_2 = trends[which(trends$trend_number=="Trend 2"),]
  trend_df = data.frame(time=trends_1$time, "cov1" = trends_1$estimate,
                        "cov2"=trends_2$estimate)

  sub[which(sub$year==yr),c("cov1")] = trend_df$cov1[nrow(trend_df)]
  sub[which(sub$year==yr),c("cov2")] = trend_df$cov2[nrow(trend_df)]
  sub$est = NA
  sub$est[which(sub$year==yr)] <- predict(fit, newdata = sub[which(sub$year==yr),])
  if(yr==2004) {
    out <- sub[which(sub$year==yr),]
  } else {
    out <- rbind(out, sub[which(sub$year==yr),])
  }
}

# DFA: 1.078794

# Do regressions on ROMS variables
enviro <- read.csv("data/roms.july_june.mean.csv")
enviro = dplyr::rename(enviro, time = yr)
enviro = tidyr::pivot_longer(enviro,cols=2:ncol(enviro)) %>%
  dplyr::rename(cov = value)
best_r2 = 0
for(i in 1:length(unique(enviro$name))) {
  sub = dplyr::left_join(rec_devs, dplyr::filter(enviro,name==unique(enviro$name)[i])) %>%
    dplyr::filter(!is.na(cov)) %>%
    dplyr::mutate(cov = (cov-mean(cov))/sd(cov))
  # ggplot(sub, aes(cov,z)) + geom_point() + facet_wrap(~species)
  sub$est <- NA
  for(yr in unique(sub$time)) {
    fit <- lm(z ~ -1 + cov:species, data=sub[which(sub$time!=yr),])
    sub$est[which(sub$time==yr)] <- predict(fit, newdata = sub[which(sub$time==yr),])
  }
  r2 = dplyr::group_by(sub, species) %>%
    dplyr::summarise(r2 = round(cor(est,z)^2, 3))
  results_table = left_join(results_table, r2)
  colnames(results_table)[ncol(results_table)] = unique(enviro$name)[i]
}


# Do regressions on CalCOFI variables
calcofi <- readRDS("data/predicted_indices.rds")
calcofi = dplyr::rename(calcofi, time = year, name=species,cov=index)
calcofi <- dplyr::group_by(calcofi, name) %>%
  dplyr::mutate(nz = length(which(n_pos_cpue!=0))) %>%
  dplyr::filter(nz >= 20) %>%
  dplyr::select(-nz)
best_r2 = 0
for(i in 1:length(unique(calcofi$name))) {
  sub = dplyr::left_join(rec_devs, dplyr::filter(calcofi,name==unique(calcofi$name)[i])) %>%
    dplyr::filter(!is.na(cov), !is.infinite(cov)) %>%
    dplyr::mutate(cov = (cov-mean(cov))/sd(cov))
  # ggplot(sub, aes(cov,z)) + geom_point() + facet_wrap(~species)
  sub$est <- NA
  for(yr in unique(sub$time)) {
    fit <- lm(z ~ -1 + cov:species, data=sub[which(sub$time!=yr),])
    sub$est[which(sub$time==yr)] <- predict(fit, newdata = sub[which(sub$time==yr),])
  }
  r2 = dplyr::group_by(sub, species) %>%
    dplyr::summarise(r2 = round(cor(est,z)^2, 3))
  results_table = left_join(results_table, r2)
  colnames(results_table)[ncol(results_table)] = unique(calcofi$name)[i]
}

# Do regressions on RREAS variables
rreas <- readRDS("data/predicted_indices rreas.rds")
rreas = dplyr::rename(rreas, time = year, name=species,cov=index)
best_r2 = 0
for(i in 1:length(unique(rreas$name))) {
  sub = dplyr::left_join(rec_devs, dplyr::filter(rreas,name==unique(rreas$name)[i])) %>%
    dplyr::filter(!is.na(cov), !is.infinite(cov)) %>%
    dplyr::mutate(cov = (cov-mean(cov))/sd(cov))
  # ggplot(sub, aes(cov,z)) + geom_point() + facet_wrap(~species)
  sub$est <- NA
  for(yr in unique(sub$time)) {
    fit <- lm(z ~ -1 + cov:species, data=sub[which(sub$time!=yr),])
    sub$est[which(sub$time==yr)] <- predict(fit, newdata = sub[which(sub$time==yr),])
  }
  r2 = dplyr::group_by(sub, species) %>%
    dplyr::summarise(r2 = round(cor(est,z)^2, 3))

  results_table = left_join(results_table, r2)
  colnames(results_table)[ncol(results_table)] = unique(rreas$name)[i]
}

# Do regressions on dfa-indices
mod_names <- c("roms","rreas","calcofi","calcofi-2")

for(i in 1:length(mod_names)) {

if(i<4) {
  mod = readRDS(paste0("fitted_models/", mod_names[i],".rds"))
  trends = dfa_trends(rotate_trends(mod[[1]])) %>% dplyr::filter(trend_number=="Trend 1")
  trends$time = sort(unique(mod[[1]]$orig_data$year))
} else {
  mod = readRDS(paste0("fitted_models/", mod_names[3],".rds"))
  trends = dfa_trends(rotate_trends(mod[[2]]))
  tdf = dplyr::filter(trends, trend_number=="Trend 1")
  tdf$estimate2 = dplyr::filter(trends, trend_number=="Trend 2")$estimate
  trends = tdf
  #trends = dfa_trends(rotate_trends(mod[[2]])) %>% dplyr::filter(trend_number=="Trend 1")
  trends$time = sort(unique(mod[[2]]$orig_data$year))
}
sub = dplyr::left_join(rec_devs, trends) %>%
  dplyr::filter(!is.na(estimate))
#fit <- lm(z ~ -1 + estimate:species, data=sub)
#sub$est = predict(fit)
sub$est <- NA
for(yr in unique(sub$time)) {
  fit <- lm(z ~ -1 + estimate:species, data=sub[which(sub$time!=yr),])
  sub$est[which(sub$time==yr)] <- predict(fit, newdata = sub[which(sub$time==yr),])
}
r2 = dplyr::group_by(sub, species) %>%
  dplyr::summarise(r2 = round(cor(est,z)^2, 3))
results_table = left_join(results_table, r2)

names(results_table)[which(names(results_table)=="r2")] = mod_names[i]
#results_table[,which(names(results_table)=="r2")]
#  dplyr::rename(calcofi_index_1trend = r2)
}





#
# # Do regressions on calcofi-index -- both trends
# mod = readRDS("calcofi_trend_2_sub.rds")
# trends = dfa_trends(rotate_trends(mod))
#
# tdf = dplyr::filter(trends, trend_number=="Trend 1")
# tdf$estimate2 = dplyr::filter(trends, trend_number=="Trend 2")$estimate
# trends = tdf
#
# trends$time = sort(unique(mod$orig_data$year))
# sub = dplyr::left_join(rec_devs, trends) %>%
#   dplyr::filter(!is.na(estimate))
# sub$est <- NA
# for(yr in unique(sub$time)) {
#   fit <- lm(z ~ -1 + estimate:species, data=sub[which(sub$time!=yr),])
#   sub$est[which(sub$time==yr)] <- predict(fit, newdata = sub[which(sub$time==yr),])
# }
# r2 = dplyr::group_by(sub, species) %>%
#   dplyr::summarise(r2 = round(cor(est,z)^2, 3))
# results_table = left_join(results_table, r2) %>%
#   dplyr::rename(calcofi_index_both = r2)
#
# # Do regressions on rreas-index
# mod = readRDS("rreas_trend_1.rds")
# trends = dfa_trends(rotate_trends(mod)) %>% dplyr::filter(trend_number=="Trend 1")
# trends$time = sort(unique(mod$orig_data$year))
# sub = dplyr::left_join(rec_devs, trends) %>%
#   dplyr::filter(!is.na(estimate))
#
# sub$est <- NA
# for(yr in unique(sub$time)) {
#   fit <- lm(z ~ -1 + estimate:species, data=sub[which(sub$time!=yr),])
#   sub$est[which(sub$time==yr)] <- predict(fit, newdata = sub[which(sub$time==yr),])
# }
# r2 = dplyr::group_by(sub, species) %>%
#   dplyr::summarise(r2 = round(cor(est,z)^2, 3))
# results_table = left_join(results_table, r2) %>%
#   dplyr::rename(rreas_index = r2)
# saveRDS(results_table,"regression_table.rds")
#
# # Do regressions on calcofi-index
# # mod = readRDS("calcofi_trend_sub.rds")
# # trends = dfa_trends(rotate_trends(mod)) %>% dplyr::filter(trend_number=="Trend 1")
# # trends$time = sort(unique(mod$orig_data$year))
# # sub = dplyr::left_join(rec_devs, trends) %>%
# #   dplyr::filter(!is.na(estimate))
# # fit <- lm(z ~ -1 + estimate:species, data=sub)
# # sub$est = predict(fit)
# # r2 = dplyr::group_by(sub, species) %>%
# #   dplyr::summarise(r2 = round(cor(est,z)^2, 3))
# # results_table = left_join(results_table, r2) %>%
# #   dplyr::rename(calcofi_sub_index = r2)
#
#
# # Do regressions on calcofi
# calcofi_spp = calcofi_table$name[grep("-cc",calcofi_table$name)]
# calcofi_indices = readRDS("data/predicted_indices.rds")
# for(i in 1:nrow(results_table)) {
#
#   for(j in 1:length(calcofi_spp)) {
#     sub = dplyr::filter(rec_devs, species == results_table$species[i]) %>%
#       dplyr::rename(year = time)
#     cov = dplyr::filter(calcofi_indices, paste0(species,"-cc") == calcofi_spp[j])
#     sub = dplyr::left_join(sub, cov,by="year")
#     sub$index[which(!is.finite(sub$index))] = 0
#     fit <- lm(dev ~ index, data=sub)
#     if(j==1) {
#       results_table$calcofi[i] = summary(fit)$r.squared
#     } else {
#       if(summary(fit)$r.squared > results_table$calcofi[i]) {
#         results_table$calcofi[i] = summary(fit)$r.squared
#       }
#     }
#   }
#
# }
#
#
