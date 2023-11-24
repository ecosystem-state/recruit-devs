library(dplyr)
library(bayesdfa)

quantile_cutoff <- 0.5
# extract assessments with high loadings on the ROMS-rec dev DFA
r = readRDS("fitted_models/roms_rec_dev.rds")
loadings <- dfa_loadings(rotate_trends(r[[1]]))
loadings$name <- levels(as.factor(r[[1]]$orig_data$name))
roms_table <- loadings[which(abs(loadings$median) >= quantile(abs(loadings$median),quantile_cutoff)),
                       c("name","median")]

# extract assessments with high loadings on the Calcofi-rec dev DFA
r = readRDS("fitted_models/calcofi_rec_dev_trend_2.rds")
loadings <- dfa_loadings(rotate_trends(r[[2]]))# %>%
  #dplyr::filter(trend=="Trend 1")
loadings$name <- levels(as.factor(r[[2]]$orig_data$name))
calcofi_table <- loadings[which(abs(loadings$median) >= quantile(abs(loadings$median),quantile_cutoff)),c("name","median")]

# extract assessments with high loadings on the Calcofi-rec dev DFA
r = readRDS("fitted_models/rreas_rec_dev_trend_1.rds")
loadings <- dfa_loadings(rotate_trends(r))
loadings$name <- levels(as.factor(r$orig_data$name))
rreas_table <- loadings[which(abs(loadings$median) >= quantile(abs(loadings$median),quantile_cutoff)),c("name","median")]

rf_roms = roms_table$name[-grep("beuti",roms_table$name)]
rf_cc = calcofi_table$name[-grep("-cc",calcofi_table$name)]
rf_rreas = rreas_table$name[-grep("-jv",rreas_table$name)]
rockfishes = unique(c(rf_roms, rf_cc, rf_rreas))

# Load response data
rec_devs = readRDS("data/clean_rec_devs.rds")
rec_devs = dplyr::filter(rec_devs, species %in% rockfishes) %>%
  dplyr::rename(time = Yr)
# Standardize
rec_devs = dplyr::group_by(rec_devs, species) %>%
  dplyr::mutate(z = (dev-mean(dev))/sd(dev)) %>%
  dplyr::filter(time<=2016)


# Do regressions on CalCOFI variables -- 2 spp
rreas <- readRDS("data/predicted_indices rreas.rds")
rreas = dplyr::rename(rreas, time = year, name=species,cov=index)

spp_combo = expand.grid("species_1" = unique(rreas$name),
                        "species_2" = unique(rreas$name))
spp_combo = dplyr::filter(spp_combo, species_1 != species_2)
spp_combo$mean_r2 = 0
spp_combo$rmse = 0
spp_combo$aic = 0
spp_combo$log_dens = 0
# first pivot wider
rreas = tidyr::pivot_wider(rreas[,1:3], names_from="name",values_from = "cov")

for(i in 1:nrow(spp_combo)) {
  tmp = rreas[,c(1,which(names(rreas) %in% c(spp_combo[i,1],spp_combo[i,2])))]
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
  for(yr in (max(sub$time)-10):max(sub$time)) {
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
  #results_table = left_join(results_table, r2)
  #colnames(results_table)[ncol(results_table)] = unique(calcofi$name)[i]
}

rreas <- readRDS("data/predicted_indices rreas.rds")
rreas = dplyr::rename(rreas, time = year, name=species,cov=index)

spp_combo = expand.grid("species_1" = unique(rreas$name),
                        "species_2" = unique(rreas$name))
spp_combo = dplyr::filter(spp_combo, species_1 != species_2)
spp_combo$mean_r2 = 0
spp_combo$rmse = 0
spp_combo$aic = 0
spp_combo$log_dens = 0
# first pivot wider

spp_combo = expand.grid("species_1" = unique(rreas$name),
                        "species_2" = unique(rreas$name),
                        "species_3" = unique(rreas$name))
spp_combo = dplyr::filter(spp_combo, species_1 != species_2,
                          species_1 != species_3,
                          species_2 != species_3)

spp_combo$mean_r2 = 0
spp_combo$aic = 0
spp_combo$rmse = 0
spp_combo$log_dens = 0
# first pivot wider
rreas = tidyr::pivot_wider(rreas[,1:3], names_from="name",values_from = "cov")

for(i in 1:nrow(spp_combo)) {
  tmp = rreas[,c(1,which(names(rreas) %in% c(spp_combo[i,1],spp_combo[i,2],spp_combo[i,3])))]
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
  for(yr in (max(sub$time)-10):max(sub$time)) {
    reg_dat = sub[which(sub$time<yr),]
    if(nrow(reg_dat)>0){
    fit <- lm(z ~ -1 + cov_1:species + cov_2:species, data=sub[which(sub$time<yr),])
    sub$est[which(sub$time==yr)] <- predict(fit, newdata = sub[which(sub$time==yr),])
    se = predict(fit, newdata = sub[which(sub$time==yr),], se.fit=TRUE)$se.fit
    sub$log_dens[which(sub$time==yr)] = dnorm(sub$z[which(sub$time==yr)],
                                              mean = sub$est[which(sub$time==yr)],
                                              sd = se, log=TRUE)
    } else {
      sub$est[which(sub$time==yr)] = 1.0e10
      sub$log_dens[which(sub$time==yr)] = -1.0e10
    }

  }
  fit_full <- lm(z ~ -1 + cov_1:species + cov_2:species, data=sub)
  spp_combo$aic[i] = AIC(fit_full)

  # calculate R2 by species
  r2 = dplyr::group_by(dplyr::filter(sub,!is.na(est)), species) %>%
    dplyr::summarise(r2 = round(cor(est,z)^2, 3))

  spp_combo$mean_r2[i] = mean(r2$r2)
  spp_combo$rmse[i] = sqrt(mean((sub$est - sub$z)^2,na.rm=T))
  spp_combo$log_dens[i] = sum(sub$log_dens,na.rm=T)
  #results_table = left_join(results_table, r2)
  #colnames(results_table)[ncol(results_table)] = unique(calcofi$name)[i]
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
