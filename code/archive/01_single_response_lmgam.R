library(dplyr)
library(tidyr)
library(mgcv)
library(recdata)

response = "hake"
predictors = "calcofi"
model = c("lm","gam")[2]
n_forecast = 15 # years to forecast for predictive modeling below

source("code/utils.r")

# Load response data
if(response=="hake") {
data("recruit_dev")
rec_devs = dplyr::rename(recruit_dev, time = Yr)
rec_devs$z = scale(rec_devs$dev)
}

if(predictors=="calcofi") {

data("calcofi")
calcofi = dplyr::rename(calcofi, time = year, name=species,cov=est) %>%
  dplyr::distinct()
calcofi <- dplyr::group_by(calcofi, name) %>%
  dplyr::mutate(nz = length(which(n_pos_cpue!=0))) %>%
  dplyr::filter(nz >= 20) %>%
  dplyr::select(-nz) %>%
  dplyr::filter(time <= max(rec_devs$time))

# standardize
calcofi = dplyr::group_by(calcofi, name) %>%
  dplyr::mutate(est_z = (log_est - mean(log_est))/sd(log_est))

dat = calcofi
}


# first pivot wider
dat = tidyr::pivot_wider(dat[,c("name","time","cov")],
                             names_from="name",
                             values_from = "cov")

spp_combo = create_df_predictors(names = names(dat)[-1], n_vars = 3)

best_rmse <- 100
for(i in 1:nrow(spp_combo)) {
  # keep time and
  tmp = dat[,c(1,which(names(dat) %in% c(spp_combo[i,])))]
  sub = dplyr::left_join(as.data.frame(rec_devs[,c("time","z")]), tmp, by="time")
  names(sub)[3:ncol(sub)] =stringr::str_replace_all(names(sub)[3:ncol(sub)], " ", "_")
  covar_names = names(sub)[3:ncol(sub)]

  if(model=="lm") {
  f <- as.formula(paste("z",
                        paste(c("-1",covar_names), collapse = " + "),
                        sep = " ~ "))
  }
  if(model=="gam") {
    covar_names = paste0("s(",covar_names,",k=4,bs='ps')")
    f <- as.formula(paste("z",
                          paste(c("-1",covar_names), collapse = " + "),
                          sep = " ~ "))
  }
  sub$est <- NA
  sub$log_dens <- NA
  for(yr in (max(sub$time)-n_forecast):max(sub$time)) {

    if(model=="lm") fit <- lm(f, data=sub[which(sub$time<yr),])
    if(model=="gam") fit <- gam(f, data=sub[which(sub$time<yr),])

    #fit <- lm(z ~ -1 + cov_1:species, data=sub[which(sub$time<yr),])
    # note -- some of these will be negatively correlated. Sign isn't important and a
    # all coefficient signs can be flipped
    sub$est[which(sub$time==yr)] <- predict(fit, newdata = sub[which(sub$time==yr),])
    se = predict(fit, newdata = sub[which(sub$time==yr),], se.fit=TRUE)$se.fit
    sub$log_dens[which(sub$time==yr)] = dnorm(sub$z[which(sub$time==yr)],
                                              mean = sub$est[which(sub$time==yr)],
                                              sd = se, log=TRUE)
  }

  if(model=="lm") fit_full <- lm(f, data=sub)
  if(model=="gam") fit_full <- gam(f, data=sub)
  spp_combo$aic[i] = AIC(fit_full)

  # calculate R2
  r2 = dplyr::filter(sub, !is.na(est)) %>%
    dplyr::summarize(r2 = cor(est,z)^2)

  spp_combo$mean_r2[i] = mean(r2$r2)
  spp_combo$rmse[i] = sqrt(mean((sub$est - sub$z)^2,na.rm=T))
  if(spp_combo$rmse[i] < best_rmse) {
    best_rmse = spp_combo$rmse[i]
    best_dat = sub
  }
  spp_combo$log_dens[i] = sum(sub$log_dens,na.rm=T)
}

saveRDS(spp_combo, paste0("fitted_models/",model,"-",response,"-",predictors,".rds"))

