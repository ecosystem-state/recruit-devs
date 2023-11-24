library(dplyr)
library(tidyr)
#library(mgcv)
library(glmnet)
library(randomForest)
response = "hake"
predictors = "calcofi"
model = c("glmnet","randomForest")[2]
n_forecast = 15 # years to forecast for predictive modeling below

if(model=="glmnet") {
tuning = expand.grid(alpha=seq(0.1,0.9,by=0.05),
                     lambda = seq(0,2,by=0.01))
}
if(model=="randomForest") {
  tuning = expand.grid(ntree=seq(300,2000,by=100),
                       mtry = seq(2,10))
}

source("code/utils.r")

# Load response data
if(response=="hake") {
rec_devs = read.csv("data/HakeRecDevForCathleen_2020_medians.csv")
rec_devs = dplyr::rename(rec_devs, time = Year)
# Standardize
rec_devs$z = scale(rec_devs$Value)
}
if(predictors=="calcofi") {

calcofi <- readRDS("data/predicted_indices_sdmtmb.rds")
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

best_rmse <- 100
tuning$rmse = 0
tuning$mean_r2 = 0
for(i in 1:nrow(tuning)) {
  # keep time and
  sub = dplyr::left_join(as.data.frame(rec_devs[,c("time","z")]), dat, by="time")

  sub$est <- NA
  sub$log_dens <- NA
  for(yr in (max(sub$time)-n_forecast):max(sub$time)) {
    x = dplyr::filter(sub, time < yr, time>= 1985) %>% dplyr::select(-z,-time,-est,-log_dens)
    y = dplyr::filter(sub, time < yr, time>= 1985) %>% dplyr::select(z)
    if(model=="glmnet") fit <- glmnet(x=x, y = unlist(y), lambda = tuning$lambda[i], alpha=tuning$alpha[i])
    if(model=="randomForest") fit <- randomForest(x = x, y = unlist(y), ntree = tuning$ntree[i], mtry = tuning$mtry[i])
    #fit <- lm(z ~ -1 + cov_1:species, data=sub[which(sub$time<yr),])
    # note -- some of these will be negatively correlated. Sign isn't important and a
    # all coefficient signs can be flipped

    newx = dplyr::filter(sub, time == yr) %>% dplyr::select(-z,-time,-est,-log_dens)
    if(model=="glmnet") sub$est[which(sub$time==yr)] <- predict(fit, newx=as.matrix(newx), s = tuning$lambda[i])
    if(model=="randomForest") sub$est[which(sub$time==yr)] <- predict(fit, as.matrix(newx))

    #se = predict(fit, newdata = sub[which(sub$time==yr),], se.fit=TRUE)$se.fit
    #sub$log_dens[which(sub$time==yr)] = dnorm(sub$z[which(sub$time==yr)],
    #                                          mean = sub$est[which(sub$time==yr)],
    #                                          sd = se, log=TRUE)
  }

  #if(model=="lm") fit_full <- lm(f, data=sub)
  #if(model=="gam") fit_full <- gam(f, data=sub)
  #spp_combo$aic[i] = AIC(fit_full)

  # calculate R2
  r2 = dplyr::filter(sub, !is.na(est)) %>%
    dplyr::summarize(r2 = cor(est,z)^2)

  tuning$mean_r2[i] = mean(r2$r2)
  tuning$rmse[i] = sqrt(mean((sub$est - sub$z)^2,na.rm=T))
  if(tuning$rmse[i] < best_rmse) {
    best_rmse = tuning$rmse[i]
    best_dat = sub
  }
  #spp_combo$log_dens[i] = sum(sub$log_dens,na.rm=T)
}

saveRDS(tuning, paste0("fitted_models/",model,"-",response,"-",predictors,".rds"))

