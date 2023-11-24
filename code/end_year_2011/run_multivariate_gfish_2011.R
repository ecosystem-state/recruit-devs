library(dplyr)
library(recdata)
library(predRecruit)
library(tidyr)

data("recruit_dev")
data("assessment_metadata")
max_year = 2011

spp_names = unique(recruit_dev$short_name)
spp_names = spp_names[which(spp_names != "Shortbelly_rockfish")]
assessment_metadata = dplyr::filter(assessment_metadata,
                                    short_name != "Shortbelly_rockfish")
df = expand.grid(response = "multi",
                 predictors = c("calcofi", "rreas", "roms","calfofi_trends","roms_trends","rreas_trends"),
                 model = c("lm","gam","glmm"),
                 n_forecast = 10,
                 years_ahead = 1)
df_ml = expand.grid(response = "multi",
                 predictors = c("calcofi", "rreas", "roms"),
                 model = c("randomForest","glmnet"),
                 n_forecast = df$n_forecast[1],
                 years_ahead = df$years_ahead[1])
df = rbind(df, df_ml)
#df = df[which(df$predictors %in% c("calcofi","calfofi_trends")),]
if(!dir.exists(paste0("output/","multivariate"))) {
  dir.create(paste0("output/","multivariate"))
}

for(i in 1:nrow(df)) {
  # Load response data and standardize rec devs
  # if(df$response[i]=="hake") {
  #   data("recruit_dev_hake")
  #   rec_devs = recruit_dev_hake      # Standardize
  #   rec_devs$dev = scale(rec_devs$Value)
  # } else {
  rec_devs = recruit_dev
  rec_devs = left_join(rec_devs, assessment_metadata[,c("start_main","end_main","short_name")], by="short_name") %>%
    dplyr::filter(Yr >= start_main, Yr <= end_main) %>%
    dplyr::filter(Yr <= max_year) %>%
    ungroup() %>%
    dplyr::rename(time = Yr) %>%
    dplyr::group_by(species) %>%
    dplyr::mutate(z = c(dev - mean(dev,na.rm=T))/sd(dev,na.rm=T)) %>%
    dplyr::select(time,z,species) %>%
    dplyr::rename(dev = z)
  # }

  if(df$predictors[i]=="calcofi") {
    data("calcofi")

    calcofi = dplyr::rename(calcofi, time = year, name=species,cov=log_est) %>%
      dplyr::distinct()
    calcofi$cov[which(is.infinite(calcofi$cov))] = NA
    calcofi <- dplyr::group_by(calcofi, name) %>%
      dplyr::mutate(nz = length(which(n_pos_cpue!=0))) %>%
      dplyr::filter(nz >= 20) %>%
      dplyr::select(-nz) %>%
      dplyr::filter(time <= max(rec_devs$time))
    # standardize
    calcofi = dplyr::group_by(calcofi, name) %>%
      #dplyr::mutate(est_z = (log_est - mean(log_est))/sd(log_est)) %>%
      dplyr::mutate(cov_z = (cov - mean(cov,na.rm=T))/sd(cov,na.rm=T)) %>%
      dplyr::select(-cov) %>%
      dplyr::rename(cov = cov_z)
    dat = calcofi
    max_vars = 3
    # pivot wider to create matrix (years on rows)
    dat = tidyr::pivot_wider(dat[,c("name","time","cov")],
                             names_from="name",
                             values_from = "cov")

    if(df$model[i] %in% c("randomForest", "glmnet")) {
      # remove NAs for random forest
      dat = dat[which(dat$time > 1987),]
      nas = 0
      for(col in 1:ncol(dat)) {
        nas[col] = length(which(is.na(dat[,col])))
      }
      if(length(which(nas>0)) > 0) dat = dat[,-which(nas>0)]
    }
  }
  if(df$predictors[i]=="rreas") {
    data("rreas")
    rreas = dplyr::rename(rreas, time = year, name=species,cov=index)
    dat = rreas
    max_vars = 3
    # standardize
    dat = dplyr::group_by(dat, name) %>%
      dplyr::mutate(z = (cov - mean(cov,na.rm=T)) / sd(cov,na.rm=T)) %>%
      dplyr::select(-cov) %>%
      dplyr::rename(cov = z)

    # catch species that have lots of missing data
    year_thresh <- 29
    dat <- dplyr::group_by(dat, name) %>%
      dplyr::mutate(n_years = length(unique(time))) %>%
      dplyr::filter(n_years >= year_thresh) %>%
      dplyr::select(-n_years)

    # pivot wider to create matrix (years on rows)
    dat = tidyr::pivot_wider(dat[,c("name","time","cov")],
                             names_from="name",
                             values_from = "cov") %>%
      dplyr::arrange(time) %>% as.data.frame()
  }
  if(df$predictors[i]=="roms") {
    data("roms")
    roms = dplyr::rename(roms, time = yr) %>%
      dplyr::filter(time <= max(rec_devs$time))
    dat = pivot_longer(roms, cols = 2:ncol(roms)) %>%
      dplyr::rename(cov=value)
    # standardize
    dat = dplyr::group_by(dat, name) %>%
      dplyr::mutate(z = (cov - mean(cov,na.rm=T)) / sd(cov,na.rm=T)) %>%
      dplyr::select(-cov) %>%
      dplyr::rename(cov = z)
    max_vars = 3
    # pivot wider to create matrix (years on rows)
    dat = tidyr::pivot_wider(dat[,c("name","time","cov")],
                             names_from="name",
                             values_from = "cov")
  }
  if(df$predictors[i]=="rreas_trends") {
    data("rreas_trends")
    rreas_trends = dplyr::rename(rreas_trends,
                                 name=trend_number,cov=estimate) %>%
      dplyr::filter(time <= max(rec_devs$time))
    dat = rreas_trends
    max_vars = 1
    # pivot wider to create matrix (years on rows)
    dat = tidyr::pivot_wider(dat[,c("name","time","cov")],
                             names_from="name",
                             values_from = "cov")
  }
  if(df$predictors[i]=="roms_trends") {
    data("roms_trends")
    roms_trends = dplyr::rename(roms_trends,
                                 name=trend_number,cov=estimate) %>%
      dplyr::filter(time <= max(rec_devs$time))
    dat = roms_trends
    max_vars = 1
    # pivot wider to create matrix (years on rows)
    dat = tidyr::pivot_wider(dat[,c("name","time","cov")],
                             names_from="name",
                             values_from = "cov")
  }
  if(df$predictors[i]=="calcofi_trends") {
    data("calcofi_trends")
    dat = dplyr::rename(calcofi_trends,
                                 name=trend_number,cov=estimate) %>%
      dplyr::filter(time <= max(rec_devs$time))
    max_vars = 3
    # pivot wider to create matrix (years on rows)
    dat = tidyr::pivot_wider(dat[,c("name","time","cov")],
                             names_from="name",
                             values_from = "cov")
  }

  if(df$model[i] %in% c("lm","gam")) {
    max_vars <- 2 # cuts down significantly on time / models
      forecast = multivariate_forecast(response = rec_devs,
                                   predictors = dat,
                                   model_type = df$model[i],
                                   n_forecast = df$n_forecast[i],
                                   n_years_ahead = df$years_ahead[i],
                                   max_vars = max_vars)
  }
  if(df$model[i] %in% c("randomForest", "glmnet")) {

    # also add quadratic effects
    dat2 <- dat[,-which(names(dat)=="time")]
    dat2 <- dat2 ^ 2
    names(dat2) = paste0(names(dat)[-which(names(dat)=="time")],"2")
    dat = cbind(dat, dat2)

    forecast = multivariate_forecast_ml(response = rec_devs,
                                   predictors = dat,
                                   model_type = df$model[i],
                                   n_forecast = df$n_forecast[i],
                                   n_years_ahead = df$years_ahead[i])
  }

  saveRDS(forecast, file = paste0("output/multivariate/",df$predictors[i],"_",df$model[i],"_",df$years_ahead[i],"step_2011.rds"))
}


