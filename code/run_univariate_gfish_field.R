library(recdata)
library(predRecruit)
library(dplyr)
library(tidyr)

data("recruit_dev")
data("assessment_metadata")
max_year = 2015

spp_names = unique(recruit_dev$short_name)
# Drop shortbelly -- details are sparse via JF
spp_names = spp_names[which(spp_names != "Shortbelly_rockfish")]
assessment_metadata = dplyr::filter(assessment_metadata,
                                    short_name != "Shortbelly_rockfish")

for(spp in 1:length(spp_names)) {

if(!dir.exists(paste0("output/",spp_names[spp]))) {
  dir.create(paste0("output/",spp_names[spp]))
}

df = expand.grid(response = spp_names[spp],
                 predictors = c("rreas"),
                 model = c("lm","gam"),
                 n_forecast = 10,
                 years_ahead = 0:2)
df_ml = expand.grid(response = spp_names[spp],
                 predictors = c("rreas"),
                 model = c("randomForest","glmnet"),
                 n_forecast = unique(df$n_forecast),
                 years_ahead = unique(df$years_ahead))
df = rbind(df, df_ml)
#df = df[which(df$predictors %in% c("calcofi","calfofi_trends")),]

for(i in 1:nrow(df)) {

  # Load response data and standardize rec devs
  if(df$response[i]=="hake") {
    data("recruit_dev_hake")
    rec_devs = recruit_dev_hake      # Standardize
    rec_devs$dev = scale(rec_devs$Value)
  } else {
    rec_devs = recruit_dev
    rec_devs = left_join(rec_devs, assessment_metadata, by="short_name") %>%
      dplyr::filter(Yr >= start_main, Yr <= end_main) %>%
      dplyr::filter(short_name == spp_names[spp],
                             Yr <= max_year) %>%
      ungroup() %>%
      dplyr::rename(time = Yr) %>%
      dplyr::mutate(z = c(dev - mean(dev,na.rm=T))/sd(dev,na.rm=T)) %>%
      dplyr::select(time,z) %>%
      dplyr::rename(dev = z)
  }

  if(df$predictors[i]=="rreas") {
    # rreas = dplyr::rename(rreas, time = year, name=species,cov=index)
    field <- readRDS("data/predicted_indices_field.rds")
    rreas <- dplyr::rename(field, name = species, time = year, cov = index)
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

    # remove spp with no data
    dat <- dplyr::select(dat, -yoy_rockfish, -yoy_shortbelly)
  }

  if(df$model[i] %in% c("lm","gam")) {
      forecast = predRecruit::univariate_forecast(response = rec_devs,
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

    forecast = predRecruit::univariate_forecast_ml(response = rec_devs,
                                   predictors = dat,
                                   model_type = df$model[i],
                                   n_forecast = df$n_forecast[i],
                                   n_years_ahead = df$years_ahead[i],
                                   control = list(alpha=seq(0.1,1.0,by=0.1),
                                                  lambda = seq(2,0,by=-0.05),
                                                  ntree=seq(300,2000,by=100),
                                                  mtry = seq(2,10)))
  }

  saveRDS(forecast, file = paste0("output/",spp_names[spp],"/",df$predictors[i],"_",df$model[i],"_",df$years_ahead[i],"step_field.rds"))
}

}
