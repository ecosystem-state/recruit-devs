library(xgboost)
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
                                                   "Yellowtail_rockfish_South"))

# CalCOFI data
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
calcofi = dplyr::select(calcofi,-mean_cpue,-n_pos_cpue)

calcofi = pivot_wider(calcofi, names_from=name,values_from="obs")
rec_devs = dplyr::select(rec_devs, z,time, species)
rec_devs = dplyr::left_join(rec_devs, calcofi) %>% as.data.frame()

# now join in environmental data
enviro <- read.csv("data/roms.july_june.mean.csv")
enviro = dplyr::rename(enviro, time = yr)
enviro = enviro[,c(1,8:13)]
enviro = tidyr::pivot_longer(enviro,cols=2:ncol(enviro)) %>%
  dplyr::rename(cov = value) %>%
  dplyr::group_by(name) %>%
  dplyr::mutate(cov = (cov-mean(cov))/sd(cov))
enviro = pivot_wider(enviro, names_from=name,values_from="cov")
rec_devs = dplyr::left_join(rec_devs, enviro) %>% as.data.frame()

for(i in 4:ncol(rec_devs)) {
  rec_devs[which(is.infinite(rec_devs[,i])),i] = NA
  rec_devs[,i] = as.numeric(rec_devs[,i])
}

rec_devs[,3] = as.numeric(as.factor(rec_devs[,3]))
rec_devs = dplyr::filter(rec_devs,time <= 2015, time>= 1985)

rec_devs = as.matrix(rec_devs)

# why not random forest? NAs aren't permitted in predictors
hyper_grid <- expand.grid(max_depth = seq(2, 10, 1),
                          eta = seq(.1, .6, .01))

data = rec_devs
years = sort(unique(data[,2]))
fold_ids <- years
custom.folds <- vector("list", length(fold_ids))
i <- 1
for( id in fold_ids){
  custom.folds[[i]] <- which( data[,2] %in% id )
  i <- i+1
}

xgb_train_rmse <- NA#matrix(NA, nrow(hyper_grid), length(years))
xgb_test_rmse <- xgb_train_rmse
dtrain <- xgb.DMatrix(data[,-1], label = data[,1])

for (j in 1:nrow(hyper_grid)) {

  xgb_test_rmse[j] <- 1000
  for(iter in 1:25) {
    set.seed(iter)
    m_xgb_untuned <- xgb.cv(
      data = dtrain,
      #label = train[, 1],
      nrounds = 1000,
      folds = custom.folds,
      objective = "reg:squarederror",
      early_stopping_rounds = 20,
      metrics = list("rmse"),
      prediction=TRUE,
      nfold = length(unique(years)),
      max_depth = hyper_grid$max_depth[j],
      eta = hyper_grid$eta[j]
    )
    test_rmse <- m_xgb_untuned$evaluation_log$test_rmse_mean[m_xgb_untuned$best_iteration]
    if(test_rmse < xgb_test_rmse[j]) xgb_test_rmse[j] = test_rmse
    #xgb_test_rmse[j] <- m_xgb_untuned$evaluation_log$test_rmse_mean[m_xgb_untuned$best_iteration]
  }
  #xgb_train_rmse[j] <- m_xgb_untuned$evaluation_log$train_rmse_mean[m_xgb_untuned$best_iteration]
  cat(j, "\n")
}

results_xgboost_south = list(xgb_test_rmse = xgb_test_rmse,
                             hyper_grid = hyper_grid)
saveRDS(results_xgboost_south,"fitted_models/xgboost_south.rds")



xgb_train_rmse <- NA#matrix(NA, nrow(hyper_grid), length(years))
xgb_test_rmse <- xgb_train_rmse
for (j in 1:nrow(hyper_grid)) {

  xgb_test_rmse[j] <- 1000
  for(iter in 1:10) {
    pred = rep(NA, nrow(data))
    for(yr in (max(data[,2])-n_forecast):max(data[,2])) {
      fit <- xgboost(data = data[which(data[,2]<yr),-1], label = data[which(data[,2]<yr), 1], nrounds = 1000,
        objective = "reg:squarederror",
        early_stopping_rounds = 20,
        metrics = list("rmse"),
        max_depth = 4,
        eta =0.2
      )
      pred[which(data[,2]==yr)] <- predict(fit, newdata = data[which(data[,2]==yr),-1])
    }
    test_rmse <- sqrt(mean((pred - data[,1])^2,na.rm=T))
    if(test_rmse < xgb_test_rmse[j]) xgb_test_rmse[j] = test_rmse
    #xgb_test_rmse[j] <- m_xgb_untuned$evaluation_log$test_rmse_mean[m_xgb_untuned$best_iteration]
  }
  #xgb_train_rmse[j] <- m_xgb_untuned$evaluation_log$train_rmse_mean[m_xgb_untuned$best_iteration]
  cat(j, "\n")
}



hyper_grid$test = xgb_test_rmse
library(ggplot2)
ggplot(hyper_grid,aes(max_depth,eta,fill=test)) + geom_tile()
#devtools::install_github("AppliedDataSciencePartners/xgboostExplainer")
library(xgboostExplainer)

best <- which.min(hyper_grid$test)

param <- list(max_depth = hyper_grid$max_depth[best], eta = hyper_grid$eta[best],
              verbose = 0, nthread = 2,
              objective = "reg:squarederror", eval_metric = "rmse")
bst <- xgb.train(param, dtrain, nrounds = 2000)

explainer = buildExplainer(bst, trainingData=dtrain, type = "regression", base_score = 0.5,
               trees_idx = NULL)
