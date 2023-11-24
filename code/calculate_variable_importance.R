library(dplyr)
# this is being applied to the regression and GAM models.
# for each predictor covariate, we can ask what the avg % improvement is when it is included in the model

# load in summaries for models
out = readRDS("output/summaries/parameteric_univariate.rds")
out = dplyr::filter(out, years_ahead == 1)

# add number of predictors
out$n_vars <- 3
out$n_vars[which(is.na(out$name3))] = 2
out$n_vars[which(is.na(out$name2))] = 1

df = expand.grid(spp = unique(out$spp),
                 model = unique(out$model),
                 cov = unique(out$name1))
d <- dplyr::group_by(out, name1) %>%
  dplyr::summarise(predictors = predictors[1]) %>%
  dplyr::rename(cov = name1)

df <- dplyr::left_join(df, d)

df$rmse_improve <- NA

for(i in 1:nrow(df)) {
  # for each row of the dataframe, subset original data to those with
  # the same model / species being predicted / 3 variables used
  sub_3vars_incl <- dplyr::filter(out, n_vars == 3,
                             spp == df$spp[i],
                             model == df$model[i],
                             predictors == df$predictors[i])
  # and further filter to those models definitely including the predictor in 'cov'
  sub_3vars_incl <- dplyr::mutate(sub_3vars_incl,
                                  match1 = ifelse(name1==df$cov[i], 1, 0),
                                  match2 = ifelse(name2==df$cov[i], 1, 0),
                                  match3 = ifelse(name3==df$cov[i], 1, 0),
                                  all_matches = match1 + match2 + match3) %>%
    dplyr::filter(all_matches == 1) %>%
    dplyr::select(-match1, -match2, -match3, -all_matches)

  # re-order the time series names in name1/name2/name3 so 'cov' is last
  for(ii in 1:nrow(sub_3vars_incl)) {
    spp <- sub_3vars_incl[ii,c("name1","name2","name3")]
    spp_no_cov <- sort(as.character(spp[which(spp != df$cov[i])]))
    sub_3vars_incl[ii,c("name1")] <- spp_no_cov[1]
    sub_3vars_incl[ii,c("name2")] <- spp_no_cov[2]
    sub_3vars_incl[ii,c("name3")] <- df$cov[i]
  }

  # Next pull out the 2-covariate models with out the 'cov' predictor included
  sub_2vars_incl <- dplyr::filter(out, n_vars == 2,
                                  spp == df$spp[i],
                                  model == df$model[i],
                                  predictors == df$predictors[i])
  sub_2vars_incl <- dplyr::mutate(sub_2vars_incl,
                                  match1 = ifelse(name1==df$cov[i], 1, 0),
                                  match2 = ifelse(name2==df$cov[i], 1, 0),
                                  all_matches = match1 + match2) %>%
    dplyr::filter(all_matches == 0) %>%
    dplyr::select(-match1, -match2, -all_matches)
  # re-order the time series names in name1/name2
  for(ii in 1:nrow(sub_2vars_incl)) {
    spp <- sub_2vars_incl[ii,c("name1","name2")]
    sub_2vars_incl[ii,c("name1")] <- sort(as.character(spp))[1]
    sub_2vars_incl[ii,c("name2")] <- sort(as.character(spp))[2]
  }
  sub_2vars_incl <- dplyr::rename(sub_2vars_incl, rmse2 = rmse)
  sub_2vars_incl$model_id <- seq(1, nrow(sub_2vars_incl))
  # for each model in the dataframe with all three models, we need to match up the
  # corresponding 2-parameter model without the 'cov' predictor included
  sub_3vars_incl <- dplyr::left_join(sub_3vars_incl,
                                     sub_2vars_incl[,c("name1","name2","rmse2", "model_id")])

  # what's the percent improvement in rmse, e.g. 0.33 = 33% improvement  -- e.g. (1500 - 1000) / 1500
  sub_3vars_incl$pct_rmse <- (sub_3vars_incl$rmse2 - sub_3vars_incl$rmse) / sub_3vars_incl$rmse2
  # raw_mean <- mean(sub_3vars_incl$pct_rmse)

  # group by model id, calculate average rmse improvement for each id, then average across ids
  # mean of means is the same here -- so order of operations doesn't really matter, raw mean above works too
  mean_rmse <- dplyr::group_by(sub_3vars_incl, model_id) %>%
    dplyr::summarize(mean_rmse = mean(pct_rmse))
  df$rmse_improve[i] <- mean(mean_rmse$mean_rmse)
}

# all kidns of ways to sort this, e.g. by biggest to smallest rmse improvement by model and species?
df <- dplyr::filter(df, !is.na(df$rmse_improve))

saveRDS(df, "output/model_importance.rds")
