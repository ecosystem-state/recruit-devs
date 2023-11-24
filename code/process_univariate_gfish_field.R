library(recdata)
library(predRecruit)
library(dplyr)
library(tidyr)

data("recruit_dev")
data("assessment_metadata")
max_year = 2015

spp_names = unique(recruit_dev$short_name)
spp_names = spp_names[which(spp_names != "Shortbelly_rockfish")]
assessment_metadata = dplyr::filter(assessment_metadata,
                                    short_name != "Shortbelly_rockfish")

df_par = expand.grid(response = spp_names,
                 predictors = c("rreas"),
                 model = c("lm","gam"),
                 n_forecast = 10,
                 years_ahead = 0:2)
df_ml = expand.grid(response = spp_names,
                 predictors = c("rreas"),
                 model = c("randomForest","glmnet"),
                 n_forecast = unique(df_par$n_forecast),
                 years_ahead = unique(df_par$years_ahead))
df = rbind(df_par, df_ml)
#df = df[which(df$predictors %in% c("calcofi","calfofi_trends")),]

obs = dplyr::rename(ungroup(recruit_dev),
                    response = short_name,
                    year = Yr) %>%
  dplyr::select(year, dev, response)

df = df_par

for(i in 1:nrow(df)) {

  d = readRDS(file = paste0("output/",df$response[i],"/",df$predictors[i],"_",df$model[i],"_",df$years_ahead[i],"step_field.rds"))
  idx = which(names(d$vars) != "id")
  names(d$vars)[idx] = paste0("name",seq(1,length(idx)))
  d = dplyr::left_join(d$pred, d$vars)
  d$years_ahead = df$years_ahead[i]
  d$model = df$model[i]
  d$spp = df$response[i]
  d$predictors = df$predictors[i]
  d = dplyr::filter(d, !is.na(est))

  # join in observations
  d = dplyr::left_join(d, obs)

  if(is.null(d$name2)) d$name2 <- NA
  if(is.null(d$name3)) d$name3 <- NA

  # bin devs and estimates into terciles
  d$dev_bin <- "mid"
  d$dev_bin[which(d$dev < -0.4308)] = "lo"
  d$dev_bin[which(d$dev > 0.4308)] = "hi"
  d$est_bin <- "mid"
  d$est_bin[which(d$est < -0.4308)] = "lo"
  d$est_bin[which(d$est > 0.4308)] = "hi"

  # calculate RMSE and R2 for each model fit
  summaries = dplyr::group_by(d, id) %>%
    dplyr::summarise(n = n(),
                     rmse = sqrt(mean((dev - est)^2)),
                     r2 = cor(dev,est)^2,
                     model = model[1],
                     predictors = predictors[1],
                     years_ahead = years_ahead[1],
                     name1 = name1[1],
                     name2 = name2[1],
                     name3 = name3[1],
                     spp = spp[1],
                     tot_count = sum(table(dev_bin,est_bin)),
                     hit_rate = sum(diag(table(dev_bin,est_bin))) / tot_count,
                     false_rate = 1 - hit_rate,
                     false_rate_lo = length(which(dev_bin=="lo" & est_bin!="lo")) / length(which(dev_bin=="lo")),
                     false_rate_hi = length(which(dev_bin=="hi" & est_bin!="hi")) / length(which(dev_bin=="hi")),
                     tss = hit_rate - false_rate) %>%
    dplyr::filter(n >= 7) %>% ungroup()

  if("cov2" %in% names(d) == FALSE) d$cov2 <- NA
  if("cov3" %in% names(d) == FALSE) d$cov3 <- NA

  if(i == 1) {
    out = summaries
    out_all = d
  } else {
    out = rbind(out, summaries)
    out_all = rbind(out_all, d)
  }
}
saveRDS(out, "output/summaries/parameteric_univariate_field.rds")

df = df_ml
for(i in 1:nrow(df)) {

  d = readRDS(file = paste0("output/",df$response[i],"/",df$predictors[i],"_",df$model[i],"_",df$years_ahead[i],"step_field.rds"))
  idx = which(names(d$vars) != "id")
  #names(d$vars)[idx] = paste0("name",seq(1,length(idx)))
  d = dplyr::left_join(d$pred, d$tuning)
  d$years_ahead = df$years_ahead[i]
  d$model = df$model[i]
  d$spp = df$response[i]
  d$predictors = df$predictors[i]
  d = dplyr::filter(d, !is.na(est)) %>%
    dplyr::rename(year=time)

  # bin devs and estimates into terciles
  d$dev_bin <- "mid"
  d$dev_bin[which(d$dev < -0.4308)] = "lo"
  d$dev_bin[which(d$dev > 0.4308)] = "hi"
  d$est_bin <- "mid"
  d$est_bin[which(d$est < -0.4308)] = "lo"
  d$est_bin[which(d$est > 0.4308)] = "hi"

  # calculate RMSE and R2 for each model fit
  if(d$model[i]=="randomForest") {
  summaries = dplyr::group_by(d, id) %>%
    dplyr::summarise(n = n(),
                     rmse = sqrt(mean((dev - est)^2)),
                     r2 = cor(dev,est)^2,
                     model = model[1],
                     predictors = predictors[1],
                     spp = spp[1],
                     mtry = mtry[1],
                     ntree = ntree[1],
                     years_ahead = years_ahead[1],
                     tot_count = sum(table(dev_bin,est_bin)),
                     hit_rate = sum(diag(table(dev_bin,est_bin))) / tot_count,
                     false_rate = 1 - hit_rate,
                     false_rate_lo = length(which(dev_bin=="lo" & est_bin!="lo")) / length(which(dev_bin=="lo")),
                     false_rate_hi = length(which(dev_bin=="hi" & est_bin!="hi")) / length(which(dev_bin=="hi")),
                     tss = hit_rate - false_rate) %>%
    dplyr::filter(n >= 7) %>% ungroup()

    summaries$lambda <- NA
    summaries$alpha <- NA
  }
  if(d$model[i]=="glmnet") {
    summaries = dplyr::group_by(d, id) %>%
      dplyr::summarise(n = n(),
                       rmse = sqrt(mean((dev - est)^2)),
                       r2 = cor(dev,est)^2,
                       model = model[1],
                       predictors = predictors[1],
                       spp = spp[1],
                       alpha = alpha[1],
                       lambda = lambda[1],
                       years_ahead = years_ahead[1],
                       tot_count = sum(table(dev_bin,est_bin)),
                       hit_rate = sum(diag(table(dev_bin,est_bin))) / tot_count,
                       false_rate = 1 - hit_rate,
                       false_rate_lo = length(which(dev_bin=="lo" & est_bin!="lo")) / length(which(dev_bin=="lo")),
                       false_rate_hi = length(which(dev_bin=="hi" & est_bin!="hi")) / length(which(dev_bin=="hi")),
                       tss = hit_rate - false_rate) %>%
      dplyr::filter(n >= 7) %>% ungroup()

    summaries$mtry <- NA
    summaries$ntree <- NA
  }

  # for each combo of response / predicotrs / model, pick model with best
  # tuning parameters
  summary_df = dplyr::group_by(summaries, spp, model, predictors) %>%
    dplyr::arrange(-rmse) %>%
    dplyr::filter(rmse == min(rmse))
  summary_df = summary_df[1,]

  if(i == 1) {
    out = summary_df
    out_all = summaries
  } else {
    out = rbind(out, summary_df)
    out_all = rbind(out_all, summaries)
  }
}
saveRDS(out, "output/summaries/nonparameteric_univariate_field.rds")
saveRDS(out_all, "output/summaries/nonparameteric_univariate_raw_field.rds")
