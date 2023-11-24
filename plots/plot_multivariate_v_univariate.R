library(dplyr)
library(ggplot2)
library(ggpubr)
library(grid)

# Process non-parameteric multivariate
out_mlmv <- readRDS("output/summaries/nonparameteric_multivariate_raw_2011.rds")

# calculate rmse across all spp
out_mlmv <- dplyr::filter(out_mlmv, years_ahead == 1) %>%
  dplyr::group_by(model, predictors, mtry, ntree, lambda, alpha) %>%
  dplyr::summarise(rmse = mean(rmse,na.rm=T),
                   r2 = mean(r2,na.rm=T),
                   hit_rate = mean(hit_rate,na.rm=T),
                   false_rate = mean(false_rate,na.rm=T),
                   tss = mean(tss,na.rm=T))

mlmv_rmse <-
  dplyr::group_by(out_mlmv, model, predictors) %>% # find best rmse across tuning parameters
  dplyr::arrange(model, predictors, rmse) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::mutate(metric = "RMSE")
mlmv_hit_rate <-
  dplyr::group_by(out_mlmv, model, predictors) %>% # find best rmse across tuning parameters
  dplyr::arrange(model, predictors, -hit_rate) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::mutate(metric = "Hit rate")
mlmv_tss <-
  dplyr::group_by(out_mlmv, model, predictors) %>% # find best rmse across tuning parameters
  dplyr::arrange(model, predictors, -tss) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::mutate(metric = "Total skill score")
mlmv <- rbind(mlmv_rmse, mlmv_hit_rate, mlmv_tss) %>%
  dplyr::select(-mtry,-ntree, -lambda,-alpha)

# Process parameteric multivariate -- lm, gam, glmm
out_mv <- readRDS("output/summaries/parameteric_multivariate.rds")
out_mv$cov <- paste(out_mv$name1, out_mv$name2, out_mv$name3)

out_mv <- dplyr::filter(out_mv, years_ahead == 1, predictors %in% c("roms","rreas","calcofi")) %>%
  dplyr::group_by(model, predictors, cov) %>%
  dplyr::summarise(rmse = mean(rmse,na.rm=T),
                   r2 = mean(r2,na.rm=T),
                   hit_rate = mean(hit_rate,na.rm=T),
                   false_rate = mean(false_rate,na.rm=T),
                   tss = mean(tss,na.rm=T))

mv_rmse <-
  dplyr::group_by(out_mv, model, predictors) %>% # find best rmse across tuning parameters
  dplyr::arrange(model, predictors, rmse) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::mutate(metric = "RMSE")
mv_hit_rate <-
  dplyr::group_by(out_mv, model, predictors) %>% # find best rmse across tuning parameters
  dplyr::arrange(model, predictors, -hit_rate) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::mutate(metric = "Hit rate")
mv_tss <-
  dplyr::group_by(out_mv, model, predictors) %>% # find best rmse across tuning parameters
  dplyr::arrange(model, predictors, -tss) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::mutate(metric = "Total skill score")

mv <- rbind(mv_rmse, mv_hit_rate, mv_tss) %>%
  dplyr::select(-cov)

mv_combined <- rbind(mv, mlmv)
mv_combined$value <- mv_combined$rmse
mv_combined$value[which(mv_combined$metric=="Hit rate")] <- mv_combined$hit_rate[which(mv_combined$metric=="Hit rate")]
mv_combined$value[which(mv_combined$metric=="Total skill score")] <- mv_combined$tss[which(mv_combined$metric=="Total skill score")]


# Process the univariate ml data
# This has the RF / lasso model for each spp across multiple tuning parameters
out_uniml <- readRDS("output/summaries/nonparameteric_univariate_raw_2011.rds")

out_uniml <- dplyr::filter(out_uniml, years_ahead == 1) %>%
  dplyr::group_by(model, predictors, mtry, ntree, lambda, alpha) %>%
  dplyr::summarise(rmse = mean(rmse,na.rm=T),
                   r2 = mean(r2,na.rm=T),
                   hit_rate = mean(hit_rate,na.rm=T),
                   false_rate = mean(false_rate,na.rm=T),
                   tss = mean(tss,na.rm=T))
uniml_rmse <-
  dplyr::group_by(out_uniml, model, predictors) %>% # find best rmse across tuning parameters
  dplyr::arrange(model, predictors, rmse) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::mutate(metric = "RMSE")
uniml_hit_rate <-
  dplyr::group_by(out_uniml, model, predictors) %>% # find best rmse across tuning parameters
  dplyr::arrange(model, predictors, -hit_rate) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::mutate(metric = "Hit rate")
uniml_tss <-
  dplyr::group_by(uniml_rmse, model, predictors) %>% # find best rmse across tuning parameters
  dplyr::arrange(model, predictors, -tss) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::mutate(metric = "Total skill score")
ml <- rbind(uniml_rmse, uniml_hit_rate, uniml_tss) %>%
  dplyr::select(-mtry,-ntree, -lambda,-alpha)

# Process parameteric multivariate -- lm, gam, glmm
out <- readRDS("output/summaries/parameteric_univariate_2011.rds")
out$cov <- paste(out$name1, out$name2, out$name3)

out <- dplyr::filter(out, years_ahead == 1, predictors %in% c("roms","rreas","calcofi")) %>%
  dplyr::mutate(mse = rmse^2) %>%
  dplyr::group_by(model, predictors, cov) %>%
  dplyr::summarise(rmse = mean(rmse,na.rm=T),
                   r2 = mean(r2,na.rm=T),
                   hit_rate = mean(hit_rate,na.rm=T),
                   false_rate = mean(false_rate,na.rm=T),
                   tss = mean(tss,na.rm=T))

rmse <-
  dplyr::group_by(out, model, predictors) %>% # find best rmse across tuning parameters
  dplyr::arrange(model, predictors, rmse) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::mutate(metric = "RMSE")
hit_rate <-
  dplyr::group_by(out, model, predictors) %>% # find best rmse across tuning parameters
  dplyr::arrange(model, predictors, -hit_rate) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::mutate(metric = "Hit rate")
tss <-
  dplyr::group_by(out, model, predictors) %>% # find best rmse across tuning parameters
  dplyr::arrange(model, predictors, -tss) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::mutate(metric = "Total skill score")

uni_par <- rbind(rmse, hit_rate, tss) %>%
  dplyr::select(-cov)

uni_combined <- rbind(uni_par, ml)
uni_combined$value <- uni_combined$rmse
uni_combined$value[which(uni_combined$metric=="Hit rate")] <- uni_combined$hit_rate[which(uni_combined$metric=="Hit rate")]
uni_combined$value[which(uni_combined$metric=="Total skill score")] <- uni_combined$tss[which(uni_combined$metric=="Total skill score")]

mv_combined$Method = "Multivariate"
uni_combined$Method = "Univariate"

combo <- rbind(mv_combined, uni_combined)

combo$predictors = toupper(combo$predictors)
combo$predictors[which(combo$predictors=="CALCOFI")] = "CalCOFI"
combo= dplyr::rename(combo, Model = model)
combo$Model = as.character(combo$Model)
combo$Model[which(combo$Model == "glmnet")] = "Lasso"
combo$Model[which(combo$Model == "glmm")] = "GLMM"
combo$Model[which(combo$Model == "randomForest")] = "RF"
combo$Model = factor(combo$Model, levels = c("lm","gam","GLMM","Lasso","RF"))

combo$metric <- factor(combo$metric, levels = c("RMSE", "Hit rate", "Total skill score"))

combo$predictors <- factor(combo$predictors, levels = c("CalCOFI","RREAS","ROMS"))

p1 <- ggplot(combo, aes(predictors, value, col=Model,group=Method)) +
  geom_point(aes(shape = Method),size=4,alpha=0.7,position = position_jitterdodge(dodge.width=0.3, jitter.width=0.7)) +
  facet_wrap(~metric, scale="free", nrow=3, ncol=1) +
  theme_bw() +
  scale_color_viridis_d(end=0.8) +
  ylab("") +
  xlab("") +
  theme(strip.background=element_rect(fill="white"))
ggsave(p1, file="plots/Figure_4.png", height = 7, width=7)
#ggsave(p1, file="plots/Figure_4.pdf", height = 7, width=7)
