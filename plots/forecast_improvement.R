library(ggplot2)

out = readRDS("output/summaries/parameteric_univariate.rds")

# unique combos of id and spp
out = dplyr::group_by(out, spp, id, model) %>%
  dplyr::summarise(rmse_01 = rmse[which(years_ahead == 0)] / rmse[which(years_ahead == 1)],
                   rmse_12 = rmse[which(years_ahead == 1)] / rmse[which(years_ahead == 2)],
                   r2_01 = r2[which(years_ahead == 1)] / r2[which(years_ahead == 0)],
                   r2_12 = r2[which(years_ahead == 2)] / r2[which(years_ahead == 1)]) %>%
  ungroup()

ggplot(summary, aes(model, diff_01)) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~spp) +
  xlab("") +
  ylab("RMSE (0 year) / RMSE (1 year)") +
  theme_bw()

ggplot(summary, aes(model, diff_12)) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~spp) +
  xlab("") +
  ylab("RMSE (0 year) / RMSE (1 year)") +
  theme_bw()

dplyr::group_by(out, spp, predictors, model,years_ahead) %>%
  dplyr::summarize(min_rmse = min(rmse),
                   r2 = r2[which(rmse==min_rmse)])
