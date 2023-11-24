library(ggplot2)
library(dplyr)
library(tidyr)
out = readRDS("output/summaries/parameteric_univariate.rds")
out_ml = readRDS("output/summaries/nonparameteric_univariate.rds")

# find best parametric model for each combo
# rmse tells us how well model can predict values in absolute terms;
# r2 tells us how well predictions are in percent terms
out = dplyr::group_by(out, predictors, model, years_ahead,spp) %>%
  dplyr::summarize(r2 = max(r2),
                   rmse = rmse[which(r2==max(r2))[1]],
                   id = id[which(r2==max(r2))[1]]) %>%
  ungroup()
out_ml <- out_ml[, which(names(out_ml) %in% names(out))]
out <- rbind(out, out_ml)


out$Forecast = as.factor(out$years_ahead)
plot_R2 <- out %>%
  dplyr::filter(!is.na(model), years_ahead%in%c(0,1)) %>%
  ggplot(aes(model, r2, fill=Forecast)) +
  geom_boxplot(outlier.shape = NA) +
  xlab("") +
  ylab(expression(R^2)) + theme_bw()

plot_RMSE <- out %>%
  dplyr::filter(!is.na(model), years_ahead%in%c(0,1)) %>%
  ggplot(aes(model, rmse, fill=Forecast)) +
  geom_boxplot(outlier.shape = NA) +
  xlab("") +
  ylab("RMSE") + theme_bw()

# quantify change in r2 forecast
fore = dplyr::group_by(out, predictors, model, spp) %>%
  dplyr::summarise(r2_1 = r2[years_ahead==1] / r2[years_ahead==0],
                   r2_2 = r2[years_ahead==2] / r2[years_ahead==0],
                   rmse_1 = rmse[years_ahead==0] / rmse[years_ahead==1],
                   rmse_2 = rmse[years_ahead==0] / rmse[years_ahead==2])

fore = tidyr::pivot_longer(fore,cols=c("r2_1","r2_2","rmse_1","rmse_2")) %>%
  dplyr::filter(!is.na(model))

ggplot(fore,aes(model, value, fill=name)) +
  geom_boxplot(outlier.shape = NA) +
  #facet_wrap(~ model,scale="free_y") +
  coord_cartesian(ylim=c(0,1.5)) +
  theme_bw() +
  xlab("") +
  ylab("Relative performance") +
  scale_fill_discrete(labels = c(expression(paste(R^2, " 1 year")),
                                 expression(paste(R^2, " 2 year")),
                                 expression(paste("RMSE 1 year")),
                                 expression(paste("RMSE 2 year")))) +
  theme(legend.text.align = 0)
