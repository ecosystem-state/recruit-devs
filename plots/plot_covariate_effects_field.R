library(ggplot2)
library(dplyr)
library(tidyr)
library(ggbeeswarm)
library(viridis)

out = readRDS("output/summaries/parameteric_univariate.rds")
out_ml = readRDS("output/summaries/nonparameteric_univariate_raw.rds")
out <- dplyr::filter(out, predictors != "rreas")
out_ml <- dplyr::filter(out_ml, predictors != "rreas")
out_rreas = readRDS("output/summaries/parameteric_univariate_field.rds")
out_ml_rreas = readRDS("output/summaries/nonparameteric_univariate_raw_field.rds")
out <- rbind(out, out_rreas)
out_ml <- rbind(out_ml, out_ml_rreas)

#
out <- dplyr::filter(out, years_ahead == 1, n == 10) %>%
  dplyr::group_by(predictors, model, spp) %>%
  dplyr::arrange(rmse) %>%
  dplyr::filter(row_number() == 1)

# For glmnet, we looked across a range of
# the elasticnet parameter, with 0≤α≤ 1 and lambda the penalty parameter
# alpha = 1 = lasso, alpha = 0 = ridge (correlated variables get similar coefficients)

# for each species / dataset, pull out best model
lasso_models = dplyr::filter(out_ml,
                  years_ahead == 1,
                  alpha == 1,
                  lambda <= 2,
                  model == "glmnet") %>%
  dplyr::group_by(spp, predictors) %>%
  dplyr::arrange(rmse, lambda) %>%
  dplyr::filter(row_number() == 1)

rf_models = dplyr::filter(out_ml,
                             years_ahead == 1,
                             model == "randomForest") %>%
  dplyr::group_by(spp, predictors) %>%
  dplyr::arrange(rmse, mtry, ntree) %>%
  dplyr::filter(row_number() == 1)

ml_models = rbind(lasso_models, rf_models)


to_keep <- names(out)[which(names(out) %in% names(out_ml))]
out <- out[,which(names(out) %in% to_keep)]
ml_models <- ml_models[,which(names(ml_models) %in% to_keep)]

out <- rbind(out, ml_models)

# rename and re-factor model
out$model <- as.character(out$model)
out$model[which(out$model == "randomForest")] = "RF"
out$model[which(out$model == "glmnet")] = "Lasso"
out <- dplyr::rename(out, Model = model)
out$Model <- as.factor(out$Model)
out$Model <- factor(out$Model, levels=c("lm","gam","Lasso","RF"))

# rename and re-label predictors
levels(out$predictors) <- list(CalCOFI = "calcofi",
                          RREAS = "rreas",
                          ROMS = "roms",
                          'CalCOFI DFA' = "calfofi_trends",
                          'RREAS DFA' = "rreas_trends",
                          'ROMS DFA' = "roms_trends")

# join in null
null <- readRDS("output/summaries/null.rds")
null <- dplyr::rename(null, spp = short_name, rmse_null = rmse)
out <- dplyr::left_join(out, null[,c("spp","rmse_null")])

# p1, but facet by species
print_names <- read.csv("data/print_names.csv") # this is just for pretty names
print_names <- dplyr::rename(print_names, spp = short_name,
                             Species = print_name)

out <- dplyr::left_join(out, print_names )

p1_by_species <- ggplot(out, aes(x = predictors, y = rmse, col=Model)) +
  geom_hline(aes(yintercept=rmse_null), col="red",alpha=0.7) +
  geom_beeswarm(alpha=0.6, dodge.width = 1) +
  xlab("") +
  ylab("RMSE") +
  scale_color_viridis(discrete = TRUE, end=0.8) +
  theme_bw() +
  coord_cartesian(ylim=c(0.2,1.8)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  facet_wrap(~Species, scale="free_y") +
  theme(strip.background=element_rect(fill="white"),
        strip.text.x = element_text(size = 6.5),
        axis.text.x = element_text(size=7))
ggsave("plots/Figure_S5_sensitivity.png", plot = p1_by_species, width=7, height=7)

# p2, but facet by species
p2_by_species <- ggplot(out, aes(x = predictors, y = r2, col=Model)) +
  geom_beeswarm(alpha=0.6, dodge.width = 1) +
  xlab("") +
  ylab("R-squared") +
  scale_color_viridis(discrete = TRUE, end=0.8) +
  theme_bw() +
  coord_cartesian(ylim=c(0.0,0.9)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  facet_wrap(~Species, scale="free_y") +
  theme(strip.background=element_rect(fill="white"),
        strip.text.x = element_text(size = 6.5),
        axis.text.x = element_text(size=7))
ggsave("plots/Figure_S6_sensitivity.png", plot = p2_by_species, width=7, height=7)

