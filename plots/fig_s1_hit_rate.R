library(dplyr)
library(ggplot2)
library(viridis)
out = readRDS("output/summaries/parameteric_univariate.rds")
out_ml = readRDS("output/summaries/nonparameteric_univariate.rds")
null <- readRDS("output/summaries/null.rds")

# find best parametric model for each combo
# rmse tells us how well model can predict values in absolute terms;
# r2 tells us how well predictions are in percent terms
best_1 = dplyr::rename(out, Model = model) %>%
  dplyr::filter(years_ahead == 1, predictors %in% c("calcofi", "rreas", "roms")) %>%
  dplyr::group_by(predictors, Model, spp) %>%
  dplyr::summarize(#r2 = max(r2),
    #rmse = rmse[which(r2==max(r2))[1]],
    hit_max = max(hit_rate),
    hit_median = median(hit_rate),
    tss_max = max(tss),
    tss_median = median(tss)) %>%
  ungroup()

out_ml <- dplyr::rename(out_ml, Model = model) %>%
  dplyr::filter(years_ahead == 1, predictors %in% c("calcofi", "rreas", "roms")) %>%
  dplyr::group_by(predictors, Model, spp) %>%
  dplyr::summarize(#r2 = max(r2),
    #rmse = rmse[which(r2==max(r2))[1]],
    hit_max = max(hit_rate),
    hit_median = median(hit_rate),
    tss_max = max(tss),
    tss_median = median(tss)) %>%
  ungroup()

best_1 <- rbind(best_1, out_ml) # combine parametric / ml

best_1$Model = as.character(best_1$Model)
#best_1$Model[which(best_1$Model=="lm")] = "Regression"
#best_1$Model[which(best_1$Model=="gam")] = "GAM"
best_1$Model[which(best_1$Model=="randomForest")] = "RF"
best_1$Model[which(best_1$Model=="glmnet")] = "Lasso"
best_1$Model = as.factor(best_1$Model)

best_1$predictors = as.character(best_1$predictors)
best_1$predictors[which(best_1$predictors=="calcofi")] = "CalCOFI"
best_1$predictors[which(best_1$predictors=="rreas")] = "RREAS"
best_1$predictors[which(best_1$predictors=="roms")] = "ROMS"

# one plot is a facet by species
spp_rowcol <- data.frame(spp = unique(best_1$spp))
spp_rowcol$row <- rep(1:6, 5)[1:nrow(spp_rowcol)]
spp_rowcol$col <- sort(rep(1:5, 6))[1:nrow(spp_rowcol)]
best_1 <- dplyr::left_join(best_1, spp_rowcol)

# join in null
null <- dplyr::rename(null, spp = short_name, hit_null =hit_rate)
best_1 <- dplyr::left_join(best_1, null[,c("spp","hit_null")])

print_names <- read.csv("data/print_names.csv") # this is just for pretty names
print_names <- dplyr::rename(print_names, spp = short_name)
best_1 <- dplyr::left_join(best_1, print_names)

best_1$Model <- factor(best_1$Model, levels = c("lm","gam","RF","Lasso"))

p1 <- best_1 %>%
  ggplot(aes(predictors, hit_max, col=Model)) +
  facet_wrap(~ print_name,scale="free") +
  #geom_violin(position = position_dodge2(0.5)) +
  #geom_linerange(aes(ymin = hit_median, ymax = hit_max), position = position_dodge2(0.5)) +
  geom_point(position = position_dodge2(0.5)) +
  geom_hline(aes(yintercept = hit_null), col="red", alpha=0.5) +
  xlab("") +
  ylab("RMSE") +
  theme_bw() +
  scale_color_viridis_d(end=0.8) +
  theme(strip.text.x = element_text(size = 6),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(size=6, angle = 90, vjust = 0.5, hjust=0.4))
ggsave(p1, filename="plots/fig_s1.jpeg", height = 7, width = 7)

