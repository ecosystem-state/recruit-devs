library(dplyr)
library(ggplot2)
library(viridis)
out = readRDS("output/summaries/parameteric_univariate.rds")
out_ml = readRDS("output/summaries/nonparameteric_univariate.rds")

# quantify change in r2 forecast
fore = dplyr::group_by(out, predictors, model, spp) %>%
  dplyr::summarise(fore_1 = rmse[years_ahead==1] / rmse[years_ahead==0],
                   fore_2 = rmse[years_ahead==2] / rmse[years_ahead==0])
fore_ml = dplyr::group_by(out_ml, predictors, model, spp) %>%
  dplyr::summarise(fore_1 = rmse[years_ahead==1] / rmse[years_ahead==0],
                   fore_2 = rmse[years_ahead==2] / rmse[years_ahead==0])

fore <- rbind(fore, fore_ml)

fore <- dplyr::rename(fore, Model = model)
fore$Model = as.character(fore$Model)
#fore$Model[which(fore$Model=="lm")] = "Regression"
#fore$Model[which(fore$Model=="gam")] = "GAM"
fore$Model[which(fore$Model=="randomForest")] = "RF"
fore$Model[which(fore$Model=="glmnet")] = "Lasso"
fore$Model = factor(fore$Model, levels = c("lm","gam","RF","Lasso"))

fore$predictors = as.character(fore$predictors)
fore$predictors[which(fore$predictors=="calcofi")] = "CalCOFI"
fore$predictors[which(fore$predictors=="rreas")] = "RREAS"
fore$predictors[which(fore$predictors=="roms")] = "ROMS"

fore <- tidyr::pivot_longer(fore,cols=c("fore_1","fore_2"))
fore$name[which(fore$name=="fore_1")] = "1 year"
fore$name[which(fore$name=="fore_2")] = "2 year"


print_names <- read.csv("data/print_names.csv") # this is just for pretty names
print_names <- dplyr::rename(print_names, spp = short_name)
fore<- dplyr::left_join(fore, print_names)

p1 <- fore %>%
  #dplyr::filter(!is.na(Model), Model %in% c("Regression","GAM")) %>%
  dplyr::filter(!is.na(Model), Model %in% c("lm","gam")) %>%
  ggplot(aes(name, value, fill = Model)) +
  geom_boxplot(outlier.color = NA, position = position_dodge(),alpha=0.6) +
  facet_wrap(~ print_name,scale="free_y") +
  coord_cartesian(ylim=c(0.9,5)) +
  xlab("") + ylab("Ratio of RMSE[t] / RMSE[t=0]") +
  theme_bw() +
  #scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis_d(end=0.5) +
  scale_fill_viridis_d(end=0.5) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 6.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))
ggsave(p1, filename="plots/fig_S9.png", height = 7, width = 7)

