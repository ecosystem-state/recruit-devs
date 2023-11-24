library(dplyr)
library(ggplot2)
library(viridis)
out = readRDS("output/summaries/parameteric_univariate.rds")

# find best parametric model for each combo
# rmse tells us how well model can predict values in absolute terms;
# r2 tells us how well predictions are in percent terms
best_1 = dplyr::rename(out, Model = model) %>%
  dplyr::filter(years_ahead == 1, predictors %in% c("calcofi"),
                Model %in% c("lm","gam")) %>%
  dplyr::group_by(predictors, Model, spp) %>%
  dplyr::mutate(rmse_lo = quantile(rmse, 0.1)) %>%
  ungroup() %>%
  dplyr::filter(rmse < rmse_lo)

# condense columns name1 - name3
best_1 <- tidyr::pivot_longer(best_1, cols = c("name1","name2","name3")) %>%
  dplyr::select(-rmse, -predictors, -id, -n, -r2, -years_ahead, -rmse_lo, -name)

print_names <- read.csv("data/print_names.csv") # this is just for pretty names
print_names <- dplyr::rename(print_names, spp = short_name)
best_1 <- dplyr::left_join(best_1, print_names)

count <- dplyr::filter(best_1, Model=="lm")
count <- sort(table(count$value))

best_1$value <- factor(best_1$value, levels = rev(names(count)))

p1 <- best_1 %>%
  dplyr::filter(!is.na(value)) %>%
  ggplot(aes(value, fill=Model, group=Model)) +
  #facet_wrap(~ print_name,scale="free") +
  geom_bar(position='dodge') +
  xlab("") +
  ylab("Count") +
  theme_bw() +
  scale_color_viridis_d(end=0.8) +
  theme(strip.text.x = element_text(size = 6),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(size=8, angle = 90, vjust = 0.5, hjust=0.4))
ggsave(p1, filename="plots/fig_s2.jpeg", height = 7, width = 7)
#
# # one plot is a facet by species
# spp_rowcol <- data.frame(spp = unique(best_1$spp))
# spp_rowcol$row <- rep(1:6, 5)[1:nrow(spp_rowcol)]
# spp_rowcol$col <- sort(rep(1:5, 6))[1:nrow(spp_rowcol)]
# best_1 <- dplyr::left_join(best_1, spp_rowcol)
#
# p1 <- dplyr::filter(best_1, row!=6) %>%
#   dplyr::filter(!is.na(value)) %>%
#   ggplot(aes(value, fill=Model, group=Model)) +
#   facet_wrap(~ print_name,scale="free") +
#   geom_bar(position='dodge') +
#   xlab("") +
#   ylab("Count") +
#   theme_bw() +
#   scale_color_viridis_d(end=0.8) +
#   theme(strip.text.x = element_text(size = 6),
#         strip.background = element_rect(fill = "white"),
#         axis.text.x=element_blank())
# ggsave(p1, filename="plots/fig_s3.jpeg", height = 7, width = 7)
#
