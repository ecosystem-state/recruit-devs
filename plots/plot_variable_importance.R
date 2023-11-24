library(dplyr)
library(ggplot2)
library(ggpubr)
library(grid)
df <- readRDS("output/model_importance.rds")

# what species / time series offer the greatest RMSE improvement across all assessments?
best_predictor <- dplyr::group_by(df, model, cov) %>%
  dplyr::summarize(m = mean(rmse_improve)) %>%
  dplyr::arrange(-m) %>%
  dplyr::filter(m > 0)

# or what species can be improved the most?
most_improved_onavg <- dplyr::group_by(df, model, spp) %>%
  dplyr::summarize(m = mean(rmse_improve)) %>%
  dplyr::arrange(-m) %>%
  dplyr::filter(m > 0)

# how many times does each predictor show up as an improvement?
how_many_improvements <- dplyr::filter(df, model=="lm") %>%
  dplyr::filter(rmse_improve > 0) %>%
  dplyr::group_by(cov) %>%
  dplyr::summarize(n = n()) %>%
  dplyr::arrange(-n)

# single best covariate for each assessment
single_best <- dplyr::filter(df, model=="lm") %>%
  dplyr::filter(rmse_improve > 0) %>%
  dplyr::group_by(spp) %>%
  dplyr::arrange(-rmse_improve) %>%
  dplyr::filter(row_number() == 1)
sort(table(single_best$cov))


# make a plot
# Bocaccio, Cabezon SCA, Chilipepper, Lingcod S, Longspine,
# Sablefish, Scorpionfish, Vermillion SCA, [dropped Gopher, Greenling]
print_names <- read.csv("data/print_names.csv")
print_names <- dplyr::rename(print_names, spp = short_name, Species = print_name)
df <- dplyr::left_join(df, print_names)

sub = dplyr::filter(df, Species %in% c("Bocaccio", "Cabezon SCA", "Chilipepper", "Lingcod S", "Longspine", "Sablefish", "Scorpionfish", "Vermillion SCA") == TRUE, model == "lm") %>%
  dplyr::arrange(-rmse_improve)
sub$rmse_improve[which(sub$rmse_improve < 0)] = NA

# join in shortened names
pretty_cov <- read.csv("data/pretty_covars.csv")
sub <- dplyr::left_join(sub, pretty_cov)

plots = list()

for(i in 1:length(unique(sub$Species))) {

  this_dat <- dplyr::filter(sub, !is.na(rmse_improve), rmse_improve > 0.05, Species == sort(unique(sub$Species))[i])
  this_dat$pretty_cov <- factor(this_dat$pretty_cov, levels = rev(this_dat$pretty_cov))
plots[[i]] <-
  this_dat %>%
  ggplot(aes(y = pretty_cov, x = rmse_improve)) +
  geom_col(fill = viridis(1), alpha=0.3) +
  theme_bw()+
  ylab("") +
  xlab("Percent improvement in RMSE") +
  facet_wrap(~Species, scale="free", nrow = 2) +
  geom_text(aes(label = pretty_cov), size=2,nudge_x = -0.05, hjust=0) +
  coord_cartesian(xlim=c(0,0.4)) +
  theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.background=element_rect(fill="white"))# +
  #theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())
}

#library(patchwork)
#p <- (plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]]) / (plots[[5]] + plots[[6]] + plots[[7]] + plots[[8]]) + plot_layout(ncol = 4)

m <- 0.2
figure <- ggarrange(plots[[1]] + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(r=m,l=m/2)),
          plots[[2]] + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(r=m,l=m)),
          plots[[3]] + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(r=m,l=m)),
          plots[[4]] + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(), plot.margin = margin(l=m)),
          plots[[5]] + theme(axis.title.x = element_blank(), plot.margin = margin(r=m)),
          plots[[6]] + theme(axis.title.x = element_blank(), plot.margin = margin(r=m,l=m)),
          plots[[7]] + theme(axis.title.x = element_blank(), plot.margin = margin(r=m,l=m)),
          plots[[8]] + theme(axis.title.x = element_blank(), plot.margin = margin(l=m,r=m/2)),
          ncol = 4,
          nrow=2,
          align = "v")
annotate_figure(figure, bottom = textGrob("Percent change in RMSE", gp=gpar(cex=0.9)))
#ggsave("plots/Figure_3.pdf", width=7, height=7)

ggsave("plots/Figure_3.png", width=7, height=7)


