library(recdata)
library(predRecruit)
library(dplyr)
library(tidyr)

data("recruit_dev")
data("assessment_metadata")
max_year = 2015

spp_names = unique(recruit_dev$short_name)
# Drop shortbelly -- details are sparse via JF
spp_names = spp_names[which(spp_names != "Shortbelly_rockfish")]
assessment_metadata = dplyr::filter(assessment_metadata,
                                    short_name != "Shortbelly_rockfish")
recruit_dev = dplyr::rename(recruit_dev, year = Yr) %>%
  dplyr::select(year, species, dev) %>%
  pivot_wider(names_from = species, values_from =dev)

data("calcofi")
calcofi <- calcofi %>%
  dplyr::distinct() %>%
  dplyr::select(species, year, log_est) %>%
  pivot_wider(names_from = species, values_from = log_est)

joined_calcofi <- dplyr::left_join(recruit_dev, calcofi) %>%
  as.data.frame()
for(i in 1:ncol(joined_calcofi)) {
  joined_calcofi[,i] = as.numeric(joined_calcofi[,i])
}

n_calcofi <- ncol(joined_calcofi) - 1
n_dev <- ncol(recruit_dev) - 1 - 1
cor_est <- cor(joined_calcofi[,-1], use =
      "pairwise.complete.obs")
cor_est <- cor_est[-28,-28]

calcofi <- cor_est[1:29, 30:57]

ggcorrplot(calcofi, ggtheme = ggplot2::theme_bw,
           colors = c("#6D9EC1", "white", "#E46726")) +
  theme(axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=7)) +
  xlab("Assessment") +
  ylab("Calcofi indicator")
ggsave("plots/calcofi_cor.png")

################### RREAS
data("rreas")
rreas = rreas %>%
  dplyr::distinct() %>%
  dplyr::select(species, year, index) %>%
  pivot_wider(names_from = species, values_from = index)

joined_rreas <- dplyr::left_join(recruit_dev, rreas) %>%
  as.data.frame()
for(i in 1:ncol(joined_rreas)) {
  joined_rreas[,i] = as.numeric(joined_rreas[,i])
}

n_rreas <- ncol(joined_rreas) - 1
n_dev <- ncol(recruit_dev) - 1 - 1
cor_est <- cor(joined_rreas[,-1], use =
                 "pairwise.complete.obs")
cor_est <- cor_est[-28,-28]

rreas <- cor_est[1:29, 30:51]

ggcorrplot(rreas, ggtheme = ggplot2::theme_bw,
           colors = c("#6D9EC1", "white", "#E46726"))+
  theme(axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=7)) +
  xlab("Assessment") +
  ylab("RREAS indicator")
ggsave("plots/rreas_cor.png")



