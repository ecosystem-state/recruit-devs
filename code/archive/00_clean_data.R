library(readxl)
library(ggplot2)
library(dplyr)

# meta data isn't used initially
meta <- read_xlsx("data/Stock Assessment History.xlsx")

#
load("data/models_5Aug2021.Rdata")
# info2 contains spp info

# read in models - can't do it fast way because cols don't match
#recruit_dat <- lapply(models, getElement, "recruit")
#dat <- do.call(rbind.data.frame, recruit_dat)
for(i in 1:length(models)) {
  if(i==1) {
    dat <- models[[1]]$recruit[,c("Yr","SpawnBio","dev")]
    dat$species <- info2$local.dir[1]
  } else {
    d <- models[[i]]$recruit[,c("Yr","SpawnBio","dev")]
    d$species <- info2$local.dir[i]
    dat <- rbind(dat,
                 d)
  }
}

dat <- dplyr::group_by(dat, species) %>%
  dplyr::filter(dev != 0, !is.na(dev)) %>%
  dplyr::mutate(s = sd(dev)) %>%
  dplyr::filter(s > 0) %>%
  dplyr::select(-s) %>%
  dplyr::filter(Yr >= 1975)

# do some renaming
short_names <- read.csv("data/short_names.csv") %>%
  dplyr::rename(species = assessed_name)
dat <- dplyr::left_join(dat, short_names)

jpeg("plots/plot_rec_devs.jpeg")
ggplot(dat, aes(Yr,dev)) +
 geom_line(col="grey30") + geom_point() +
 facet_wrap(~short_name,scale="free_y") +
  theme_bw() +
  xlab("Year") + ylab("Recruit dev.") +
  theme(strip.background =element_rect(fill="white")) +
  xlim(c(1975,2015)) +
  theme(strip.text.x = element_text(size = 7))
dev.off()

saveRDS(dat,"data/clean_rec_devs.rds")
