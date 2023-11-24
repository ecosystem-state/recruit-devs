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

recruit_dev = left_join(recruit_dev, assessment_metadata, by="short_name")

# The null model predictions are just the running average
recruit_dev$pred <- NA
for(spp in 1:length(unique(recruit_dev$short_name))) {
  z <- recruit_dev$dev[which(recruit_dev$short_name == unique(recruit_dev$short_name)[spp])]
  recruit_dev$dev[which(recruit_dev$short_name == unique(recruit_dev$short_name)[spp])] <- (z - mean(z))/sd(z)
  for(y in seq(max_year-10+1, max_year)) {
    running_mean <- mean(recruit_dev$dev[which(recruit_dev$short_name == unique(recruit_dev$short_name)[spp] &
                                                 recruit_dev$Yr < y)], na.rm=T)
    recruit_dev$pred[which(recruit_dev$short_name == unique(recruit_dev$short_name)[spp] &
                             recruit_dev$Yr == y)] <- running_mean
  }
}

recruit_dev$dev_bin <- "mid"
recruit_dev$dev_bin[which(recruit_dev$dev < -0.4308)] = "lo"
recruit_dev$dev_bin[which(recruit_dev$dev > 0.4308)] = "hi"
recruit_dev$est_bin <- "mid"
recruit_dev$est_bin[which(recruit_dev$pred < -0.4308)] = "lo"
recruit_dev$est_bin[which(recruit_dev$pred > 0.4308)] = "hi"


# calculate rmse and r2 by spp
out <- dplyr::filter(recruit_dev, !is.na(pred)) %>%
  dplyr::group_by(short_name) %>%
  dplyr::summarise(r2 = cor(pred, dev)^2,
                   rmse = sqrt(mean((pred-dev)^2)),
                   tot_count = sum(table(dev_bin,est_bin)),
                   hit_rate = sum(diag(table(dev_bin,est_bin))) / tot_count,
                   false_rate = 1 - hit_rate,
                   tss = hit_rate - false_rate) %>%
  as.data.frame()
out$model <- "null"
saveRDS(out, "output/summaries/null.rds")


