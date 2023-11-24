library(dplyr)
library(glmnet)
library(doFuture)
registerDoFuture()
plan(multisession, workers = 4L)

# Load response data
rec_devs = readRDS("data/clean_rec_devs.rds")
rec_devs = dplyr::rename(rec_devs, time = Yr)
# Standardize
rec_devs = dplyr::group_by(rec_devs, species) %>%
  dplyr::mutate(z = (dev-mean(dev))/sd(dev)) %>%
  dplyr::filter(time<=2016)

rec_devs = dplyr::filter(rec_devs, short_name%in%c("Black_rockfish_CA",
                                                   "Blue/Deacon_rockfish_CA","Cabezon_SCA","California_scorpionfish",
                                                   "Chilipepper_rockfish","Gopher/black-and-yellow_rockfish",
                                                   "Yellowtail_rockfish_South"))

# Do regressions on ROMS variables
enviro <- read.csv("data/roms.july_june.mean.csv")
enviro = dplyr::rename(enviro, time = yr)
enviro = enviro[,c(1,8:13)]
enviro = tidyr::pivot_longer(enviro,cols=2:ncol(enviro)) %>%
  dplyr::rename(cov = value)

# Do regressions on CalCOFI variables
calcofi <- readRDS("data/predicted_indices.rds")
calcofi = dplyr::rename(calcofi, time = year, name=species,cov=index)
calcofi <- dplyr::group_by(calcofi, name) %>%
  dplyr::mutate(nz = length(which(n_pos_cpue!=0))) %>%
  dplyr::filter(nz >= 20, !is.infinite(cov), !is.na(cov)) %>%
  dplyr::select(-nz) %>%
  dplyr::mutate(cov = (cov-mean(cov))/sd(cov)) %>%
  dplyr::select(name,time,cov)

rec_devs = dplyr::filter(rec_devs, time >= min(calcofi$time))
enviro = dplyr::filter(enviro, time>=min(enviro$time))
#rreas = dplyr::filter(rreas, time >= min(rreas$time))
cov_dat = rbind(enviro, calcofi)
dat = dplyr::left_join(rec_devs[,c("time","species","z")],
                       cov_dat,
                       by="time")

dat = dplyr::filter(dat, !is.na(cov))

fit <- lm(z ~ -1 + cov:species:name, data=dat)

# LASSO regression - minimize sum of abs(coefficients)
x = model.matrix(fit)
y_train = fit$model$z
lambdas <- 10^seq(0, -5, by = -.025)

# Setting alpha = 1 implements lasso regression
lasso_reg <- cv.glmnet(x, y_train,
                       alpha = 1,
                       lambda = lambdas,
                       foldid = dat$time,
                       standardize = FALSE)

elastic_reg <- cv.glmnet(x, y_train,
                       alpha = 0.5,
                       lambda = lambdas,
                       foldid = dat$time,
                       standardize = FALSE)

ridge_reg <- cv.glmnet(x, y_train,
                       alpha = 0.2,
                       lambda = lambdas,
                       foldid = dat$time,
                       standardize = FALSE)

# Best
lambda_best <- lasso_reg$lambda.min
lambda_best
m = coef(lasso_reg)

indx = which(abs(as.numeric(coef(lasso_reg))) > 0.0005)
rownames(coef(lasso_reg))[indx]


indx = which(abs(as.numeric(coef(ridge_reg)))>0.001)
rownames(coef(ridge_reg))[indx]



sub$est <- NA
for(yr in unique(sub$time)) {
  fit <- lm(z ~ -1 + cov:species, data=sub[which(sub$time!=yr),])
  sub$est[which(sub$time==yr)] <- predict(fit, newdata = sub[which(sub$time==yr),])
}
r2 = dplyr::group_by(sub, species) %>%
  dplyr::summarise(r2 = round(cor(est,z)^2, 3))
results_table = left_join(results_table, r2)
colnames(results_table)[ncol(results_table)] = unique(calcofi$name)[i]
