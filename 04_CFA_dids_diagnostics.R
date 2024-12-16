##########################################
###########04_CFA_DIDS_Diagnosis##########
##########################################

# Load libraries
library('tidyverse')
library('lavaan')
library('semTools')

# Load data
hearts <- as_tibble(read.csv("./resources/data_files/hearts_clean.csv"))

hearts_br <- hearts %>% filter(retirement_status == "bridge_employment")
hearts_lr <- hearts %>% filter(retirement_status == "long_time_retiree")
hearts_lw <- hearts %>% filter(retirement_status == "long_time_worker")
hearts_rr <- hearts %>% filter(retirement_status == "recently_retired")
hearts_sr <- hearts %>% filter(retirement_status == "soon_retired")


dids <- '
commitment_making =~ com_ma_1 + com_ma_2 + com_ma_3

identification_with_commitment =~ com_id_1 + com_id_2 + com_id_3

exploration_breadth =~ exp_br_1 + exp_br_2 + exp_br_3

exploration_depth =~ exp_de_2 + exp_de_3

ruminative_exploration =~ exp_ru_1 + exp_ru_2 + exp_ru_3

'

fit_all <- lavaan::cfa(dids, data = hearts, estimator = "MLR", missing = "ML")

sink("./resources/DIDS_CFA/model_statistics.txt")
  lavaan::summary(fit_all, standardized = TRUE, fit.measures = TRUE)
  lavaan::fitted(fit_all)
  lavaan::residuals(fit_all, type = "raw")
  lavaan::residuals(fit_all, type = "standardized")
  lavaan::residuals(fit_all, type = "cor.bollen")
sink()

##Assess model for each group of retirement status
fit_br  <- lavaan::cfa(dids, data = hearts_br, estimator = "MLR", missing = "ML")
fit_lr  <- lavaan::cfa(dids, data = hearts_lr, estimator = "MLR", missing = "ML")
fit_lw  <- lavaan::cfa(dids, data = hearts_lw, estimator = "MLR", missing = "ML")
fit_rr  <- lavaan::cfa(dids, data = hearts_rr, estimator = "MLR", missing = "ML")
fit_sr  <- lavaan::cfa(dids, data = hearts_sr, estimator = "MLR", missing = "ML")

sink("./resources/DIDS_CFA/CFA_group_bridge_employment.txt")
  summary(fit_br, fit.measures = TRUE, standardized = TRUE)
  lavaan::fitted(fit_br)
  lavaan::residuals(fit_br, type = "raw")
  lavaan::residuals(fit_br, type = "standardized")
  lavaan::residuals(fit_br, type = "cor.bollen")
sink()

sink("./resources/DIDS_CFA/CFA_group_long_retired.txt")
  summary(fit_lr, fit.measures = TRUE, standardized = TRUE)
  lavaan::fitted(fit_lr)
  lavaan::residuals(fit_lr, type = "raw")
  lavaan::residuals(fit_lr, type = "standardized")
  lavaan::residuals(fit_lr, type = "cor.bollen")
sink()

sink("./resources/DIDS_CFA/CFA_group_long_working.txt")
  summary(fit_lw, fit.measures = TRUE, standardized = TRUE)
  lavaan::fitted(fit_lw)
  lavaan::residuals(fit_lw, type = "raw")
  lavaan::residuals(fit_lw, type = "standardized")
  lavaan::residuals(fit_lw, type = "cor.bollen")
sink()

sink("./resources/DIDS_CFA/CFA_group_recent_retired.txt")
  summary(fit_rr, fit.measures = TRUE, standardized = TRUE)
  lavaan::fitted(fit_br)
  lavaan::residuals(fit_rr, type = "raw")
  lavaan::residuals(fit_rr, type = "standardized")
  lavaan::residuals(fit_rr, type = "cor.bollen")
sink()

sink("./resources/DIDS_CFA/CFA_group_soon_retired.txt")
  summary(fit_sr, fit.measures = TRUE, standardized = TRUE)
  lavaan::fitted(fit_sr)
  lavaan::residuals(fit_sr, type = "raw")
  lavaan::residuals(fit_sr, type = "standardized")
  lavaan::residuals(fit_sr, type = "cor.bollen")
sink()

# Comparison of model fit across groups
compare_groups <- compareFit(fit_br, fit_lr, fit_lw, fit_rr, fit_sr)

sink("./resources/DIDS_CFA/CFA_groups_comparison.txt")
  summary(compare_groups, fit.measures = c(
    "cfi",
    "rmsea",
    "cfi.scaled",
    "rmsea.ci.lower",
    "rmsea.ci.upper",
    "cfi.robust",
    "rmsea.scaled",
    "rmsea.robust",
    "srmr"
    )
  )
sink()

# Modification indices
sink("./resources/DIDS_CFA/modindices.txt")
  modificationindices(fit_all, sort = T)
sink()

# Clear up environment
rm(list = ls())







