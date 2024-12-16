##########################################
###########08_CFA_CESD_DIDS###############
##########################################

# Load libraries
library('tidyverse')
library('lavaan')
library('jmv')

# Load data
hearts <- as_tibble(read.csv("./resources/data_files/hearts_clean.csv"))

# Model syntax
model_dids_cesd <-'
commitment_making =~ com_ma_1 + com_ma_2 + com_ma_3
identification_with_commitment =~ com_id_1 + com_id_2 + com_id_3
exploration_breadth =~ exp_br_1 + exp_br_2 + exp_br_3
exploration_depth =~ exp_de_2 + exp_de_3
ruminative_exploration =~ exp_ru_1 + exp_ru_2 + exp_ru_3

somatic_symptoms  =~ depr_1 + depr_3 + depr_4 + depr_9 + depr_10
negative_affect   =~ depr_2 + depr_6 + depr_8
anhedonia         =~ depr_5 + depr_7

depressive_symptoms =~ somatic_symptoms + negative_affect + anhedonia
'

# Fit model
fit_dids_cesd <- lavaan::cfa(
  model_dids_cesd,
  data = hearts,
  estimator = "MLR",
  missing = "ML")

# Model outputs
sink("./resources/DIDS_CESD_CFA/CFA_dids_cesd.txt")
  lavaan::summary(fit_dids_cesd, standardized = TRUE, fit.measures = TRUE)
  lavaan::fitted(fit_dids_cesd)
  lavaan::residuals(fit_dids_cesd, type = "raw")
  lavaan::residuals(fit_dids_cesd, type = "standardized")
  lavaan::residuals(fit_dids_cesd, type = "cor.bollen")
sink()

# Multivariate normality CES-D plus DIDS
hearts_cesd_dids <- hearts %>% select(
  depr_1, depr_2, depr_3, depr_4, depr_5, depr_6, depr_7, depr_8, depr_9,
  depr_10, com_ma_1, com_ma_2, com_ma_3, com_id_1, com_id_2, com_id_3, exp_br_1,
  exp_br_2,exp_br_3, exp_de_2 , exp_de_3, exp_ru_1, exp_ru_2, exp_ru_3)

sink("./resources/descriptive_statistics/dids14_cesd_mardia.txt")
mvn(hearts_cesd_dids, mvnTest = "mardia")
sink()

# Clear up environment
rm(list = ls())
