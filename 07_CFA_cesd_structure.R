##########################################
###########07_CFA_CESD_STRUCTURE##########
##########################################

# Load libraries
library('tidyverse')
library('lavaan')
library('jmv')

# Load data
hearts <- as_tibble(read.csv("./resources/data_files/hearts_clean.csv"))

# CFA syntax one factor########
model_one <-'
depression =~ depr_1 + depr_2 + depr_3 + depr_4 + depr_5 + depr_6 +
              depr_7 + depr_8 + depr_9 + depr_10
'

# CFA syntax three factors#####
model_three <-'
somatic_symptoms  =~ depr_1 + depr_3 + depr_4 + depr_9 + depr_10
negative_affect   =~ depr_2 + depr_6 + depr_8
anhedonia         =~ depr_5 + depr_7
'

# CFA fitting
fit_one <- lavaan::cfa(model_one, data = hearts, estimator = "MLR", missing = "ML")
fit_three <- lavaan::cfa(model_three, data = hearts,  estimator = "MLR", missing = "ML")

# Extract results
sum_one <- summary(fit_one,fit.measures = TRUE, standardized = TRUE)
sum_three <- summary(fit_three,fit.measures = TRUE, standardized = TRUE)

# Write results into txt-files
sink("./resources/CESD_CFA/CFA_cesd_structure_one.txt")
  sum_one
sink()

sink("./resources/CESD_CFA/CFA_cesd_structure_three.txt")
  sum_three
sink()

# Create and save plots
pdf("./resources/CESD_CFA/CFA_cesd_structure.pdf")
  semPaths(fit_one,what = "paths", whatLabels = 'std')
  semPaths(fit_three,what = "paths", whatLabels = 'std')
dev.off()

# Internal reliability measures
rel_cesd_somatic_symptoms <-  hearts %>%
  select(depr_1, depr_3, depr_4, depr_9, depr_10) %>%
  jmv::reliability(omegaScale = TRUE, omegaItems = TRUE)

rel_cesd_negative_affect <-  hearts %>% select(depr_2,  depr_6, depr_8) %>%
  jmv::reliability(omegaScale = TRUE, omegaItems = TRUE)

rel_cesd_anhedonia <-  hearts %>% select(depr_5, depr_7) %>%
  jmv::reliability(omegaScale = TRUE, omegaItems = TRUE)

# Save internal reliability into file
sink("./resources/descriptive_statistics/intrnal_reliability_cesd_CFA.txt")
  print('Somatic symptoms:')
  rel_cesd_somatic_symptoms
  print('Negative afecct:')
  rel_cesd_negative_affect
  print('Anhedonia:')
  rel_cesd_anhedonia
sink()

# Clear up environment
rm(list = ls())
