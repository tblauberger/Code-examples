##########################################
###########03_CFA_DIDS_STRUCTURE##########
##########################################

# Load libraries
library('tidyverse')
library('lavaan')
library('jmv')
library('semTools')

# Load data
hearts <- as_tibble(read.csv("./resources/data_files/hearts_clean.csv"))

# Definitions of models

# Model syntax with all 15 indicators
# Two factors: commitment and exploration
two_factors_15items <- '

commitment =~ com_ma_1 + com_ma_2 + com_ma_3 + com_id_1 + com_id_2 + com_id_3

exploration =~ exp_br_1 + exp_br_2 + exp_br_3 + exp_de_1 + exp_de_2 + exp_de_3 +
               exp_ru_1 + exp_ru_2 + exp_ru_3

'

# Three factors: commitment, reflective exploration, ruminative exploration
three_factors_15items <- '

commitment =~ com_ma_1 + com_ma_2 + com_ma_3 + com_id_1 + com_id_2 + com_id_3

reflective_exploration =~ exp_br_1 + exp_br_2 + exp_br_3 +
                          exp_de_1 + exp_de_2 + exp_de_3

ruminative exploration =~ exp_ru_1 + exp_ru_2 + exp_ru_3

'

# Five factors
five_factors_15items <- '

commitment_making =~ com_ma_1 + com_ma_2 + com_ma_3

identification_with_commitment =~ com_id_1 + com_id_2 + com_id_3

exploration_breadth =~ exp_br_1 + exp_br_2 + exp_br_3

exploration_depth =~ exp_de_1 + exp_de_2 + exp_de_3

ruminative_exploration =~ exp_ru_1 + exp_ru_2 + exp_ru_3

'

# Model syntax without dids_13 (14 items)
# Two factors: commitment and exploration
two_factors_14items <- '

commitment =~ com_ma_1 + com_ma_2 + com_ma_3 + com_id_1 + com_id_2 + com_id_3

exploration =~ exp_br_1 + exp_br_2 + exp_br_3 +  exp_de_2 + exp_de_3 +
               exp_ru_1 + exp_ru_2 + exp_ru_3

'

# Three factors: commitment, reflective exploration, ruminative exploration
three_factors_14items <- '

commitment =~ com_ma_1 + com_ma_2 + com_ma_3 + com_id_1 + com_id_2 + com_id_3

reflective_exploration =~ exp_br_1 + exp_br_2 + exp_br_3 +
                           exp_de_2 + exp_de_3

ruminative exploration =~ exp_ru_1 + exp_ru_2 + exp_ru_3

'

# Five factors
five_factors_14items <- '

commitment_making =~ com_ma_1 + com_ma_2 + com_ma_3

identification_with_commitment =~ com_id_1 + com_id_2 + com_id_3

exploration_breadth =~ exp_br_1 + exp_br_2 + exp_br_3

exploration_depth =~ exp_de_2 + exp_de_3

ruminative_exploration =~ exp_ru_1 + exp_ru_2 + exp_ru_3

'

# Model syntax without exploration in depth (12 items)
# Two factors: commitment and exploration
two_factors_12items <- '

commitment =~ com_ma_1 + com_ma_2 + com_ma_3 + com_id_1 + com_id_2 + com_id_3

exploration =~ exp_br_1 + exp_br_2 + exp_br_3 +
               exp_ru_1 + exp_ru_2 + exp_ru_3

'

# Three factors: commitment, reflective exploration, ruminative exploration
three_factors_12items <- '

commitment =~ com_ma_1 + com_ma_2 + com_ma_3 + com_id_1 + com_id_2 + com_id_3

reflective_exploration =~ exp_br_1 + exp_br_2 + exp_br_3


ruminative exploration =~ exp_ru_1 + exp_ru_2 + exp_ru_3

'

# Four factors
four_factors_12items <- '

commitment_making =~ com_ma_1 + com_ma_2 + com_ma_3

identification_with_commitment =~ com_id_1 + com_id_2 + com_id_3

exploration_breadth =~ exp_br_1 + exp_br_2 + exp_br_3

ruminative_exploration =~ exp_ru_1 + exp_ru_2 + exp_ru_3

'

# Fitting the models
fit_two_factors_15items <- lavaan::cfa(
  two_factors_15items,
  data = hearts,
  estimator = "MLR",
  missing = "ML"
  )


fit_three_factors_15items <- lavaan::cfa(
  three_factors_15items,
  data = hearts,
  estimator = "MLR",
  missing = "ML"
  )

fit_five_factors_15items <- lavaan::cfa(
  five_factors_15items,
  data = hearts,
  estimator = "MLR",
  missing = "ML"
  )

fit_two_factors_14items <- lavaan::cfa(
  two_factors_14items,
  data = hearts,
  estimator = "MLR",
  missing = "ML"
  )

fit_three_factors_14items <- lavaan::cfa(
  three_factors_14items,
  data = hearts,
  estimator = "MLR",
  missing = "ML"
  )

fit_five_factors_14items <- lavaan::cfa(
  five_factors_14items,
  data = hearts,
  estimator = "MLR",
  missing = "ML"
  )

fit_two_factors_12items <- lavaan::cfa(
  two_factors_12items,
  data = hearts,
  estimator = "MLR",
  missing = "ML"
  )

fit_three_factors_12items <- lavaan::cfa(
  three_factors_12items,
  data = hearts,
  estimator = "MLR",
  missing = "ML"
  )

fit_four_factors_12items <- lavaan::cfa(
  four_factors_12items,
  data = hearts,
  estimator = "MLR",
  missing = "ML"
  )

# Extract information
sink("./resources/DIDS_CFA/CFA_dids_two_factors_15items.txt")
  summary(fit_two_factors_15items, fit.measures = TRUE, standardized = TRUE)
sink()

sink("./resources/DIDS_CFA/CFA_dids_three_factors_15items.txt")
  summary(fit_three_factors_15items, fit.measures = TRUE, standardized = TRUE)
sink()

sink("./resources/DIDS_CFA/CFA_dids_five_factors_15items.txt")
  summary(fit_five_factors_15items, fit.measures = TRUE, standardized = TRUE)
sink()

sink("./resources/DIDS_CFA/CFA_dids_two_factors_14items.txt")
  summary(fit_two_factors_14items, fit.measures = TRUE, standardized = TRUE)
sink()

sink("./resources/DIDS_CFA/CFA_dids_three_factors_14items.txt")
  summary(fit_three_factors_14items, fit.measures = TRUE, standardized = TRUE)
sink()

sink("./resources/DIDS_CFA/CFA_dids_five_factors_14items.txt")
  summary(fit_five_factors_14items, fit.measures = TRUE, standardized = TRUE)
sink()

sink("./resources/DIDS_CFA/CFA_dids_two_factors_12items.txt")
  summary(fit_two_factors_12items, fit.measures = TRUE, standardized = TRUE)
sink()

sink("./resources/DIDS_CFA/CFA_dids_three_factors_12items.txt")
  summary(fit_three_factors_12items, fit.measures = TRUE, standardized = TRUE)
sink()

sink("./resources/DIDS_CFA/CFA_dids_four_factors_12items.txt")
  summary(fit_four_factors_12items, fit.measures = TRUE, standardized = TRUE)
sink()

# Comparing models with all indicators
comp_15items <- compareFit(
  fit_two_factors_15items,
  fit_three_factors_15items,
  fit_five_factors_15items
)

# Comparing models with 14 indicators
comp_14items <- compareFit(
  fit_two_factors_14items,
  fit_three_factors_14items,
  fit_five_factors_14items
)

# Comparing models with 12 indicators
comp_12items <- compareFit(
  fit_two_factors_12items,
  fit_three_factors_12items,
  fit_four_factors_12items
)

# Comparing all models
comp_all <- compareFit(
  fit_two_factors_15items,
  fit_three_factors_15items,
  fit_five_factors_15items,
  fit_two_factors_14items,
  fit_three_factors_14items,
  fit_five_factors_14items,
  fit_two_factors_12items,
  fit_three_factors_12items,
  fit_four_factors_12items
)

# Print summaries into files

cfa_fit_measures = c(
  "cfi",
  "rmsea",
  "cfi.scaled",
  "cfi.robust",
  "rmsea.scaled",
  "rmsea.robust",
  "srmr")

sink("CFA_dids_comparison_15_items.txt")
  summary(comp_15items, fit.measures = cfa_fit_measures)
sink()

sink("CFA_dids_comparison_14items.txt")
  summary(comp_14items, fit.measures = cfa_fit_measures)
sink()

sink("CFA_dids_comparison_12items.txt")
  summary(comp_12items, fit.measures = cfa_fit_measures)
sink()

sink("./resources/DIDS_CFA/CFA_dids_comparison_all.txt")
  summary(comp_all,fit.measures = cfa_fit_measures)
sink()

# Internal reliability: According to factor structure decided on (14 items, 5 factors)
rel_commitment_making <-  hearts %>%
  select(com_ma_1,com_ma_2,com_ma_3) %>%
  jmv::reliability(omegaScale = TRUE, omegaItems = TRUE)

rel_identification_with_commitment <-  hearts %>%
  select(com_id_1, com_id_2 , com_id_3) %>%
  jmv::reliability(omegaScale = TRUE, omegaItems = TRUE)

rel_exploration_breadth <-  hearts %>%
  select(exp_br_1, exp_br_2, exp_br_3) %>%
  jmv::reliability(omegaScale = TRUE, omegaItems = TRUE)

rel_exploration_depth <-  hearts %>%
  select(exp_de_2 , exp_de_3) %>%
  jmv::reliability(omegaScale = TRUE, omegaItems = TRUE)

rel_ruminative_exploration <-  hearts %>%
  select(exp_ru_1, exp_ru_2, exp_ru_3) %>%
  jmv::reliability(omegaScale = TRUE, omegaItems = TRUE)

sink("./resources/descriptive_statistics/DIDS_internal_reliability_CFA_solution.txt")
  print("Commitment Making:")
  rel_commitment_making
  print("Identification with commitment:")
  rel_identification_with_commitment
  print("Exploration in breadth")
  rel_exploration_breadth
  print("Exploration in depth ")
  rel_exploration_depth
  print("Ruminative exploration")
  rel_ruminative_exploration
sink()

# Clear up environment
rm(list = ls() )
