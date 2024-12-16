##########################################
###########05_CFA_DIDS_INVARIANCE#########
##########################################

# Load libraries
library('tidyverse')
library('lavaan')
library('semTools')

# Load data
hearts <- as_tibble(read.csv("./resources/data_files/hearts_clean.csv"))

hearts <- hearts[!is.na(hearts$retirement_status), ]

cfa_fit_measures = c(
  "cfi",
  "rmsea",
  "cfi.scaled",
  "cfi.robust",
  "rmsea.scaled",
  "rmsea.robust",
  "srmr"
  )

##########################################################################
##########################################################################
########## Configural invariance: unconstrained model ####################
##########################################################################
##########################################################################

# Model syntax -> paste from previous analysis (accepted factor structure)
dids_model <- '
commitment_making =~ com_ma_1 + com_ma_2 + com_ma_3

identification_with_commitment =~ com_id_1 + com_id_2 + com_id_3

exploration_breadth =~ exp_br_1 + exp_br_2 + exp_br_3

exploration_depth =~ exp_de_2 + exp_de_3

ruminative_exploration =~ exp_ru_1 + exp_ru_2 + exp_ru_3
'

# Fit unconstrained model
fit_unconstrained <- lavaan::cfa(
  dids_model,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status"
  )

# Extract information
sink("./resources/DIDS_CFA/CFA_invariance_configural.txt")
  summary(fit_unconstrained, fit.measures = TRUE, standardized = TRUE)
sink()


###########################################################################
###########################################################################
############### Test for weak measurement invariance ######################
###########################################################################
###########################################################################

# Fit weak invariance model
fit_weak <- lavaan::cfa(
  dids_model,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings")
  )

# Extract information
sink("./resources/DIDS_CFA/CFA_invariance_weak.txt")
  summary(fit_weak, fit.measures = TRUE, standardized = TRUE)
sink()

# Test for weak invariance
comp_weak <- compareFit(fit_unconstrained,
                        fit_weak
)

sink("CFA_invariance_comparison_weak.txt")
  summary(comp_weak, fit.measures = cfa_fit_measures)
sink()


##########################################################################
##########################################################################
###when weak / partial invariance is established: test strong invariance##
##########################################################################
##########################################################################

# Fit strong invariance model
fit_strong <- lavaan::cfa(
  dids_model,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts")
  )

# Extract information
sink("./resources/DIDS_CFA/CFA_invariance_strong.txt")
  summary(fit_strong, fit.measures = TRUE, standardized = TRUE)
sink()

# Compare models
comp_strong <- compareFit(fit_weak, fit_strong)

sink("./resources/DIDS_CFA/CFA_invariance_comparison_strong.txt")
  summary(comp_strong, fit.measures = cfa_fit_measures)
sink()

# Clear up environment
rm(list = ls())




