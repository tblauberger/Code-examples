##########################################
###########06_LATENT_MEANS################
##########################################

library('tidyverse')
library('lavaan')
library('semTools')

# Load data
hearts <- as_tibble(read.csv("hearts_clean.csv"))

hearts <- hearts[!is.na(hearts$retirement_status), ]

fit_measures = c(
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

# Model syntax (baseline): No constraints on latent means
model_baseline <- '
commitment_making =~ com_ma_1 + com_ma_2 + com_ma_3

identification_with_commitment =~ com_id_1 + com_id_2 + com_id_3

exploration_breadth =~ exp_br_1 + exp_br_2 + exp_br_3

exploration_depth =~ exp_de_2 + exp_de_3

ruminative_exploration =~ exp_ru_1 + exp_ru_2 + exp_ru_3

'

# Fit (baseline model)  with effect coding
fit_baseline <- lavaan::cfa(
  model_baseline,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
  )

# Output baseline model
sink("baseline_effect_coded.txt")
  lavaan::summary(fit_baseline, standardized = TRUE, fit.measures = TRUE)
  lavaan::fitted(fit_baseline)
  lavaan::residuals(fit_baseline, type = "raw")
  lavaan::residuals(fit_baseline, type = "standardized")
  lavaan::residuals(fit_baseline, type = "cor.bollen")
sink()

# Omnibus tests
# Commitment making
cm_omni_constraints <-'commitment_making ~ c(g,g,g,g,g)*1'

cm_omni_model <- paste(model_baseline, cm_omni_constraints)

fit_cm_omni <- lavaan::cfa(
  cm_omni_model,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
  )

# Comparison vs baseline model
comp_cm_omni <- compareFit(fit_baseline, fit_cm_omni)

# Identification_with_commitment
ic_omni_constraints <-'identification_with_commitment ~ c(g,g,g,g,g)*1'

ic_omni_model <- paste(model_baseline, ic_omni_constraints)

fit_ic_omni <- lavaan::cfa(
  ic_omni_model,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
  )

# Comparison vs baseline model
comp_ic_omni <- compareFit(fit_baseline, fit_ic_omni)

# Exploration in breadth
eb_omni_constraints <-'exploration_breadth ~ c(g,g,g,g,g)*1'

eb_omni_model <- paste(model_baseline, eb_omni_constraints)

fit_eb_omni <- lavaan::cfa(
  eb_omni_model,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
  )

# Comparison vs baseline model
comp_eb_omni <- compareFit(fit_baseline, fit_eb_omni)

# Exploration in depth
ed_omni_constraints <-'exploration_depth ~ c(g,g,g,g,g)*1'

ed_omni_model <- paste(model_baseline, ed_omni_constraints)

fit_ed_omni <- lavaan::cfa(
  ed_omni_model,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
  )

# Comparison vs baseline model
comp_ed_omni <- compareFit(fit_baseline, fit_ed_omni)


# Ruminative exploration
re_omni_constraints <-'ruminative_exploration ~ c(g,g,g,g,g)*1'

re_omni_model <- paste(model_baseline, re_omni_constraints)
fit_re_omni <- lavaan::cfa(
  re_omni_model,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
  )

# Comparison vs baseline model
comp_re_omni <- compareFit(fit_baseline, fit_re_omni)


sink("omnibus_tests.txt")
  summary(comp_cm_omni, fit.measures = fit_measures)
  summary(comp_ic_omni, fit.measures = fit_measures)
  summary(comp_eb_omni, fit.measures = fit_measures)
  summary(comp_ed_omni, fit.measures = fit_measures)
  summary(comp_re_omni, fit.measures = fit_measures)
sink()


#######################################################################################################

# Pairwise comparisons

# Commitment making
# 1) Soon retired vs. Long retired
# Constraints for each comparison
cm1 <- paste(model_baseline, 'commitment_making ~ c(z,z,a,b,c)*1')
cm2 <- paste(model_baseline, 'commitment_making ~ c(z,a,z,b,c)*1')
cm3 <- paste(model_baseline, 'commitment_making ~ c(z,b,a,z,c)*1')
cm4 <- paste(model_baseline, 'commitment_making ~ c(z,c,a,b,z)*1')
cm5 <- paste(model_baseline, 'commitment_making ~ c(a,z,z,b,c)*1')
cm6 <- paste(model_baseline, 'commitment_making ~ c(a,z,b,z,c)*1')
cm7 <- paste(model_baseline, 'commitment_making ~ c(a,z,c,b,z)*1')
cm8 <- paste(model_baseline, 'commitment_making ~ c(a,b,z,z,c)*1')
cm9 <- paste(model_baseline, 'commitment_making ~ c(a,b,z,c,z)*1')
cm10 <- paste(model_baseline, 'commitment_making ~ c(a,b,c,z,z)*1')

# Fitting each constrained model
fit_cm1 <- lavaan::cfa(
  cm1,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)

fit_cm2 <- lavaan::cfa(
  cm2,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)

fit_cm3 <- lavaan::cfa(
  cm3,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)

fit_cm4 <- lavaan::cfa(
  cm4,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)

fit_cm5 <- lavaan::cfa(
  cm5,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)

fit_cm6 <- lavaan::cfa(
  cm6,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)

fit_cm7 <- lavaan::cfa(
  cm7,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)

fit_cm8 <- lavaan::cfa(
  cm8,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)

fit_cm9 <- lavaan::cfa(
  cm9,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)

fit_cm10 <- lavaan::cfa(
  cm10,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)

# Comparison of each model to baseline model
comp_cm1 <- compareFit(fit_baseline, fit_cm1)
comp_cm2 <- compareFit(fit_baseline, fit_cm2)
comp_cm3 <- compareFit(fit_baseline, fit_cm3)
comp_cm4 <- compareFit(fit_baseline, fit_cm4)
comp_cm5 <- compareFit(fit_baseline, fit_cm5)
comp_cm6 <- compareFit(fit_baseline, fit_cm6)
comp_cm7 <- compareFit(fit_baseline, fit_cm7)
comp_cm8 <- compareFit(fit_baseline, fit_cm8)
comp_cm9 <- compareFit(fit_baseline, fit_cm9)
comp_cm10 <- compareFit(fit_baseline, fit_cm10)

# Cohen's d for each comparison
# Extract latent means and variances for each group
intercept_cm_group_1 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      1) %>%
  filter(op == '~1') %>% filter(lhs == 'commitment_making') %>% select(est)
variance_cm_group_1 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     1) %>%
  filter(op == '~~') %>% filter(lhs == 'commitment_making') %>%
  filter(rhs == 'commitment_making') %>% select(est)
SD_cm_group_1 <- sqrt(variance_cm_group_1)

intercept_cm_group_2 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      2) %>%
  filter(op == '~1') %>% filter(lhs == 'commitment_making') %>% select(est)
variance_cm_group_2 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     2) %>%
  filter(op == '~~') %>% filter(lhs == 'commitment_making') %>%
  filter(rhs == 'commitment_making') %>% select(est)
SD_cm_group_2 <- sqrt(variance_cm_group_2)

intercept_cm_group_3 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      3) %>%
  filter(op == '~1') %>% filter(lhs == 'commitment_making') %>% select(est)
variance_cm_group_3 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     3) %>%
  filter(op == '~~') %>% filter(lhs == 'commitment_making') %>%
  filter(rhs == 'commitment_making') %>% select(est)
SD_cm_group_3 <- sqrt(variance_cm_group_3)

intercept_cm_group_4 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      4) %>%
  filter(op == '~1') %>% filter(lhs == 'commitment_making') %>% select(est)
variance_cm_group_4 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     4) %>%
  filter(op == '~~') %>% filter(lhs == 'commitment_making') %>%
  filter(rhs == 'commitment_making') %>% select(est)
SD_cm_group_4 <- sqrt(variance_cm_group_4)

intercept_cm_group_5 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      5) %>%
  filter(op == '~1') %>% filter(lhs == 'commitment_making') %>% select(est)
variance_cm_group_5 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     5) %>%
  filter(op == '~~') %>% filter(lhs == 'commitment_making') %>%
  filter(rhs == 'commitment_making') %>% select(est)
SD_cm_group_5 <- sqrt(variance_cm_group_5)

# Calculate Cohens d
d_cm1_1 <- (intercept_cm_group_1 - intercept_cm_group_2) / SD_cm_group_1
d_cm1_2 <- (intercept_cm_group_1 - intercept_cm_group_2) / SD_cm_group_2

d_cm2_1 <- (intercept_cm_group_1 - intercept_cm_group_3) / SD_cm_group_1
d_cm2_3 <- (intercept_cm_group_1 - intercept_cm_group_3) / SD_cm_group_3

d_cm3_1 <- (intercept_cm_group_1 - intercept_cm_group_4) / SD_cm_group_1
d_cm3_4 <- (intercept_cm_group_1 - intercept_cm_group_4) / SD_cm_group_4

d_cm4_1 <- (intercept_cm_group_1 - intercept_cm_group_5) / SD_cm_group_1
d_cm4_5 <- (intercept_cm_group_1 - intercept_cm_group_5) / SD_cm_group_5

d_cm5_2 <- (intercept_cm_group_2 - intercept_cm_group_3) / SD_cm_group_2
d_cm5_3 <- (intercept_cm_group_2 - intercept_cm_group_3) / SD_cm_group_3

d_cm6_2 <- (intercept_cm_group_2 - intercept_cm_group_4) / SD_cm_group_2
d_cm6_4 <- (intercept_cm_group_2 - intercept_cm_group_4) / SD_cm_group_4

d_cm7_2 <- (intercept_cm_group_2 - intercept_cm_group_5) / SD_cm_group_2
d_cm7_5 <- (intercept_cm_group_2 - intercept_cm_group_5) / SD_cm_group_5

d_cm8_3 <- (intercept_cm_group_3 - intercept_cm_group_4) / SD_cm_group_3
d_cm8_4 <- (intercept_cm_group_3 - intercept_cm_group_4) / SD_cm_group_4

d_cm9_3 <- (intercept_cm_group_3 - intercept_cm_group_5) / SD_cm_group_3
d_cm9_5 <- (intercept_cm_group_3 - intercept_cm_group_5) / SD_cm_group_5

d_cm10_4 <- (intercept_cm_group_4 - intercept_cm_group_5) / SD_cm_group_4
d_cm10_5 <- (intercept_cm_group_4 - intercept_cm_group_5) / SD_cm_group_5

# Print comparisons into .txt file
# Order of comparisons adjusted to desired order in output
# Effect sizes that are multiplied with -1 (*-1) have been reversed to fit the
# Order of the groups in the output

sink("pairwise_commitment_making.txt")

print("(1) Soon retired vs Long working")
summary(
  comp_cm4,
  fit.measures = fit_measures
)
print("(1) Cohen's d - Soon retired vs Long working - SD 1")
d_cm4_1
print("(1) Cohen's d - Soon retired vs Long working - SD 2")
d_cm4_5

print("(2) Bridge employment vs. Long working")
summary(
  comp_cm10,
  fit.measures = fit_measures
)
print("(2) Cohen's d - Bridge employment vs. Long working - SD 1")
d_cm10_4
print("(2) Cohen's d - Bridge employment vs. Long working - SD 2")
d_cm10_5

print("(3) Recently retired vs Long working")
summary(
  comp_cm9,
  fit.measures = fit_measures
)
print("(3) Cohen's d - Recently retired vs Long working - SD 1")
d_cm9_3
print("(3) Cohen's d - Recently retired vs Long working - SD 2")
d_cm9_5

print("(4) Long retired vs Long working")
summary(
  comp_cm7,
  fit.measures = fit_measures
)
print("(4) Cohen's d - Long retired vs Long working - SD 1")
d_cm7_2
print("(4) Cohen's d - Long retired vs Long working - SD 2")
d_cm7_5

print("(5) Bridge employment vs Soon retired")
summary(
  comp_cm3,
  fit.measures = fit_measures
)
print("(5) Cohen's d - Bridge employment vs Soon retired - SD 1")
d_cm3_1 * -1
print("(5) Cohen's d - Bridge employment vs Soon retired - SD 2")
d_cm3_4 * -1

print("(6) Recently retired vs Soon retired ")
summary(
  comp_cm2,
  fit.measures = fit_measures
)
print("(6) Cohen's d - Recently retired vs Soon retired - SD 1")
d_cm2_1 * -1
print("(6) Cohen's d - Recently retired vs Soon retired - SD 2")
d_cm2_3 * -1

print("(7) Long retired vs Soon retired ")
summary(
  comp_cm1,
  fit.measures = fit_measures
)
print("(7) Cohen's d - Long retired vs Soon retired - SD 1")
d_cm1_1 * -1
print("(7) Cohen's d - Long retired vs Soon retired - SD 2")
d_cm1_2 * -1


print("(8) Recently retired vs Bridge employment")
summary(
  comp_cm8,
  fit.measures = fit_measures
)
print("(8) Cohen's d - Recently retired vs Bridge employment - SD 1")
d_cm8_3
print("(8) Cohen's d - Recently retired vs Bridge employment - SD 2")
d_cm8_4


print("(9) Long retired vs Bridge employment")
summary(
  comp_cm6,
  fit.measures = fit_measures
)
print("(9) Cohen's d - Long retired vs Bridge employment - SD 1")
d_cm6_2
print("(9) Cohen's d - Long retired vs Bridge employment - SD 2")
d_cm6_4

print("(10) Long retired vs recently retired")
summary(
  comp_cm5,
  fit.measures = fit_measures
)
print("(10) Cohen's d - Long retired vs recently retired - SD 1")
d_cm5_2
print("(10) Cohen's d - Long retired vs recently retired - SD 2")
d_cm5_3


sink()

# Exploration in breadth
# Constraints for each comparison
eb1 <- paste(model_baseline, 'exploration_breadth ~ c(z,z,a,b,c)*1')
eb2 <- paste(model_baseline, 'exploration_breadth ~ c(z,a,z,b,c)*1')
eb3 <- paste(model_baseline, 'exploration_breadth ~ c(z,b,a,z,c)*1')
eb4 <- paste(model_baseline, 'exploration_breadth ~ c(z,c,a,b,z)*1')
eb5 <- paste(model_baseline, 'exploration_breadth ~ c(a,z,z,b,c)*1')
eb6 <- paste(model_baseline, 'exploration_breadth ~ c(a,z,b,z,c)*1')
eb7 <- paste(model_baseline, 'exploration_breadth ~ c(a,z,c,b,z)*1')
eb8 <- paste(model_baseline, 'exploration_breadth ~ c(a,b,z,z,c)*1')
eb9 <- paste(model_baseline, 'exploration_breadth ~ c(a,b,z,c,z)*1')
eb10 <- paste(model_baseline, 'exploration_breadth ~ c(a,b,c,z,z)*1')

#Fitting each constrained model
fit_eb1 <- lavaan::cfa(
  eb1,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)

fit_eb2 <- lavaan::cfa(
  eb2,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_eb3 <- lavaan::cfa(
  eb3,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_eb4 <- lavaan::cfa(
  eb4,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_eb5 <- lavaan::cfa(
  eb5,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_eb6 <- lavaan::cfa(
  eb6,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_eb7 <- lavaan::cfa(
  eb7,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_eb8 <- lavaan::cfa(
  eb8,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_eb9 <- lavaan::cfa(
  eb9,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_eb10 <- lavaan::cfa(
  eb10,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)

# Comparison of each model to baseline model
comp_eb1 <- compareFit(fit_baseline, fit_eb1)
comp_eb2 <- compareFit(fit_baseline, fit_eb2)
comp_eb3 <- compareFit(fit_baseline, fit_eb3)
comp_eb4 <- compareFit(fit_baseline, fit_eb4)
comp_eb5 <- compareFit(fit_baseline, fit_eb5)
comp_eb6 <- compareFit(fit_baseline, fit_eb6)
comp_eb7 <- compareFit(fit_baseline, fit_eb7)
comp_eb8 <- compareFit(fit_baseline, fit_eb8)
comp_eb9 <- compareFit(fit_baseline, fit_eb9)
comp_eb10 <- compareFit(fit_baseline, fit_eb10)


# Cohen's d for each comparison
# Extract latent means and variances for each group
intercept_eb_group_1 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      1) %>%
  filter(op == '~1') %>% filter(lhs == 'exploration_breadth') %>% select(est)
variance_eb_group_1 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     1) %>%
  filter(op == '~~') %>% filter(lhs == 'exploration_breadth') %>%
  filter(rhs == 'exploration_breadth') %>% select(est)
SD_eb_group_1 <- sqrt(variance_eb_group_1)

intercept_eb_group_2 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      2) %>%
  filter(op == '~1') %>% filter(lhs == 'exploration_breadth') %>% select(est)
variance_eb_group_2 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     2) %>%
  filter(op == '~~') %>% filter(lhs == 'exploration_breadth') %>%
  filter(rhs == 'exploration_breadth') %>% select(est)
SD_eb_group_2 <- sqrt(variance_eb_group_2)

intercept_eb_group_3 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      3) %>%
  filter(op == '~1') %>% filter(lhs == 'exploration_breadth') %>% select(est)
variance_eb_group_3 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     3) %>%
  filter(op == '~~') %>% filter(lhs == 'exploration_breadth') %>%
  filter(rhs == 'exploration_breadth') %>% select(est)
SD_eb_group_3 <- sqrt(variance_eb_group_3)

intercept_eb_group_4 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      4) %>%
  filter(op == '~1') %>% filter(lhs == 'exploration_breadth') %>% select(est)
variance_eb_group_4 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     4) %>%
  filter(op == '~~') %>% filter(lhs == 'exploration_breadth') %>%
  filter(rhs == 'exploration_breadth') %>% select(est)
SD_eb_group_4 <- sqrt(variance_eb_group_4)

intercept_eb_group_5 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      5) %>%
  filter(op == '~1') %>% filter(lhs == 'exploration_breadth') %>% select(est)
variance_eb_group_5 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     5) %>%
  filter(op == '~~') %>% filter(lhs == 'exploration_breadth') %>%
  filter(rhs == 'exploration_breadth') %>% select(est)
SD_eb_group_5 <- sqrt(variance_eb_group_5)

# Calculate Cohens d
d_eb1_1 <- (intercept_eb_group_1 - intercept_eb_group_2) / SD_eb_group_1
d_eb1_2 <- (intercept_eb_group_1 - intercept_eb_group_2) / SD_eb_group_2

d_eb2_1 <- (intercept_eb_group_1 - intercept_eb_group_3) / SD_eb_group_1
d_eb2_3 <- (intercept_eb_group_1 - intercept_eb_group_3) / SD_eb_group_3

d_eb3_1 <- (intercept_eb_group_1 - intercept_eb_group_4) / SD_eb_group_1
d_eb3_4 <- (intercept_eb_group_1 - intercept_eb_group_4) / SD_eb_group_4

d_eb4_1 <- (intercept_eb_group_1 - intercept_eb_group_5) / SD_eb_group_1
d_eb4_5 <- (intercept_eb_group_1 - intercept_eb_group_5) / SD_eb_group_5

d_eb5_2 <- (intercept_eb_group_2 - intercept_eb_group_3) / SD_eb_group_2
d_eb5_3 <- (intercept_eb_group_2 - intercept_eb_group_3) / SD_eb_group_3

d_eb6_2 <- (intercept_eb_group_2 - intercept_eb_group_4) / SD_eb_group_2
d_eb6_4 <- (intercept_eb_group_2 - intercept_eb_group_4) / SD_eb_group_4

d_eb7_2 <- (intercept_eb_group_2 - intercept_eb_group_5) / SD_eb_group_2
d_eb7_5 <- (intercept_eb_group_2 - intercept_eb_group_5) / SD_eb_group_5

d_eb8_3 <- (intercept_eb_group_3 - intercept_eb_group_4) / SD_eb_group_3
d_eb8_4 <- (intercept_eb_group_3 - intercept_eb_group_4) / SD_eb_group_4

d_eb9_3 <- (intercept_eb_group_3 - intercept_eb_group_5) / SD_eb_group_3
d_eb9_5 <- (intercept_eb_group_3 - intercept_eb_group_5) / SD_eb_group_5

d_eb10_4 <- (intercept_eb_group_4 - intercept_eb_group_5) / SD_eb_group_4
d_eb10_5 <- (intercept_eb_group_4 - intercept_eb_group_5) / SD_eb_group_5


# Print comparisons into .txt file
# Order of comparisons adjusted to desired order in output
# Effect sizes that are multiplied with -1 (*-1) have been reversed to fit the
# Order of the groups in the output

sink("pairwise_exploration_breadth.txt")

print("(1) Soon retired vs Long working")
summary(
  comp_eb4,
  fit.measures = fit_measures
)
print("(1) Cohen's d - Soon retired vs Long working - SD 1")
d_eb4_1
print("(1) Cohen's d - Soon retired vs Long working - SD 2")
d_eb4_5


print("(2) Bridge employment vs. Long working")
summary(
  comp_eb10,
  fit.measures = fit_measures
)
print("(2) Cohen's d - Bridge employment vs. Long working - SD 1")
d_eb10_4
print("(2) Cohen's d - Bridge employment vs. Long working - SD 2")
d_eb10_5


print("(3) Recently retired vs Long working")
summary(
  comp_eb9,
  fit.measures = fit_measures
)
print("(3) Cohen's d - Recently retired vs Long working - SD 1")
d_eb9_3
print("(3) Cohen's d - Recently retired vs Long working - SD 2")
d_eb9_5


print("(4) Long retired vs Long working")
summary(
  comp_eb7,
  fit.measures = fit_measures
)
print("(4) Cohen's d - Long retired vs Long working - SD 1")
d_eb7_2
print("(4) Cohen's d - Long retired vs Long working - SD 2")
d_eb7_5


print("(5) Bridge employment vs Soon retired")
summary(
  comp_eb3,
  fit.measures = fit_measures
)
print("(5) Cohen's d - Bridge employment vs Soon retired - SD 1")
d_eb3_1 * -1
print("(5) Cohen's d - Bridge employment vs Soon retired - SD 2")
d_eb3_4 * -1


print("(6) Recently retired vs Soon retired ")
summary(
  comp_eb2,
  fit.measures = fit_measures
)
print("(6) Cohen's d - Recently retired vs Soon retired - SD 1")
d_eb2_1 * -1
print("(6) Cohen's d - Recently retired vs Soon retired - SD 2")
d_eb2_3 * -1



print("(7) Long retired vs Soon retired ")
summary(
  comp_eb1,
  fit.measures = fit_measures
)
print("(7) Cohen's d - Long retired vs Soon retired - SD 1")
d_eb1_1 * -1
print("(7) Cohen's d - Long retired vs Soon retired - SD 2")
d_eb1_2 * -1


print("(8) Recently retired vs Bridge employment")
summary(
  comp_eb8,
  fit.measures = fit_measures
)
print("(8) Cohen's d - Recently retired vs Bridge employment - SD 1")
d_eb8_3
print("(8) Cohen's d - Recently retired vs Bridge employment - SD 2")
d_eb8_4



print("(9) Long retired vs Bridge employment")
summary(
  comp_eb6,
  fit.measures = fit_measures
)
print("(9) Cohen's d - Long retired vs Bridge employment - SD 1")
d_eb6_2
print("(9) Cohen's d - Long retired vs Bridge employment - SD 2")
d_eb6_4


print("(10) Long retired vs recently retired")
summary(
  comp_eb5,
  fit.measures = fit_measures
)
print("(10) Cohen's d - Long retired vs recently retired - SD 1")
d_eb5_2
print("(10) Cohen's d - Long retired vs recently retired - SD 2")
d_eb5_3


sink()

# Exploration in depth
# Constraints for each comparison
ed1 <- paste(model_baseline, 'exploration_depth ~ c(z,z,a,b,c)*1')
ed2 <- paste(model_baseline, 'exploration_depth ~ c(z,a,z,b,c)*1')
ed3 <- paste(model_baseline, 'exploration_depth ~ c(z,b,a,z,c)*1')
ed4 <- paste(model_baseline, 'exploration_depth ~ c(z,c,a,b,z)*1')
ed5 <- paste(model_baseline, 'exploration_depth ~ c(a,z,z,b,c)*1')
ed6 <- paste(model_baseline, 'exploration_depth ~ c(a,z,b,z,c)*1')
ed7 <- paste(model_baseline, 'exploration_depth ~ c(a,z,c,b,z)*1')
ed8 <- paste(model_baseline, 'exploration_depth ~ c(a,b,z,z,c)*1')
ed9 <- paste(model_baseline, 'exploration_depth ~ c(a,b,z,c,z)*1')
ed10 <- paste(model_baseline, 'exploration_depth ~ c(a,b,c,z,z)*1')

#Fitting each constrained model
fit_ed1 <- lavaan::cfa(
  ed1,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)

fit_ed2 <- lavaan::cfa(
  ed2,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_ed3 <- lavaan::cfa(
  ed3,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_ed4 <- lavaan::cfa(
  ed4,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_ed5 <- lavaan::cfa(
  ed5,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_ed6 <- lavaan::cfa(
  ed6,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_ed7 <- lavaan::cfa(
  ed7,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_ed8 <- lavaan::cfa(
  ed8,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_ed9 <- lavaan::cfa(
  ed9,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_ed10 <- lavaan::cfa(
  ed10,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)

# Comparison of each model to baseline model
comp_ed1 <- compareFit(fit_baseline, fit_ed1)
comp_ed2 <- compareFit(fit_baseline, fit_ed2)
comp_ed3 <- compareFit(fit_baseline, fit_ed3)
comp_ed4 <- compareFit(fit_baseline, fit_ed4)
comp_ed5 <- compareFit(fit_baseline, fit_ed5)
comp_ed6 <- compareFit(fit_baseline, fit_ed6)
comp_ed7 <- compareFit(fit_baseline, fit_ed7)
comp_ed8 <- compareFit(fit_baseline, fit_ed8)
comp_ed9 <- compareFit(fit_baseline, fit_ed9)
comp_ed10 <- compareFit(fit_baseline, fit_ed10)


# Cohen's d for each comparison
# Extract latent means and variances for each group
intercept_ed_group_1 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      1) %>%
  filter(op == '~1') %>% filter(lhs == 'exploration_depth') %>% select(est)
variance_ed_group_1 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     1) %>%
  filter(op == '~~') %>% filter(lhs == 'exploration_depth') %>%
  filter(rhs == 'exploration_depth') %>% select(est)
SD_ed_group_1 <- sqrt(variance_ed_group_1)

intercept_ed_group_2 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      2) %>%
  filter(op == '~1') %>% filter(lhs == 'exploration_depth') %>% select(est)
variance_ed_group_2 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     2) %>%
  filter(op == '~~') %>% filter(lhs == 'exploration_depth') %>%
  filter(rhs == 'exploration_depth') %>% select(est)
SD_ed_group_2 <- sqrt(variance_ed_group_2)

intercept_ed_group_3 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      3) %>%
  filter(op == '~1') %>% filter(lhs == 'exploration_depth') %>% select(est)
variance_ed_group_3 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     3) %>%
  filter(op == '~~') %>% filter(lhs == 'exploration_depth') %>%
  filter(rhs == 'exploration_depth') %>% select(est)
SD_ed_group_3 <- sqrt(variance_ed_group_3)

intercept_ed_group_4 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      4) %>%
  filter(op == '~1') %>% filter(lhs == 'exploration_depth') %>% select(est)
variance_ed_group_4 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     4) %>%
  filter(op == '~~') %>% filter(lhs == 'exploration_depth') %>%
  filter(rhs == 'exploration_depth') %>% select(est)
SD_ed_group_4 <- sqrt(variance_ed_group_4)

intercept_ed_group_5 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      5) %>%
  filter(op == '~1') %>% filter(lhs == 'exploration_depth') %>% select(est)
variance_ed_group_5 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     5) %>%
  filter(op == '~~') %>% filter(lhs == 'exploration_depth') %>%
  filter(rhs == 'exploration_depth') %>% select(est)
SD_ed_group_5 <- sqrt(variance_ed_group_5)

# Calculate Cohens d
d_ed1_1 <- (intercept_ed_group_1 - intercept_ed_group_2) / SD_ed_group_1
d_ed1_2 <- (intercept_ed_group_1 - intercept_ed_group_2) / SD_ed_group_2

d_ed2_1 <- (intercept_ed_group_1 - intercept_ed_group_3) / SD_ed_group_1
d_ed2_3 <- (intercept_ed_group_1 - intercept_ed_group_3) / SD_ed_group_3

d_ed3_1 <- (intercept_ed_group_1 - intercept_ed_group_4) / SD_ed_group_1
d_ed3_4 <- (intercept_ed_group_1 - intercept_ed_group_4) / SD_ed_group_4

d_ed4_1 <- (intercept_ed_group_1 - intercept_ed_group_5) / SD_ed_group_1
d_ed4_5 <- (intercept_ed_group_1 - intercept_ed_group_5) / SD_ed_group_5

d_ed5_2 <- (intercept_ed_group_2 - intercept_ed_group_3) / SD_ed_group_2
d_ed5_3 <- (intercept_ed_group_2 - intercept_ed_group_3) / SD_ed_group_3

d_ed6_2 <- (intercept_ed_group_2 - intercept_ed_group_4) / SD_ed_group_2
d_ed6_4 <- (intercept_ed_group_2 - intercept_ed_group_4) / SD_ed_group_4

d_ed7_2 <- (intercept_ed_group_2 - intercept_ed_group_5) / SD_ed_group_2
d_ed7_5 <- (intercept_ed_group_2 - intercept_ed_group_5) / SD_ed_group_5

d_ed8_3 <- (intercept_ed_group_3 - intercept_ed_group_4) / SD_ed_group_3
d_ed8_4 <- (intercept_ed_group_3 - intercept_ed_group_4) / SD_ed_group_4

d_ed9_3 <- (intercept_ed_group_3 - intercept_ed_group_5) / SD_ed_group_3
d_ed9_5 <- (intercept_ed_group_3 - intercept_ed_group_5) / SD_ed_group_5

d_ed10_4 <- (intercept_ed_group_4 - intercept_ed_group_5) / SD_ed_group_4
d_ed10_5 <- (intercept_ed_group_4 - intercept_ed_group_5) / SD_ed_group_5

# Print comparisons into .txt file
# Order of comparisons adjusted to desired order in output
# Effect sizes that are multiplied with -1 (*-1) have been reversed to fit the
# Order of the groups in the output
sink("pairwise_exploration_depth.txt")

print("(1) Soon retired vs Long working")
summary(
  comp_ed4,
  fit.measures = fit_measures
)
print("(1) Cohen's d - Soon retired vs Long working - SD 1")
d_ed4_1
print("(1) Cohen's d - Soon retired vs Long working - SD 2")
d_ed4_5


print("(2) Bridge employment vs. Long working")
summary(
  comp_ed10,
  fit.measures = fit_measures
)
print("(2) Cohen's d - Bridge employment vs. Long working - SD 1")
d_ed10_4
print("(2) Cohen's d - Bridge employment vs. Long working - SD 2")
d_ed10_5


print("(3) Recently retired vs Long working")
summary(
  comp_ed9,
  fit.measures = fit_measures
)
print("(3) Cohen's d - Recently retired vs Long working - SD 1")
d_ed9_3
print("(3) Cohen's d - Recently retired vs Long working - SD 2")
d_ed9_5


print("(4) Long retired vs Long working")
summary(
  comp_ed7,
  fit.measures = fit_measures
)
print("(4) Cohen's d - Long retired vs Long working - SD 1")
d_ed7_2
print("(4) Cohen's d - Long retired vs Long working - SD 2")
d_ed7_5


print("(5) Bridge employment vs Soon retired")
summary(
  comp_ed3,
  fit.measures = fit_measures
)
print("(5) Cohen's d - Bridge employment vs Soon retired - SD 1")
d_ed3_1 * -1
print("(5) Cohen's d - Bridge employment vs Soon retired - SD 2")
d_ed3_4 * -1


print("(6) Recently retired vs Soon retired ")
summary(
  comp_ed2,
  fit.measures = fit_measures
)
print("(6) Cohen's d - Recently retired vs Soon retired - SD 1")
d_ed2_1 * -1
print("(6) Cohen's d - Recently retired vs Soon retired - SD 2")
d_ed2_3 * -1



print("(7) Long retired vs Soon retired ")
summary(
  comp_ed1,
  fit.measures =fit_measures
)
print("(7) Cohen's d - Long retired vs Soon retired - SD 1")
d_ed1_1 * -1
print("(7) Cohen's d - Long retired vs Soon retired - SD 2")
d_ed1_2 * -1


print("(8) Recently retired vs Bridge employment")
summary(
  comp_ed8,
  fit.measures = fit_measures
)
print("(8) Cohen's d - Recently retired vs Bridge employment - SD 1")
d_ed8_3
print("(8) Cohen's d - Recently retired vs Bridge employment - SD 2")
d_ed8_4



print("(9) Long retired vs Bridge employment")
summary(
  comp_ed6,
  fit.measures = fit_measures
)
print("(9) Cohen's d - Long retired vs Bridge employment - SD 1")
d_ed6_2
print("(9) Cohen's d - Long retired vs Bridge employment - SD 2")
d_ed6_4


print("(10) Long retired vs recently retired")
summary(
  comp_ed5,
  fit.measures =fit_measures
)
print("(10) Cohen's d - Long retired vs recently retired - SD 1")
d_ed5_2
print("(10) Cohen's d - Long retired vs recently retired - SD 2")
d_ed5_3


sink()

# Identification with commitment
# Constraints for each comparison
ic1 <- paste(model_baseline,
             'identification_with_commitment ~ c(z,z,a,b,c)*1')
ic2 <- paste(model_baseline,
             'identification_with_commitment ~ c(z,a,z,b,c)*1')
ic3 <- paste(model_baseline,
             'identification_with_commitment ~ c(z,b,a,z,c)*1')
ic4 <- paste(model_baseline,
             'identification_with_commitment ~ c(z,c,a,b,z)*1')
ic5 <- paste(model_baseline,
             'identification_with_commitment ~ c(a,z,z,b,c)*1')
ic6 <- paste(model_baseline,
             'identification_with_commitment ~ c(a,z,b,z,c)*1')
ic7 <- paste(model_baseline,
             'identification_with_commitment ~ c(a,z,c,b,z)*1')
ic8 <- paste(model_baseline,
             'identification_with_commitment ~ c(a,b,z,z,c)*1')
ic9 <- paste(model_baseline,
             'identification_with_commitment ~ c(a,b,z,c,z)*1')
ic10 <- paste(model_baseline,
              'identification_with_commitment ~ c(a,b,c,z,z)*1')

# Fitting each constrained model
fit_ic1 <- lavaan::cfa(
  ic1,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)

fit_ic2 <- lavaan::cfa(
  ic2,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_ic3 <- lavaan::cfa(
  ic3,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_ic4 <- lavaan::cfa(
  ic4,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_ic5 <- lavaan::cfa(
  ic5,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_ic6 <- lavaan::cfa(
  ic6,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_ic7 <- lavaan::cfa(
  ic7,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_ic8 <- lavaan::cfa(
  ic8,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_ic9 <- lavaan::cfa(
  ic9,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_ic10 <- lavaan::cfa(
  ic10,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)

# Comparison of each model to baseline model
comp_ic1 <- compareFit(fit_baseline, fit_ic1)
comp_ic2 <- compareFit(fit_baseline, fit_ic2)
comp_ic3 <- compareFit(fit_baseline, fit_ic3)
comp_ic4 <- compareFit(fit_baseline, fit_ic4)
comp_ic5 <- compareFit(fit_baseline, fit_ic5)
comp_ic6 <- compareFit(fit_baseline, fit_ic6)
comp_ic7 <- compareFit(fit_baseline, fit_ic7)
comp_ic8 <- compareFit(fit_baseline, fit_ic8)
comp_ic9 <- compareFit(fit_baseline, fit_ic9)
comp_ic10 <- compareFit(fit_baseline, fit_ic10)

# Cohen's d for each comparison
# Extract latent means and variances for each group
intercept_ic_group_1 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      1) %>%
  filter(op == '~1') %>% filter(lhs == 'identification_with_commitment') %>% select(est)
variance_ic_group_1 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     1) %>%
  filter(op == '~~') %>% filter(lhs == 'identification_with_commitment') %>%
  filter(rhs == 'identification_with_commitment') %>% select(est)
SD_ic_group_1 <- sqrt(variance_ic_group_1)

intercept_ic_group_2 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      2) %>%
  filter(op == '~1') %>% filter(lhs == 'identification_with_commitment') %>% select(est)
variance_ic_group_2 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     2) %>%
  filter(op == '~~') %>% filter(lhs == 'identification_with_commitment') %>%
  filter(rhs == 'identification_with_commitment') %>% select(est)
SD_ic_group_2 <- sqrt(variance_ic_group_2)

intercept_ic_group_3 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      3) %>%
  filter(op == '~1') %>% filter(lhs == 'identification_with_commitment') %>% select(est)
variance_ic_group_3 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     3) %>%
  filter(op == '~~') %>% filter(lhs == 'identification_with_commitment') %>%
  filter(rhs == 'identification_with_commitment') %>% select(est)
SD_ic_group_3 <- sqrt(variance_ic_group_3)

intercept_ic_group_4 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      4) %>%
  filter(op == '~1') %>% filter(lhs == 'identification_with_commitment') %>% select(est)
variance_ic_group_4 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     4) %>%
  filter(op == '~~') %>% filter(lhs == 'identification_with_commitment') %>%
  filter(rhs == 'identification_with_commitment') %>% select(est)
SD_ic_group_4 <- sqrt(variance_ic_group_4)

intercept_ic_group_5 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      5) %>%
  filter(op == '~1') %>% filter(lhs == 'identification_with_commitment') %>% select(est)
variance_ic_group_5 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     5) %>%
  filter(op == '~~') %>% filter(lhs == 'identification_with_commitment') %>%
  filter(rhs == 'identification_with_commitment') %>% select(est)
SD_ic_group_5 <- sqrt(variance_ic_group_5)

# Calculate Cohens d
d_ic1_1 <- (intercept_ic_group_1 - intercept_ic_group_2) / SD_ic_group_1
d_ic1_2 <- (intercept_ic_group_1 - intercept_ic_group_2) / SD_ic_group_2

d_ic2_1 <- (intercept_ic_group_1 - intercept_ic_group_3) / SD_ic_group_1
d_ic2_3 <- (intercept_ic_group_1 - intercept_ic_group_3) / SD_ic_group_3

d_ic3_1 <- (intercept_ic_group_1 - intercept_ic_group_4) / SD_ic_group_1
d_ic3_4 <- (intercept_ic_group_1 - intercept_ic_group_4) / SD_ic_group_4

d_ic4_1 <- (intercept_ic_group_1 - intercept_ic_group_5) / SD_ic_group_1
d_ic4_5 <- (intercept_ic_group_1 - intercept_ic_group_5) / SD_ic_group_5

d_ic5_2 <- (intercept_ic_group_2 - intercept_ic_group_3) / SD_ic_group_2
d_ic5_3 <- (intercept_ic_group_2 - intercept_ic_group_3) / SD_ic_group_3

d_ic6_2 <- (intercept_ic_group_2 - intercept_ic_group_4) / SD_ic_group_2
d_ic6_4 <- (intercept_ic_group_2 - intercept_ic_group_4) / SD_ic_group_4

d_ic7_2 <- (intercept_ic_group_2 - intercept_ic_group_5) / SD_ic_group_2
d_ic7_5 <- (intercept_ic_group_2 - intercept_ic_group_5) / SD_ic_group_5

d_ic8_3 <- (intercept_ic_group_3 - intercept_ic_group_4) / SD_ic_group_3
d_ic8_4 <- (intercept_ic_group_3 - intercept_ic_group_4) / SD_ic_group_4

d_ic9_3 <- (intercept_ic_group_3 - intercept_ic_group_5) / SD_ic_group_3
d_ic9_5 <- (intercept_ic_group_3 - intercept_ic_group_5) / SD_ic_group_5

d_ic10_4 <- (intercept_ic_group_4 - intercept_ic_group_5) / SD_ic_group_4
d_ic10_5 <- (intercept_ic_group_4 - intercept_ic_group_5) / SD_ic_group_5

# Print comparisons into .txt file
# Order of comparisons adjusted to desired order in output
# Effect sizes that are multiplied with -1 (*-1) have been reversed to fit the
# Order of the groups in the output
sink("pairwise_identification_with_commitment.txt")

print("(1) Soon retired vs Long working")
summary(
  comp_ic4,
  fit.measures = fit_measures
)
print("(1) Cohen's d - Soon retired vs Long working - SD 1")
d_ic4_1
print("(1) Cohen's d - Soon retired vs Long working - SD 2")
d_ic4_5


print("(2) Bridge employment vs. Long working")
summary(
  comp_ic10,
  fit.measures = fit_measures
)
print("(2) Cohen's d - Bridge employment vs. Long working - SD 1")
d_ic10_4
print("(2) Cohen's d - Bridge employment vs. Long working - SD 2")
d_ic10_5


print("(3) Recently retired vs Long working")
summary(
  comp_ic9,
  fit.measures = fit_measures
)
print("(3) Cohen's d - Recently retired vs Long working - SD 1")
d_ic9_3
print("(3) Cohen's d - Recently retired vs Long working - SD 2")
d_ic9_5


print("(4) Long retired vs Long working")
summary(
  comp_ic7,
  fit.measures = fit_measures
)
print("(4) Cohen's d - Long retired vs Long working - SD 1")
d_ic7_2
print("(4) Cohen's d - Long retired vs Long working - SD 2")
d_ic7_5


print("(5) Bridge employment vs Soon retired")
summary(
  comp_ic3,
  fit.measures = fit_measures
)
print("(5) Cohen's d - Bridge employment vs Soon retired - SD 1")
d_ic3_1 * -1
print("(5) Cohen's d - Bridge employment vs Soon retired - SD 2")
d_ic3_4 * -1


print("(6) Recently retired vs Soon retired ")
summary(
  comp_ic2,
  fit.measures =fit_measures
)
print("(6) Cohen's d - Recently retired vs Soon retired - SD 1")
d_ic2_1 * -1
print("(6) Cohen's d - Recently retired vs Soon retired - SD 2")
d_ic2_3 * -1



print("(7) Long retired vs Soon retired ")
summary(
  comp_ic1,
  fit.measures = fit_measures
)
print("(7) Cohen's d - Long retired vs Soon retired - SD 1")
d_ic1_1 * -1
print("(7) Cohen's d - Long retired vs Soon retired - SD 2")
d_ic1_2 * -1


print("(8) Recently retired vs Bridge employment")
summary(
  comp_ic8,
  fit.measures =fit_measures
)
print("(8) Cohen's d - Recently retired vs Bridge employment - SD 1")
d_ic8_3
print("(8) Cohen's d - Recently retired vs Bridge employment - SD 2")
d_ic8_4



print("(9) Long retired vs Bridge employment")
summary(
  comp_ic6,
  fit.measures = fit_measures
)
print("(9) Cohen's d - Long retired vs Bridge employment - SD 1")
d_ic6_2
print("(9) Cohen's d - Long retired vs Bridge employment - SD 2")
d_ic6_4


print("(10) Long retired vs recently retired")
summary(
  comp_ic5,
  fit.measures = fit_measures
  )
)
print("(10) Cohen's d - Long retired vs recently retired - SD 1")
d_ic5_2
print("(10) Cohen's d - Long retired vs recently retired - SD 2")
d_ic5_3

sink()

# Ruminative exploration
# Constraints for each comparison
re1 <- paste(model_baseline, 'ruminative_exploration ~ c(z,z,a,b,c)*1')
re2 <- paste(model_baseline, 'ruminative_exploration ~ c(z,a,z,b,c)*1')
re3 <- paste(model_baseline, 'ruminative_exploration ~ c(z,b,a,z,c)*1')
re4 <- paste(model_baseline, 'ruminative_exploration ~ c(z,c,a,b,z)*1')
re5 <- paste(model_baseline, 'ruminative_exploration ~ c(a,z,z,b,c)*1')
re6 <- paste(model_baseline, 'ruminative_exploration ~ c(a,z,b,z,c)*1')
re7 <- paste(model_baseline, 'ruminative_exploration ~ c(a,z,c,b,z)*1')
re8 <- paste(model_baseline, 'ruminative_exploration ~ c(a,b,z,z,c)*1')
re9 <- paste(model_baseline, 'ruminative_exploration ~ c(a,b,z,c,z)*1')
re10 <- paste(model_baseline, 'ruminative_exploration ~ c(a,b,c,z,z)*1')

# Fitting each constrained model
fit_re1 <- lavaan::cfa(
  re1,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)

fit_re2 <- lavaan::cfa(
  re2,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_re3 <- lavaan::cfa(
  re3,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_re4 <- lavaan::cfa(
  re4,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_re5 <- lavaan::cfa(
  re5,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_re6 <- lavaan::cfa(
  re6,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_re7 <- lavaan::cfa(
  re7,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_re8 <- lavaan::cfa(
  re8,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_re9 <- lavaan::cfa(
  re9,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)
fit_re10 <- lavaan::cfa(
  re10,
  data = hearts,
  estimator = "MLR",
  missing = "ML",
  group = "retirement_status",
  group.equal = c("loadings", "intercepts"),
  effect.coding = TRUE
)

# Comparison of each model to baseline model
comp_re1 <- compareFit(fit_baseline, fit_re1)
comp_re2 <- compareFit(fit_baseline, fit_re2)
comp_re3 <- compareFit(fit_baseline, fit_re3)
comp_re4 <- compareFit(fit_baseline, fit_re4)
comp_re5 <- compareFit(fit_baseline, fit_re5)
comp_re6 <- compareFit(fit_baseline, fit_re6)
comp_re7 <- compareFit(fit_baseline, fit_re7)
comp_re8 <- compareFit(fit_baseline, fit_re8)
comp_re9 <- compareFit(fit_baseline, fit_re9)
comp_re10 <- compareFit(fit_baseline, fit_re10)


# Cohen's d for each comparison
# Extract latent means and variances for each group
intercept_re_group_1 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      1) %>%
  filter(op == '~1') %>% filter(lhs == 'ruminative_exploration') %>% select(est)
variance_re_group_1 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     1) %>%
  filter(op == '~~') %>% filter(lhs == 'ruminative_exploration') %>%
  filter(rhs == 'ruminative_exploration') %>% select(est)
SD_re_group_1 <- sqrt(variance_re_group_1)

intercept_re_group_2 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      2) %>%
  filter(op == '~1') %>% filter(lhs == 'ruminative_exploration') %>% select(est)
variance_re_group_2 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     2) %>%
  filter(op == '~~') %>% filter(lhs == 'ruminative_exploration') %>%
  filter(rhs == 'ruminative_exploration') %>% select(est)
SD_re_group_2 <- sqrt(variance_re_group_2)

intercept_re_group_3 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      3) %>%
  filter(op == '~1') %>% filter(lhs == 'ruminative_exploration') %>% select(est)
variance_re_group_3 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     3) %>%
  filter(op == '~~') %>% filter(lhs == 'ruminative_exploration') %>%
  filter(rhs == 'ruminative_exploration') %>% select(est)
SD_re_group_3 <- sqrt(variance_re_group_3)

intercept_re_group_4 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      4) %>%
  filter(op == '~1') %>% filter(lhs == 'ruminative_exploration') %>% select(est)
variance_re_group_4 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     4) %>%
  filter(op == '~~') %>% filter(lhs == 'ruminative_exploration') %>%
  filter(rhs == 'ruminative_exploration') %>% select(est)
SD_re_group_4 <- sqrt(variance_re_group_4)

intercept_re_group_5 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                      5) %>%
  filter(op == '~1') %>% filter(lhs == 'ruminative_exploration') %>% select(est)
variance_re_group_5 <- parameterEstimates(fit_baseline) %>% filter(group ==
                                                                     5) %>%
  filter(op == '~~') %>% filter(lhs == 'ruminative_exploration') %>%
  filter(rhs == 'ruminative_exploration') %>% select(est)
SD_re_group_5 <- sqrt(variance_re_group_5)

# Calculate Cohens d

d_re1_1 <- (intercept_re_group_1 - intercept_re_group_2) / SD_re_group_1
d_re1_2 <- (intercept_re_group_1 - intercept_re_group_2) / SD_re_group_2

d_re2_1 <- (intercept_re_group_1 - intercept_re_group_3) / SD_re_group_1
d_re2_3 <- (intercept_re_group_1 - intercept_re_group_3) / SD_re_group_3

d_re3_1 <- (intercept_re_group_1 - intercept_re_group_4) / SD_re_group_1
d_re3_4 <- (intercept_re_group_1 - intercept_re_group_4) / SD_re_group_4

d_re4_1 <- (intercept_re_group_1 - intercept_re_group_5) / SD_re_group_1
d_re4_5 <- (intercept_re_group_1 - intercept_re_group_5) / SD_re_group_5

d_re5_2 <- (intercept_re_group_2 - intercept_re_group_3) / SD_re_group_2
d_re5_3 <- (intercept_re_group_2 - intercept_re_group_3) / SD_re_group_3

d_re6_2 <- (intercept_re_group_2 - intercept_re_group_4) / SD_re_group_2
d_re6_4 <- (intercept_re_group_2 - intercept_re_group_4) / SD_re_group_4

d_re7_2 <- (intercept_re_group_2 - intercept_re_group_5) / SD_re_group_2
d_re7_5 <- (intercept_re_group_2 - intercept_re_group_5) / SD_re_group_5

d_re8_3 <- (intercept_re_group_3 - intercept_re_group_4) / SD_re_group_3
d_re8_4 <- (intercept_re_group_3 - intercept_re_group_4) / SD_re_group_4

d_re9_3 <- (intercept_re_group_3 - intercept_re_group_5) / SD_re_group_3
d_re9_5 <- (intercept_re_group_3 - intercept_re_group_5) / SD_re_group_5

d_re10_4 <- (intercept_re_group_4 - intercept_re_group_5) / SD_re_group_4
d_re10_5 <- (intercept_re_group_4 - intercept_re_group_5) / SD_re_group_5

# Print comparisons into .txt file
# Order of comparisons adjusted to desired order in output
# Effect sizes that are multiplied with -1 (*-1) have been reversed to fit the
# Order of the groups in the output
sink("pairwise_ruminative_exploration.txt")

print("(1) Soon retired vs Long working")
summary(
  comp_re4,
  fit.measures = fit_measures
)
print("(1) Cohen's d - Soon retired vs Long working - SD 1")
d_re4_1
print("(1) Cohen's d - Soon retired vs Long working - SD 2")
d_re4_5


print("(2) Bridge employment vs. Long working")
summary(
  comp_re10,
  fit.measures = fit_measures
)
print("(2) Cohen's d - Bridge employment vs. Long working - SD 1")
d_re10_4
print("(2) Cohen's d - Bridge employment vs. Long working - SD 2")
d_re10_5


print("(3) Recently retired vs Long working")
summary(
  comp_re9,
  fit.measures = fit_measures
)
print("(3) Cohen's d - Recently retired vs Long working - SD 1")
d_re9_3
print("(3) Cohen's d - Recently retired vs Long working - SD 2")
d_re9_5


print("(4) Long retired vs Long working")
summary(
  comp_re7,
  fit.measures = fit_measures
)
print("(4) Cohen's d - Long retired vs Long working - SD 1")
d_re7_2
print("(4) Cohen's d - Long retired vs Long working - SD 2")
d_re7_5


print("(5) Bridge employment vs Soon retired")
summary(
  comp_re3,
  fit.measures = fit_measures
)
print("(5) Cohen's d - Bridge employment vs Soon retired - SD 1")
d_re3_1 * -1
print("(5) Cohen's d - Bridge employment vs Soon retired - SD 2")
d_re3_4 * -1


print("(6) Recently retired vs Soon retired ")
summary(
  comp_re2,
  fit.measures = fit_measures
)
print("(6) Cohen's d - Recently retired vs Soon retired - SD 1")
d_re2_1 * -1
print("(6) Cohen's d - Recently retired vs Soon retired - SD 2")
d_re2_3 * -1



print("(7) Long retired vs Soon retired ")
summary(
  comp_re1,
  fit.measures = fit_measures
)
print("(7) Cohen's d - Long retired vs Soon retired - SD 1")
d_re1_1 * -1
print("(7) Cohen's d - Long retired vs Soon retired - SD 2")
d_re1_2 * -1


print("(8) Recently retired vs Bridge employment")
summary(
  comp_re8,
  fit.measures = fit_measures
)
print("(8) Cohen's d - Recently retired vs Bridge employment - SD 1")
d_re8_3
print("(8) Cohen's d - Recently retired vs Bridge employment - SD 2")
d_re8_4



print("(9) Long retired vs Bridge employment")
summary(
  comp_re6,
  fit.measures = fit_measures
)
print("(9) Cohen's d - Long retired vs Bridge employment - SD 1")
d_re6_2
print("(9) Cohen's d - Long retired vs Bridge employment - SD 2")
d_re6_4


print("(10) Long retired vs recently retired")
summary(
  comp_re5,
  fit.measures = fit_measures
)
print("(10) Cohen's d - Long retired vs recently retired - SD 1")
d_re5_2
print("(10) Cohen's d - Long retired vs recently retired - SD 2")
d_re5_3


sink()

################################################################################

# Latent means and SDs for each group
sink("latent_means_SDs.txt")

print("Long working:")
print("Commitment making mean")
intercept_cm_group_5
print("Commitment making SD")
SD_cm_group_5
print("Exploration Breadth mean")
intercept_eb_group_5
print("Exploration Breadth SD")
SD_eb_group_5
print("Identification w/ Commitment  mean")
intercept_ic_group_5
print("Identification w/ Commitment SD")
SD_ic_group_5
print("Exploration Depth mean")
intercept_ed_group_5
print("Exploration Depth SD")
SD_ed_group_5
print("Ruminative exploration mean")
intercept_re_group_5
print("Ruminative exploration SD")
SD_re_group_5

print("Soon retired:")
print("Commitment making mean")
intercept_cm_group_1
print("Commitment making SD")
SD_cm_group_1
print("Exploration Breadth mean")
intercept_eb_group_1
print("Exploration Breadth SD")
SD_eb_group_1
print("Identification w/ Commitment  mean")
intercept_ic_group_1
print("Identification w/ Commitment SD")
SD_ic_group_1
print("Exploration Depth mean")
intercept_ed_group_1
print("Exploration Depth SD")
SD_ed_group_1
print("Ruminative exploration mean")
intercept_re_group_1
print("Ruminative exploration SD")
SD_re_group_1

print("Bridge employment:")
print("Commitment making mean")
intercept_cm_group_4
print("Commitment making SD")
SD_cm_group_4
print("Exploration Breadth mean")
intercept_eb_group_4
print("Exploration Breadth SD")
SD_eb_group_4
print("Identification w/ Commitment  mean")
intercept_ic_group_4
print("Identification w/ Commitment SD")
SD_ic_group_4
print("Exploration Depth mean")
intercept_ed_group_4
print("Exploration Depth SD")
SD_ed_group_4
print("Ruminative exploration mean")
intercept_re_group_4
print("Ruminative exploration SD")
SD_re_group_4

print("Recently retired:")
print("Commitment making mean")
intercept_cm_group_3
print("Commitment making SD")
SD_cm_group_3
print("Exploration Breadth mean")
intercept_eb_group_3
print("Exploration Breadth SD")
SD_eb_group_3
print("Identification w/ Commitment  mean")
intercept_ic_group_3
print("Identification w/ Commitment SD")
SD_ic_group_3
print("Exploration Depth mean")
intercept_ed_group_3
print("Exploration Depth SD")
SD_ed_group_3
print("Ruminative exploration mean")
intercept_re_group_3
print("Ruminative exploration SD")
SD_re_group_3

print("Long retired:")
print("Commitment making mean")
intercept_cm_group_2
print("Commitment making SD")
SD_cm_group_2
print("Exploration Breadth mean")
intercept_eb_group_2
print("Exploration Breadth SD")
SD_eb_group_2
print("Identification w/ Commitment  mean")
intercept_ic_group_2
print("Identification w/ Commitment SD")
SD_ic_group_2
print("Exploration Depth mean")
intercept_ed_group_2
print("Exploration Depth SD")
SD_ed_group_2
print("Ruminative exploration mean")
intercept_re_group_2
print("Ruminative exploration SD")
SD_re_group_2

sink()
