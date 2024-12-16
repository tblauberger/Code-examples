##########################################
###########02_DATA_SCREENING##############
##########################################

# Load libraries
library('tidyverse')
library('naniar')
library('jmv')
library('MVN')
library('lavaan')
library('psych')
library('misty')

# Load data
hearts_full <- as_tibble(read.csv("./resources/data_files/full_data_processed.csv"))
hearts_clean <- as_tibble(read.csv("./resources/data_files/hearts_clean.csv"))

# Get N for wave 5
hearts_n <- hearts_full %>% select(w5)
hearts_n <- as_tibble(factor(hearts_n$w5))

sink("./resources/descriptive_statistics/n_wave_5.txt")
descriptives(hearts_n, freq = TRUE)
sink()

# Save univariate descriptives
sink("./resources/descriptive_statistics/descriptives_jmv.txt")
i <- 0
for (i in 1:ncol(hearts_clean)) {
  descriptives_out <- hearts_clean %>%
    select (i) %>%
    descriptives(skew = T, kurt = T)
  print(descriptives_out)
}
sink()

sink("./resources/descriptive_statistics/descriptives_psych.txt")
describe(hearts_clean)
sink()

# Save frequency tables for dichotomous variables
dem_freq <- select(hearts_clean, gender, retirement_status)
dem_freq$gender <- factor(dem_freq$gender)
dem_freq$retirement_status <- factor(dem_freq$retirement_status)

sink("./resources/descriptive_statistics/descriptives_frequencies.txt")
descriptives(dem_freq, freq = TRUE)
sink()

# Save univariate plots
pdf("./resources/descriptive_statistics/histograms.pdf")
descriptives(
  hearts_clean,
  n = FALSE,
  missing = FALSE,
  mean= FALSE,
  median = FALSE,
  sd = FALSE,
  min = FALSE,
  max = FALSE,
  hist = T
)
dev.off()

# Age and gender by group
sink("./resources/descriptive_statistics/group_demographics.txt")
hearts_clean %>%  descriptives(vars = vars(age), splitBy = c("retirement_status"))
print("bridge_employment")
dem_freq %>%
  filter(retirement_status == "bridge_employment") %>%
  descriptives(vars= vars(gender), freq = TRUE)
print("long_time_retiree")
dem_freq %>%
  filter(retirement_status == "long_time_retiree") %>%
  descriptives(vars= vars(gender), freq = TRUE)
print("long_time_worker")
dem_freq %>%
  filter(retirement_status == "long_time_worker") %>%
  descriptives(vars= vars(gender), freq = TRUE)
print("recently_retired")
dem_freq %>%
  filter(retirement_status == "recently_retired") %>%
  descriptives(vars= vars(gender), freq = TRUE)
print("soon_retired")
dem_freq %>%
  filter(retirement_status == "soon_retired") %>%
  descriptives(vars= vars(gender), freq = TRUE)
print("NA")
dem_freq[is.na(dem_freq$retirement_status),] %>%
  descriptives(vars= vars(gender), freq = TRUE)
hearts_clean[is.na(hearts_clean$retirement_status),] %>%
  descriptives(vars= vars(age))
sink()

age_na <- hearts_clean[is.na(hearts_clean$retirement_status),] %>% descriptives(vars= vars(age))

# Internal reliability all items
sink("./resources/descriptive_statistics/DIDS_internal_reliability_all_items.txt")
rel_commitment_making <-  hearts_clean %>%
  select(com_ma_1,com_ma_2,com_ma_3) %>%
  jmv::reliability(omegaScale = TRUE, omegaItems = TRUE)
rel_identification_with_commitment <-  hearts_clean %>%
  select(com_id_1, com_id_2 , com_id_3) %>%
  jmv::reliability(omegaScale = TRUE, omegaItems = TRUE)
rel_exploration_breadth <-  hearts_clean %>%
  select(exp_br_1, exp_br_2, exp_br_3) %>%
  jmv::reliability(omegaScale = TRUE, omegaItems = TRUE)
rel_exploration_depth <-  hearts_clean %>%
  select(exp_de_1,exp_de_2 , exp_de_3) %>%
  jmv::reliability(omegaScale = TRUE, omegaItems = TRUE)
rel_ruminative_exploration <-  hearts_clean %>%
  select(exp_ru_1, exp_ru_2, exp_ru_3) %>% j

mv::reliability(omegaScale = TRUE, omegaItems = TRUE)

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


# Multivariate normality: Mardia's kurtosis & skewness DIDS (all items)
hearts_dids <- hearts_clean %>%
  select(com_ma_1, com_ma_2, com_ma_3, com_id_1, com_id_2, com_id_3, exp_br_1,
  exp_br_2, exp_br_3 , exp_de_1 , exp_de_2 , exp_de_3,exp_ru_1 , exp_ru_2 ,
  exp_ru_3)

mardia_dids <- mvn(hearts_dids, mvnTest = "mardia")

# Multivariate normality: Mardia's kurtosis & skewness DIDS (14 items)
hearts_dids14 <- hearts_clean %>%
  select(com_ma_1, com_ma_2, com_ma_3, com_id_1,com_id_2, com_id_3, exp_br_1,
  exp_br_2, exp_br_3,  exp_de_2, exp_de_3, exp_ru_1, exp_ru_2, exp_ru_3)

mardia_dids14 <- mvn(hearts_dids14, mvnTest = "mardia")

# Multivariate normality: Mardia's kurtosis & skewness CESD
hearts_cesd <- hearts_clean %>%
  select(depr_1, depr_2, depr_3, depr_4, depr_5, depr_6,depr_7, depr_8, depr_9,
  depr_10)

mardia_cesd <-mvn(hearts_cesd, mvnTest = "mardia")

# Multivariate normality: Mardia's kurtosis & skewness DIDS (all items) + CESD
hearts_dids_cesd <- hearts_clean %>%
  select(com_ma_1, com_ma_2, com_ma_3, com_id_1,com_id_2, com_id_3, exp_br_1,
  exp_br_2, exp_br_3,  exp_de_1, exp_de_2, exp_de_3, exp_ru_1, exp_ru_2,
  exp_ru_3, depr_1, depr_2, depr_3, depr_4, depr_5, depr_6, depr_7, depr_8,
  depr_9, depr_10)

mardia_dids_cesd <-mvn(hearts_dids_cesd, mvnTest = "mardia")

# Multivariate normality: Mardia's kurtosis & skewness DIDS (14 items) + CESD
hearts_dids14_cesd <- hearts_clean %>%
  select(com_ma_1, com_ma_2, com_ma_3, com_id_1, com_id_2, com_id_3, exp_br_1,
  exp_br_2,exp_br_3,  exp_de_2, exp_de_3,exp_ru_1, exp_ru_2, exp_ru_3,
  depr_1, depr_2, depr_3, depr_4, depr_5, depr_6, depr_7, depr_8,
  depr_9, depr_10)

mardia_dids14_cesd <-mvn(hearts_dids14_cesd, mvnTest = "mardia")

sink("./resources/descriptive_statistics/mardia.txt")
print("DIDS (15 items):")
mardia_dids
print("DIDS (14 items):")
mardia_dids14
print("CESD:")
mardia_cesd
print("DIDS (15 items) + CESD:")
mardia_dids_cesd
print("DIDS (14 items) + CESD:")
mardia_dids14_cesd
sink()

# Explore missingness
# Proportion of missing data total
miss_clean <- prop_miss(hearts_clean)
miss_dids_cesd <- prop_miss(hearts_dids_cesd)

# Proportion of missing data per column
miss_var <- miss_var_summary(hearts_clean)

# Plot missing data per column
miss_var_plot <- gg_miss_var(hearts_clean)

# Write missingness into files
sink("./resources/descriptive_statistics/missing_total.txt")
print("Including composite scores, after removal of their component items:")
miss_clean
print("DIDS + CESD items:")
miss_dids_cesd
sink()

sink("./resources/descriptive_statistics/missing_var.txt")
print(miss_var, n = 100)
sink()

# Save plots
pdf("./resources/descriptive_statistics/missing_data_plots.pdf")
miss_var_plot
dev.off()

# Clear up environment
rm(list = ls() )
