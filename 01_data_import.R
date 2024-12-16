##########################################
###########01_DATA_IMPORT#################
##########################################

library('tidyverse')
library('naniar')
library('haven')
library('labelled')

# Load dataset
hearts_raw <- as_tibble(read_sav("./resources/data_files/hearts_raw.sav"))

# Remove labels
hearts_raw <- labelled::remove_labels(hearts_raw)

# Replace missing values with NA
hearts_raw <- naniar::replace_with_na_all(data = hearts_raw,
  condition = ~.x == 98)

# Fill in empty cells with NA
tidyr::complete(hearts_raw)

# Rename variables
hearts_raw <- rename(
  hearts_raw,
  age = TRUE_AGE,
  gender = A7,
  education = A6,
  rel_status_1 = E8,
  rel_status_2 =  E9,
  com_ma_1 = E730_1,
  com_ma_2 = E730_2,
  com_ma_3 = E730_3,
  exp_br_1 = E730_4,
  exp_br_2 = E730_5,
  exp_br_3 = E730_6,
  exp_ru_1 = E730_7,
  exp_ru_2 = E730_8,
  exp_ru_3 = E730_9,
  com_id_1 = E730_10,
  com_id_2 = E730_11,
  com_id_3 = E730_12,
  exp_de_1 = E730_13,
  exp_de_2 = E730_14,
  exp_de_3 = E730_15,
  depr_1 = E83_1,
  depr_2 = E83_2,
  depr_3 = E83_3,
  depr_4 = E83_4,
  depr_5 = E83_5,
  depr_6 = E83_6,
  depr_7 = E83_8,
  depr_8 = E83_9,
  depr_9 = E83_11,
  depr_10 = E83_12,
  w1 = AWeb_vs_papper,
  w2 = BWebb_vs_papper,
  w3 = CWeb_vs_paper,
  w4 = DWebvsPaper,
  w5 = EWebVsPaper,
  w6 = FWebvsPaper,
  w7 = GWebvsPaper,
  w8 = HWebvsPaper,
  w9 = IWebvsPaper,
  retired_w1 = A25,
  retired_w2 = B25,
  retired_w3 = C25,
  retired_w4 = D25,
  retired_w5 = E25,
  retired_w6 = F25,
  retired_w7 = G25,
  retired_w8 = H25,
  retired_w9 = I25
)

# Reverse coding
hearts_raw <- hearts_raw %>%
  mutate(
    depr_5 =
    ifelse(depr_5 == 0, 3,
    ifelse(depr_5 == 1, 2,
    ifelse(depr_5 == 2, 1,
    ifelse(depr_5 == 3, 0,
    NA_integer_))))
  )

hearts_raw <- hearts_raw %>%
  mutate(
    depr_7 =
    ifelse(depr_7 == 0, 3,
    ifelse(depr_7 == 1, 2,
    ifelse(depr_7 == 2, 1,
    ifelse(depr_7 == 3, 0,
    NA_integer_))))
  )

# Create set for subjects who responded to wave 5
hearts <- hearts_raw

# Write set with all participants into .csv file

write.csv(hearts_raw, "./resources/data_files/full_data_processed.csv")

# Recode WebvsPaper variable
hearts <- hearts %>% mutate(
  w1=ifelse(w1 == 0, 1,
     ifelse(w1 == 1, 1,
     NA_integer_)),
  w2=ifelse(w2 == 0, 1,
     ifelse(w2 == 1, 1,
     NA_integer_)),
  w3=ifelse(w3 == 0, 1,
     ifelse(w3 == 1, 1,
     NA_integer_)),
  w4=ifelse(w4 == 0, 1,
     ifelse(w4 == 1, 1,
     NA_integer_)),
  w5=ifelse(w5 == 0, 1,
     ifelse(w5 == 1, 1,
     NA_integer_)),
  w6=ifelse(w6 == 0, 1,
     ifelse(w6 == 1, 1,
     NA_integer_)),
  w7=ifelse(w7 == 0, 1,
     ifelse(w7 == 1, 1,
     NA_integer_)),
  w8=ifelse(w8 == 0, 1,
     ifelse(w8 == 1, 1,
     NA_integer_)),
  w9=ifelse(w9 == 0, 1,
     ifelse(w9 == 1, 1,
     NA_integer_))
)

# Omit participants who did not participate in wave 5
hearts <- hearts[!is.na(hearts$w5), ]

# Recode gender
hearts$gender <- ifelse(hearts$gender == 1, 0,
                 ifelse(hearts$gender == 2, 1,
                 NA_integer_))


# Create retirement status groups
hearts$retirement_status <- NA
hearts$retired_w3 <- replace(hearts$retired_w3, is.na(hearts$retired_w3), 0)
hearts$retired_w4 <- replace(hearts$retired_w4, is.na(hearts$retired_w4), 0)
hearts$retired_w5 <- replace(hearts$retired_w5, is.na(hearts$retired_w5), 0)
hearts$retired_w6 <- replace(hearts$retired_w6, is.na(hearts$retired_w6), 0)
hearts$retired_w7 <- replace(hearts$retired_w7, is.na(hearts$retired_w7), 0)

for (d in 1:nrow(hearts)) {
  if (hearts$retired_w5[d] == 2) {
    hearts$retirement_status[d] <- "bridge_employment"
  } else if (hearts$retired_w5[d] == 3) {
    hearts$retirement_status[d] <- "bridge_employment"
  } else if (hearts$retired_w5[d] == 1 && hearts$retired_w6[d] >1) {
    hearts$retirement_status[d] <- "soon_retired"
  } else if (hearts$retired_w5[d] == 1 && hearts$retired_w6[d]  == 1 && hearts$retired_w7[d] >1) {
    hearts$retirement_status[d] <- "soon_retired"
  } else if (hearts$retired_w5[d] == 1 && hearts$retired_w6[d]  == 1 && hearts$retired_w7[d] == 1) {
    hearts$retirement_status[d] <- "long_time_worker"
  } else if (hearts$retired_w5[d] == 4 && hearts$retired_w4[d] == 1) {
    hearts$retirement_status[d] <- "recently_retired"
  } else if (hearts$retired_w5[d] == 4 && hearts$retired_w4[d] > 1 && hearts$retired_w3[d] == 1) {
    hearts$retirement_status[d] <- "recently_retired"
  } else if (hearts$retired_w5[d] == 4 && hearts$retired_w4[d] > 1 && hearts$retired_w3[d] > 1) {
    hearts$retirement_status[d] <- "long_time_retiree"
  } else {
    NA_character_
  }
}

# Remove items which are not required from dataset
hearts <- hearts %>% select(-c(starts_with("E79")))
hearts <- hearts %>% select(-c(starts_with("E80")))
hearts <- hearts %>% select(-c(starts_with("w")))
hearts <- hearts %>% select(-c(starts_with("retired")))
hearts <- hearts %>% select(-c(starts_with("rel_status")))
hearts <- hearts %>% select(-c(starts_with("edu")))

# Write cleaned up data set in file
write.csv(hearts,"./resources/data_files/hearts_clean.csv")

# Clear up environment
rm(list = ls() )
