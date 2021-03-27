# This script providess full Bayesian version for a variety of variables included in the model
# This version contains the new variables smoking amount (by county) and medical insurance

# The model is:
# P(HNC| HPV, covars) = P(HPV | HNC, covars, site, histology) * p(HNC | covars)  / p(HPV | covars)
# These are shortened to: p_answer = p_model * p_histology / p_HPV_incidence
# This uses a BART machine model run and should be able to produce posterior distributions of predictive probabilities too (perhaps in a future version)

# P(HPV | HNC, covars) comes from the machine learning model
# The covariates here are:
# AgeAtDiagnosis
# range(x[,'AgeAtDiagnosis'])
# 0 98 - Age 0 is observation 5083 in the original data set
# MaritalStatusAtDiagnosis
# Married (including common law), Single (never married), Divorced, Widowed, Unknown, Unmarried or Domestic Partner, Separated
# Race
# White_Non-Spanish-Hispanic-Latino, Filipino, Black, White_Spanish-Hispanic-Latino, Chinese, Japanese, American Indian/Alaska Native, Pacific Islander, Asian Indian or Pakistani, Other_Non-Spanish-Hispanic-Latino, South East Asian, Other Asian, Korean, Unknown_Non-Spanish-Hispanic-Latino, Other_Spanish-Hispanic-Latino, Unknown_Spanish-Hispanic-Latino
# CurrentSmokerPercentage
# range(x$CurrentSmokerPercentage)
# [1]  684 3838
# This is 6.84% to 38.38%
# Insurance
# table(x$Insurance)
# Insured         Medicaid Medicare enabled        Uninsured
# 6866              918               55              191
# Unknown
# 76
# 84.7% insured!

# p(HNC | covars) comes from the comes from HNC incidence rates
# HNC_inc %>% select(Sex) %>% unique
# First is race
# 1 White
# 2 Black
# 3 American Indian/Alaska Native (CHSDA Only)
# 4 Asian or Pacific Islander
# 5 Hispanic
# 6 White Hispanic
# 7 White Non-Hispanic
# Second is Sex
# Male
# Female
# Third is age
# 1 00-14
# 2 15-34
# 3 35-44
# 4 45-54
# 5 55-64
# 6 65-74
# 7 75+

# p(HPV | covars) comes from HPV incidence
# HPV_inc %>% select() %>% unique
# FIrst is sex - watch out for opposite coding
# 1 Female
# 2 Male
# Second is age
# 1 15-34
# 2 35-44
# 3 45-54
# 4 55-64
# 5 65-74
# Third is Ethnicity
# 1 Asian
# 2 Black
# 3 Hispanic
# 4 Other
# 5 White

# Now write a prediction function which takes values of these covariates and then produces the P(HNC | HPV, covars)

# ------------------------------------------------------------------------
# predict_fun_ci ----------------------------------------------------------
# ------------------------------------------------------------------------

predict_fun_ci <- function(age = 60,
                           smoker_pertenthousand = 1800,
                           sex = c("Male", "Female"),
                           ethnicity = c(
                             "American Indian/Alaska Native",
                             "Asian Indian or Pakistani",
                             "Black",
                             "Chinese",
                             "Filipino",
                             "Japanese",
                             "Korean",
                             "Other Asian",
                             "Other_Non-Spanish-Hispanic-Latino",
                             "Other_Spanish-Hispanic-Latino",
                             "Pacific Islander",
                             "South East Asian",
                             "Unknown_Non-Spanish-Hispanic-Latino",
                             "Unknown_Spanish-Hispanic-Latino",
                             "White_Non-Spanish-Hispanic-Latino",
                             "White_Spanish-Hispanic-Latino"
                           ),
                           marital_status_at_diagnosis = c(
                             "Married (including common law)",
                             "Single (never married)",
                             "Divorced",
                             "Widowed",
                             "Unknown",
                             "Unmarried or Domestic Partner",
                             "Separated"
                           ),
                           insurance = c(
                             "Insured",
                             "Medicaid",
                             "Medicare enabled",
                             "Uninsured",
                             "Unknown"
                           ),
                           levels = c(0.025, 0.5, 0.975)) {
  # Probably should do some error checking here, especially to see if the coded answers above match

  # Need to calculate
  # P(HNC, site, histology | HPV, covars) = P(HPV | HNC, covars, site, histology) * p(HNC | covars)  / p(HPV | covars)
  # These are shortened to: p_answer = p_model * p_histology / p_HPV_incidence

  # Tidy up arguments
  sex <- match.arg(sex, several.ok = TRUE)
  ethnicity <- match.arg(ethnicity, several.ok = TRUE)
  marital_status_at_diagnosis <- match.arg(marital_status_at_diagnosis,
    several.ok = TRUE
  )
  insurance <- match.arg(insurance,
    several.ok = TRUE
  )
  n_pred <- length(age)
  # browser()

  # p_model -----------------------------------------------------------------

  # Read in the model
  mod <- readRDS("../OPSCC_private/bart_model_reduced_20210326.rds")

  # This has 3 variables:
  # AgeAtDiagnosis, MaritalStatusAtDiagnostis, and Race
  new_x <- data.frame(
    AgeAtDiagnosis = age,
    CurrentSmokerPercentage = smoker_pertenthousand,
    Sex = factor(sex, levels = c(c(
      "Female",
      "Male"
    ))),
    MaritalStatusAtDiagnosis = factor(
      marital_status_at_diagnosis,
      levels =
        c(
          "Divorced",
          "Married (including common law)",
          "Separated",
          "Single (never married)",
          "Unknown",
          "Unmarried or Domestic Partner",
          "Widowed"
        )
    ),
    Race = factor(
      ethnicity,
      levels = c(
        "American Indian/Alaska Native",
        "Asian Indian or Pakistani",
        "Black",
        "Chinese",
        "Filipino",
        "Japanese",
        "Korean",
        "Other Asian",
        "Other_Non-Spanish-Hispanic-Latino",
        "Other_Spanish-Hispanic-Latino",
        "Pacific Islander",
        "South East Asian",
        "Unknown_Non-Spanish-Hispanic-Latino",
        "Unknown_Spanish-Hispanic-Latino",
        "White_Non-Spanish-Hispanic-Latino",
        "White_Spanish-Hispanic-Latino"
      )
    ),
    Insurance = factor(insurance,
      levels = c(
        "Insured",
        "Medicaid",
        "Medicare enabled",
        "Uninsured",
        "Unknown"
      )
    )
  ) %>%
    bartModelMatrix()

  full_post <- predict(mod, newdata = new_x)$prob.test
  p_model_all <- 1 - apply(full_post, 2, quantile, probs = rev(levels))

  # p_HNC_incidence --------------------------------------------------------

  # Load in the incidence probabilities
  HNC_inc_raw <- readRDS("../OPSCC_private/seer_incidence_rates_wide_20191812.rds")

  # Recode the ethnicity and age variables correctly
  eth_new <- case_when(
    ethnicity == "American Indian/Alaska Native" ~ "American_Indian",
    ethnicity == "Asian Indian or Pakistani" ~ "Asian",
    ethnicity == "Black" ~ "Black",
    ethnicity == "Chinese" ~ "Asian",
    ethnicity == "Filipino" ~ "Asian",
    ethnicity == "Japanese" ~ "Asian",
    ethnicity == "Korean" ~ "Asian",
    ethnicity == "Other Asian" ~ "Asian",
    ethnicity == "Other_Spanish-Hispanic-Latino" ~ "White_Hispanic",
    ethnicity == "Pacific Islander" ~ "Asian",
    ethnicity == "South East Asian" ~ "Asian",
    ethnicity == "Unknown_Non-Spanish-Hispanic-Latino" ~ "White_Hispanic",
    ethnicity == "Unknown_Spanish-Hispanic-Latino" ~ "White_Hispanic",
    ethnicity == "White_Non-Spanish-Hispanic-Latino" ~ "White",
    ethnicity == "White_Spanish-Hispanic-Latino" ~ "White_Hispanic"
  )
  age_new <- case_when(
    between(age, 15, 34) ~ "15-34",
    between(age, 35, 44) ~ "35-44",
    between(age, 45, 54) ~ "45-54",
    between(age, 55, 64) ~ "55-64",
    between(age, 65, 74) ~ "65-74",
    age >= 75 ~ "75+"
  )
  sex_new <- case_when(
    sex == "Male" ~ "M",
    sex == "Female" ~ "F"
  )

  # Now gather up and recode ethnicity
  HNC_inc <- HNC_inc_raw %>% gather(
    Age,
    Probability, -Ethnicity, -Sex
  )

  # Calculate p_HNC_incidence
  p_HNC_incidence <- rep(NA, n_pred)
  for (i in 1:n_pred) {
    p_HNC_incidence[i] <- HNC_inc %>%
      filter(
        Ethnicity == eth_new[i],
        Sex == sex_new[i],
        Age == age_new[i]
      ) %>%
      select(Probability) %>%
      pull()
  }

  # p_HPV_incidence ---------------------------------------------------------

  # Load in data
  HPV_inc <- read_csv("../OPSCC_private/Incidence_rates_HPV.csv", col_types = cols())

  # Recode ethnicity again
  eth_new2 <- case_when(
    ethnicity == "American Indian/Alaska Native" ~ "Other",
    ethnicity == "Asian Indian or Pakistani" ~ "Asian",
    ethnicity == "Black" ~ "Black",
    ethnicity == "Chinese" ~ "Asian",
    ethnicity == "Filipino" ~ "Asian",
    ethnicity == "Japanese" ~ "Asian",
    ethnicity == "Korean" ~ "Asian",
    ethnicity == "Other Asian" ~ "Asian",
    ethnicity == "Other_Non-Spanish-Hispanic-Latino" ~ "Hispanic",
    ethnicity == "Other_Spanish-Hispanic-Latino" ~ "Hispanic",
    ethnicity == "Pacific Islander" ~ "Other",
    ethnicity == "South East Asian" ~ "Asian",
    ethnicity == "Unknown_Non-Spanish-Hispanic-Latino" ~ "Hispanic",
    ethnicity == "Unknown_Spanish-Hispanic-Latino" ~ "Hispanic",
    ethnicity == "White_Non-Spanish-Hispanic-Latino" ~ "White",
    ethnicity == "White_Spanish-Hispanic-Latino" ~ "Hispanic"
  )

  # HPV incidence
  p_HPV_incidence <- rep(NA, n_pred)
  for (i in 1:n_pred) {
    p_HPV_incidence[i] <- HPV_inc %>%
      filter(
        Ethnicity == eth_new2[i],
        Sex == sex[i],
        Age == age_new[i]
      ) %>%
      select(freq) %>%
      pull()
  }

  # Final calculation -------------------------------------------------------

  p_HNC_incidence_all <- matrix(rep(p_HNC_incidence, 3),
    nrow = 3,
    byrow = TRUE
  )
  p_HPV_incidence_all <- matrix(rep(p_HPV_incidence, 3),
    nrow = 3,
    byrow = TRUE
  )
  p_answer <- p_model_all * p_HNC_incidence_all / p_HPV_incidence_all
  p_opp_answer <- (1 - p_model_all) * p_HNC_incidence_all / (1 - p_HPV_incidence_all)

  out <- data.frame(age,
    smoker_pertenthousand,
    sex,
    ethnicity,
    marital_status_at_diagnosis,
    insurance,
    p_model_low = p_model_all[1, ],
    p_model = p_model_all[2, ],
    p_model_high = p_model_all[3, ],
    p_HNC_incidence_low = p_HNC_incidence_all[1, ],
    p_HNC_incidence = p_HNC_incidence_all[2, ],
    p_HNC_incidence_high = p_HNC_incidence_all[3, ],
    p_HPV_incidence_low = p_HPV_incidence_all[1, ],
    p_HPV_incidence = p_HPV_incidence_all[2, ],
    p_HPV_incidence_high = p_HPV_incidence_all[3, ],
    p_HNC_no_HPV_low = p_opp_answer[1, ],
    p_HNC_no_HPV = p_opp_answer[2, ],
    p_HNC_no_HPV_high = p_opp_answer[3, ],
    p_HNC_low = p_answer[1, ],
    p_HNC = p_answer[2, ],
    p_HNC_high = p_answer[3, ]
  )

  return(out)
}
