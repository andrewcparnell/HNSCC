# Run the full inversion model and produce probabilities of HNC | HPV + covars

# Clear the workspace
rm(list = ls())

# Load in packages
library(tidyverse)
library(bartMachine)
library(readxl)
library(directlabels)

# Source in the model code
source('full_bayes_inversion_20191218.R')

# Basic plot --------------------------------------------------------------

# Choose some covariate values
age_vals = c(25, 40, 50, 60, 70)
n_pred = length(age_vals)
p1 = predict_fun_ci(age = age_vals,
                sex = rep('Male', n_pred),
                smoker_pertenthousand = rep(1800, n_pred),
                ethnicity = rep('White_Non-Spanish-Hispanic-Latino', n_pred),
                marital_status_at_diagnosis =
                  rep('Married (including common law)', n_pred),
                insurance = rep('Insured', n_pred))

ggplot(p1, aes(x = age, y = p_HNC*10000)) +
  geom_ribbon(aes(ymin=p_HNC_low*10000, ymax=p_HNC_high*10000), alpha = 0.4) +
  geom_line() +
  theme_bw() +
  labs(x = 'Age', y = 'Cases per ten thousand',
       title = 'Probability of HNC given HPV for married, insured, white males')

# Compare sexes -----------------------------------------------------------

# Predict over different sexes
age_vals2 = rep(age_vals, 2)
sex_vals = rep(c('Male', 'Female'), each = n_pred)
n_pred2 = length(age_vals2)
p2 = predict_fun_ci(age = age_vals2,
                    smoker_pertenthousand = rep(1800, n_pred2),
                    sex = sex_vals,
                    ethnicity = rep('White_Non-Spanish-Hispanic-Latino', n_pred2),
                    marital_status_at_diagnosis = 
                      rep('Married (including common law)', n_pred2),
                    insurance = rep('Insured', n_pred2))

# Have a look at the difference between the different parts
# p2a = p2 %>% select(age, sex, p_model, p_HNC_incidence, p_HPV_incidence, p_HNC)

ggplot(p2, aes(x = age, y = p_HNC*10000, colour = sex)) +
  geom_dl(aes(label = sex), 
          method = 'last.points') +
  scale_x_continuous(limits = c(25, 75)) +
  geom_ribbon(aes(ymin=p_HNC_low*10000, ymax=p_HNC_high*10000, fill = sex),
              alpha = 0.4, colour = NA, show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  theme_bw() +
  labs(x = 'Age', y = 'Cases per ten thousand',
       title = 'Probability of HNSCC given HPV for married insured whites',
       subtitle = '(with 95% uncertainty interval)')
stop()
ggsave(file = 'p_HNC_by_sex_20191218.pdf', width = 10, height = 5)
write.csv(p2, file = 'p_HNC_by_sex_20191218.csv', quote = FALSE, row.names = FALSE)

# Marital status ----------------------------------------------------------

# Predict over marriage types 2 sexes
sex_vals = c('Male', 'Female')
marriage_vals = c('Single (never married)', 
                  'Married (including common law)')
grid_covars = expand.grid(age_vals, sex_vals, marriage_vals,
                          stringsAsFactors = FALSE)
#table(grid_covars)
n_pred3 = nrow(grid_covars)
p3 = predict_fun_ci(age = grid_covars[,1],
                    smoker_pertenthousand = rep(1800, n_pred3),
                 sex = grid_covars[,2],
                 ethnicity = rep('White_Non-Spanish-Hispanic-Latino', n_pred3),
                 marital_status_at_diagnosis = grid_covars[,3],
                 insurance = rep('Insured', n_pred3))
levels(p3$marital_status_at_diagnosis) = c("Married", "Single")    

ggplot(p3, aes(x = age, y = p_HNC*10000,
               colour = marital_status_at_diagnosis)) + 
  geom_line(show.legend = FALSE) +
  geom_dl(aes(label = marital_status_at_diagnosis), 
          method = 'last.points') +
  scale_x_continuous(limits = c(25, 75)) +
  guides(colour = guide_legend(title = "Sex"),
         linetype = guide_legend(title = 'Marital Status')) +
  geom_ribbon(aes(ymin=p_HNC_low*10000, ymax=p_HNC_high*10000, 
                  fill = marital_status_at_diagnosis),
              alpha = 0.4, colour = NA, show.legend = FALSE) +
  theme_bw() +
  facet_grid(sex ~ ., scales = 'free') +
  labs(x = 'Age', y = 'Cases per ten thousand',
       title = 'Probability of HNSCC given HPV for insured whites by marital status',
       subtitle = '(with 95% uncertainty interval)')
ggsave(file = 'p_HNC_by_marital_status_20191218.pdf', width = 10, height = 5)
write.csv(p3, file = 'p_HNC_by_marital_status_20191218.csv', quote = FALSE, row.names = FALSE)

# Race --------------------------------------------------------------------

# Look at race
sex_vals = c('Male', 'Female')
race_vals = c('White_Non-Spanish-Hispanic-Latino',
              'Black',
              'White_Spanish-Hispanic-Latino')
grid_covars2 = expand.grid(age_vals, sex_vals, race_vals,
                          stringsAsFactors = FALSE)
n_pred4 = nrow(grid_covars2)
#table(grid_covars2)
p4 = predict_fun_ci(age = grid_covars2[,1],
                    smoker_pertenthousand = rep(1800, n_pred4),
                    sex = grid_covars2[,2],
                    ethnicity = grid_covars2[,3],
                    marital_status_at_diagnosis = 'Married (including common law)',
                    insurance = rep('Insured', n_pred4))
levels(p4$ethnicity) = c('Black', 'White', 'Hispanic')

# Look at values
p4a = p4 %>% select(age, sex, ethnicity, p_model, p_HNC_incidence, p_HPV_incidence, p_HNC)

ggplot(p4, aes(x = age, y = p_HNC*10000, colour = ethnicity)) + 
  geom_line(show.legend = FALSE) +
  geom_dl(aes(label = ethnicity), 
          method = 'last.points') +
  scale_x_continuous(limits = c(25, 75)) +
  guides(colour = guide_legend(title = "Sex"),
         linetype = guide_legend(title = 'Ethnicity')) +
  geom_ribbon(aes(ymin=p_HNC_low*10000, ymax=p_HNC_high*10000, fill = ethnicity),
              alpha = 0.4, colour = NA, show.legend = FALSE) +
  guides(colour = guide_legend(title = "Ethnicity")) +
  facet_grid(sex ~ ., scales = 'free') +
  theme_bw() +
  labs(x = 'Age', y = 'Cases per ten thousand',
       title = 'Probability of HNSCC given HPV for married insured people by ethnicity',
       subtitle = '(with 95% uncertainty interval)')
ggsave(file = 'p_HNC_by_ethnicity_20191218.pdf', width = 10, height = 5)
write.csv(p4, file = 'p_HNC_by_ethnicity_20191218.csv', quote = FALSE, row.names = FALSE)

# Smoking -----------------------------------------------------------------

# Look at effect of smoker pergentage
sex_vals = c('Male', 'Female')
smoker_vals = seq(5, 35, by = 10) * 100
grid_covars3 = expand.grid(age_vals, sex_vals, smoker_vals,
                           stringsAsFactors = FALSE)
n_pred5 = nrow(grid_covars3)

# Create predictions
p5 = predict_fun_ci(age = grid_covars3[,1],
                    smoker_pertenthousand = grid_covars3[,3],
                    sex = grid_covars3[,2],
                    ethnicity = rep('White_Non-Spanish-Hispanic-Latino',n_pred5),
                    marital_status_at_diagnosis = rep('Married (including common law)', n_pred5),
                    insurance = rep('Insured', n_pred5))
p5$smoker_pertenthousand = factor(p5$smoker_pertenthousand,
                                  labels = as.character(paste0(seq(5, 35, by = 10),'%')))

ggplot(p5, aes(x = age, y = p_HNC*10000, colour = smoker_pertenthousand)) + 
  geom_line(show.legend = FALSE) +
  geom_dl(aes(label = smoker_pertenthousand), 
          method = 'last.points') +
  scale_x_continuous(limits = c(25, 75)) +
  guides(colour = guide_legend(title = "Sex"),
         linetype = guide_legend(title = 'Smoking % (by county)')) +
  geom_ribbon(aes(ymin=p_HNC_low*10000, ymax=p_HNC_high*10000, fill = smoker_pertenthousand),
              alpha = 0.4, colour = NA, show.legend = FALSE) +
  guides(colour = guide_legend(title = 'Smoking % (by county)')) +
  facet_grid(sex ~ ., scales = 'free') +
  theme_bw() +
  labs(x = 'Age', y = 'Cases per ten thousand',
       title = 'Probability of HNSCC given HPV for married insured whites by regional smoking percentage',
       subtitle = '(with 95% uncertainty interval)')
ggsave(file = 'p_HNC_by_smoking_20191218.pdf', width = 10, height = 5)
write.csv(p5, file = 'p_HNC_by_smoking_20191218.csv', quote = FALSE, row.names = FALSE)

# Insurance types ---------------------------------------------------------

# Look at insurance types
sex_vals = c('Male', 'Female')
insurance_vals = c("Insured", 
                    "Uninsured")
grid_covars4 = expand.grid(age_vals, sex_vals, insurance_vals,
                           stringsAsFactors = FALSE)
n_pred6 = nrow(grid_covars4)

# Create predictions
p6 = predict_fun_ci(age = grid_covars4[,1],
                    smoker_pertenthousand = rep(1800, n_pred6),
                    sex = grid_covars4[,2],
                    ethnicity = rep('White_Non-Spanish-Hispanic-Latino', n_pred6),
                    marital_status_at_diagnosis = rep('Married (including common law)', n_pred6),
                    insurance = grid_covars4[,3])

ggplot(p6, aes(x = age, y = p_HNC*10000, colour = insurance)) + 
  geom_line(show.legend = FALSE) +
  geom_dl(aes(label = insurance), 
          method = 'last.points') +
  scale_x_continuous(limits = c(25, 75)) +
  guides(colour = guide_legend(title = "Sex"),
         linetype = guide_legend(title = 'Insurance')) +
  geom_ribbon(aes(ymin=p_HNC_low*10000, ymax=p_HNC_high*10000, fill = insurance),
              alpha = 0.4, colour = NA, show.legend = FALSE) +
  guides(colour = guide_legend(title = 'Insurance')) +
  facet_grid(sex ~ ., scales = 'free') +
  theme_bw() +
  labs(x = 'Age', y = 'Cases per ten thousand',
       title = 'Probability of HNC given HPV for married insured whites by medical insurance',
       subtitle = '(with 95% uncertainty interval)')
ggsave(file = 'p_HNC_by_insurance_20191218.pdf', width = 10, height = 5)
write.csv(p6, file = 'p_HNC_by_insurance_20191218.csv', quote = FALSE, row.names = FALSE)

