# Run the full inversion model and produce probabilities of HNC | HPV + covars

# Clear the workspace
rm(list = ls())

# Load in packages
library(tidyverse)
library(BART)
library(readxl)
library(gridExtra)
library(directlabels)
library(ggpubr)
library(grid)
library(grDevices)

# Source in the model code
source("full_bayes_inversion_20210326.R")

# Basic plot --------------------------------------------------------------

# Choose some covariate values
age_vals <- c(25, 40, 50, 60, 70)
n_pred <- length(age_vals)
p1 <- predict_fun_ci(
  age = age_vals,
  sex = rep("Male", n_pred),
  smoker_pertenthousand = rep(1800, n_pred),
  ethnicity = rep("White_Non-Spanish-Hispanic-Latino", n_pred),
  marital_status_at_diagnosis =
    rep("Married (including common law)", n_pred),
  insurance = rep("Insured", n_pred)
)

# Now want to compute:
# P(OPSCC | Not HPV) = (1 - P(HPV | OPSCC)) * P(OPSCC) / (1 - P(HPV))
# p_answer = p_model_all * p_HNC_incidence_all / p_HPV_incidence_all
# New p_answer = p_model_all * (1 - p_HNC_incidence_all) / 1 - p_HPV_incidence_all)

ggplot(p1, aes(x = age, y = p_HNC * 10000)) +
  geom_ribbon(aes(ymin = p_HNC_low * 10000, ymax = p_HNC_high * 10000), alpha = 0.4) +
  # geom_ribbon(aes(ymin=p_HNC_no_HPV_low*10000, ymax=p_HNC_no_HPV_high*10000), alpha = 0.4, fill = "red") +
  geom_line() +
  theme_bw() +
  labs(
    x = "Age", y = "Cases per ten thousand",
    title = "Probability of HNC given HPV for married, insured, white males"
  )

# Compare sexes -----------------------------------------------------------

# Predict over different sexes
age_vals2 <- rep(age_vals, 2)
sex_vals <- rep(c("Male", "Female"), each = n_pred)
n_pred2 <- length(age_vals2)
p2 <- predict_fun_ci(
  age = age_vals2,
  smoker_pertenthousand = rep(1800, n_pred2),
  sex = sex_vals,
  ethnicity = rep("White_Non-Spanish-Hispanic-Latino", n_pred2),
  marital_status_at_diagnosis =
    rep("Married (including common law)", n_pred2),
  insurance = rep("Insured", n_pred2)
) %>% rename(Sex = sex)

# Have a look at the difference between the different parts
# p2a = p2 %>% select(age, sex, p_model, p_HNC_incidence, p_HPV_incidence, p_HNC)

plot2a <- ggplot(p2, aes(x = age, y = p_HNC * 10000, colour = Sex, linetype = Sex)) +
  # geom_dl(aes(label = sex),
  #   method = "last.points"
  # ) +
  # scale_x_continuous(limits = c(25, 75)) +
  geom_ribbon(aes(ymin = p_HNC_low * 10000, ymax = p_HNC_high * 10000, fill = Sex),
    alpha = 0.4, colour = NA#, show.legend = FALSE
  ) +
  #geom_line(show.legend = FALSE) +
  geom_line() +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(
    title = "Probability of OPSCC given HPV for married insured whites",
    subtitle = "(with 95% credible interval)"
  )
plot2b <- ggplot(p2, aes(x = age, y = p_HNC_no_HPV * 10000, colour = Sex, linetype = Sex)) +
  # geom_dl(aes(label = Sex),
  #   method = "last.points"
  # ) +
  # scale_x_continuous(limits = c(25, 75)) +
  geom_ribbon(aes(ymin = p_HNC_no_HPV_low * 10000, ymax = p_HNC_no_HPV_high * 10000, fill = Sex),
    alpha = 0.4, colour = NA#, show.legend = FALSE
  ) +
  geom_line() + #show.legend = FALSE) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(
    title = "Probability of OPSCC given no HPV for married insured whites",
    subtitle = "(with 95% credible interval)"
  )

plot2 <-  ggarrange(plot2a, plot2b, ncol=1, nrow=2, common.legend = TRUE, legend = 'right')
plot2 <- annotate_figure(plot2, left = textGrob("Cases per ten thousand", rot = 90, vjust = 1),
                bottom = textGrob("Age"))
#mylegend<-g_legend(plot2a)
# plot2 <- grid.arrange(plot2a, plot2b,
#   ncol = 1,
#   bottom = "Age",
#   left = "Cases per ten thousand"
# )
ggsave(plot2, file = "output/p_HNC_by_sex_20210705.pdf", width = 10, height = 5)
ggsave(plot2, file = "output/p_HNC_by_sex_20210705.png", width = 10, height = 5)
#ggsave(plot2, file = "output/p_HNC_by_sex_20210705.tiff", width = 10, height = 5) #, compression = "lzw", type = "Xlib")
#ggsave(plot2, file = "output/p_HNC_by_sex_20210705.eps", width = 10, height = 5, device = cairo_ps)
write.csv(p2, file = "output/p_HNC_by_sex_20210705.csv", quote = FALSE, row.names = FALSE)
system("pdf2ps output/p_HNC_by_sex_20210705.pdf output/p_HNC_by_sex_20210705.eps")

# Marital status ----------------------------------------------------------

# Predict over marriage types 2 sexes
sex_vals <- c("Male", "Female")
marriage_vals <- c(
  "Single (never married)",
  "Married (including common law)"
)
grid_covars <- expand.grid(age_vals, sex_vals, marriage_vals,
  stringsAsFactors = FALSE
)
# table(grid_covars)
n_pred3 <- nrow(grid_covars)
p3 <- predict_fun_ci(
  age = grid_covars[, 1],
  smoker_pertenthousand = rep(1800, n_pred3),
  sex = grid_covars[, 2],
  ethnicity = rep("White_Non-Spanish-Hispanic-Latino", n_pred3),
  marital_status_at_diagnosis = grid_covars[, 3],
  insurance = rep("Insured", n_pred3)
)
levels(p3$marital_status_at_diagnosis) <- c("Married", "Single")

plot3a <- ggplot(p3, aes(
  x = age, y = p_HNC * 10000,
  colour = marital_status_at_diagnosis
)) +
  geom_line() +
  geom_line(show.legend = FALSE) +
  geom_dl(aes(label = marital_status_at_diagnosis),
    method = "last.points"
  ) +
  scale_x_continuous(limits = c(25, 75)) +
  guides(
    colour = guide_legend(title = "Sex"),
    linetype = guide_legend(title = "Marital Status")
  ) +
  geom_ribbon(aes(
    ymin = p_HNC_low * 10000, ymax = p_HNC_high * 10000,
    fill = marital_status_at_diagnosis
  ),
  alpha = 0.4, colour = NA, show.legend = FALSE
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  facet_grid(sex ~ ., scales = "free") +
  labs(
    title = "Probability of OPSCC given HPV for insured whites by marital status",
    subtitle = "(with 95% credible interval)"
  )
plot3b <- ggplot(p3, aes(
  x = age, y = p_HNC_no_HPV * 10000,
  colour = marital_status_at_diagnosis
)) +
  geom_line(show.legend = FALSE) +
  geom_dl(aes(label = marital_status_at_diagnosis),
    method = "last.points"
  ) +
  scale_x_continuous(limits = c(25, 75)) +
  guides(
    colour = guide_legend(title = "Sex"),
    linetype = guide_legend(title = "Marital Status")
  ) +
  geom_ribbon(aes(
    ymin = p_HNC_no_HPV_low * 10000, ymax = p_HNC_no_HPV_high * 10000,
    fill = marital_status_at_diagnosis
  ),
  alpha = 0.4, colour = NA, show.legend = FALSE
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  facet_grid(sex ~ ., scales = "free") +
  labs(
    title = "Probability of OPSCC given no HPV for insured whites by marital status",
    subtitle = "(with 95% credible interval)"
  )
plot3 <- grid.arrange(plot3a, plot3b,
  ncol = 1,
  bottom = "Age",
  left = "Cases per ten thousand"
)
ggsave(plot3, file = "output/p_HNC_by_marital_status_20210705.pdf", width = 10, height = 5)
write.csv(p3, file = "output/p_HNC_by_marital_status_20210705.csv", quote = FALSE, row.names = FALSE)

# Race --------------------------------------------------------------------

# Look at race
sex_vals <- c("Male", "Female")
race_vals <- c(
  "White_Non-Spanish-Hispanic-Latino",
  "Black",
  "White_Spanish-Hispanic-Latino"
)
grid_covars2 <- expand.grid(age_vals, sex_vals, race_vals,
  stringsAsFactors = FALSE
)
n_pred4 <- nrow(grid_covars2)
# table(grid_covars2)
p4 <- predict_fun_ci(
  age = grid_covars2[, 1],
  smoker_pertenthousand = rep(1800, n_pred4),
  sex = grid_covars2[, 2],
  ethnicity = grid_covars2[, 3],
  marital_status_at_diagnosis = "Married (including common law)",
  insurance = rep("Insured", n_pred4)
)
p4$Ethnicity <- factor(p4$ethnicity)
levels(p4$Ethnicity) <- c("Black", "White", "Hispanic")

# Look at values
p4a <- p4 %>% select(age, sex, Ethnicity, p_model, p_HNC_incidence, p_HPV_incidence, p_HNC)
plot4a <- ggplot(p4, aes(x = age, y = p_HNC * 10000, colour = Ethnicity, linetype = Ethnicity)) +
  geom_line() +
  # geom_line(show.legend = FALSE) +
  # geom_dl(aes(label = ethnicity),
  #   method = "last.points"
  # ) +
  # scale_x_continuous(limits = c(25, 75)) +
  # guides(
  #   colour = guide_legend(title = "Sex"),
  #   linetype = guide_legend(title = "Ethnicity")
  # ) +
  geom_ribbon(aes(ymin = p_HNC_low * 10000, ymax = p_HNC_high * 10000, fill = Ethnicity),
    alpha = 0.4, colour = NA #, show.legend = FALSE
  ) +
  #guides(colour = guide_legend(title = "Ethnicity")) +
  facet_grid(sex ~ ., scales = "free") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(
    title = "Probability of OPSCC given HPV for married insured people by ethnicity",
    subtitle = "(with 95% credible interval)"
  )
plot4b <- ggplot(p4, aes(x = age, y = p_HNC_no_HPV * 10000, colour = Ethnicity, linetype = Ethnicity)) +
  geom_line() +
  # geom_line(show.legend = FALSE) +
  # geom_dl(aes(label = ethnicity),
  #   method = "last.points"
  # ) +
  # scale_x_continuous(limits = c(25, 75)) +
  # guides(
  #   colour = guide_legend(title = "Sex"),
  #   linetype = guide_legend(title = "Ethnicity")
  # ) +
  geom_ribbon(aes(ymin = p_HNC_no_HPV_low * 10000, ymax = p_HNC_no_HPV_high * 10000, fill = Ethnicity),
    alpha = 0.4, colour = NA
  ) +
  # guides(colour = guide_legend(title = "Ethnicity")) +
  facet_grid(sex ~ ., scales = "free") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(
    title = "Probability of OPSCC given no HPV for married insured people by ethnicity",
    subtitle = "(with 95% credible interval)"
  )
# plot4 <- grid.arrange(plot4a, plot4b,
#   ncol = 1,
#   bottom = "Age",
#   left = "Cases per ten thousand"
# )
plot4 <-  ggarrange(plot4a, plot4b, ncol=1, nrow=2, common.legend = TRUE, legend = 'right')
plot4 <- annotate_figure(plot4, left = textGrob("Cases per ten thousand", rot = 90, vjust = 1),
                         bottom = textGrob("Age"))
ggsave(plot4, file = "output/p_HNC_by_ethnicity_20210705.pdf", width = 10, height = 5)
ggsave(plot4, file = "output/p_HNC_by_ethnicity_20210705.png", width = 10, height = 5)
write.csv(p4, file = "output/p_HNC_by_ethnicity_20210705.csv", quote = FALSE, row.names = FALSE)
system("pdf2ps output/p_HNC_by_ethnicity_20210705.pdf output/p_HNC_by_ethnicity_20210705.eps")

# Smoking -----------------------------------------------------------------

# Look at effect of smoker percentage
sex_vals <- c("Male", "Female")
smoker_vals <- seq(5, 35, by = 10) * 100
grid_covars3 <- expand.grid(age_vals, sex_vals, smoker_vals,
  stringsAsFactors = FALSE
)
n_pred5 <- nrow(grid_covars3)

# Create predictions
p5 <- predict_fun_ci(
  age = grid_covars3[, 1],
  smoker_pertenthousand = grid_covars3[, 3],
  sex = grid_covars3[, 2],
  ethnicity = rep("White_Non-Spanish-Hispanic-Latino", n_pred5),
  marital_status_at_diagnosis = rep("Married (including common law)", n_pred5),
  insurance = rep("Insured", n_pred5)
)
p5$smoker_pertenthousand <- factor(p5$smoker_pertenthousand,
  labels = as.character(paste0(seq(5, 35, by = 10), "%"))
)

plot5a <- ggplot(p5, aes(x = age, y = p_HNC * 10000, colour = smoker_pertenthousand)) +
  geom_line(show.legend = FALSE) +
  geom_dl(aes(label = smoker_pertenthousand),
    method = "last.points"
  ) +
  scale_x_continuous(limits = c(25, 75)) +
  guides(
    colour = guide_legend(title = "Sex"),
    linetype = guide_legend(title = "Smoking % (by county)")
  ) +
  geom_ribbon(aes(ymin = p_HNC_low * 10000, ymax = p_HNC_high * 10000, fill = smoker_pertenthousand),
    alpha = 0.4, colour = NA, show.legend = FALSE
  ) +
  guides(colour = guide_legend(title = "Smoking % (by county)")) +
  facet_grid(sex ~ ., scales = "free") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(
    title = "Probability of OPSCC given HPV for married insured whites by regional smoking percentage",
    subtitle = "(with 95% credible interval)"
  )
plot5b <- ggplot(p5, aes(x = age, y = p_HNC_no_HPV * 10000, colour = smoker_pertenthousand)) +
  geom_line(show.legend = FALSE) +
  geom_dl(aes(label = smoker_pertenthousand),
    method = "last.points"
  ) +
  scale_x_continuous(limits = c(25, 75)) +
  guides(
    colour = guide_legend(title = "Sex"),
    linetype = guide_legend(title = "Smoking % (by county)")
  ) +
  geom_ribbon(aes(ymin = p_HNC_no_HPV_low * 10000, ymax = p_HNC_no_HPV_high * 10000, fill = smoker_pertenthousand),
    alpha = 0.4, colour = NA, show.legend = FALSE
  ) +
  guides(colour = guide_legend(title = "Smoking % (by county)")) +
  facet_grid(sex ~ ., scales = "free") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(
    title = "Probability of OPSCC given no HPV for married insured whites by regional smoking percentage",
    subtitle = "(with 95% credible interval)"
  )
plot5 <- grid.arrange(plot5a, plot5b,
  ncol = 1,
  bottom = "Age",
  left = "Cases per ten thousand"
)
ggsave(plot5, file = "output/p_HNC_by_smoking_20210705.pdf", width = 10, height = 5)
write.csv(p5, file = "output/p_HNC_by_smoking_20210705.csv", quote = FALSE, row.names = FALSE)

# Insurance types ---------------------------------------------------------

# Look at insurance types
sex_vals <- c("Male", "Female")
insurance_vals <- c(
  "Insured",
  "Uninsured"
)
grid_covars4 <- expand.grid(age_vals, sex_vals, insurance_vals,
  stringsAsFactors = FALSE
)
n_pred6 <- nrow(grid_covars4)

# Create predictions
p6 <- predict_fun_ci(
  age = grid_covars4[, 1],
  smoker_pertenthousand = rep(1800, n_pred6),
  sex = grid_covars4[, 2],
  ethnicity = rep("White_Non-Spanish-Hispanic-Latino", n_pred6),
  marital_status_at_diagnosis = rep("Married (including common law)", n_pred6),
  insurance = grid_covars4[, 3]
)

plot6a <- ggplot(p6, aes(x = age, y = p_HNC * 10000, colour = insurance)) +
  geom_line(show.legend = FALSE) +
  geom_dl(aes(label = insurance),
    method = "last.points"
  ) +
  scale_x_continuous(limits = c(25, 75)) +
  guides(
    colour = guide_legend(title = "Sex"),
    linetype = guide_legend(title = "Insurance")
  ) +
  geom_ribbon(aes(ymin = p_HNC_low * 10000, ymax = p_HNC_high * 10000, fill = insurance),
    alpha = 0.4, colour = NA, show.legend = FALSE
  ) +
  guides(colour = guide_legend(title = "Insurance")) +
  facet_grid(sex ~ ., scales = "free") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(
    title = "Probability of HNC given HPV for married insured whites by medical insurance",
    subtitle = "(with 95% credible interval)"
  )
plot6b <- ggplot(p6, aes(x = age, y = p_HNC_no_HPV * 10000, colour = insurance)) +
  geom_line(show.legend = FALSE) +
  geom_dl(aes(label = insurance),
    method = "last.points"
  ) +
  scale_x_continuous(limits = c(25, 75)) +
  guides(
    colour = guide_legend(title = "Sex"),
    linetype = guide_legend(title = "Insurance")
  ) +
  geom_ribbon(aes(ymin = p_HNC_no_HPV_low * 10000, ymax = p_HNC_no_HPV_high * 10000, fill = insurance),
    alpha = 0.4, colour = NA, show.legend = FALSE
  ) +
  guides(colour = guide_legend(title = "Insurance")) +
  facet_grid(sex ~ ., scales = "free") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  labs(
    title = "Probability of HNC given no HPV for married insured whites by medical insurance",
    subtitle = "(with 95% credible interval)"
  )
plot6 <- grid.arrange(plot6a, plot6b,
  ncol = 1,
  bottom = "Age",
  left = "Cases per ten thousand"
)
ggsave(file = "output/p_HNC_by_insurance_20210705.pdf", width = 10, height = 5)
write.csv(p6, file = "output/p_HNC_by_insurance_20210705.csv", quote = FALSE, row.names = FALSE)
