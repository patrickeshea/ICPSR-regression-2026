## Set your working directory to the ICPSR_regression folder
## Change the path below to match your computer
# setwd("~/ICPSR_regression")
setwd("C:/Users/Patrick Shea/Dropbox/My Projects/Spring 2026/ICPSR/ICPSR_OLS26")

# =============================================================================
# Session 4 Lab - Example Solutions
# Women in Politics and Military Spending
# Multivariate OLS and Dummy Variables
# =============================================================================
rm(list=ls())

# Load required libraries
library(haven)    # for reading .dta files
library(ggplot2)  # for plotting
library(stargazer) # for nice tables (optional)
library(modelsummary)
library(marginaleffects)


# =============================================================================
# SETUP AND PART 1: DATA EXPLORATION
# =============================================================================

# Load the data
women12 <- read_dta("data/women12.dta")

# Examine the data structure
str(women12)
head(women12)

# Create the variables we need from existing data
women12$leftright <- women12$DPI_Left  # Use left dummy as ideology measure
women12$partycontrol <- women12$partcont  # Rename party control variable

# 1.1 Explore key variables

# Military expenditure (dependent variable)
summary(women12$mil_exp_pergdp2)

# Women in parliament
summary(women12$m_woman2)

# Female leaders
table(women12$m_femlead, useNA = "always")
round(mean(women12$m_femlead, na.rm = TRUE) * 100, 1)

# Government ideology (left dummy)
table(women12$leftright, useNA = "always")
round(mean(women12$leftright, na.rm = TRUE) * 100, 1)

# Party control (0-2 scale)
summary(women12$partycontrol)

# 1.2 Summary statistics for key variables
key_vars <- women12[, c("mil_exp_pergdp2", "m_woman2", "m_femlead", "leftright", "partycontrol")]
summary(key_vars)

# Check for missing data
sapply(key_vars, function(x) sum(is.na(x)))

# 1.3 Initial visualization
# Scatter plot: Women in Parliament vs Military Spending
plot(women12$m_woman2, women12$mil_exp_pergdp2,
     xlab = "Women in Parliament (%)",
     ylab = "Military Expenditure (% GDP)",
     main = "Women's Representation vs Military Spending",
     pch = 19, col = "darkblue", cex = 0.8)

abline(lm(mil_exp_pergdp2 ~ m_woman2, data = women12), col = "red", lwd = 2)

correlation <- cor(women12$m_woman2, women12$mil_exp_pergdp2, use = "complete.obs")
correlation

# Additional exploratory plots
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

hist(women12$mil_exp_pergdp2, breaks = 20, col = "lightblue",
     main = "Distribution of Military Spending",
     xlab = "Military Expenditure (% GDP)")

hist(women12$m_woman2, breaks = 20, col = "lightcoral",
     main = "Distribution of Women in Parliament",
     xlab = "Women in Parliament (%)")

boxplot(mil_exp_pergdp2 ~ m_femlead, data = women12,
        names = c("Male Leader", "Female Leader"),
        col = c("lightgray", "pink"),
        main = "Military Spending by Leader Gender",
        ylab = "Military Expenditure (% GDP)")

plot(women12$m_woman2, women12$mil_exp_pergdp2,
     col = ifelse(women12$m_femlead == 1, "red", "blue"),
     pch = 19, cex = 0.8,
     xlab = "Women in Parliament (%)",
     ylab = "Military Expenditure (% GDP)",
     main = "By Leader Gender")
legend("topright", c("Male Leader", "Female Leader"),
       col = c("blue", "red"), pch = 19, cex = 0.8)

par(mfrow = c(1, 1))

# =============================================================================
# PART 2: SIMPLE VS MULTIVARIATE COMPARISON
# =============================================================================

# 2.1 Simple bivariate model
simple_model <- lm(mil_exp_pergdp2 ~ m_woman2, data = women12)
summary(simple_model)

# 2.2 Multivariate model with ideology control
multi_model <- lm(mil_exp_pergdp2 ~ m_woman2 + leftright, data = women12)
summary(multi_model)

# 2.3 Compare coefficients
simple_coef <- coef(simple_model)[2]
multi_coef <- coef(multi_model)[2]

# Women in Parliament coefficient comparison
simple_coef
multi_coef
multi_coef - simple_coef

# R-squared comparison
simple_r2 <- summary(simple_model)$r.squared
multi_r2 <- summary(multi_model)$r.squared
simple_r2
multi_r2
multi_r2 - simple_r2

# =============================================================================
# PART 3: UNDERSTANDING DUMMY VARIABLES
# =============================================================================

# 3.1 Examine female leader variable
table(women12$m_femlead, useNA = "always")
round(mean(women12$m_femlead, na.rm = TRUE) * 100, 1)

# 3.2 Add female leader dummy
dummy_model <- lm(mil_exp_pergdp2 ~ m_woman2 + m_femlead + leftright, data = women12)
summary(dummy_model)

# 3.3 Interpret dummy coefficient
dummy_coef <- coef(dummy_model)
dummy_coef[3]

# Statistical significance of female leader
p_value <- summary(dummy_model)$coefficients[3, 4]
p_value

# =============================================================================
# PART 4: CREATING YOUR OWN DUMMY VARIABLES
# =============================================================================

# 4.1 Create high women's representation dummy (above median)
women12$high_women <- ifelse(women12$m_woman2 > median(women12$m_woman2, na.rm = TRUE), 1, 0)

median_women <- median(women12$m_woman2, na.rm = TRUE)
median_women
table(women12$high_women, useNA = "always")

# 4.2 Create party control categories
women12$party_control_cat <- cut(women12$partycontrol,
                                 breaks = c(-1, 0.5, 1.5, 3),
                                 labels = c("Low", "Medium", "High"),
                                 include.lowest = TRUE)

table(women12$party_control_cat, useNA = "always")

# 4.3 Models with new dummy variables

# Binary vs continuous women's representation
binary_model <- lm(mil_exp_pergdp2 ~ high_women + m_femlead + leftright, data = women12)
summary(binary_model)

# Party control model
party_model <- lm(mil_exp_pergdp2 ~ m_woman2 + m_femlead + leftright + party_control_cat, data = women12)
summary(party_model)

# =============================================================================
# PART 5: MODEL COMPARISON AND INTERPRETATION
# =============================================================================

# 5.1 Comprehensive full model
full_model <- lm(mil_exp_pergdp2 ~ m_woman2 + m_femlead + leftright + party_control_cat,
                 data = women12)
summary(full_model)

# 5.2 Model comparison table
models <- list("Simple" = simple_model,
               "+ Ideology" = multi_model,
               "+ Female Leader" = dummy_model,
               "Full Model" = full_model)

comparison_df <- data.frame(
  Model = names(models),
  R_squared = sapply(models, function(x) round(summary(x)$r.squared, 3)),
  Adj_R_squared = sapply(models, function(x) round(summary(x)$adj.r.squared, 3)),
  N_obs = sapply(models, function(x) nobs(x)),
  stringsAsFactors = FALSE
)
print(comparison_df)

# 5.3 Interpret full model coefficients
full_coefs <- coef(full_model)

# Intercept: military spending when all other variables = 0
full_coefs[1]

# Women in Parliament: each 1% increase changes mil spending by this amount
full_coefs[2]

# Female Leader: difference compared to male leaders
full_coefs[3]

# Left Government
dummy_coef[4]

# Party control (reference: Low)
if(length(full_coefs) > 5) {
  full_coefs[5]  # Medium
  full_coefs[6]  # High
}

# =============================================================================
# PART 6: PREDICTIONS AND SCENARIOS
# =============================================================================

# 6.1 Prediction scenarios

# Scenario 1: Low women's representation, male leader, non-left government
scenario1 <- data.frame(
  m_woman2 = 10,
  m_femlead = 0,
  leftright = 0,
  party_control_cat = factor("Medium", levels = c("Low", "Medium", "High"))
)

pred1 <- predict(full_model, newdata = scenario1, interval = "prediction")
pred1

# Scenario 2: High women's representation, female leader, left government
scenario2 <- data.frame(
  m_woman2 = 40,
  m_femlead = 1,
  leftright = 1,
  party_control_cat = factor("Medium", levels = c("Low", "Medium", "High"))
)

pred2 <- predict(full_model, newdata = scenario2, interval = "prediction")
pred2

# Difference between scenarios
pred2[1] - pred1[1]

# 6.2 Leadership gender effect — same country, different leader gender
base_scenario <- data.frame(
  m_woman2 = 25,
  leftright = 0,
  party_control_cat = factor("Medium", levels = c("Low", "Medium", "High"))
)

male_leader <- cbind(base_scenario, m_femlead = 0)
female_leader <- cbind(base_scenario, m_femlead = 1)

pred_male <- predict(full_model, newdata = male_leader)
pred_female <- predict(full_model, newdata = female_leader)

pred_male
pred_female
pred_female - pred_male

# 6.3 Party control effects
party_scenarios <- data.frame(
  m_woman2 = 25,
  m_femlead = 0,
  leftright = 0,
  party_control_cat = factor(c("Low", "Medium", "High"), levels = c("Low", "Medium", "High"))
)

party_preds <- predict(full_model, newdata = party_scenarios)
party_preds

# =============================================================================
# PART 7: TESTING KOCH & FULTON HYPOTHESES
# =============================================================================

# 7.1 Legislative hypothesis
# H1: As women's representation increases, defense spending decreases
women_coef <- coef(full_model)[2]
women_pvalue <- summary(full_model)$coefficients[2, 4]
women_coef
women_pvalue

# Effect size: 10% increase in women's representation
women_coef * 10

# 7.2 Executive hypothesis
# H2: Female chief executives increase defense spending
leader_coef <- coef(full_model)[3]
leader_pvalue <- summary(full_model)$coefficients[3, 4]
leader_coef
leader_pvalue

# 7.3 Institutional context — interaction between women's representation and party control
interaction_model <- lm(mil_exp_pergdp2 ~ m_woman2 * party_control_cat + m_femlead + leftright,
                        data = women12)
summary(interaction_model)

# =============================================================================
# PART 8: ADVANCED ANALYSIS
# =============================================================================

# 8.1 Women's representation x Female leader interaction
advanced_model <- lm(mil_exp_pergdp2 ~ m_woman2 + m_femlead + m_woman2:m_femlead + leftright,
                     data = women12)
summary(advanced_model)

# 8.2 Interpret interaction
interaction_coef <- coef(advanced_model)[5]
interaction_coef

# 8.3 Plot interaction
women_seq <- seq(0, 50, by = 5)
pred_male_leader <- predict(advanced_model,
                            newdata = data.frame(m_woman2 = women_seq,
                                                 m_femlead = 0,
                                                 leftright = 0))
pred_female_leader <- predict(advanced_model,
                              newdata = data.frame(m_woman2 = women_seq,
                                                   m_femlead = 1,
                                                   leftright = 0))

plot(women_seq, pred_male_leader, type = "l", col = "blue", lwd = 2,
     xlab = "Women in Parliament (%)",
     ylab = "Predicted Military Spending (% GDP)",
     main = "Interaction: Women's Representation x Leader Gender",
     ylim = range(c(pred_male_leader, pred_female_leader), na.rm = TRUE))
lines(women_seq, pred_female_leader, col = "red", lwd = 2)
legend("topright", c("Male Leader", "Female Leader"),
       col = c("blue", "red"), lwd = 2)

# Add confidence intervals
pred_male_ci <- predict(advanced_model,
                        newdata = data.frame(m_woman2 = women_seq,
                                             m_femlead = 0,
                                             leftright = 0),
                        interval = "confidence")
pred_female_ci <- predict(advanced_model,
                          newdata = data.frame(m_woman2 = women_seq,
                                               m_femlead = 1,
                                               leftright = 0),
                          interval = "confidence")

lines(women_seq, pred_male_ci[, "lwr"], col = "blue", lty = 2)
lines(women_seq, pred_male_ci[, "upr"], col = "blue", lty = 2)
lines(women_seq, pred_female_ci[, "lwr"], col = "red", lty = 2)
lines(women_seq, pred_female_ci[, "upr"], col = "red", lty = 2)

# =============================================================================
# PART 9: VISUALIZATION
# =============================================================================

# 9.1 Enhanced scatter plot with leader gender
plot(women12$m_woman2, women12$mil_exp_pergdp2,
     col = ifelse(women12$m_femlead == 1, "red", "blue"),
     pch = ifelse(women12$m_femlead == 1, 17, 19),
     cex = 1.2,
     xlab = "Women in Parliament (%)",
     ylab = "Military Expenditure (% GDP)",
     main = "Military Spending by Women's Representation and Leader Gender")

male_data <- subset(women12, m_femlead == 0)
female_data <- subset(women12, m_femlead == 1)

if(nrow(male_data) > 0) {
  abline(lm(mil_exp_pergdp2 ~ m_woman2, data = male_data), col = "blue", lwd = 2)
}
if(nrow(female_data) > 0) {
  abline(lm(mil_exp_pergdp2 ~ m_woman2, data = female_data), col = "red", lwd = 2)
}

legend("topright",
       c("Male Leader", "Female Leader", "Male Trend", "Female Trend"),
       col = c("blue", "red", "blue", "red"),
       pch = c(19, 17, NA, NA),
       lty = c(NA, NA, 1, 1),
       lwd = c(NA, NA, 2, 2))

# 9.2 Box plots by party control
par(mfrow = c(1, 2))

complete_data <- women12[!is.na(women12$party_control_cat) & !is.na(women12$mil_exp_pergdp2), ]

boxplot(mil_exp_pergdp2 ~ party_control_cat, data = complete_data,
        col = c("lightblue", "lightgreen", "lightcoral"),
        main = "Military Spending by Party Control",
        xlab = "Party Control Level",
        ylab = "Military Expenditure (% GDP)")

hist(women12$mil_exp_pergdp2, breaks = 20, prob = TRUE,
     col = "lightgray", border = "white",
     main = "Distribution of Military Spending",
     xlab = "Military Expenditure (% GDP)")
lines(density(women12$mil_exp_pergdp2, na.rm = TRUE), col = "red", lwd = 2)

par(mfrow = c(1, 1))

# 9.3 Coefficient plot
coef_data <- summary(full_model)$coefficients
coef_names <- rownames(coef_data)
coef_values <- coef_data[, "Estimate"]
coef_se <- coef_data[, "Std. Error"]

# Remove intercept for cleaner plot
keep_coefs <- 2:length(coef_values)
coef_names <- coef_names[keep_coefs]
coef_values <- coef_values[keep_coefs]
coef_se <- coef_se[keep_coefs]

ci_lower <- coef_values - 1.96 * coef_se
ci_upper <- coef_values + 1.96 * coef_se

plot(coef_values, 1:length(coef_values),
     xlim = range(c(ci_lower, ci_upper)),
     pch = 19, cex = 1.2,
     yaxt = "n",
     xlab = "Coefficient Estimate",
     ylab = "",
     main = "Coefficient Plot with 95% Confidence Intervals")

segments(ci_lower, 1:length(coef_values), ci_upper, 1:length(coef_values))
axis(2, at = 1:length(coef_names), labels = coef_names, las = 2, cex.axis = 0.8)
abline(v = 0, lty = 2, col = "red")


# =============================================================================
# SUBSTANTIVE RESULTS VISUALIZATION
# =============================================================================

# Calculate means for "holding other variables constant"
mean_women <- mean(women12$m_woman2, na.rm = TRUE)
mean_left <- mean(women12$leftright, na.rm = TRUE)
modal_party <- names(sort(table(women12$party_control_cat), decreasing = TRUE))[1]

print(paste("Mean women in parliament:", round(mean_women, 1), "%"))
print(paste("Proportion left government:", round(mean_left, 2)))
print(paste("Modal party control:", modal_party))

par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

# =============================================================================
# FEMALE LEADERS VISUALIZATION
# =============================================================================

# Bar plot with confidence intervals
pred_data_male <- data.frame(
  m_woman2 = mean_women,
  m_femlead = 0,
  leftright = mean_left,
  party_control_cat = factor(modal_party, levels = levels(women12$party_control_cat))
)

pred_data_female <- data.frame(
  m_woman2 = mean_women,
  m_femlead = 1,
  leftright = mean_left,
  party_control_cat = factor(modal_party, levels = levels(women12$party_control_cat))
)

pred_male <- predict(full_model, newdata = pred_data_male, interval = "confidence")
pred_female <- predict(full_model, newdata = pred_data_female, interval = "confidence")

means <- c(pred_male[1], pred_female[1])
lower_ci <- c(pred_male[2], pred_female[2])
upper_ci <- c(pred_male[3], pred_female[3])
names <- c("Male Leader", "Female Leader")

bp <- barplot(means, names.arg = names,
              ylim = c(0,6),
              col = c("lightblue", "red"),
              main = "Military Spending by Leader Gender\n(Other variables held at means)",
              ylab = "Military Expenditure (% GDP)")

arrows(bp, lower_ci, bp, upper_ci, angle = 90, code = 3, length = 0.1)
text(bp, means + 0.1, round(means, 2), pos = 3)

# Dot plot with error bars
plot(1:2, means, xlim = c(0.5, 2.5), ylim = c(min(lower_ci) - 0.2, max(upper_ci) + 0.2),
     pch = 19, cex = 2, col = c("blue", "red"),
     xaxt = "n", xlab = "Leader Gender", ylab = "Military Expenditure (% GDP)",
     main = "Military Spending by Leader Gender\n(95% Confidence Intervals)")

arrows(1:2, lower_ci, 1:2, upper_ci, angle = 90, code = 3, length = 0.1, lwd = 2)
axis(1, at = 1:2, labels = names)
abline(h = seq(floor(min(lower_ci)), ceiling(max(upper_ci)), 0.5),
       col = "lightgray", lty = 2)
text(1:2, means + 0.15, paste(round(means, 2), "%"), pos = 3, font = 2)

# modelsummary coefficient plot
modelplot(full_model, coef_omit = "Intercept") +
  labs(title = "Regression Coefficients with 95% Confidence Intervals",
       subtitle = "Effect on Military Spending (% GDP)",
       x = "Coefficient Estimate") +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red")

# =============================================================================
# WOMEN IN PARLIAMENT VISUALIZATION
# =============================================================================

# Marginal effects plot
plot_predictions(full_model, condition = "m_woman2") +
  labs(title = "Effect of Women's Parliamentary Representation",
       subtitle = "Other variables held at observed values",
       x = "Women in Parliament (%)",
       y = "Predicted Military Expenditure (% GDP)") +
  theme_minimal() +
  geom_rug(data = women12, aes(x = m_woman2), alpha = 0.3)


# Line plot showing effect across range
women_seq <- seq(min(women12$m_woman2, na.rm = TRUE),
                 max(women12$m_woman2, na.rm = TRUE),
                 length.out = 50)

pred_data_women <- data.frame(
  m_woman2 = women_seq,
  m_femlead = round(mean(women12$m_femlead, na.rm = TRUE)),
  leftright = round(mean(women12$leftright, na.rm = TRUE)),
  party_control_cat = factor(rep(modal_party, length(women_seq)),
                             levels = levels(women12$party_control_cat))
)

pred_women_line <- predict(full_model, newdata = pred_data_women, interval = "confidence")

plot(women_seq, pred_women_line[, "fit"], type = "l", lwd = 3, col = "darkgreen",
     xlab = "Women in Parliament (%)", ylab = "Military Expenditure (% GDP)",
     main = "Effect of Women's Representation\n(Other variables held at means)",
     ylim = range(pred_women_line))

polygon(c(women_seq, rev(women_seq)),
        c(pred_women_line[, "lwr"], rev(pred_women_line[, "upr"])),
        col = adjustcolor("darkgreen", alpha = 0.3), border = NA)

points(women12$m_woman2, women12$mil_exp_pergdp2, pch = 16,
       col = adjustcolor("black", alpha = 0.3), cex = 0.8)

grid(col = "lightgray", lty = 2)


# ggplot version
women_seq <- seq(min(women12$m_woman2, na.rm = TRUE),
                 max(women12$m_woman2, na.rm = TRUE),
                 length.out = 100)

pred_data <- data.frame(
  m_woman2 = women_seq,
  m_femlead = round(mean(women12$m_femlead, na.rm = TRUE)),
  leftright = round(mean(women12$leftright, na.rm = TRUE)),
  party_control_cat = factor(rep(modal_party, length(women_seq)),
                             levels = levels(women12$party_control_cat))
)

preds <- predict(full_model, newdata = pred_data, interval = "confidence")
pred_df <- data.frame(
  women_pct = women_seq,
  fit = preds[, "fit"],
  lwr = preds[, "lwr"],
  upr = preds[, "upr"]
)

ggplot(pred_df, aes(x = women_pct, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3, fill = "blue") +
  geom_line(color = "darkblue", size = 1.2) +
  geom_point(data = women12, aes(x = m_woman2, y = mil_exp_pergdp2),
             alpha = 0.4, color = "gray50") +
  labs(title = "Women's Parliamentary Representation and Military Spending",
       subtitle = paste("Other variables held constant: Female leader =",
                        round(mean(women12$m_femlead, na.rm = TRUE)),
                        ", Left govt =", round(mean(women12$leftright, na.rm = TRUE)),
                        ", Party control =", modal_party),
       x = "Women in Parliament (%)",
       y = "Predicted Military Expenditure (% GDP)") +
  theme_minimal() +
  theme(plot.subtitle = element_text(size = 10, color = "gray60"))
