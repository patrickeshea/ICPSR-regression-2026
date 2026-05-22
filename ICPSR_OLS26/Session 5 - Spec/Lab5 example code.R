## Set your working directory to the ICPSR_regression folder
## Change the path below to match your computer
# setwd("~/ICPSR_regression")
setwd("C:/Users/Patrick Shea/Dropbox/My Projects/Spring 2026/ICPSR/ICPSR_OLS26")

# =============================================================================
# Lab Session 5 - Model Specification
# Women in Politics and Military Spending Analysis
#
# NOTE: Line ~200 in the original code references `women12_clean` which is
# never defined. Should be `women12` with complete.cases applied first.
# FIX: Added library(interflex) — was missing, causing interflex() calls to fail.
# =============================================================================

# Load necessary libraries
library(haven)      # For reading Stata files
library(ggplot2)    # For plotting
library(dplyr)      # For data manipulation
library(psych)      # For descriptive statistics
library(interflex)  # For interaction diagnostics

# Load the dataset
women12 <- read_dta("data/women12.dta")

# =============================================================================
# TASK 1: EXPLORE THE DATASET
# =============================================================================

# Key variables:
# - mil_exp_pergdp2: Military spending per GDP (outcome)
# - m_woman2: Percentage of women in parliament
# - m_femlead: Binary indicator for female leader (0/1)
# - cap: State capacity measure
# - mad_gdppc: GDP per capita
# - postcold2: Post-Cold War period indicator

dim(women12)
summary(women12[c("mil_exp_pergdp2", "m_woman2", "m_femlead", "cap", "mad_gdppc", "postcold2")])

# Detailed exploration of key variables

# Military expenditure (outcome variable)
describe(women12$mil_exp_pergdp2)

# Check for data quality issues
sum(is.na(women12$mil_exp_pergdp2))
sum(women12$mil_exp_pergdp2 == 0, na.rm = TRUE)
sum(women12$mil_exp_pergdp2 < 0, na.rm = TRUE)

# Visualize the distribution of military spending
hist(women12$mil_exp_pergdp2,
     main = "Distribution of Military Expenditure per GDP",
     xlab = "Military Expenditure per GDP",
     breaks = 30, col = "lightblue")

ggplot(women12, aes(x = mil_exp_pergdp2)) +
  geom_density(fill = "blue", alpha = 0.5) +
  geom_rug() +
  labs(title = "Distribution of Military Expenditure per GDP",
       subtitle = "Note the right-skewed distribution",
       x = "Military Expenditure per GDP",
       y = "Density") +
  theme_minimal()

# Explore key predictors
describe(women12$m_woman2)

table(women12$m_femlead, useNA = "ifany")
round(mean(women12$m_femlead, na.rm = TRUE), 3)

# =============================================================================
# TASK 2: FIT A BASIC LINEAR REGRESSION MODEL
# =============================================================================

# Military Spending = Women in Parliament + Female Leader + Controls
model1 <- lm(mil_exp_pergdp2 ~ m_woman2 + m_femlead + cap + mad_gdppc + postcold2,
             data = women12)
summary(model1)

# m_woman2: each 1% increase in women in parliament changes mil spending by coef[2]
coef(model1)[2]
# m_femlead: having a female leader changes mil spending by coef[3] vs male leaders
coef(model1)[3]

# =============================================================================
# TASK 3: FIT MODEL WITH INTERACTION TERM
# =============================================================================

# Does the effect of women in parliament depend on having a female leader?
women12$interaction <- women12$m_woman2 * women12$m_femlead

model2 <- lm(mil_exp_pergdp2 ~ m_woman2*m_femlead + cap + mad_gdppc + postcold2,
             data = women12)
summary(model2)

# =============================================================================
# TASK 4: INTERPRET THE INTERACTION MODEL COEFFICIENTS
# =============================================================================

b0 <- coef(model2)[1]  # Intercept
b1 <- coef(model2)[2]  # m_woman2 (main effect)
b2 <- coef(model2)[3]  # m_femlead (main effect)
b3 <- coef(model2)[7]  # interaction term (m_woman2:m_femlead)

# b1: Effect of women in parliament when there is NO female leader (m_femlead = 0)
b1
# b2: Effect of female leader when women in parliament = 0%
b2
# b3: How much the effect of women in parliament CHANGES when there IS a female leader
b3

# Conditional effects:
# When NO female leader (m_femlead = 0): effect = b1
b1
# When female leader present (m_femlead = 1): effect = b1 + b3
b1 + b3
# Difference between the two contexts = b3
b3

# =============================================================================
# TASK 5: PLOT THE INTERACTION EFFECT
# =============================================================================

min_women <- min(women12$m_woman2, na.rm = TRUE)
max_women <- max(women12$m_woman2, na.rm = TRUE)

# Generate predictions for both female leader conditions
new_data <- expand.grid(
  m_woman2 = seq(min_women, max_women, length.out = 100),
  m_femlead = c(0, 1),
  cap = mean(women12$cap, na.rm = TRUE),
  mad_gdppc = mean(women12$mad_gdppc, na.rm = TRUE),
  postcold2 = mean(women12$postcold2, na.rm = TRUE)
)

predictions <- predict(model2, newdata = new_data, interval = "confidence")

new_data$fit <- predictions[, "fit"]
new_data$lwr <- predictions[, "lwr"]
new_data$upr <- predictions[, "upr"]

new_data$leader_label <- factor(new_data$m_femlead,
                                labels = c("Male Leader", "Female Leader"))

interaction_plot <- ggplot(new_data, aes(x = m_woman2, y = fit, color = leader_label)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = leader_label),
              alpha = 0.2, color = NA) +
  labs(
    title = "Interaction Effect: Women in Parliament x Female Leadership",
    subtitle = "How the effect of women in parliament depends on executive leadership",
    x = "Percentage of Women in Parliament",
    y = "Predicted Military Expenditure per GDP",
    color = "Executive Leadership",
    fill = "Executive Leadership"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(interaction_plot)

# =============================================================================
# TASK 5B: MANUAL BINNING ANALYSIS (ALTERNATIVE TO INTERFLEX)
# =============================================================================

# BUG: women12_clean is never defined — should use women12 with complete.cases
# Creating women12_clean to fix this
interflex_vars <- c("mil_exp_pergdp2", "m_woman2", "m_femlead", "cap", "mad_gdppc", "postcold2")
women12_clean <- women12[complete.cases(women12[interflex_vars]), ]

# Create 3 bins based on quantiles (equal-sized groups)
women_quantiles <- quantile(women12_clean$m_woman2, probs = c(0, 0.33, 0.67, 1))
women_quantiles

women12_clean$women_bins <- cut(women12_clean$m_woman2,
                                breaks = women_quantiles,
                                labels = c("Low", "Medium", "High"),
                                include.lowest = TRUE)

table(women12_clean$women_bins)

# Explore each bin
for(bin_name in c("Low", "Medium", "High")) {
  bin_data <- women12_clean[women12_clean$women_bins == bin_name, ]

  print(sprintf("%s bin: n=%d, women range=%.1f-%.1f%%, avg mil spending=%.2f, female leaders=%d",
                bin_name, nrow(bin_data),
                min(bin_data$m_woman2), max(bin_data$m_woman2),
                mean(bin_data$mil_exp_pergdp2), sum(bin_data$m_femlead)))
}


# Effect of female leaders in each bin
effect_results <- data.frame(
  Bin = c("Low", "Medium", "High"),
  Male_Leader_Avg = NA,
  Female_Leader_Avg = NA,
  Difference = NA,
  N_Male = NA,
  N_Female = NA
)

for(i in 1:3) {
  bin_name <- c("Low", "Medium", "High")[i]
  bin_data <- women12_clean[women12_clean$women_bins == bin_name, ]

  male_avg <- mean(bin_data$mil_exp_pergdp2[bin_data$m_femlead == 0])
  female_avg <- mean(bin_data$mil_exp_pergdp2[bin_data$m_femlead == 1])

  effect_results$Male_Leader_Avg[i] <- male_avg
  effect_results$Female_Leader_Avg[i] <- female_avg
  effect_results$Difference[i] <- female_avg - male_avg
  effect_results$N_Male[i] <- sum(bin_data$m_femlead == 0)
  effect_results$N_Female[i] <- sum(bin_data$m_femlead == 1)
}

print(effect_results)

# =============================================================================
# VISUALIZATION
# =============================================================================

par(mfrow = c(2, 2))

# Plot 1: Boxplot of military spending by women in parliament bins
boxplot(mil_exp_pergdp2 ~ women_bins, data = women12_clean,
        main = "Military Spending by Women in Parliament",
        xlab = "Women in Parliament Level",
        ylab = "Military Spending per GDP",
        col = c("lightblue", "lightgreen", "lightcoral"))

# Plot 2: Effect of female leaders across bins
bins_with_data <- which(!is.na(effect_results$Difference))
if(length(bins_with_data) > 0) {
  plot(bins_with_data, effect_results$Difference[bins_with_data],
       main = "Effect of Female Leaders Across Bins",
       xlab = "Women in Parliament Level",
       ylab = "Difference in Military Spending\n(Female - Male Leaders)",
       pch = 16, cex = 2, col = "darkred",
       xlim = c(0.5, 3.5), xaxt = "n")

  axis(1, at = 1:3, labels = c("Low", "Medium", "High"))
  abline(h = 0, lty = 2, col = "gray")

  for(j in bins_with_data) {
    text(j, effect_results$Difference[j] + 0.2,
         paste("n =", effect_results$N_Female[j]), cex = 0.8)
  }
}

# Plot 3: Bar chart comparing male vs female leaders by bin
bins_to_plot <- which(!is.na(effect_results$Female_Leader_Avg))
if(length(bins_to_plot) > 0) {
  barplot(rbind(effect_results$Male_Leader_Avg[bins_to_plot],
                effect_results$Female_Leader_Avg[bins_to_plot]),
          main = "Military Spending: Male vs Female Leaders",
          xlab = "Women in Parliament Level",
          ylab = "Average Military Spending per GDP",
          col = c("lightblue", "pink"),
          legend = c("Male Leaders", "Female Leaders"),
          names.arg = effect_results$Bin[bins_to_plot],
          beside = TRUE)
}

# =============================================================================
# TASK 6: ASSESS ASSUMPTIONS OF THE INTERACTION MODEL
# =============================================================================

# Diagnostic plots
par(mfrow = c(2, 2))
plot(model2, main = "Diagnostic Plots for Interaction Model")
par(mfrow = c(1, 1))

# What to look for:
# 1. Residuals vs Fitted: no clear pattern (linearity)
# 2. Q-Q Plot: points follow diagonal (normality)
# 3. Scale-Location: roughly horizontal line (homoscedasticity)
# 4. Residuals vs Leverage: influential outliers

# Prepare clean data for interflex
interflex_data <- women12[complete.cases(women12[interflex_vars]), interflex_vars]

# Convert to numeric
interflex_data$mil_exp_pergdp2 <- as.numeric(interflex_data$mil_exp_pergdp2)
interflex_data$m_woman2 <- as.numeric(interflex_data$m_woman2)
interflex_data$m_femlead <- as.numeric(interflex_data$m_femlead)
interflex_data$cap <- as.numeric(interflex_data$cap)
interflex_data$mad_gdppc <- as.numeric(interflex_data$mad_gdppc)
interflex_data$postcold2 <- as.numeric(interflex_data$postcold2)

dim(interflex_data)
sapply(interflex_data, function(x) sum(is.na(x)))
table(interflex_data$m_femlead)

interflex_data_df <- as.data.frame(interflex_data)

# interflex binning (3 bins)
interflex(
  estimator = "binning",
  Y = "mil_exp_pergdp2",
  D = "m_femlead",
  X = "m_woman2",
  Z = c("cap", "mad_gdppc", "postcold2"),
  data = interflex_data_df,
  nbins = 3,
  main = "Effect of Female Leadership (Binning)",
  Ylabel = "Military Spending per GDP",
  Xlabel = "Women in Parliament (%)",
  CI = TRUE,
  na.rm = TRUE
)

# interflex binning (5 bins)
interflex(
  estimator = "binning",
  Y = "mil_exp_pergdp2",
  D = "m_femlead",
  X = "m_woman2",
  Z = c("cap", "mad_gdppc", "postcold2"),
  data = interflex_data_df,
  nbins = 5,
  main = "Effect of Female Leadership (Binning)",
  Ylabel = "Military Spending per GDP",
  Xlabel = "Women in Parliament (%)",
  CI = TRUE,
  na.rm = TRUE
)


# =============================================================================
# TASK 7: FIT A LOG-LINEAR MODEL
# =============================================================================

# ln(Military Spending) = Women in Parliament + Controls

# Check for values that would cause problems with log transformation
sum(women12$mil_exp_pergdp2 <= 0, na.rm = TRUE)

model4 <- lm(log(mil_exp_pergdp2) ~ m_woman2 + cap + mad_gdppc + postcold2,
             data = women12,
             subset = mil_exp_pergdp2 > 0)

summary(model4)

# In log-linear models, coefficients represent percentage changes
# Formula: (exp(coefficient) - 1) x 100 = percentage change

log_coefs <- summary(model4)$coefficients[, 1]
percentage_effects <- (exp(log_coefs) - 1) * 100

for(i in 2:length(log_coefs)) {
  var_name <- names(log_coefs)[i]
  print(sprintf("%s: %.2f%% change in military spending", var_name, percentage_effects[i]))
}

# Compare fit of linear vs log-linear models
summary(model1)$r.squared
summary(model4)$r.squared

# Quick diagnostic comparison
par(mfrow = c(1, 2))
plot(model1, which = 1, main = "Linear Model Residuals")
plot(model4, which = 1, main = "Log-Linear Model Residuals")
par(mfrow = c(1, 1))

# =============================================================================
# TASK 8: FIT A POLYNOMIAL MODEL (QUADRATIC)
# =============================================================================

# Military Spending = Women + Women^2 + Controls
model5 <- lm(mil_exp_pergdp2 ~ m_woman2 + I(m_woman2^2) + cap + mad_gdppc + postcold2,
             data = women12)

summary(model5)

# In quadratic models, marginal effect = b1 + 2*b2*X
b1_poly <- coef(model5)[2]
b2_poly <- coef(model5)[3]

# Marginal effects at different levels
women_levels <- c(10, 20, 30, 40)
for(level in women_levels) {
  marginal_effect <- b1_poly + 2 * b2_poly * level
  print(sprintf("At %d%% women: marginal effect = %.4f per additional %%", level, marginal_effect))
}

# =============================================================================
# TASK 9: PLOT THE POLYNOMIAL MODEL
# =============================================================================

new_data_poly <- data.frame(
  m_woman2 = seq(min_women, max_women, length.out = 100),
  cap = mean(women12$cap, na.rm = TRUE),
  mad_gdppc = mean(women12$mad_gdppc, na.rm = TRUE),
  postcold2 = mean(women12$postcold2, na.rm = TRUE)
)

predictions_poly <- predict(model5, newdata = new_data_poly, interval = "confidence")

new_data_poly$fit <- predictions_poly[, "fit"]
new_data_poly$lwr <- predictions_poly[, "lwr"]
new_data_poly$upr <- predictions_poly[, "upr"]

poly_plot <- ggplot(new_data_poly, aes(x = m_woman2, y = fit)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "darkred", alpha = 0.2) +
  labs(
    title = "Quadratic Relationship: Women in Parliament and Military Spending",
    subtitle = "Curved relationship allows for non-linear effects",
    x = "Percentage of Women in Parliament",
    y = "Predicted Military Expenditure per GDP"
  ) +
  theme_minimal()

print(poly_plot)

# =============================================================================
# SUMMARY AND COMPARISON
# =============================================================================

# Model comparison
summary(model1)$r.squared  # Basic Linear
summary(model2)$r.squared  # Interaction
summary(model4)$r.squared  # Log-linear
summary(model5)$r.squared  # Polynomial

# Key takeaways:
# 1. Interactions allow effects to vary by context
# 2. Log transformations handle skewed data and give percentage interpretations
# 3. Polynomial terms capture curved relationships
# 4. Manual binning provides intuitive way to understand interactions
# 5. Always check model assumptions with diagnostic plots
# 6. Theory should guide specification choices, not just R-squared
