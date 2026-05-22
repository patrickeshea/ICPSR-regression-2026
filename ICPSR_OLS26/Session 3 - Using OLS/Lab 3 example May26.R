## Set your working directory to the ICPSR_regression folder
## Change the path below to match your computer
# setwd("~/ICPSR_regression")
setwd("C:/Users/Patrick Shea/Dropbox/My Projects/Spring 2026/ICPSR/ICPSR_OLS26")

# =============================================================================
# Session 3 Lab - Example Solutions
# Using OLS: Interpretation, Hypothesis Testing & Confidence Intervals
# =============================================================================

# Load necessary libraries
library(ggplot2)

# =============================================================================
# SETUP AND PART 1: DATA EXPLORATION
# =============================================================================

# Load the US state data (built into R)
data(state)
states <- data.frame(state.x77)
head(states)

# 1.1 Explore the dataset
dim(states)
names(states)
summary(states[, c("Life.Exp", "Income")])

?state.x77

# 1.2 Scatter plot of income vs life expectancy
plot(states$Income, states$Life.Exp,
     xlab = "Income (dollars)",
     ylab = "Life Expectancy (years)",
     main = "Life Expectancy vs Income by State (1977)",
     pch = 19, col = "darkblue")

# ggplot2 alternative
ggplot(states, aes(x = Income, y = Life.Exp)) +
  geom_point(color = "darkblue", size = 2) +
  labs(x = "Income (dollars)",
       y = "Life Expectancy (years)",
       title = "Life Expectancy vs Income by State (1977)")

# =============================================================================
# PART 2: SIMPLE LINEAR REGRESSION
# =============================================================================

# 2.1 Fit the model
model1 <- lm(Life.Exp ~ Income, data = states)

# 2.2 & 2.3 Examine and interpret
summary(model1)

coefs <- coef(model1)

# Intercept: predicted life expectancy when Income = $0 (outside data range)
# Slope: change in life expectancy per $1 increase in income
# More useful: per $1000 increase
coefs[2] * 1000

# R-squared: proportion of variance in life expectancy explained by income
r_squared <- summary(model1)$r.squared
r_squared

# =============================================================================
# PART 3: MANUAL HYPOTHESIS TESTING
# =============================================================================

# H0: beta_1 = 0 (Income has no effect on Life Expectancy)
# Ha: beta_1 != 0

slope_est <- coefs[2]
slope_se <- summary(model1)$coefficients[2, "Std. Error"]
df <- summary(model1)$df[2]

# Manual t-statistic: estimate / standard error
t_manual <- slope_est / slope_se
t_manual

# Critical value at alpha = 0.05
alpha <- 0.05
critical_val <- qt(1 - alpha/2, df = df)
critical_val

# Manual p-value (two-sided)
p_manual <- 2 * pt(-abs(t_manual), df = df)
p_manual

# Decision: reject H0 if |t| > critical value
abs(t_manual) > critical_val

# Verify against model output
summary(model1)$coefficients[2, ]

# =============================================================================
# PART 4: CONFIDENCE INTERVALS
# =============================================================================

# 4.1 Manual 95% CI: estimate +/- (critical value * SE)
margin_error <- critical_val * slope_se
ci_lower <- slope_est - margin_error
ci_upper <- slope_est + margin_error
c(ci_lower, ci_upper)

# Verify with confint()
confint(model1, "Income", level = 0.95)

# 4.2 Compare CIs at different confidence levels (higher confidence = wider)
conf_levels <- c(0.90, 0.95, 0.99)
for(conf in conf_levels) {
  alpha_level <- 1 - conf
  crit <- qt(1 - alpha_level/2, df = df)
  margin <- crit * slope_se
  lower <- slope_est - margin
  upper <- slope_est + margin
  width <- upper - lower
  print(sprintf("%g%% CI: [%.6f, %.6f], Width: %.6f", conf*100, lower, upper, width))
}

# 4.3 Does the CI include 0? If not, consistent with rejecting H0
ci_lower <= 0 && ci_upper >= 0

# =============================================================================
# PART 5: PREDICTIONS AND RESIDUALS
# =============================================================================

# 5.1 Predictions at specific income levels
pred_4000 <- predict(model1, newdata = data.frame(Income = 4000))
pred_6000 <- predict(model1, newdata = data.frame(Income = 6000))
pred_6000 - pred_4000  # Should equal 2000 * slope

# 5.2 Examine extreme cases
max_income_idx <- which.max(states$Income)
min_income_idx <- which.min(states$Income)

# Highest income state
rownames(states)[max_income_idx]
states$Income[max_income_idx]
states$Life.Exp[max_income_idx]
pred_max <- predict(model1, newdata = data.frame(Income = states$Income[max_income_idx]))
states$Life.Exp[max_income_idx] - pred_max  # Residual

# Lowest income state
rownames(states)[min_income_idx]
states$Income[min_income_idx]
states$Life.Exp[min_income_idx]
pred_min <- predict(model1, newdata = data.frame(Income = states$Income[min_income_idx]))
states$Life.Exp[min_income_idx] - pred_min  # Residual

# 5.3 All residuals
residuals_all <- residuals(model1)
fitted_all <- fitted(model1)

# OLS property: residuals sum to zero
mean(residuals_all)

# Find states with largest residuals
max_resid_idx <- which.max(residuals_all)
min_resid_idx <- which.min(residuals_all)
rownames(states)[max_resid_idx]
residuals_all[max_resid_idx]
rownames(states)[min_resid_idx]
residuals_all[min_resid_idx]

# =============================================================================
# PART 6: UNDERSTANDING SAMPLING VARIABILITY (BOOTSTRAP)
# =============================================================================
# Bootstrapping is a resampling technique for estimating the variability of a 
# statistic without relying on analytical assumptions like homoskedasticity or 
# normality. The idea is to draw many random samples (with replacement) 
# from your original data, re-estimate the model each time, and see how much the 
# estimates vary. The standard deviation of those resampled slope estimates is the
# bootstrap standard error. Comparing it to the model-based SE serves as a useful 
# diagnostic — if they diverge, it may indicate that the classical assumptions 
# underlying the analytical SE are not well-suited to your data.


# 6.1 Bootstrap: resample with replacement to estimate the SE of the slope
set.seed(123)
n_bootstrap <- 100
bootstrap_slopes <- numeric(n_bootstrap)

for(i in 1:n_bootstrap) {
  boot_indices <- sample(nrow(states), replace = TRUE)
  boot_data <- states[boot_indices, ]
  boot_model <- lm(Life.Exp ~ Income, data = boot_data)
  bootstrap_slopes[i] <- coef(boot_model)[2]
}

# Compare bootstrap SE to model SE
bootstrap_se <- sd(bootstrap_slopes)
bootstrap_se
slope_se

# Plot bootstrap distribution
hist(bootstrap_slopes, breaks = 15, col = "lightblue",
     main = "Bootstrap Distribution of Slope Estimates",
     xlab = "Slope Estimate",
     ylab = "Frequency")
abline(v = slope_est, col = "red", lwd = 2)
abline(v = mean(bootstrap_slopes), col = "blue", lwd = 2, lty = 2)
legend("topright", c("Original Estimate", "Bootstrap Mean"),
       col = c("red", "blue"), lwd = 2, lty = c(1, 2))

# =============================================================================
# PART 7: EFFECT SIZES AND PRACTICAL SIGNIFICANCE
# =============================================================================

# Effect of moving from 25th to 75th percentile of income
q25_income <- quantile(states$Income, 0.25)
q75_income <- quantile(states$Income, 0.75)
iqr_effect <- (q75_income - q25_income) * slope_est
iqr_effect

# Compare to overall variation in life expectancy
life_exp_sd <- sd(states$Life.Exp)
iqr_effect / life_exp_sd  # Effect as fraction of 1 SD

# =============================================================================
# FINAL VISUALIZATION
# =============================================================================

# Scatter plot with regression line and confidence bands
plot(states$Income, states$Life.Exp,
     xlab = "Income (dollars)",
     ylab = "Life Expectancy (years)",
     main = "Life Expectancy vs Income: Regression Analysis",
     pch = 19, col = "darkblue", cex = 1.2)

abline(model1, col = "red", lwd = 2)

# Add confidence bands
income_seq <- seq(min(states$Income), max(states$Income), length.out = 100)
pred_ci <- predict(model1,
                   newdata = data.frame(Income = income_seq),
                   interval = "confidence")

lines(income_seq, pred_ci[, "lwr"], col = "red", lty = 2)
lines(income_seq, pred_ci[, "upr"], col = "red", lty = 2)

# Label the states with largest residuals
text(states$Income[max_resid_idx], states$Life.Exp[max_resid_idx],
     rownames(states)[max_resid_idx], pos = 3, cex = 0.8)
text(states$Income[min_resid_idx], states$Life.Exp[min_resid_idx],
     rownames(states)[min_resid_idx], pos = 1, cex = 0.8)

legend("bottomright",
       c("Data Points", "Regression Line", "95% Confidence Band"),
       col = c("darkblue", "red", "red"),
       pch = c(19, NA, NA),
       lty = c(NA, 1, 2),
       pt.cex = c(1.2, NA, NA))
