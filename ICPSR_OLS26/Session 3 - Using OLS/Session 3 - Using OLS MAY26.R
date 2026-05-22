## Set your working directory to the ICPSR_regression folder
## Change the path below to match your computer
# setwd("~/ICPSR_regression")
setwd("C:/Users/Patrick Shea/Dropbox/My Projects/Spring 2026/ICPSR/ICPSR_OLS26")

# =============================================================================
# Session 3: Using OLS - Interpretation, Hypothesis Testing & Confidence Intervals
#
# NOTE: Line ~370 (Part 6 residuals plot) references `fitted_vals` and `residuals`
# which are defined in Part 4 of the model summary but get cleared by rm(list=ls())
# on line 96. This will cause an error. To fix, recompute them from m1 before Part 6.
# =============================================================================

rm(list=ls()) # Clear workspace



# =============================================================================
# PART 1: WORKING WITH REAL DATA - THE SWISS DATASET
# =============================================================================

rm(list=ls())

require(datasets)
data(swiss)

# === Activity 2a: Explore the data ===
head(swiss)
dim(swiss)
names(swiss)
?swiss

summary(swiss[, c("Fertility", "Agriculture")])

# === Activity 2b: Visualize the relationship ===
plot(swiss$Agriculture, swiss$Fertility,
     xlab="Agriculture (%)",
     ylab="Fertility Rate",
     main="Fertility vs Agriculture in Swiss Provinces",
     pch=19, col="darkblue", cex=1.2)

abline(lm(Fertility ~ Agriculture, data=swiss), col="red", lwd=2)

# =============================================================================
# PART 2: REGRESSION ANALYSIS AND INTERPRETATION
# =============================================================================

# === Activity 3a: Fit a simple regression ===
m1 <- lm(Fertility ~ Agriculture, data=swiss)
summary(m1)

# === Activity 3b: Interpret the coefficients ===
coefs <- coef(m1)
intercept <- coefs[1]
slope <- coefs[2]

# The intercept is the predicted fertility when Agriculture = 0%
# But Agriculture ranges from the minimum to the maximum in the data,
# so Agriculture = 0 is outside our data range — interpret with caution
min(swiss$Agriculture)
max(swiss$Agriculture)

# The slope tells us: for each 1% increase in Agriculture,
# Fertility changes by this amount on average

# === Activity 3c: Make predictions ===

# Prediction at the mean of Agriculture
mean_ag <- mean(swiss$Agriculture)
pred_at_mean <- intercept + slope * mean_ag

# OLS property: the regression line passes through the point (mean X, mean Y)
mean(swiss$Fertility)

# Predictions for specific values
ag_values <- c(20, 40, 60, 80)
for(ag in ag_values) {
  pred <- intercept + slope * ag
  print(paste("Agriculture =", ag, "% -> Predicted Fertility =", round(pred, 2)))
}

# Alternative: using predict()
new_data <- data.frame(Agriculture = c(20, 40, 60, 80))
predictions <- predict(m1, newdata = new_data)
data.frame(Agriculture = new_data$Agriculture, Predicted_Fertility = round(predictions, 2))


# =============================================================================
# PART 3: REVIEW - Simulating OLS to Build Intuition
# =============================================================================

# Simulate data from a known DGP to see how well OLS recovers the true parameters

set.seed(123456)
reps <- 500
par.est <- matrix(NA, nrow = reps, ncol = 2)
b0 <- 0.2  # True intercept
b1 <- 0.5  # True slope
n <- 1000
X <- runif(n, -1, 1)

# === Activity 1a: Single replication — understanding the DGP ===
Y <- b0 + b1*X + rnorm(n, 0, 1)

# Run regression on simulated data
model1 <- lm(Y ~ X)
summary(model1)

# === Activity 1b: Compare estimates to the true values ===
coefs <- coef(model1)
coefs[1] - b0  # Intercept difference from truth
coefs[2] - b1  # Slope difference from truth

# === Activity 1c: Repeat 500 times to see sampling variability ===
for(i in 1:reps){
  Y <- b0 + b1*X + rnorm(n, 0, 1)
  model <- lm(Y ~ X)
  par.est[i, 1] <- model$coef[1]
  par.est[i, 2] <- model$coef[2]
}

# Summary of simulation results
mean(par.est[, 1])  # Mean of intercept estimates
mean(par.est[, 2])  # Mean of slope estimates
sd(par.est[, 1])    # SD of intercept estimates
sd(par.est[, 2])    # SD of slope estimates

# Visualize the sampling distributions
par(mfrow=c(1,2), mar=c(4,4,3,2))

# Intercept distribution
hist(par.est[, 1], breaks=25, col="lightblue",
     main="Distribution of Intercept Estimates",
     xlab=expression(hat(beta)[0]),
     ylab="Frequency")
abline(v=b0, col="red", lwd=3)
abline(v=mean(par.est[, 1]), col="blue", lwd=2, lty=2)
legend("topright", c("True Value", "Sample Mean"),
       col=c("red", "blue"), lwd=c(3,2), lty=c(1,2))

# Slope distribution
hist(par.est[, 2], breaks=25, col="lightgreen",
     main="Distribution of Slope Estimates",
     xlab=expression(hat(beta)[1]),
     ylab="Frequency")
abline(v=b1, col="red", lwd=3)
abline(v=mean(par.est[, 2]), col="blue", lwd=2, lty=2)
legend("topright", c("True Value", "Sample Mean"),
       col=c("red", "blue"), lwd=c(3,2), lty=c(1,2))

par(mfrow=c(1,1))



# =============================================================================
# PART 4: HYPOTHESIS TESTING DEEP DIVE
# =============================================================================

# === Activity 4a: Test whether Agriculture affects Fertility ===
# H0: beta_1 = 0 (no effect)
# Ha: beta_1 != 0 (there is an effect)

summary_m1 <- summary(m1)

slope_est <- coefs[2]
slope_se <- summary_m1$coefficients[2, "Std. Error"]
t_stat <- summary_m1$coefficients[2, "t value"]
p_value <- summary_m1$coefficients[2, "Pr(>|t|)"]
df <- summary_m1$df[2]

# === Activity 4b: Verify the t-statistic manually ===
# t = estimate / standard error
t_manual <- slope_est / slope_se
t_manual
t_stat

# Calculate p-value manually (two-sided test)
p_manual <- 2 * pt(-abs(t_stat), df=df)
p_manual

# === Activity 4c: Compare to critical value and make decision ===
alpha <- 0.05
critical_val <- qt(1 - alpha/2, df=df)

# If |t| > critical value, reject H0
abs(t_stat) > critical_val

# =============================================================================
# PART 5: CONFIDENCE INTERVALS
# =============================================================================

# === Activity 5a: 95% CI for the slope ===

# CI = estimate +/- (critical value * standard error)
margin_error <- critical_val * slope_se
ci_lower <- slope_est - margin_error
ci_upper <- slope_est + margin_error
c(ci_lower, ci_upper)

# Verify with built-in function
confint(m1, level=0.95)

# === Activity 5b: Understanding CI interpretation ===
# CORRECT: If we repeated sampling many times, ~95% of CIs would contain the true value
# INCORRECT: "95% probability the true value is in this specific interval"
# The true parameter is fixed — our interval either contains it or doesn't

# === Simulation to demonstrate what 95% coverage means ===
set.seed(123)
n_sims <- 100
sample_size <- 30
true_slope <- 0.194  # Use our estimated slope as "truth"

ci_results <- matrix(NA, nrow = n_sims, ncol = 3)
colnames(ci_results) <- c("lower", "upper", "contains_true")

for(i in 1:n_sims) {
  sample_indices <- sample(nrow(swiss), sample_size, replace = TRUE)
  sample_data <- swiss[sample_indices, ]
  sample_model <- lm(Fertility ~ Agriculture, data = sample_data)
  sample_ci <- confint(sample_model, "Agriculture", level = 0.95)
  ci_results[i, "lower"] <- sample_ci[1]
  ci_results[i, "upper"] <- sample_ci[2]
  ci_results[i, "contains_true"] <- (sample_ci[1] <= true_slope) & (true_slope <= sample_ci[2])
}

# How many CIs contain the true value?
coverage_rate <- mean(ci_results[, "contains_true"])
sum(ci_results[, "contains_true"])
coverage_rate

# Visualize the CIs (blue = contains true value, red = misses)
plot(1:n_sims, ci_results[, "lower"], type = "n",
     ylim = range(ci_results[, c("lower", "upper")]),
     xlab = "Sample Number", ylab = "Slope Estimate",
     main = paste("95% Confidence Intervals from", n_sims, "Samples"))

for(i in 1:n_sims) {
  color <- ifelse(ci_results[i, "contains_true"], "blue", "red")
  lines(c(i, i), c(ci_results[i, "lower"], ci_results[i, "upper"]),
        col = color, lwd = 1)
}

abline(h = true_slope, col = "black", lwd = 3, lty = 1)

legend("topright",
       c("True Slope", "CI Contains True Value", "CI Misses True Value"),
       col = c("black", "blue", "red"),
       lwd = c(3, 1, 1),
       lty = c(1, 1, 1))

# === Activity 5c: Compare CIs at different confidence levels ===
# Higher confidence = wider interval
confidence_levels <- c(0.90, 0.95, 0.99)

for(conf_level in confidence_levels) {
  alpha_level <- 1 - conf_level
  crit_val <- qt(1 - alpha_level/2, df=df)
  margin <- crit_val * slope_se
  lower <- slope_est - margin
  upper <- slope_est + margin
  width <- upper - lower

  print(sprintf("%g%% CI: [%.4f, %.4f], Width: %.4f",
              conf_level*100, lower, upper, width))
}

# =============================================================================
# PART 6: ENHANCED VISUALIZATION
# =============================================================================

# Recompute residuals and fitted values from m1 (needed after rm(list=ls()) earlier)
fitted_vals <- fitted(m1)
residuals <- residuals(m1)

par(mfrow=c(2,2), mar=c(4,4,3,2))

# Scatter plot with regression line and prediction intervals
plot(swiss$Agriculture, swiss$Fertility,
     xlab="Agriculture (%)", ylab="Fertility Rate",
     main="Regression Line with Data",
     pch=19, col="darkblue", cex=1.2)
abline(m1, col="red", lwd=2)

ag_seq <- seq(min(swiss$Agriculture), max(swiss$Agriculture), length.out=100)
pred_data <- data.frame(Agriculture = ag_seq)
pred_intervals <- predict(m1, pred_data, interval="prediction")
lines(ag_seq, pred_intervals[,"lwr"], col="red", lty=2)
lines(ag_seq, pred_intervals[,"upr"], col="red", lty=2)
legend("topright", c("Regression Line", "95% Prediction Interval"),
       col=c("red", "red"), lty=c(1,2), lwd=c(2,1))

# Residuals vs Fitted
plot(fitted_vals, residuals,
     xlab="Fitted Values", ylab="Residuals",
     main="Residuals vs Fitted",
     pch=19, col="darkgreen")
abline(h=0, col="red", lty=2)

# Q-Q plot for normality
qqnorm(residuals, main="Q-Q Plot of Residuals", pch=19, col="purple")
qqline(residuals, col="red", lwd=2)

# Histogram of residuals
hist(residuals, breaks=10, col="lightcoral",
     main="Distribution of Residuals",
     xlab="Residuals", ylab="Frequency")
abline(v=0, col="red", lwd=2, lty=2)

par(mfrow=c(1,1))

# =============================================================================
# PART 7: EXERCISES FOR STUDENTS
# =============================================================================

# Exercise 1: Test H0: beta_1 = 0.1 instead of beta_1 = 0
# Hint: t = (estimate - 0.1) / SE
h0_value <- 0.1
t_stat_new <- (slope_est - h0_value) / slope_se
p_value_new <- 2 * pt(-abs(t_stat_new), df=df)
t_stat_new
p_value_new

# Exercise 2: What is the 95% CI for the intercept? Is it significantly different from 0?

# Exercise 3: What fertility rate would you predict for 30% agriculture? 70%?

# Exercise 4: What's the predicted difference between 20% and 80% agriculture?

# === Session 3 Complete ===
