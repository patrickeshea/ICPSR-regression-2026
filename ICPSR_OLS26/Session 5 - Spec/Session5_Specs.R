## Set your working directory to the ICPSR_regression folder
## Change the path below to match your computer
# setwd("~/ICPSR_regression")
setwd("C:/Users/Patrick Shea/Dropbox/My Projects/Spring 2026/ICPSR/ICPSR_OLS26")

# =============================================================================
# Session 5: OLS Specifications and Interactions - Interactive R Demo
# ICPSR Summer Program in Quantitative Methods
# Instructor: Patrick Shea
# =============================================================================

# Learning Objectives:
# 1. Understand different functional forms in regression
# 2. Learn to interpret log transformations
# 3. Master interaction effects and their interpretation
# 4. Practice diagnostic thinking about model specification

# Clear workspace and load libraries
rm(list = ls())

# Load required libraries
library(haven)      # For reading Stata files
library(ggplot2)    # For beautiful plots
library(dplyr)      # For data manipulation
library(car)        # For diagnostic tests

# =============================================================================
# PART 1: FUNCTIONAL FORMS AND LOG TRANSFORMATIONS
# =============================================================================

# Load the California school performance dataset
elemapi2 <- read_dta("data/elemapi2.dta")

# Key variables:
# - api00: School performance score (outcome)
# - enroll: Number of students enrolled
# - meals: Percent of students eligible for free/reduced lunch
# - mobility: Percent of students who changed schools

summary(elemapi2[c("api00", "enroll", "meals", "mobility")])

# =============================================================================
# Question 1: How does school size (enrollment) affect performance?
# =============================================================================

# Simple linear model
model1 <- lm(api00 ~ enroll, data = elemapi2)
summary(model1)

# For each additional student, performance decreases by coef[2] points
round(coef(model1)[2], 3)

# Check the distribution of enrollment
hist(elemapi2$enroll, main = "Distribution of School Enrollment",
     xlab = "Number of Students", breaks = 20, col = "lightblue")

# Enrollment is right-skewed — suggests log transformation

# =============================================================================
# LOG-LINEAR MODEL: Taking log of the independent variable
# =============================================================================

# Y = a + b*ln(X) + error
# Interpretation: A 1% increase in X leads to b/100 units change in Y

elemapi2$ln_enroll <- log(elemapi2$enroll)

model2 <- lm(api00 ~ ln_enroll, data = elemapi2)
summary(model2)

# A 1% increase in enrollment changes test scores by:
percent_effect <- coef(model2)[2] / 100
percent_effect

# Concrete examples
enrollment_effect_20pct <- coef(model2)[2] * log(1.20)  # 20% increase
enrollment_effect_20pct

enrollment_effect_double <- coef(model2)[2] * log(2)  # Doubling enrollment
enrollment_effect_double

# =============================================================================
# LINEAR-LOG MODEL: Taking log of the dependent variable
# =============================================================================

# ln(Y) = a + b*X + error
# Interpretation: One unit increase in X leads to 100*[exp(b)-1]% change in Y

model3 <- lm(log(api00) ~ meals, data = elemapi2)
summary(model3)

# Percentage effect per 1-unit increase in meals
percent_change <- (exp(coef(model3)[2]) - 1) * 100
percent_change

# =============================================================================
# LOG-LOG MODEL: Both variables logged
# =============================================================================

# ln(Y) = a + b*ln(X) + error
# b is the elasticity: a 1% increase in X leads to b% change in Y

model4 <- lm(log(api00) ~ log(mobility + 1), data = elemapi2)  # Add 1 to avoid log(0)
summary(model4)

elasticity <- coef(model4)[2]
elasticity

# =============================================================================
# PART 2: POLYNOMIAL SPECIFICATIONS
# =============================================================================

# Y = a + b1*X + b2*X^2 + error
# The effect of enrollment depends on the current level
# At enrollment = X, the marginal effect is: b1 + 2*b2*X

model5 <- lm(api00 ~ enroll + I(enroll^2), data = elemapi2)
summary(model5)

b1 <- coef(model5)[2]
b2 <- coef(model5)[3]

# Marginal effects at different enrollment levels
enrollments <- c(200, 500, 800)
for(enroll_val in enrollments) {
  marginal_effect <- b1 + 2 * b2 * enroll_val
  print(sprintf("At %d students, marginal effect = %.4f", enroll_val, marginal_effect))
}

library(modelsummary)
linear_model <- lm(api00 ~ enroll, data = elemapi2)

modelsummary(list("Linear" = linear_model, "Polynomial" = model5))

library(marginaleffects)
slopes(model5, variables = "enroll", newdata = datagrid(enroll = c(200, 500, 800)))

plot(elemapi2$enroll, elemapi2$api00,
     main = "API Scores vs School Enrollment",
     xlab = "School Enrollment (Number of Students)",
     ylab = "API Test Score (2000)",
     pch = 16, col = "lightblue", cex = 0.8)

# Add linear fit line (dashed red)
linear_model <- lm(api00 ~ enroll, data = elemapi2)
abline(linear_model, col = "red", lwd = 2, lty = 2)

# Add polynomial fit line (solid blue)
enroll_seq <- seq(min(elemapi2$enroll), max(elemapi2$enroll), length.out = 100)
poly_pred <- predict(model5, newdata = data.frame(enroll = enroll_seq))
lines(enroll_seq, poly_pred, col = "blue", lwd = 2)

legend("topright",
       legend = c("Linear fit", "Polynomial fit"),
       col = c("red", "blue"),
       lty = c(2, 1),
       lwd = 2)


# =============================================================================
# PART 3: INTERACTION EFFECTS
# =============================================================================

# Does the effect of one variable depend on another variable?

data(swiss)
swiss <- swiss %>%
  mutate(CatholicBin = ifelse(Catholic > 50, 1, 0),
         CatholicBin_factor = factor(CatholicBin, labels = c("Not Majority Catholic", "Majority Catholic")))

# Does the relationship between agriculture and fertility
# depend on whether a province is majority Catholic?

# =============================================================================
# Step 1: No interaction (parallel lines)
# =============================================================================

# Fertility = b0 + b1*Agriculture + b2*Catholic + error
model_no_interaction <- lm(Fertility ~ Agriculture + CatholicBin, data = swiss)
summary(model_no_interaction)

b0 <- coef(model_no_interaction)[1]
b1 <- coef(model_no_interaction)[2]
b2 <- coef(model_no_interaction)[3]

# b0: Fertility when Agriculture=0 and Catholic=0
# b1: Effect of Agriculture (same for both groups)
# b2: Difference between Catholic and non-Catholic provinces

# Visualize parallel lines
p1 <- ggplot(swiss, aes(x = Agriculture, y = Fertility, color = CatholicBin_factor)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "No Interaction: Parallel Lines",
       subtitle = "Same slope for both groups",
       color = "Religion") +
  theme_minimal()
print(p1)

# =============================================================================
# Step 2: With interaction (different slopes)
# =============================================================================

# Fertility = b0 + b1*Agriculture + b2*Catholic + b3*Agriculture*Catholic + error
model_interaction <- lm(Fertility ~ Agriculture * CatholicBin, data = swiss)
summary(model_interaction)

b0 <- coef(model_interaction)[1]
b1 <- coef(model_interaction)[2]
b2 <- coef(model_interaction)[3]
b3 <- coef(model_interaction)[4]

# b0: Fertility when Agriculture=0 in non-Catholic provinces
# b1: Effect of Agriculture in NON-Catholic provinces
# b2: Difference in intercept for Catholic provinces
# b3: Difference in slope for Catholic provinces

# Conditional effects:
# In non-Catholic provinces: Effect of Agriculture = b1
b1
# In Catholic provinces: Effect of Agriculture = b1 + b3
b1 + b3

# Visualize interaction
p2 <- ggplot(swiss, aes(x = Agriculture, y = Fertility, color = CatholicBin_factor)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "With Interaction: Different Slopes",
       subtitle = "Agriculture effect depends on religious context",
       color = "Religion") +
  theme_minimal()
print(p2)

# =============================================================================
# CONTINUOUS INTERACTION EXAMPLE
# =============================================================================

# Does weight's effect on MPG depend on number of cylinders?
data(mtcars)

model_continuous <- lm(mpg ~ wt * cyl, data = mtcars)
summary(model_continuous)

b0 <- coef(model_continuous)[1]
b1 <- coef(model_continuous)[2]
b2 <- coef(model_continuous)[3]
b3 <- coef(model_continuous)[4]

# Effect of weight depends on cylinders:
b1 + b3*4   # When cylinders = 4
b1 + b3*6   # When cylinders = 6
b1 + b3*8   # When cylinders = 8

# =============================================================================
# INTERACTION VISUALIZATION AND INTERPRETATION TOOLS
# =============================================================================

model_interaction <- lm(Fertility ~ Agriculture + CatholicBin + Agriculture:CatholicBin, data = swiss)

b1 <- coef(model_interaction)[2]
b3 <- coef(model_interaction)[4]

# Scatter plot with two lines
plot(swiss$Agriculture, swiss$Fertility,
     col = ifelse(swiss$CatholicBin == 1, "red", "blue"),
     pch = 16,
     main = "Fertility vs Agriculture by Catholic Status",
     xlab = "Agriculture %",
     ylab = "Fertility Rate")

non_catholic <- swiss[swiss$CatholicBin == 0, ]
catholic <- swiss[swiss$CatholicBin == 1, ]

abline(lm(Fertility ~ Agriculture, data = non_catholic), col = "blue", lwd = 2)
abline(lm(Fertility ~ Agriculture, data = catholic), col = "red", lwd = 2)

legend("topright",
       legend = c("Non-Catholic", "Catholic"),
       col = c("blue", "red"),
       pch = 16)

# Bar chart of effects with confidence intervals
effect_non_catholic <- b1 + b3 * 0
effect_catholic <- b1 + b3 * 1

vcov_matrix <- vcov(model_interaction)
se_non_catholic <- sqrt(vcov_matrix[2,2])
se_catholic <- sqrt(vcov_matrix[2,2] + vcov_matrix[4,4] + 2*vcov_matrix[2,4])

ci_non_catholic <- c(effect_non_catholic - 1.96*se_non_catholic,
                     effect_non_catholic + 1.96*se_non_catholic)
ci_catholic <- c(effect_catholic - 1.96*se_catholic,
                 effect_catholic + 1.96*se_catholic)

effects <- c(effect_non_catholic, effect_catholic)
groups <- c("Non-Catholic", "Catholic")

bp <- barplot(effects,
              names.arg = groups,
              main = "Effect of Agriculture on Fertility",
              ylab = "Effect Size",
              col = c("lightblue", "lightcoral"),
              ylim = c(min(c(ci_non_catholic[1], ci_catholic[1])) - 0.005,
                       max(c(ci_non_catholic[2], ci_catholic[2])) + 0.005))

arrows(bp[1], ci_non_catholic[1], bp[1], ci_non_catholic[2],
       angle = 90, code = 3, length = 0.1, lwd = 2)
arrows(bp[2], ci_catholic[1], bp[2], ci_catholic[2],
       angle = 90, code = 3, length = 0.1, lwd = 2)

abline(h = 0, lty = 2, col = "gray")

# Non-Catholic effect
effect_non_catholic
ci_non_catholic
# Catholic effect
effect_catholic
ci_catholic
# Difference (interaction)
effect_catholic - effect_non_catholic


# Using interplot package
library(interplot)

# Marginal effect of Agriculture conditional on Catholic status
interplot(m = model_interaction,
          var1 = "Agriculture",
          var2 = "CatholicBin",
          hist = TRUE,
          point = TRUE) +
  ggtitle("Marginal Effect of Agriculture by Catholic Status") +
  xlab("Catholic Status (0=Non-Catholic, 1=Catholic)") +
  ylab("Marginal Effect of Agriculture on Fertility")

# Marginal effect of Catholic status conditional on Agriculture levels
interplot(m = model_interaction,
          var1 = "CatholicBin",
          var2 = "Agriculture",
          hist = TRUE,
          point = TRUE) +
  ggtitle("Marginal Effect of Catholic Status by Agriculture Level") +
  xlab("Agriculture Level (%)") +
  ylab("Marginal Effect of Catholic Status on Fertility")

# Alternative with filled CI
interplot(m = model_interaction,
          var1 = "CatholicBin",
          var2 = "Agriculture",
          hist = TRUE,
          point = FALSE,
          ci_fill = TRUE,
          ci_fill_color = "lightgray",
          ci_fill_alpha = 0.3) +
  ggtitle("Marginal Effect of Catholic Status by Agriculture Level") +
  xlab("Agriculture Level (%)") +
  ylab("Marginal Effect of Catholic Status on Fertility")

# Using interflex package
library(interflex)

# Linear interflex plot
interflex(estimator = "linear",
          Y = "Fertility",
          D = "CatholicBin",
          X = "Agriculture",
          data = swiss,
          main = "Catholic Effect Across Agriculture Levels")

# Binning interflex plot
interflex(estimator = "binning",
          Y = "Fertility",
          D = "CatholicBin",
          X = "Agriculture",
          data = swiss,
          nbins = 3,
          main = "Binning Approach: ME Catholic Status")

interflex(estimator = "kernel",
          Y = "Fertility",
          D = "CatholicBin",
          X = "Agriculture",
          data = swiss,
          bw=8.1745 ,
          main = "Kernal Approach: MargEffect Catholic Status")

# Comparison of methods:
# - Base R plots: Simple, show raw data clearly
# - interplot: Good for showing marginal effects with confidence intervals
# - interflex: More sophisticated, handles continuous moderators well
