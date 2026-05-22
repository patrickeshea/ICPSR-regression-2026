## Set your working directory to the ICPSR_regression folder
## Change the path below to match your computer
# setwd("~/ICPSR_regression")
setwd("C:/Users/Patrick Shea/Dropbox/My Projects/Spring 2026/ICPSR/ICPSR_OLS26")

# =============================================================================
# Session 4: Multivariate OLS
# Theory, Interpretation & Dummy Variables
# =============================================================================

#######################################################################################
## Multivariate OLS

# Demonstrate  theorem:
# The coefficient on x1 in a multivariate regression equals the coefficient
# from regressing the residuals of y~x2 on the residuals of x1~x2

rm(list=ls())

n = 100; x = rnorm(n); x2 = rnorm(n)
cor(x, x2) # x and x2 are uncorrelated

y = 1 + x + x2 + rnorm(n, sd = .1)

ey = resid(lm(y ~ x2 ))
ex = resid(lm(x ~ x2 ))
sum(ey * ex) / sum(ex ^ 2)
coef(lm(ey ~ ex - 1)) # - 1 suppresses Constant
coef(lm(y ~ x + x2 ))
coef(lm(y ~ x  ))
coef(lm(y ~ x2 ))

# Plot the partial effect of x1 on y, after accounting for x2
plot(ex,ey,pch=21,bg="blue")

# Does it matter if we include x2?
summary(lm(y ~ x + x2))


# Show how adding a correlated control can change the sign of an effect
n <- 100; x2 <- 1 : n; x1 <- .01 * x2 + runif(n, -.1, .1); y = -x1 + x2 + rnorm(n, sd = .01)
y = 1 + x1 + x2 + rnorm(n, sd = .1)

ey = resid(lm(y ~ x2 ))
ex = resid(lm(x1 ~ x2 ))
sum(ey * ex) / sum(ex ^ 2)
coef(lm(ey ~ ex - 1 ))
coef(lm(y ~ x1 + x2 ))


plot(ex,ey,pch=21,bg="blue")


summary(lm(y ~ x1))$coef
summary(lm(y ~ x2))$coef
summary(lm(y ~ x1 + x2))$coef


# Visualize what's happening: x1 and x2 are correlated, so the bivariate
# relationship between y and x1 conflates the effect of x2
dat = data.frame(y = y, x1 = x1, x2 = x2, ey = resid(lm(y ~ x2)), ex1 = resid(lm(x1 ~ x2)))
library(ggplot2)
g = ggplot(dat, aes(y = y, x = x1, colour = x2))
g = g + geom_point(colour="grey50", size = 5) + geom_smooth(method = lm, se = FALSE, colour = "black")
g = g + geom_point(size = 4)
g


# Same plot after partialling out x2
g2 = ggplot(dat, aes(y = ey, x = ex1, colour = x2))
g2 = g2 + geom_point(colour="grey50", size = 5) + geom_smooth(method = lm, se = FALSE, colour = "black") + geom_point(size = 4)
g2


# =============================================================================
# PART 2: SWISS DATA - REAL WORLD APPLICATION
# =============================================================================

rm(list=ls())
library(datasets)
data(swiss)

# === Activity 3a: Explore the Swiss fertility data ===
# Fertility: birth rate per 1000
# Agriculture: % of population in agriculture
# Examination: % receiving highest mark on army exam
# Education: % with education beyond primary school
# Catholic: % Catholic
# Infant.Mortality: infant mortality rate

head(swiss)
summary(swiss)

# === Activity 3b: Compare bivariate vs multivariate models ===

# Simple model: just Agriculture
simple_model <- lm(Fertility ~ Agriculture, data = swiss)

# Full multivariate model
full_model <- lm(Fertility ~ ., data = swiss)

summary(simple_model)$coefficients
summary(full_model)$coefficients

# The Agriculture coefficient changes because it's correlated with
# other variables that also affect Fertility
coef(simple_model)[2]
coef(full_model)[2]

# === Activity 3c: Interpret multivariate coefficients ===
# Each coefficient shows the effect HOLDING ALL OTHER VARIABLES CONSTANT
coef(full_model)

# =============================================================================
# PART 4: DUMMY VARIABLES - CATEGORICAL PREDICTORS
# =============================================================================

# === Activity 4a: Create a binary variable from Catholic percentage ===
swiss$high_catholic <- ifelse(swiss$Catholic > median(swiss$Catholic), 1, 0)
table(swiss$high_catholic)

# === Activity 4b: Regression with dummy variables ===
# The dummy coefficient is the difference in predicted Y between groups
dummy_model <- lm(Fertility ~ Agriculture + Education + high_catholic, data = swiss)
summary(dummy_model)

# === Activity 4c: Multiple category dummy variables ===
# R automatically creates k-1 dummies for k categories (reference = first level)
swiss$catholic_level <- cut(swiss$Catholic,
                            breaks = c(0, 33, 66, 100),
                            labels = c("Low", "Medium", "High"),
                            include.lowest = TRUE)

table(swiss$catholic_level)

multi_dummy_model <- lm(Fertility ~ Agriculture + Education + catholic_level, data = swiss)
summary(multi_dummy_model)

# =============================================================================
# PART 5: MODEL COMPARISON AND EVALUATION
# =============================================================================

# === Activity 5a: R-squared vs Adjusted R-squared ===
# R-squared always increases with more variables; Adjusted R-squared penalizes for complexity
models <- list("Simple" = simple_model,
               "Full" = full_model,
               "With Dummies" = multi_dummy_model)

for(name in names(models)) {
  r2 <- summary(models[[name]])$r.squared
  adj_r2 <- summary(models[[name]])$adj.r.squared
  print(sprintf("%-15s R2 = %.3f, Adjusted R2 = %.3f",
              paste0(name, ":"), r2, adj_r2))
}

# === Activity 5b: F-test for nested model comparison ===
anova_result <- anova(simple_model, full_model)
print(anova_result)

# =============================================================================
# PART 6: EFFECTS OF ADDING VARIABLES
# =============================================================================

# === Activity 6a: Adding an irrelevant variable inflates standard errors ===
set.seed(123)
swiss$random_var <- rnorm(nrow(swiss))

model_with_noise <- lm(Fertility ~ Agriculture + Education + random_var, data = swiss)
model_without_noise <- lm(Fertility ~ Agriculture + Education, data = swiss)

summary(model_without_noise)$coefficients
summary(model_with_noise)$coefficients

# === Activity 6b: Perfect multicollinearity — R drops the redundant variable ===
swiss$perfect_corr <- swiss$Agriculture + swiss$Education

tryCatch({
  perfect_model <- lm(Fertility ~ Agriculture + Education + perfect_corr, data = swiss)
  summary(perfect_model)
}, error = function(e) {
  print(paste("Error:", e$message))
})

# === Activity 6c: High (but not perfect) multicollinearity inflates SEs ===
set.seed(123)
swiss$high_corr <- swiss$Agriculture + rnorm(nrow(swiss), mean = 0, sd = 0.1)

cor(swiss$Agriculture, swiss$high_corr)

high_corr_model <- lm(Fertility ~ Agriculture + Education + high_corr, data = swiss)
normal_model <- lm(Fertility ~ Agriculture + Education, data = swiss)

# Compare SEs — multicollinearity blows them up
summary(normal_model)$coefficients[2, "Std. Error"]
summary(high_corr_model)$coefficients[2, "Std. Error"]

# =============================================================================
# PART 7: PREDICTION WITH MULTIVARIATE MODELS
# =============================================================================

# === Activity 7a: Predictions with continuous variables ===
new_data <- data.frame(
  Agriculture = c(20, 40, 60),
  Education = c(10, 20, 30),
  Examination = c(15, 20, 25),
  Catholic = c(25, 50, 75),
  Infant.Mortality = c(15, 20, 25)
)

predictions <- predict(full_model, newdata = new_data, interval = "prediction")
cbind(new_data[, c("Agriculture", "Education")], round(predictions, 2))

# === Activity 7b: Predictions with dummy variables ===
# Must specify the factor level for categorical predictors
dummy_new_data <- data.frame(
  Agriculture = c(40, 40, 40),
  Education = c(20, 20, 20),
  catholic_level = factor(c("Low", "Medium", "High"), levels = c("Low", "Medium", "High"))
)

dummy_predictions <- predict(multi_dummy_model, newdata = dummy_new_data)

# Differences between groups
dummy_predictions[2] - dummy_predictions[1]  # Medium vs Low
dummy_predictions[3] - dummy_predictions[1]  # High vs Low
dummy_predictions[3] - dummy_predictions[2]  # High vs Medium

# === Session 4 Complete ===
