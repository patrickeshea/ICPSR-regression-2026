## Set your working directory to the ICPSR_regression folder
## Change the path below to match your computer
# setwd("~/ICPSR_regression")
setwd("C:/Users/Patrick Shea/Dropbox/My Projects/Spring 2026/ICPSR/ICPSR_OLS26")

##### Diagnostics
## Clear all existing objects from the workspace to start fresh
rm(list = ls())

## Load the car package to access the Prestige dataset and additional diagnostic plots
library(car)

## Fit a linear regression model predicting 'prestige' from 'income'
fm <- lm(prestige ~ income, data = Prestige)


## Basic scatter plot of income vs. prestige with regression line
plot(Prestige$income, Prestige$prestige)
abline(lm(Prestige$prestige ~ Prestige$income))

## Summary of the fitted model to view coefficients, R-squared, etc.
summary(fm)

## Diagnostic plots for assessing model assumptions and fit
plot(fm)
# The plot(model) command generates 4 diagnostic plots:

## Residuals vs. Fitted: Checks for non-linear patterns, homoscedasticity
# Should not observe clear patterns or funnels.

## Normal Q-Q: Assesses normality of residuals
# Points should closely follow the reference line if residuals are normally distributed.

## Scale-Location (Spread-Location): Similar to Residuals vs. Fitted but focuses on standardized residuals
# Helps to check the assumption of equal variance (homoscedasticity).

## Residuals vs. Leverage: Identifies influential observations
# Observations with high leverage or Cook's distance might unduly influence the model.

## Commentary on ambiguous Q-Q plot interpretation
# Looking for deviations from linearity like "banana" or "S" shapes.

## Using qqPlot to add confidence intervals around the normality line for clearer interpretation
qqPlot(fm)
# Observations far from the line, especially outside CIs, may indicate issues.

## Breusch-Pagan test for heteroskedasticity
library(lmtest)
bptest(fm)
# P-value > 0.05 suggests no evidence of heteroskedasticity.

## Further examination of residuals for potential non-linearity or misspecification
residualPlots(fm)
# Non-random patterns may suggest misspecification such as needing a non-linear model.

####################################################
## Outliers, Leverage, and Influence
###############################################

## Outlier test with Bonferroni correction
outlierTest(fm)
# Identifies outliers; significance suggests observations not explained well by the model.

## Influence index plot highlighting the top 3 influential points
influenceIndexPlot(fm, id.n=3)
# Cook's distance, leverage, and studentized residuals plotted for each observation.

## Testing model sensitivity by excluding identified outliers
fm_wo <- update(fm, subset = rownames(Prestige) != "general.managers")
compareCoefs(fm, fm_wo)
# Compares coefficients to assess the impact of removing outliers.

## Influence plots before and after removing outliers
influencePlot(fm, id.n=3)    # Before removing
influencePlot(fm_wo, id.n=3) # After removing

## Re-specifying the model with income transformed to log scale to address skewness and non-linearity
fm2 <- lm(prestige ~ log(income), data = Prestige)
plot(fm2)

residualPlots(fm2)
# Log transformation often stabilizes variance and makes relationships more linear.

## Influence plot for the model with log-transformed income
influencePlot(fm2, id.n=3)
# Check if log transformation reduces undue influence of any observations.

## Adding control variables to the model (education, log(income), type)
fm21 <- lm(prestige ~ education + log(income) + type, data = Prestige)

## Diagnostic plots for the model with controls
plot(fm21)

residualPlots(fm21)

## Plotting predicted values against residuals to check for any systematic patterns
plot(predict(fm21), residuals(fm21))

## Comparing fitted values from the model object to those obtained from predict function
fit1 <- fitted.values(fm21)
fit2 <- predict(fm21)
plot(fit1, fit2) # Should ideally form a straight line y=x if both are consistent


######Simulated Examples. 

## Clear the workspace
rm(list = ls())

## Set a seed for reproducible simulations
set.seed(11234)

## Simulation 1: z is unrelated to y and x
n <- 1000
x <- rnorm(n, 3, 3)
z <- runif(n)
error1 <- rnorm(n, 0, 1)
y <- 1.5 + 3*x + error1

## Regression of y on x
m1 <- lm(y ~ x)

## Regression of y on x and z
m2 <- lm(y ~ x + z)

## Compare the models
summary(m1)
summary(m2)

## Simulation 2: z is unrelated to y, but causes x
z <- runif(n)
x <- rnorm(n, 3, 3) - 1.75*z
y <- 1.5 + 3*x + error1

m3 <- lm(y ~ x)
m4 <- lm(y ~ x + z)

summary(m3)
summary(m4)

## Simulation 3: z is related to y and x
set.seed(15234)
z <- rnorm(n, 4, 10)
x <- rnorm(n, 3, 9) + 3.75*z
y <- 1.5 + 3*x + 10*z + error1

m5 <- lm(y ~ x)
m6 <- lm(y ~ x + z)

summary(m5)
summary(m6)






# Load the swiss dataset
data("swiss")
library(car)

# Fit a linear regression model
model <- lm(Fertility ~ ., data = swiss)

# Calculate and display the Variance Inflation Factor (VIF) for each predictor
vif(model)
