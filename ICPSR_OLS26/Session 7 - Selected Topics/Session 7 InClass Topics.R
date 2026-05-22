## Set your working directory to the ICPSR_regression folder
## Change the path below to match your computer
# setwd("~/ICPSR_regression")
setwd("C:/Users/Patrick Shea/Dropbox/My Projects/Spring 2026/ICPSR/ICPSR_OLS26")

# Session 7: Extending OLS - Code Demonstration
# Panel Data and Two-Stage Models

rm(list=ls()) # Clear workspace

# ============================================================================
# PART 1: PANEL DATA METHODS
# ============================================================================

# Load required packages
library(plm)
library(foreign)
library(gplots)
library(car)
library(stargazer)

# Load panel dataset
Panel <- read.dta("http://dss.princeton.edu/training/Panel101.dta")
head(Panel)

# VISUALIZING PANEL DATA STRUCTURE
# Shows how units vary over time
coplot(y ~ year|country, type="l", data=Panel) # Lines
coplot(y ~ year|country, type="b", data=Panel) # Points and lines

# More sophisticated visualization
scatterplot(y~year|country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=Panel)

# DEMONSTRATE THE HETEROGENEITY PROBLEM
# Different countries have different baseline levels
plotmeans(y ~ country, main="Heterogeneity across countries", data=Panel)

# Time effects also exist
plotmeans(y ~ year, main="Heterogeneity across years", data=Panel)

# ============================================================================
# COMPARING DIFFERENT APPROACHES
# ============================================================================

# 1. POOLED OLS (ignores panel structure)
ols <- lm(y ~ x1, data=Panel)
summary(ols)

# Visualize pooled relationship
plot(Panel$x1, Panel$y, pch=19, xlab="x1", ylab="y")
abline(lm(Panel$y~Panel$x1), lwd=3, col="red")

# 2. FIXED EFFECTS - Manual calculation to show the logic
# Separate within and between variation
btw.x <- ave(Panel$x1, Panel$country)  # Between (country means)
wi.x <- Panel$x1 - btw.x              # Within (deviations from country means)

# Fixed effects = regression on within variation only
fixed.hand <- lm(Panel$y ~ wi.x)
summary(fixed.hand)

# Can also include both within and between for comparison
fixed.hand2 <- lm(Panel$y ~ wi.x + btw.x)
summary(fixed.hand2)

# 3. FIXED EFFECTS - Using dummy variables (LSDV)
fixed.dum <- lm(y ~ x1 + factor(country) - 1, data=Panel)
summary(fixed.dum)

# Visualize fixed effects
yhat2 <- fixed.dum$fitted
scatterplot(yhat2~Panel$x1|Panel$country, boxplots=FALSE, 
            xlab="x1", ylab="Fitted values", smooth=FALSE)

# 4. FIXED EFFECTS - Using plm package (most common)
fixed <- plm(y ~ x1, data=Panel, index=c("country", "year"), model="within")
summary(fixed)

# Get the fixed effects (country intercepts)
fixef(fixed)

# Test whether fixed effects are needed
pFtest(fixed, ols) # Null: OLS is adequate

# 5. RANDOM EFFECTS MODEL
random <- plm(y ~ x1, data=Panel, index=c("country", "year"), model="random")
summary(random)

# COMPARE ALL MODELS
stargazer(ols, fixed, random, type = "text",
          column.labels = c("Pooled OLS", "Fixed Effects", "Random Effects"))

# CHOOSE BETWEEN FIXED AND RANDOM EFFECTS
# Hausman test: Null = Random effects is consistent
phtest(fixed, random)
# If p < 0.05, use fixed effects; otherwise random effects

# ADDITIONAL TESTS
# Test for time fixed effects
fixed.time <- plm(y ~ x1 + factor(year), data=Panel, 
                  index=c("country", "year"), model="within")
pFtest(fixed.time, fixed) # Null: no time effects needed

# Test for serial correlation
pbgtest(fixed)

# ============================================================================
# PART 2: TWO-STAGE MODELS
# ============================================================================

# Load data for two-stage demonstrations
library(haven)
library(mediation)
library(sampleSelection)
library(AER)

# For this demo, we'll use the AJR data
ajr2 <- read_dta("data/ajr2.dta")

# ============================================================================
# MEDIATION ANALYSIS
# ============================================================================

# Research Question: Does settler mortality affect GDP through institutions?
# Settler mortality -> Institutions (risk) -> GDP

# Step 1: Total effect (without mediator)
total_model <- lm(loggdp ~ logmort0, data = ajr2)
summary(total_model)

# Step 2: Effect on mediator
mediator_model <- lm(risk ~ logmort0, data = ajr2)
summary(mediator_model)

# Step 3: Direct and indirect effects
outcome_model <- lm(loggdp ~ risk + logmort0, data = ajr2)
summary(outcome_model)

# Manual calculation of indirect effect
coef_risk <- coef(outcome_model)["risk"]
coef_logmort0 <- coef(mediator_model)["logmort0"]
indirect_effect <- coef_risk * coef_logmort0
cat("Indirect effect:", indirect_effect, "\n")

# Formal mediation analysis with confidence intervals
mediation_result <- mediate(mediator_model, outcome_model, 
                            treat = "logmort0", mediator = "risk", 
                            sims = 1000, boot = TRUE)
summary(mediation_result)

# Visualize mediation results
plot(mediation_result)

# ============================================================================
# SAMPLE SELECTION MODEL
# ============================================================================

# Load classic dataset for selection models
data("Mroz87", package = "sampleSelection")

# Problem: We only observe wages for women who choose to work
# This creates selection bias

# Naive approach (biased)
working_women <- subset(Mroz87, lfp == 1)
naive_model <- lm(log(wage) ~ educ + exper + I(exper^2) + age, data = working_women)
summary(naive_model)

# Heckman selection model
# Selection equation: What determines labor force participation?
selection_formula <- lfp ~ nwifeinc + educ + exper + I(exper^2) + age + kids5 + kids618

# Outcome equation: What determines wages (for those who work)?
outcome_formula <- log(wage) ~ educ + exper + I(exper^2) + age

# Fit Heckman model
heckman_result <- selection(selection_formula, outcome_formula, 
                            data = Mroz87, method = "2step")
summary(heckman_result)

# Compare naive vs selection-corrected results
stargazer(naive_model, heckman_result, type = "text",
          column.labels = c("Naive OLS", "Heckman Selection"))




# ============================================================================
# INSTRUMENTAL VARIABLES
# ============================================================================

# Problem: Institutions might be endogenous to economic outcomes
# Solution: Use settler mortality as instrument for institutions

# First stage: Instrument predicts endogenous variable
first_stage <- lm(risk ~ logmort0, data = ajr2)
summary(first_stage)

# Reduced form: Instrument affects outcome
reduced_form <- lm(loggdp ~ logmort0, data = ajr2)
summary(reduced_form)

# IV estimation using ivreg
iv_model <- ivreg(loggdp ~ risk | logmort0, data = ajr2)
summary(iv_model)

# Compare OLS vs IV
ols_biased <- lm(loggdp ~ risk, data = ajr2)

stargazer(ols_biased, iv_model, type = "text",
          column.labels = c("OLS (biased?)", "IV (consistent)"))

# ============================================================================
# SUMMARY COMPARISON
# ============================================================================

# Show how different methods address different problems:

cat("\n=== SUMMARY OF METHODS ===\n")
cat("Panel Data: Controls for unobserved heterogeneity\n")
cat("Mediation: Identifies causal pathways\n") 
cat("Selection: Corrects for non-random samples\n")
cat("IV: Addresses endogeneity bias\n")

