## Set your working directory to the ICPSR_regression folder
## Change the path below to match your computer
# setwd("~/ICPSR_regression")
setwd("C:/Users/Patrick Shea/Dropbox/My Projects/Spring 2026/ICPSR/ICPSR_OLS26")

# Session 7 Lab: Extending OLS in Social Science Research
# Complete Code with Explanations

# Clear workspace and load required libraries
rm(list=ls())

# ============================================================================
# PART 1: PANEL DATA ANALYSIS - CHARITABLE GIVING
# ============================================================================

# Load required packages for panel data analysis
library(plm)        # For panel data models
library(dplyr)      # For data manipulation
library(haven)      # For reading Stata files
library(stargazer)  # For nice regression tables

# Load the charitable giving dataset
# Note: Adjust the file path to where you saved the data
charity <- read_dta("data/charity.dta")

# Examine the data structure first
head(charity)
str(charity)

# IMPORTANT: Tell R this is panel data
# This creates a special panel data frame that knows about individuals and time
charity <- pdata.frame(charity, index = c("subject", "time"))

# Let's see what we're working with
summary(charity)

# ============================================================================
# TASK 1.2: POOLED OLS MODEL
# ============================================================================

# Pooled OLS ignores the panel structure - treats all observations as independent
# This assumes all individuals are identical (no heterogeneity)
pooled_ols <- lm(charity ~ age + income + price + deps + ms, data = charity)
summary(pooled_ols)

# What does this tell us?
# - We're assuming the relationship between income and charity is the same for everyone
# - We ignore that some people might be naturally more generous than others

# ============================================================================
# TASK 1.3: FIXED EFFECTS MODEL
# ============================================================================

# Fixed effects controls for all time-invariant individual characteristics
# It asks: "When person i's income changes, how does their giving change?"
fixed_effects <- plm(charity ~ age + income + price + deps + ms, 
                     data = charity, model = "within")
summary(fixed_effects)

# What happened to some variables?
# Notice that time-invariant variables (like gender, if we had it) would disappear
# Fixed effects can only estimate effects of variables that change over time

# ============================================================================
# TASK 1.4: RANDOM EFFECTS MODEL
# ============================================================================

# Random effects assumes individual differences are random and uncorrelated with X
# It's a compromise between pooled OLS and fixed effects
random_effects <- plm(charity ~ age + income + price + deps + ms, 
                      data = charity, model = "random")
summary(random_effects)

# ============================================================================
# TASK 1.5: HAUSMAN TEST - CHOOSE YOUR MODEL
# ============================================================================

# The Hausman test helps us choose between fixed and random effects
# H0: Random effects is consistent (individual effects uncorrelated with X)
# H1: Only fixed effects is consistent (correlation exists)

hausman_test <- phtest(fixed_effects, random_effects)
print(hausman_test)

# Interpretation:
# If p < 0.05: Reject H0, use FIXED EFFECTS
# If p > 0.05: Fail to reject H0, RANDOM EFFECTS is fine

# Let's also run the Breusch-Pagan test
# This tests whether we need panel models at all vs. simple OLS
bp_test <- plmtest(random_effects, type = "bp")
print(bp_test)

# Compare all models in a nice table
stargazer(pooled_ols, fixed_effects, random_effects, type="text",
          column.labels = c("Pooled OLS", "Fixed Effects", "Random Effects"),
          title = "Comparison of Panel Data Models")

#######################
##Alternative code

##Dummy variables for fixed effects
dummy_fe <- lm(charity ~ age + income + price + deps + ms + factor(subject), 
               data = charity)

summary(dummy_fe)


charity_demeaned <- charity %>%
  group_by(subject) %>%
  mutate(
    charity = charity - mean(charity),
    age = age - mean(age),
    income = income - mean(income),
    price= price - mean(price),
    deps = deps - mean(deps),
    ms = ms - mean(ms)
  )

# Regression on demeaned variables (no intercept)
manual_fe <- lm(charity ~ age + income + price + deps + ms - 1, 
                data = charity_demeaned)

print("Fixed Effects with Manual Demeaning:")
summary(manual_fe)

stargazer(fixed_effects, dummy_fe, manual_fe, type="text",
          column.labels = c("PLM", "Dummies", "Manual"),
          title = "Comparison of Panel Data Models")


# ============================================================================
# TWO-WAY FIXED EFFECTS (Unit + Time)
# ============================================================================
print("\n4. TWO-WAY FIXED EFFECTS")

# Add both individual and time dummies
twoway_dummy <- lm(charity ~ age + income + price + deps + ms + 
                     factor(subject) + factor(time), 
                   data = charity)

print("Two-way FE with Dummies:")
summary(twoway_dummy)$coefficients[1:6, ]

# Two-way FE with plm
twoway_plm <- plm(charity ~ age + income + price + deps + ms, 
                  data = charity, model = "within", effect = "twoways")

print("Two-way FE with PLM:")
summary(twoway_plm)



# ============================================================================
# PART 2: TWO-STAGE MODELS - CONFLICT AND ECONOMIC SHOCKS
# ============================================================================

# Load additional packages for two-stage models
library(mediation)      # For mediation analysis
library(AER)           # For instrumental variables
library(ggplot2)       # For plotting

# Load the Miguel et al. (2004) conflict dataset
# This famous paper studies how economic shocks (rainfall) affect civil conflict
conflict_data <- read_dta("data/rainconflict.dta")

# Examine the data
head(conflict_data)
summary(conflict_data[, c("any_prio", "gdp_g", "GPCP_g", "polity2l", 
                          "ethfrac", "Oil", "lpopl1")])

# Let's visualize the conflict variable
hist(conflict_data$any_prio, main="Distribution of Civil Conflict", 
     xlab="Conflict (0=No, 1=Yes)")

# ============================================================================
# TASK 2.1: MEDIATION ANALYSIS
# ============================================================================

# Research Question: Does rainfall affect conflict through economic growth?
# Theory: Rainfall → Economic Growth → Conflict
#         Rainfall → Conflict (direct effect)

# Define our control variables for easier reference
controls <- c("polity2l", "ethfrac", "Oil", "lpopl1")

# STEP 1: Mediator Model (X → M)
# Does rainfall affect economic growth?
mediator_model <- lm(gdp_g ~ GPCP_g + polity2l + ethfrac + Oil + lpopl1, 
                     data = conflict_data)
summary(mediator_model)

# STEP 2: Outcome Model (X + M → Y)  
# Do both rainfall and growth affect conflict?
outcome_model <- lm(any_prio ~ GPCP_g + gdp_g + polity2l + ethfrac + Oil + lpopl1,
                    data = conflict_data)
summary(outcome_model)

# STEP 3: Formal Mediation Analysis
# This calculates confidence intervals and tests significance
mediation_result <- mediate(mediator_model, outcome_model, 
                            treat = "GPCP_g", mediator = "gdp_g",
                            sims = 1000, boot = TRUE)

# View results
summary(mediation_result)

# Visualize the mediation
plot(mediation_result)

# Interpretation:
# - ACME (Average Causal Mediation Effect) = Indirect effect through GDP growth
# - ADE (Average Direct Effect) = Direct effect of rainfall on conflict  
# - Total Effect = ACME + ADE

# ============================================================================
# TASK 2.2: INSTRUMENTAL VARIABLES
# ============================================================================

# Research Question: What's the causal effect of economic growth on conflict?
# Problem: Growth and conflict might affect each other (endogeneity)
# Solution: Use rainfall as instrument - affects growth but not conflict directly

# First, let's see the "biased" OLS estimate
ols_biased <- lm(any_prio ~ gdp_g + polity2l + ethfrac + Oil + lpopl1,
                 data = conflict_data)
summary(ols_biased)

# Now the IV estimation
# Syntax: outcome ~ endogenous_var + controls | instruments + controls
iv_model <- ivreg(any_prio ~ gdp_g + polity2l + ethfrac + Oil + lpopl1 | 
                    GPCP_g + GPCP_g_l + polity2l + ethfrac + Oil + lpopl1,
                  data = conflict_data)

# Get detailed IV results with diagnostic tests
summary(iv_model, diagnostics = TRUE)

# Compare OLS vs IV estimates
stargazer(ols_biased, iv_model, type="text",
          column.labels = c("OLS (potentially biased)", "IV (consistent)"),
          title = "Economic Growth and Conflict: OLS vs IV")

# ============================================================================
# INTERPRETATION OF IV DIAGNOSTICS
# ============================================================================

# The summary(iv_model, diagnostics = TRUE) gives us three key tests:

# 1. WEAK INSTRUMENTS TEST (Cragg-Donald F-statistic)
#    - Tests if instruments are strong enough
#    - Rule of thumb: F > 10 suggests instruments are strong
#    - If F < 10, IV estimates may be unreliable

# 2. WU-HAUSMAN TEST  
#    - Tests whether we need IV (is there endogeneity?)
#    - H0: OLS is consistent (no endogeneity)
#    - If p < 0.05: Reject H0, endogeneity exists, use IV
#    - If p > 0.05: OLS might be fine

# 3. SARGAN TEST (if overidentified - more instruments than endogenous vars)
#    - Tests instrument validity  
#    - H0: Instruments are valid (uncorrelated with error)
#    - If p < 0.05: Some instruments may be invalid
#    - If p > 0.05: Instruments appear valid

# ============================================================================
# SUMMARY AND REFLECTION
# ============================================================================

cat("\n=== LAB SUMMARY ===\n")
cat("Panel Data Analysis:\n")
cat("- Pooled OLS assumes no individual heterogeneity\n") 
cat("- Fixed effects controls for unobserved individual characteristics\n")
cat("- Random effects assumes individual effects are uncorrelated with X\n")
cat("- Use Hausman test to choose between fixed and random effects\n\n")

cat("Two-Stage Models:\n")
cat("- IV estimation addresses endogeneity bias\n") 
cat("- Always check instrument strength and validity\n")

# Clean up workspace
# rm(list=ls())