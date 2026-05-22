# ============================================
# R Demonstration: Understanding Our Data
# Topic 1: Basic Statistics and Visualization
# ============================================

# Clear the workspace and load data
rm(list = ls())
data <- mtcars  # We'll use the built-in mtcars dataset

# -----------------
# 1. EXPLORING DATA 
# -----------------
# Look at the first few rows
head(data)

# Get basic summary statistics
summary(data$mpg)  # Miles per gallon
summary(data$wt)   # Weight

# ----------------------------------
# 2. MEASURES OF CENTER
# ----------------------------------
# Calculate mean (average)
mean(data$mpg)

# Find the median (middle value)
median(data$mpg)

# Look at the mode (most common value)
table(data$cyl)  # Example with number of cylinders

# ----------------------------------
# 3. MEASURES OF SPREAD 
# ----------------------------------

# Calculate variance
var(data$mpg)

# Calculate standard deviation
sd(data$mpg)

# Create a basic histogram
hist(data$mpg, 
     main="Distribution of Miles per Gallon",
     xlab="Miles Per Gallon",
     col="skyblue",
     border="white")

##Want to examine the options?
?hist

# Add a boxplot to show spread
boxplot(data$mpg, 
        horizontal=TRUE,
        main="Boxplot of MPG",
        col="skyblue")

# Density plot
plot(density(data$mpg),
     main = "Density Plot of MPG",
     xlab = "Miles Per Gallon")

# Boxplot by cylinder
boxplot(mpg ~ cyl,
        data = data,
        main = "MPG by Number of Cylinders",
        xlab = "Cylinders",
        ylab = "Miles Per Gallon")

# Additional statistics
var(data$mpg)  # Variance
range(data$mpg)  # Range
quantile(data$mpg)  # Quartiles



# ============================================
# R Demonstration: Women in Politics and Military Spending
# Data from Koch & Fulton (2011) #Data from Koch, Michael T., and Sarah A. Fulton. "In the defense of women: Gender, 
#office holding, and national security policy in established democracies." 
#The Journal of politics 73.1 (2011): 1-16
# ============================================

# Load required library

#install.packages(haven)  ##Install library first, if not previously installed
library(haven)  ##load library


# -----------------
# 3. LOAD AND EXAMINE DATA
# -----------------
# Load the data (make sure the directory is correct-- you can also go under file / import to do this manually)
#setwd("C:/Users/Patrick Shea/Dropbox/My Projects/Spring 2025/ICPSR/Sp25 ICPSR/IntroOLS/Session 1 - Intro Stats")
women12 <- read_dta("women12.dta")

# Look at the structure of our data
str(women12)

# Look at the first few rows
head(women12)

# Key variables:
# mil_exp_pergdp2: Military expenditure as % of GDP
# m_woman2: Percentage of women in parliament
# m_femlead: Binary variable (1 = female leader)

# ----------------------------------
# 4. EXAMINING MILITARY SPENDING
# ----------------------------------
# Basic summary of military spending
summary(women12$mil_exp_pergdp2)

# Calculate mean and standard deviation
mean_mil <- mean(women12$mil_exp_pergdp2, na.rm = TRUE)
sd_mil <- sd(women12$mil_exp_pergdp2, na.rm = TRUE)

# Create a histogram of military spending
hist(women12$mil_exp_pergdp2, 
     main = "Distribution of Military Spending (% GDP)",
     xlab = "Military Spending (% GDP)",
     col = "lightblue",
     border = "white")



# Add a boxplot
boxplot(women12$mil_exp_pergdp2,
        horizontal = TRUE,
        main = "Military Spending Distribution",
        col = "lightblue")

# ----------------------------------
# 5. WOMEN IN POLITICS
# ----------------------------------
# First, look at women leaders (binary variable)
table(women12$m_femlead)  # Count of female leaders

# Create a bar plot for female leaders
barplot(table(women12$m_femlead),
        main = "Number of Female Leaders",
        xlab = "Female Leader (1 = Yes)",
        col = "red",
        ylim = c(0, 1000))

# Look at women in parliament
summary(women12$m_woman2)

# Histogram of women in parliament
hist(women12$m_woman2,
     main = "Distribution of Women in Parliament",
     xlab = "Percentage of Women",
     col = "red",
     border = "white")






# ----------------------------------
# 6. RELATIONSHIPS
# ----------------------------------
# Create a scatter plot of weight vs. mpg
plot(data$wt, data$mpg,
     xlab = "Weight (1000 lbs)", 
     ylab = "Miles Per Gallon",
     main = "Car Weight vs. MPG",
     pch = 19,  # Solid circles
     col = "darkblue")

# Calculate covaration
cov(data$wt, data$mpg)

# Calculate correlation
cor(data$wt, data$mpg)

# Fit a simple line
model <- lm(mpg ~ wt, data = data)
abline(model, col = "red", lwd = 2)



# Example with multiple variables

  # Correlation matrix
  cor(data[, c("mpg", "wt", "hp", "disp")])
  
  # Multiple regression
  summary(lm(mpg ~ wt + hp, data = data))
  
  
  
  # ----------------------------------
  # 7. RELATIONSHIPS 
  # ----------------------------------
  # Create a scatter plot: Women in Parliament vs Military Spending
  plot(women12$m_woman2, women12$mil_exp_pergdp2,
       xlab = "Percentage of Women in Parliament",
       ylab = "Military Spending (% GDP)",
       main = "Women in Parliament vs Military Spending",
       pch = 19,
       col = "darkred")
  
  # Calculate correlation
  cor(women12$m_woman2, women12$mil_exp_pergdp2, 
      use = "complete.obs")
  
  # Add a trend line
  model <- lm(mil_exp_pergdp2 ~ m_woman2, data = women12)
  abline(model, col = "blue", lwd = 2)  
  
  # ============================================
  # EXPLORING MULTIPLE RELATIONSHIPS WITH PAIRS()
  # ============================================
  
  # Let's look at relationships between multiple variables at once
  # Select a few key variables for analysis
  key_vars <- c("mil_exp_pergdp2", "m_woman2", "m_femlead", "gdp_capita2", "polity2")
  
  # Check which variables are available in our dataset
  names(women12)
  
  # Create a pairs plot to see all relationships at once
  # First, let's use only the variables we know exist
  available_vars <- c("mil_exp_pergdp2", "m_woman2", "m_femlead")
  
  # Basic pairs plot
  pairs(women12[, available_vars],
        main = "Relationships Between Women in Politics and Military Spending")
  
  # Enhanced pairs plot with better labels
  pairs(women12[, available_vars],
        main = "Multiple Variable Relationships",
        labels = c("Military Spending\n(% GDP)", 
                   "Women in\nParliament (%)", 
                   "Female Leader\n(1=Yes)"),
        pch = 19,
        col = "darkblue")
  

  # ----------------------------------
  # 8. SUBSETTING DATA
  # ----------------------------------
  
  # METHOD 1: Using bracket notation [rows, columns] **Not recommended
  # Get first 10 rows of military spending data
  first_10_mil <- women12[1:10, "mil_exp_pergdp2"]
  first_10_mil
  
  # Get specific countries (first 5 rows, multiple columns)
  subset_data <- women12[1:5, c("mil_exp_pergdp2", "m_woman2", "m_femlead")]
  subset_data
  
  # METHOD 2: Conditional subsetting
  # Countries with female leaders
  female_leaders <- women12[women12$m_femlead == 1, ]
  nrow(female_leaders)  # How many countries have female leaders?
  
  # Countries with high military spending (above average)
  mean_spending <- mean(women12$mil_exp_pergdp2, na.rm = TRUE)
  high_mil_spending <- women12[women12$mil_exp_pergdp2 > mean_spending, ]
  
  # METHOD 3: Using subset() function (often easier to read)
  # Countries with low women representation (less than 20% in parliament)
  low_women_rep <- subset(women12, m_woman2 < 20)
  
  # Countries with both female leaders AND high women representation
  female_leaders_high_rep <- subset(women12, 
                                    m_femlead == 1 & m_woman2 > 30)
  
  #  METHOD 4: Subset for specific categories using %in%
  # Example: Countries with either very high OR very low women representation
  # (less than 10% OR more than 40%)
  extreme_representation <- subset(women12, 
                                   m_woman2 %in% c(0:10, 40:100))
  
   summary(women12$mil_exp_pergdp2)
   summary(extreme_representation$mil_exp_pergdp2)
  # ----------------------------------
  # 9. COMPARING SUBSETS
  # ----------------------------------
  
  # Compare military spending between countries with/without female leaders
  # Countries WITH female leaders
  mil_with_female <- women12$mil_exp_pergdp2[women12$m_femlead == 1]
  mean(mil_with_female, na.rm = TRUE)
  
  # Countries WITHOUT female leaders  
  mil_without_female <- women12$mil_exp_pergdp2[women12$m_femlead == 0]
  mean(mil_without_female, na.rm = TRUE)
  
  # Create side-by-side boxplots
  boxplot(mil_exp_pergdp2 ~ m_femlead, 
          data = women12,
          main = "Military Spending by Female Leadership",
          xlab = "Female Leader (0=No, 1=Yes)",
          ylab = "Military Spending (% GDP)",
          col = c("lightblue", "pink"),
          names = c("Male Leader", "Female Leader"))
  
  
