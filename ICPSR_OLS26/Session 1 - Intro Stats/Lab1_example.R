## Set your working directory to the ICPSR_regression folder
## Change the path below to match your computer
# setwd("~/ICPSR_regression")
setwd("C:/Users/Patrick Shea/Dropbox/My Projects/Spring 2026/ICPSR/ICPSR_OLS26")

# Install and load the ggplot2 package if not already installed
# install.packages("ggplot2")
library(ggplot2)

# Load the diamonds dataset
data(diamonds)

# Task 1: Data Exploration
head(diamonds)
str(diamonds)
summary(diamonds)

# Task 2: Univariate Analysis
hist(diamonds$price, main = "Distribution of Price", xlab = "Price")
mean(diamonds$price)
median(diamonds$price)
sd(diamonds$price)
table(diamonds$cut)

# Task 3: Bivariate Analysis
plot(diamonds$carat, diamonds$price, main = "Carat vs. Price", xlab = "Carat", ylab = "Price")
cor(diamonds$carat, diamonds$price)
boxplot(price ~ cut, data = diamonds, main = "Price by Cut", xlab = "Cut", ylab = "Price")

# Task 4: Subset Analysis
high_quality <- subset(diamonds, cut %in% c("Very Good", "Premium"))
mean(high_quality$price)
median(high_quality$price)
mean(high_quality$price) - mean(diamonds$price)

# Task 5: Visualization
plot(density(diamonds$depth), main = "Density Plot of Depth", xlab = "Depth")
barplot(table(diamonds$clarity), main = "Count of Diamonds by Clarity", xlab = "Clarity", ylab = "Count")
pairs(~ carat + depth + table + price, data = diamonds, main = "Pairs Plot")

