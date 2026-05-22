## Set your working directory to the ICPSR_regression folder
## Change the path below to match your computer
# setwd("~/ICPSR_regression")
setwd("C:/Users/Patrick Shea/Dropbox/My Projects/Spring 2026/ICPSR/ICPSR_OLS26")

rm(list=ls())


# Load the necessary package
library(MASS)

# Load the USArrests dataset
data(USArrests)

?USArrests

head(USArrests)

# Task 1: Calculate the covariance 

cov(USArrests$Murder, USArrests$UrbanPop)

cov_matrix <- cov(USArrests)
print(cov_matrix)
# The cov() function calculates the covariance matrix of the USArrests dataset.
# The covariance matrix shows the pairwise covariances between the variables.

# Task 2: Calculate the correlation
cor(USArrests$Murder, USArrests$UrbanPop)


cor_matrix <- cor(USArrests)
print(cor_matrix)
# The cor() function calculates the correlation matrix of the USArrests dataset.
# The correlation matrix shows the pairwise correlations between the variables.

# Task 3: Perform OLS regression with murder rate as the dependent variable

model1 <- lm(Murder ~ UrbanPop, data = USArrests)
summary(model1)

model2 <- lm(Murder ~ UrbanPop + Assault, data = USArrests)
summary(model2)


model3 <- lm(Murder ~ ., data = USArrests)
summary(model3)

library(stargazer)
stargazer(model1, model2, model3, type="text")


# The lm() function performs OLS regression with the murder rate as the dependent variable
# and all other variables (assault rate, urbanization, and average education) as independent variables.
# The summary() function provides a summary of the regression model, including coefficients, standard errors,
# t-values, p-values, and model fit statistics.

# Task 4: Interpret the coefficients
# The coefficients in the regression output represent the change in the murder rate
# associated with a one-unit change in each independent variable, holding other variables constant.
# For example, the coefficient for Assault is 0.02827, meaning that a one-unit increase in the assault rate
# is associated with a 0.02827 increase in the murder rate, holding urbanization and average education constant.

# Task 5: Calculate the R-squared and interpret it
r_squared1 <- summary(model1)$r.squared
print(r_squared1)

r_squared3 <- summary(model3)$r.squared
print(r_squared3)

# Calculate the predicted values (fitted values)
predicted_values <- fitted(model1)

# Calculate the mean of the murder rate
mean_murder <- mean(USArrests$Murder)

# Calculate the total sum of squares (SST)
sst <- sum((USArrests$Murder - mean_murder)^2)

# Calculate the residual sum of squares (SSR)
ssr <- sum((USArrests$Murder - predicted_values)^2)

# Calculate the R-squared manually
r_squared_manual <- 1 - (ssr / sst)
print(r_squared_manual)


# The R-squared value represents the proportion of variance in the murder rate
# that is explained by the independent variables (assault rate, urbanization, and average education).
# In this case, the R-squared value is 0.8018, meaning that approximately 80.18% of the variation
# in the murder rate can be explained by the independent variables included in the model.

# Task 6: Create a scatter plot with the fitted regression line
plot(USArrests$UrbanPop, USArrests$Murder, xlab = "Urbanization Rate", ylab = "Murder Rate")
abline(model1, col = "red")
# The plot() function creates a scatter plot with urbanization rate on the x-axis and murder rate on the y-axis.
# The abline() function adds the fitted regression line to the plot, using the coefficients from the regression model.
# The col argument sets the color of the regression line to red.





#################################
  
rm(list=ls())

data<- read.csv("data/day1.csv")
data2 <- data

summary(data)

model1 <- lm(y1 ~ x1, data)
model2 <- lm(y2 ~ x2, data)
model3 <- lm(y3 ~ x3, data)
model4 <- lm(y4 ~ x4, data)

library(stargazer)
stargazer(model1, model2, model3, model4, type="text")

library(faraway)
sumary(model1)
sumary(model2)
sumary(model3)
sumary(model4)

cor1 <- format(cor(data$x1, data$y1), digits=4)
cor2 <- format(cor(data$x2, data$y2), digits=4)
cor3 <- format(cor(data$x3, data$y3), digits=4)
cor4 <- format(cor(data$x4, data$y4), digits=4)

#define the OLS regression
line1 <- lm(y1 ~ x1, data=data)
line2 <- lm(y2 ~ x2, data=data)
line3 <- lm(y3 ~ x3, data=data)
line4 <- lm(y4 ~ x4, data=data)

circle.size = 5
colors = list('red', '#0066CC', '#4BB14B', '#FCE638')

library(ggplot2)

#plot1
plot1 <- ggplot(data, aes(x=x1, y=y1)) + geom_point(size=circle.size, pch=21, fill=colors[[1]]) +
  geom_abline(intercept=line1$coefficients[1], slope=line1$coefficients[2]) +
  annotate("text", x = 12, y = 5, label = paste("correlation = ", cor1))

#plot2
plot2 <- ggplot(data, aes(x=x2, y=y2)) + geom_point(size=circle.size, pch=21, fill=colors[[2]]) +
  geom_abline(intercept=line2$coefficients[1], slope=line2$coefficients[2]) +
  annotate("text", x = 12, y = 3, label = paste("correlation = ", cor2))

#plot3
plot3 <- ggplot(data, aes(x=x3, y=y3)) + geom_point(size=circle.size, pch=21, fill=colors[[3]]) +
  geom_abline(intercept=line3$coefficients[1], slope=line3$coefficients[2]) +
  annotate("text", x = 12, y = 6, label = paste("correlation = ", cor3))

#plot4
plot4 <- ggplot(data, aes(x=x4, y=y4)) + geom_point(size=circle.size, pch=21, fill=colors[[4]]) +
  geom_abline(intercept=line4$coefficients[1], slope=line4$coefficients[2]) +
  annotate("text", x = 15, y = 6, label = paste("correlation = ", cor4))

library(gridExtra)
grid.arrange(plot1, plot2, plot3, plot4, top='data Quadrant -- Correlation Demostration')