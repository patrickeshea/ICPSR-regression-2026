## Set your working directory to the ICPSR_regression folder
## Change the path below to match your computer
# setwd("~/ICPSR_regression")
setwd("C:/Users/Patrick Shea/Dropbox/My Projects/Spring 2026/ICPSR/ICPSR_OLS26")

# ============================================================
# Part 1: The Conditional Expectation Function (CEF)
# ============================================================
# Regression estimates the CEF:  E[Y | X = x]
# "How does the average of Y change as X changes?"
# We build intuition by starting simple and adding complexity.

data(mtcars)
head(mtcars)


# --- 1a: CEF with a Binary X --------------------------------
# Y = mpg,  X = am (0 = automatic, 1 = manual)
# With only two values of X, the CEF is just two group means:
#   E[mpg | am = 0]   and   E[mpg | am = 1]

mean_auto   <- mean(mtcars$mpg[mtcars$am == 0])
mean_manual <- mean(mtcars$mpg[mtcars$am == 1])

mean_auto
mean_manual

stripchart(mpg ~ am, data = mtcars, vertical = TRUE, pch = 1,
           group.names = c("Automatic", "Manual"),
           xlab = "Transmission", ylab = "Miles per Gallon",
           main = "CEF with Binary X: E[MPG | Transmission]")
points(1:2, c(mean_auto, mean_manual), pch = 19, col = "red", cex = 2)
legend("topleft", legend = "Conditional mean (CEF)", pch = 19, col = "red")


# --- 1b: CEF with a Discrete X ------------------------------
# Y = mpg,  X = cyl (4, 6, or 8 cylinders)
# More values of X, but still a small number.
# The CEF is a conditional mean at each value of X.

mean_by_cyl <- tapply(mtcars$mpg, mtcars$cyl, mean)
mean_by_cyl

stripchart(mpg ~ cyl, data = mtcars, vertical = TRUE, pch = 1,
           xlab = "Cylinders", ylab = "Miles per Gallon",
           main = "CEF with Discrete X: E[MPG | Cylinders]")
points(1:3, mean_by_cyl, pch = 19, col = "red", cex = 2)
lines(1:3, mean_by_cyl, col = "red", lwd = 2)

# The mean of Y shifts as X changes — that pattern IS the CEF.


# --- 1c: CEF with a Continuous X -----------------------------
# Y = mpg,  X = wt (weight, 1000 lbs) — many possible values.
# We can't list a conditional mean for every value of X.
# We need a *function* to summarize how E[Y|X] changes with X.

# Approximate the CEF by binning X and computing means per bin
mtcars$wt_bin <- cut(mtcars$wt, breaks = 5)
bin_means     <- tapply(mtcars$mpg, mtcars$wt_bin, mean)
bin_mids      <- tapply(mtcars$wt,  mtcars$wt_bin, mean)

plot(mtcars$wt, mtcars$mpg, pch = 1, col = "grey50",
     xlab = "Weight (1000 lbs)", ylab = "Miles per Gallon",
     main = "CEF with Continuous X: Binned Means → Linear Fit")
points(bin_mids, bin_means, pch = 19, col = "red", cex = 2)
lines(bin_mids, bin_means, col = "red", lwd = 2)

# A line is a natural (and simple) summary: E[Y|X=x] ≈ α + βx
abline(lm(mpg ~ wt, data = mtcars), col = "blue", lwd = 2)
legend("topright",
       legend = c("Binned conditional means", "OLS line"),
       col = c("red", "blue"), lwd = 2, pch = c(19, NA))

##############################################################################

##############################################################################


# The OLS line approximates the CEF.
# Next question: how do we find this "best" line?

# Part 2: Understanding Covariance and Correlation
# Calculate covariance
cov_wt_mpg <- cov(mtcars$wt, mtcars$mpg)
# Calculate correlation
cor_wt_mpg <- cor(mtcars$wt, mtcars$mpg)

# Demonstrate relationship between correlation and covariance
# cor = cov/(sd_x * sd_y)
manual_cor <- cov_wt_mpg / (sd(mtcars$wt) * sd(mtcars$mpg))
print(paste("Manual correlation:", manual_cor))
print(paste("R correlation:", cor_wt_mpg))

# Part 3: Fitting OLS Line
# Fit the model
model <- lm(mpg ~ wt, data = mtcars)

# Look at model summary
summary(model)

# Visualize the fitted line
plot(mtcars$wt, mtcars$mpg, 
     main="OLS Fitted Line",
     xlab="Weight (1000s of lbs)", 
     ylab="Miles per Gallon",
     pch=19)
abline(model, col="red", lwd=2)

# Part 4: Understanding Residuals
# Calculate fitted values and residuals
fitted_values <- fitted(model)
residuals <- residuals(model)

# Plot residuals
plot(fitted_values, residuals,
     main="Residual Plot",
     xlab="Fitted Values",
     ylab="Residuals",
     pch=19)
abline(h=0, col="red", lwd=2)

# Part 5: Decomposition of Variance (R-squared)
# Total Sum of Squares (TSS)
tss <- sum((mtcars$mpg - mean(mtcars$mpg))^2)
# Residual Sum of Squares (RSS)
rss <- sum(residuals^2)
# Explained Sum of Squares (ESS)
ess <- sum((fitted_values - mean(mtcars$mpg))^2)

# Calculate R-squared manually
r_squared_manual <- 1 - (rss/tss)
print(paste("Manual R-squared:", r_squared_manual))
print(paste("R summary R-squared:", summary(model)$r.squared))





##Galton's data on Parent/Child Height

library(UsingR)
data(galton)

##Descriptive look at data
plot(galton$child, galton$parent)

scatter.smooth(galton$parent, galton$child,
               xlab = "Parent's Height", ylab = "Child's Height",
               main = "Relationship between Child's Height and Parent's Height")


### Double check OLS calculations using R

y <- galton$child
x <- galton$parent
beta1 <- cor(y, x) *  sd(y) / sd(x)


beta0 <- mean(y) - beta1 * mean(x)
rbind(c(beta0, beta1), coef(lm(y ~ x)))

beta1alt <- cov(y, x)  / var(x)
beta1alt

beta1alt2 <- sum((y-mean(y))*(x-mean(x))) / sum((x-mean(x))^2)
beta1alt2

## Revisiting Galton's data
### Normalizing variables results in the slope being the correlation

yn <- (y - mean(y))/sd(y)
mean(y)
mean(yn)
xn <- (x - mean(x))/sd(x)
c(cor(y, x), cor(yn, xn), coef(lm(yn ~ xn))[2])


summary(x)

##What is the expect height of a child if parent is 64,68, or 72inches tall?

fit<-lm(y ~ x)
coef(fit)

##Predictions
newx <- c(64,68,72)
coef(fit)[1] + coef(fit)[2] * newx


##Relate the regression line to the predictions
plot(galton$parent,galton$child,  
     xlab = "Parent's height", 
     ylab = "Child's Height", 
     bg = "lightblue", 
     col = "black", cex = 1.1, pch = 21,frame = FALSE)
abline(fit, lwd = 2)
