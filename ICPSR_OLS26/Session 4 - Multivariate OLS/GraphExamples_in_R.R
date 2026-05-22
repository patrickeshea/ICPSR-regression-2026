# =============================================================================
# Publication-Quality Regression Visualization Tutorial
# Focus: Binary vs Continuous Variables in Multivariate Models
# Author: Patrick Shea - University of Glasgow
# Course: Introduction to Linear Regression Analysis
# =============================================================================

## Set your working directory to the ICPSR_regression folder
## Change the path below to match your computer
# setwd("~/ICPSR_regression")
setwd("C:/Users/Patrick Shea/Dropbox/My Projects/Spring 2026/ICPSR/ICPSR_OLS26")

# Clear workspace and load libraries
rm(list=ls())

# Required libraries (install if needed)
library(haven)         # for reading .dta files
library(ggplot2)       # for advanced plotting
library(modelsummary)  # for coefficient plots and model tables
library(marginaleffects) # for marginal effects plots
library(RColorBrewer)  # for color palettes
library(viridis)       # for color-blind friendly palettes
library(pandoc)

# Optional libraries for enhanced plots
library(gridExtra)     # for combining plots
library(scales)        # for axis formatting

# =============================================================================
# DATA SETUP
# =============================================================================

# Load the data
women12 <- read_dta("data/women12.dta")

# Create clean variables
women12$leftright <- women12$DPI_Left
women12$partycontrol <- women12$partcont

# Create party control categories for better visualization
women12$party_control_cat <- cut(women12$partycontrol, 
                                 breaks = c(-1, 0.5, 1.5, 3), 
                                 labels = c("Low", "Medium", "High"),
                                 include.lowest = TRUE)

# Fit our multivariate model
full_model <- lm(mil_exp_pergdp2 ~ m_woman2 + m_femlead + leftright + party_control_cat, 
                 data = women12)

# Get model summary for reference
summary(full_model)

# Calculate key statistics for "holding other variables constant"
mean_women <- mean(women12$m_woman2, na.rm = TRUE)
mean_left <- mean(women12$leftright, na.rm = TRUE)
modal_party <- names(sort(table(women12$party_control_cat), decreasing = TRUE))[1]

cat("Reference values for 'holding other variables constant':\n")
cat("Mean women in parliament:", round(mean_women, 1), "%\n")
cat("Proportion left government:", round(mean_left, 2), "\n")
cat("Modal party control:", modal_party, "\n\n")

# =============================================================================
# PART 1: BINARY VARIABLE VISUALIZATION (Female Leaders)
# Publication-Quality Examples
# =============================================================================

cat("=== PART 1: BINARY VARIABLE VISUALIZATION ===\n")
cat("Visualizing Female Leaders Effect (Binary Variable)\n\n")

# -------------------------------------------------------------------------
# 1A. MANUAL APPROACH: Base R Bar Plot (Professional Style)
# -------------------------------------------------------------------------

cat("1A. Base R Bar Plot - Professional Style\n")

# Create prediction data holding other variables constant
pred_data_male <- data.frame(
  m_woman2 = mean_women,
  m_femlead = 0,  # Male leader
  leftright = mean_left,
  party_control_cat = factor(modal_party, levels = levels(women12$party_control_cat))
)

pred_data_female <- data.frame(
  m_woman2 = mean_women,
  m_femlead = 1,  # Female leader
  leftright = mean_left,
  party_control_cat = factor(modal_party, levels = levels(women12$party_control_cat))
)

# Get predictions with confidence intervals
pred_male <- predict(full_model, newdata = pred_data_male, interval = "confidence")
pred_female <- predict(full_model, newdata = pred_data_female, interval = "confidence")

# Prepare data for plotting
means <- c(pred_male[1], pred_female[1])
lower_ci <- c(pred_male[2], pred_female[2])
upper_ci <- c(pred_male[3], pred_female[3])
categories <- c("Male Leader", "Female Leader")

# Create publication-quality bar plot
pdf("binary_barplot_base.pdf", width = 8, height = 6)  # Saving pdg image for paper
par(mar = c(5, 5, 4, 2), las = 1)  #  margins and labels

bp <- barplot(means, 
              names.arg = categories,
              ylim = c(0, max(upper_ci) * 1.15),
              col = c("#2E86AB", "#A23B72"),  # Professional color scheme
              border = "white",
              main = "Military Spending by Leader Gender",
              ylab = "Military Expenditure (% of GDP)",
              cex.main = 1.3,
              cex.lab = 1.2,
              cex.names = 1.1)

# Add confidence intervals with professional styling
arrows(bp, lower_ci, bp, upper_ci, 
       angle = 90, code = 3, length = 0.08, lwd = 2, col = "black")

# Add value labels on bars
text(bp, means + 0.05, paste0(round(means, 2), "%"), 
     pos = 3, cex = 1.1, font = 2)

# Add subtitle with model information
mtext("Other variables held at sample means", 
      side = 3, line = 0.5, cex = 0.9, col = "gray40")
dev.off()  # Close PDF device

# Also display on screen - GRAYSCALE VERSION
par(mar = c(5, 5, 4, 2), las = 1)
bp <- barplot(means, names.arg = categories, ylim = c(0, max(upper_ci) * 1.15),
              col = c("gray70", "gray30"), border = "black",  # Grayscale colors
              main = "Military Spending by Leader Gender",
              ylab = "Military Expenditure (% of GDP)",
              cex.main = 1.3, cex.lab = 1.2, cex.names = 1.1)
arrows(bp, lower_ci, bp, upper_ci, angle = 90, code = 3, length = 0.08, lwd = 2)
# Remove the text labels that show percentages on bars
# text(bp, means + 0.05, paste0(round(means, 2), "%"), pos = 3, cex = 1.1, font = 2)
mtext("Other variables held at sample means", side = 3, line = 0.5, cex = 0.9, col = "gray40")




# -------------------------------------------------------------------------
# 1B. MANUAL APPROACH: Base R Dot Plot (Cleveland Style)
# -------------------------------------------------------------------------

cat("1B. Base R Dot Plot - Cleveland Style\n")

pdf("binary_dotplot_base.pdf", width = 8, height = 5)
par(mar = c(5, 8, 4, 2), las = 1)

# Create dot plot
plot(means, 1:2, 
     xlim = c(min(lower_ci) * 0.9, max(upper_ci) * 1.1),
     ylim = c(0.5, 2.5),
     pch = 19, cex = 2.5, 
     col = c("#2E86AB", "#A23B72"),
     yaxt = "n", 
     xlab = "Military Expenditure (% of GDP)",
     ylab = "",
     main = "Military Spending by Leader Gender",
     cex.main = 1.3, cex.lab = 1.2)

# Add confidence intervals
arrows(lower_ci, 1:2, upper_ci, 1:2, 
       angle = 90, code = 3, length = 0.05, lwd = 3, 
       col = c("#2E86AB", "#A23B72"))

# Add category labels
axis(2, at = 1:2, labels = categories, cex.axis = 1.1)

# Add vertical reference lines
abline(v = seq(floor(min(lower_ci)), ceiling(max(upper_ci)), 0.2), 
       col = "gray90", lty = 1, lwd = 0.5)

# Add value labels
text(means + 0.08, 1:2, paste0(round(means, 2), "%"), 
     pos = 4, cex = 1.1, font = 2)

# Add difference annotation
difference <- means[2] - means[1]
text(mean(means), 2.3, 
     paste("Difference:", round(difference, 2), "percentage points"),
     cex = 1.0, col = "gray40", font = 3)

dev.off()

# Display on screen
par(mar = c(5, 8, 4, 2), las = 1)
plot(means, 1:2, xlim = c(min(lower_ci) * 0.9, max(upper_ci) * 1.1), ylim = c(0.5, 2.5),
     pch = 19, cex = 2.5, col = c("gray70", "gray30"), yaxt = "n", 
     xlab = "Military Expenditure (% of GDP)", ylab = "",
     main = "Military Spending by Leader Gender", cex.main = 1.3, cex.lab = 1.2)
arrows(lower_ci, 1:2, upper_ci, 1:2, angle = 90, code = 3, length = 0.05, lwd = 3, 
       col = c("gray70", "gray30"))
axis(2, at = 1:2, labels = categories, cex.axis = 1.1)
abline(v = seq(floor(min(lower_ci)), ceiling(max(upper_ci)), 0.2), 
       col = "gray90", lty = 1, lwd = 0.5)
text(means + 0.08, 1:2, paste0(round(means, 2), "%"), pos = 4, cex = 1.1, font = 2)

##Adjust as needed (I don't like the vertical lines or the numbers in the graph; I also want the x-axis to start at zero)
par(mar = c(5, 8, 4, 2), las = 1)
plot(means, 1:2, xlim = c(0, max(upper_ci) * 1.1), ylim = c(0.5, 2.5),
     pch = 19, cex = 2.5, col = c("gray70", "gray30"), yaxt = "n", 
     xlab = "Military Expenditure (% of GDP)", ylab = "",
     main = "Military Spending by Leader Gender", cex.main = 1.3, cex.lab = 1.2)
arrows(lower_ci, 1:2, upper_ci, 1:2, angle = 90, code = 3, length = 0.05, lwd = 3, 
       col = c("gray70", "gray30"))
axis(2, at = 1:2, labels = categories, cex.axis = 1.1)




# -------------------------------------------------------------------------
# 1C. GGPLOT2 APPROACH: Modern Bar Plot
# -------------------------------------------------------------------------

cat("1C. ggplot2 Bar Plot - Modern Style\n")

# Create data frame for ggplot
plot_data_binary <- data.frame(
  Leader = factor(categories, levels = categories),
  Estimate = means,
  Lower = lower_ci,
  Upper = upper_ci
)




p2 <- ggplot(plot_data_binary, aes(x = Leader, y = Estimate, fill = Leader)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                width = 0.2, linewidth = 0.8, color = "black") +
  geom_text(aes(label = paste0(round(Estimate, 2), "%")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("#2E86AB", "#A23B72")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = function(x) paste0(x, "%")) +
  labs(title = "Military Spending by Leader Gender",
       subtitle = "Error bars show 95% confidence intervals\nOther variables held at sample means",
       x = NULL,
       y = "Military Expenditure (% of GDP)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, color = "gray40"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

print(p2)
ggsave("binary_predictions_marginaleffects.pdf", p2, width = 8, height = 6, dpi = 300)


# Create modern ggplot bar chart
p1 <- ggplot(plot_data_binary, aes(x = Leader, y = Estimate, fill = Leader)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                width = 0.2, linewidth = 0.8, color = "black") +
  scale_fill_manual(values = c("gray70", "gray30")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = function(x) paste0(x, "%")) +
  labs(title = "Military Spending by Leader Gender",
       subtitle = "Error bars show 95% confidence intervals\nOther variables held at sample means",
       x = NULL,
       y = "Military Expenditure (% of GDP)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", 
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, color = "gray40"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

print(p1)
ggsave("binary_predictions_marginaleffects.pdf", p1, width = 8, height = 6, dpi = 300)

# -------------------------------------------------------------------------
# 3D. marginaleffects with by group
# -------------------------------------------------------------------------

cat("3D. marginaleffects with multiple conditions\n")

# Create predictions plot with two conditions
p8 <- plot_predictions(full_model, 
                       condition = c("m_woman2", "m_femlead")) +
  labs(title = "Military Spending: Women's Representation by Leader Gender",
       subtitle = "Interaction between continuous and binary variables",
       x = "Women in Parliament (%)", 
       y = "Predicted Military Expenditure (% of GDP)",
       color = "Leader Gender") +
  scale_color_manual(values = c("#2E86AB", "#A23B72"),
                     labels = c("Male Leader", "Female Leader")) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, color = "gray40"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.position = "bottom")

print(p8)
ggsave("marginaleffects_by_group.pdf", p8, width = 10, height = 7, dpi = 300)



# -------------------------------------------------------------------------
# 1D. GGPLOT2 APPROACH: Coefficient Plot Style
# -------------------------------------------------------------------------

cat("1D. ggplot2 Coefficient Plot Style\n")

# Create coefficient-style plot
p3 <- ggplot(plot_data_binary, aes(x = Estimate, y = Leader, color = Leader)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), 
                 height = 0.1, size = 1.2) +
  geom_text(aes(label = paste0(round(Estimate, 2), "%")), 
            hjust = -0.3, size = 4, fontface = "bold") +
  scale_color_manual(values = c("#2E86AB", "#A23B72")) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Military Spending by Leader Gender",
       subtitle = "Point estimates with 95% confidence intervals",
       x = "Military Expenditure (% of GDP)",
       y = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray40"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())

print(p3)
ggsave("binary_coefplot_ggplot.pdf", p3, width = 8, height = 5, dpi = 300)

# =============================================================================
# PART 2: CONTINUOUS VARIABLE VISUALIZATION (Women in Parliament)
# Publication-Quality Examples
# =============================================================================

cat("\n=== PART 2: CONTINUOUS VARIABLE VISUALIZATION ===\n")
cat("Visualizing Women in Parliament Effect (Continuous Variable)\n\n")

# -------------------------------------------------------------------------
# 2A. MANUAL APPROACH: Base R Line Plot with Confidence Band
# -------------------------------------------------------------------------

cat("2A. Base R Line Plot with Confidence Band\n")

# Create sequence of women's representation values
women_seq <- seq(min(women12$m_woman2, na.rm = TRUE), 
                 max(women12$m_woman2, na.rm = TRUE), 
                 length.out = 100)

# Create prediction data
pred_data_women <- data.frame(
  m_woman2 = women_seq,
  m_femlead = round(median(women12$m_femlead, na.rm = TRUE)),
  leftright = round(mean(women12$leftright, na.rm = TRUE)),
  party_control_cat = factor(rep(modal_party, length(women_seq)), 
                             levels = levels(women12$party_control_cat))
)

# Get predictions with confidence intervals
pred_women <- predict(full_model, newdata = pred_data_women, interval = "confidence")

# Create publication-quality line plot
pdf("continuous_lineplot_base.pdf", width = 10, height = 7)
par(mar = c(5, 5, 4, 2), las = 1)

# Set up plot area
plot(women_seq, pred_women[, "fit"], 
     type = "n",
     xlim = range(women_seq),
     ylim = range(pred_women),
     xlab = "Women in Parliament (%)",
     ylab = "Predicted Military Expenditure (% of GDP)",
     main = "Effect of Women's Parliamentary Representation on Military Spending",
     cex.main = 1.2, cex.lab = 1.1)

# Add confidence band
polygon(c(women_seq, rev(women_seq)), 
        c(pred_women[, "lwr"], rev(pred_women[, "upr"])),
        col = adjustcolor("#2E86AB", alpha = 0.3), border = NA)

# Add main prediction line
lines(women_seq, pred_women[, "fit"], 
      lwd = 3, col = "#2E86AB")

# Add data points with transparency
points(women12$m_woman2, women12$mil_exp_pergdp2, 
       pch = 16, col = adjustcolor("black", alpha = 0.4), cex = 0.8)

# Add grid
grid(col = "gray90", lty = 1, lwd = 0.5)

# Add informative subtitle
mtext(paste("95% confidence interval shown in shaded area",
            "\nOther variables held constant at observed values"), 
      side = 3, line = 0.5, cex = 0.9, col = "gray40")

dev.off()

# Display on screen
par(mar = c(5, 5, 4, 2), las = 1)
plot(women_seq, pred_women[, "fit"], type = "n", xlim = range(women_seq), ylim = range(pred_women),
     xlab = "Women in Parliament (%)", ylab = "Predicted Military Expenditure (% of GDP)",
     main = "Effect of Women's Parliamentary Representation", cex.main = 1.2, cex.lab = 1.1)
polygon(c(women_seq, rev(women_seq)), c(pred_women[, "lwr"], rev(pred_women[, "upr"])),
        col = adjustcolor("#2E86AB", alpha = 0.3), border = NA)
lines(women_seq, pred_women[, "fit"], lwd = 3, col = "#2E86AB")
points(women12$m_woman2, women12$mil_exp_pergdp2, 
       pch = 16, col = adjustcolor("black", alpha = 0.4), cex = 0.8)
grid(col = "gray90", lty = 1, lwd = 0.5)

# -------------------------------------------------------------------------
# 2B. MANUAL APPROACH: Base R with Rug Plot and Enhanced Styling
# -------------------------------------------------------------------------

cat("2B. Base R Enhanced Line Plot with Rug\n")

pdf("continuous_enhanced_base.pdf", width = 10, height = 7)
par(mar = c(6, 5, 4, 2), las = 1)

# Main plot
plot(women_seq, pred_women[, "fit"], 
     type = "n",
     xlim = range(women_seq),
     ylim = range(pred_women),
     xlab = "",
     ylab = "Predicted Military Expenditure (% of GDP)",
     main = "Women's Parliamentary Representation and Military Spending",
     cex.main = 1.3, cex.lab = 1.2)

# Add confidence band with gradient effect
polygon(c(women_seq, rev(women_seq)), 
        c(pred_women[, "lwr"], rev(pred_women[, "upr"])),
        col = adjustcolor("#2E86AB", alpha = 0.25), border = NA)

# Add main line
lines(women_seq, pred_women[, "fit"], 
      lwd = 4, col = "#2E86AB")

# Add confidence interval boundaries
lines(women_seq, pred_women[, "lwr"], 
      lwd = 1.5, col = "#2E86AB", lty = 2)
lines(women_seq, pred_women[, "upr"], 
      lwd = 1.5, col = "#2E86AB", lty = 2)

# Add rug plot for actual data distribution
rug(women12$m_woman2, col = "gray30", lwd = 1.5)

# Add x-axis label with more space
mtext("Women in Parliament (%)", side = 1, line = 3, cex = 1.2)
mtext("Distribution of actual values shown below", side = 1, line = 4.5, cex = 0.9, col = "gray40")

# Add effect size annotation
coef_women <- coef(full_model)["m_woman2"]
text(max(women_seq) * 0.7, max(pred_women) * 0.9,
     paste("Slope:", round(coef_women, 3), "percentage points per 1% increase"),
     cex = 1.0, col = "gray20", font = 3,
     adj = 0, 
     box(col = "white", lwd = 2))

dev.off()

# Display on screen (simplified version)
par(mar = c(6, 5, 4, 2), las = 1)
plot(women_seq, pred_women[, "fit"], type = "n", xlim = range(women_seq), ylim = range(pred_women),
     xlab = "", ylab = "Predicted Military Expenditure (% of GDP)",
     main = "Women's Parliamentary Representation and Military Spending", cex.main = 1.3, cex.lab = 1.2)
polygon(c(women_seq, rev(women_seq)), c(pred_women[, "lwr"], rev(pred_women[, "upr"])),
        col = adjustcolor("#2E86AB", alpha = 0.25), border = NA)
lines(women_seq, pred_women[, "fit"], lwd = 4, col = "#2E86AB")
rug(women12$m_woman2, col = "gray30", lwd = 1.5)
mtext("Women in Parliament (%)", side = 1, line = 3, cex = 1.2)

# -------------------------------------------------------------------------
# 2C. GGPLOT2 APPROACH: Modern Line Plot
# -------------------------------------------------------------------------

cat("2C. ggplot2 Modern Line Plot\n")

# Create data frame for ggplot
pred_df <- data.frame(
  women_pct = women_seq,
  fit = pred_women[, "fit"],
  lwr = pred_women[, "lwr"],
  upr = pred_women[, "upr"]
)

# Create modern ggplot
p4 <- ggplot(pred_df, aes(x = women_pct, y = fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), 
              alpha = 0.3, fill = "#2E86AB") +
  geom_line(color = "#2E86AB", linewidth = 1.5) +  
  geom_point(data = women12, 
             aes(x = m_woman2, y = mil_exp_pergdp2), 
             alpha = 0.5, color = "gray30", size = 1.5) + 
  geom_rug(data = women12, 
           aes(x = m_woman2), 
           alpha = 0.7, color = "gray40", linewidth = 0.8,
           inherit.aes = FALSE) +  # Added inherit.aes = FALSE
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(labels = function(y) paste0(y, "%"),
                     expand = expansion(mult = c(0.05, 0.05))) +
  labs(title = "Women's Parliamentary Representation and Military Spending",
       subtitle = paste("Shaded area shows 95% confidence interval",
                        "\nPoints show actual observations, rug shows distribution of women's representation"),
       x = "Women in Parliament (%)",
       y = "Predicted Military Expenditure (% of GDP)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray40"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90", linewidth = 0.5))  

print(p4)
ggsave("continuous_modern_ggplot.pdf", p4, width = 10, height = 7, dpi = 300)





# =============================================================================
# PART 3: SPECIALIZED LIBRARIES (modelsummary and marginaleffects)
# =============================================================================

cat("\n=== PART 3: SPECIALIZED LIBRARIES ===\n")
cat("Using modelsummary and marginaleffects for professional plots\n\n")

# -------------------------------------------------------------------------
# 3A. modelsummary coefficient plot
# -------------------------------------------------------------------------

cat("3A. modelsummary coefficient plot\n")

# Create professional coefficient plot
p5 <- modelplot(full_model, 
                coef_omit = "Intercept",
                coef_rename = c("m_woman2" = "Women in Parliament (%)",
                                "m_femlead" = "Female Leader",
                                "leftright" = "Left Government",
                                "party_control_catMedium" = "Medium Party Control",
                                "party_control_catHigh" = "High Party Control")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
  labs(title = "Regression Coefficients: Military Spending Model",
       subtitle = "Point estimates with 95% confidence intervals",
       x = "Coefficient Estimate (Percentage Points of GDP)",
       caption = "Reference categories: Male Leader, Non-Left Government, Low Party Control") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, color = "gray40"),
        plot.caption = element_text(size = 9, color = "gray50"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

print(p5)
ggsave("coefficients_modelsummary.pdf", p5, width = 10, height = 6, dpi = 300)

# -------------------------------------------------------------------------
# 3B. marginaleffects predictions plot
# -------------------------------------------------------------------------

cat("3B. marginaleffects predictions plot\n")

# Create predictions plot for continuous variable
p6 <- plot_predictions(full_model, 
                       condition = "m_woman2",
                       points = 0.5) +  # Add some data points
  labs(title = "Predicted Military Spending by Women's Parliamentary Representation",
       subtitle = "Other variables held at observed values",
       x = "Women in Parliament (%)", 
       y = "Predicted Military Expenditure (% of GDP)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, color = "gray40"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

print(p6)
ggsave("predictions_marginaleffects.pdf", p6, width = 10, height = 6, dpi = 300)

# -------------------------------------------------------------------------
# 3C. marginaleffects predictions for binary variable
# -------------------------------------------------------------------------

p8 <- plot_predictions(full_model, 
                       condition = "m_femlead",
                       points = 0.3) +
  scale_x_discrete(labels = c("Male Leader", "Female Leader")) +  # Changed to discrete
  labs(title = "Predicted Military Spending by Leader Gender",
       subtitle = "Other variables held at observed values",
       x = "Leader Gender", 
       y = "Predicted Military Expenditure (% of GDP)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, color = "gray40"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

print(p8)
ggsave("binary_predictions_marginaleffects.pdf", p8, width = 8, height = 6, dpi = 300)


# =============================================================================
# ADDITIONAL MODELSUMMARY EXAMPLES
# =============================================================================

cat("=== ADDITIONAL MODELSUMMARY EXAMPLES ===\n")

# -------------------------------------------------------------------------
# Modelsummary Example 1: Basic coefficient plot (grayscale)
# -------------------------------------------------------------------------

cat("Modelsummary Example 1: Basic coefficient plot\n")

p_coef1 <- modelplot(full_model, 
                     coef_omit = "Intercept") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.7) +
  labs(title = "Regression Coefficients",
       x = "Coefficient Estimate (Percentage Points of GDP)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(size = 14, face = "bold"))

print(p_coef1)

# -------------------------------------------------------------------------
# Modelsummary Example 2: Coefficient plot with renamed variables (grayscale)
# -------------------------------------------------------------------------

cat("Modelsummary Example 2: Renamed coefficients\n")

p_coef2 <- modelplot(full_model, 
                     coef_omit = "Intercept",
                     coef_rename = c("m_woman2" = "Women in Parliament (%)",
                                     "m_femlead" = "Female Leader",
                                     "leftright" = "Left Government",
                                     "party_control_catMedium" = "Medium Party Control",
                                     "party_control_catHigh" = "High Party Control")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.7) +
  labs(title = "Military Spending Determinants",
       subtitle = "Point estimates with 95% confidence intervals",
       x = "Effect on Military Expenditure (% of GDP)",
       caption = "Reference: Male Leader, Non-Left Government, Low Party Control") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, color = "gray40"),
        plot.caption = element_text(size = 9, color = "gray50"),
        axis.text.y = element_text(size = 10))

print(p_coef2)
ggsave("modelsummary_renamed_coefs.pdf", p_coef2, width = 10, height = 6, dpi = 300)

# -------------------------------------------------------------------------
# Modelsummary Example 3: Multiple models comparison
# -------------------------------------------------------------------------

cat("Modelsummary Example 3: Multiple models\n")

# Create different model specifications
model_simple <- lm(mil_exp_pergdp2 ~ m_woman2, data = women12)
model_gender <- lm(mil_exp_pergdp2 ~ m_woman2 + m_femlead, data = women12)
model_ideology <- lm(mil_exp_pergdp2 ~ m_woman2 + m_femlead + leftright, data = women12)

models_list <- list(
  "Simple" = model_simple,
  "Add Gender" = model_gender,  
  "Add Ideology" = model_ideology,
  "Full Model" = full_model
)

p_multi <- modelplot(models_list,
                     coef_omit = "Intercept",
                     coef_rename = c("m_woman2" = "Women in Parliament",
                                     "m_femlead" = "Female Leader",
                                     "leftright" = "Left Government")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.7) +
  labs(title = "Model Comparison: Progressive Controls",
       subtitle = "How coefficients change as we add control variables",
       x = "Coefficient Estimate") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray40"),
        legend.position = "bottom")

print(p_multi)
ggsave("modelsummary_multiple_models.pdf", p_multi, width = 12, height = 8, dpi = 300)

# -------------------------------------------------------------------------
# Modelsummary Example 4: Publication table
# -------------------------------------------------------------------------

cat("Modelsummary Example 4: Publication table\n")

# Create a professional regression table
modelsummary(models_list,
             output = "regression_table.docx",  # Word document
             stars = TRUE,
             coef_rename = c("m_woman2" = "Women in Parliament (%)",
                             "m_femlead" = "Female Leader",
                             "leftright" = "Left Government",
                             "party_control_catMedium" = "Medium Party Control",
                             "party_control_catHigh" = "High Party Control"),
             title = "Military Expenditure Regression Results",
             notes = "Standard errors in parentheses. Reference categories: Male Leader, Non-Left Government, Low Party Control.")

# Also create HTML version
modelsummary(models_list,
             output = "regression_table.html",
             stars = TRUE,
             coef_rename = c("m_woman2" = "Women in Parliament (%)",
                             "m_femlead" = "Female Leader", 
                             "leftright" = "Left Government",
                             "party_control_catMedium" = "Medium Party Control",
                             "party_control_catHigh" = "High Party Control"),
             title = "Military Expenditure Regression Results")

# Display in console
modelsummary(models_list,
             stars = TRUE,
             coef_rename = c("m_woman2" = "Women in Parliament (%)",
                             "m_femlead" = "Female Leader",
                             "leftright" = "Left Government"))

# -------------------------------------------------------------------------
# Modelsummary Example 5: Custom styling and statistics
# -------------------------------------------------------------------------

cat("Modelsummary Example 5: Custom statistics\n")

# Table with custom statistics
modelsummary(full_model,
             stars = TRUE,
             statistic = c("std.error", "p.value"),
             coef_rename = c("m_woman2" = "Women in Parliament (%)",
                             "m_femlead" = "Female Leader",
                             "leftright" = "Left Government",
                             "party_control_catMedium" = "Medium Party Control", 
                             "party_control_catHigh" = "High Party Control"),
             gof_map = c("nobs", "r.squared", "adj.r.squared", "rmse"),
             title = "Detailed Regression Results",
             notes = "Standard errors and p-values reported.")
