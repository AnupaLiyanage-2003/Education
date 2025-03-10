# Load required libraries
library(readxl)
library(ggplot2)

# Load the dataset
data <- read_excel("C:\\Users\\anupa\\OneDrive\\Desktop\\Prestige_New.xlsx")

# Ensure income, education, and prestige have valid values (remove rows with missing or non-positive income)
clean_data <- subset(data, !is.na(income) & income > 0 & !is.na(prestige) & !is.na(education))

# 1. Central Tendency and Summary Statistics for Income
min_income <- min(clean_data$income, na.rm = TRUE)  # Handle missing values
max_income <- max(clean_data$income, na.rm = TRUE)
mean_income <- mean(clean_data$income, na.rm = TRUE)
median_income <- median(clean_data$income, na.rm = TRUE)

# Mode Calculation
mode_income <- as.numeric(names(sort(table(clean_data$income), decreasing = TRUE)[1]))

# Print Statistics
cat("Income Summary Statistics:\n")
cat("Min Income:", min_income, "\n")
cat("Max Income:", max_income, "\n")
cat("Mean Income:", mean_income, "\n")
cat("Median Income:", median_income, "\n")
cat("Mode Income:", mode_income, "\n")

# 2. Log Transformation of Income
clean_data$log_income <- log(clean_data$income)

# Central Tendency and Standard Deviation for Log-Income
mean_log_income <- mean(clean_data$log_income, na.rm = TRUE)
median_log_income <- median(clean_data$log_income, na.rm = TRUE)
sd_log_income <- sd(clean_data$log_income, na.rm = TRUE)

# 3. Bell Curve Plot Function
plot_bell_curve <- function(data, variable, mean_val, sd_val, color, fill_color, title, x_label) {
  ggplot(data, aes_string(x = variable)) +
    stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val), color = color, size = 1) +
    geom_area(stat = "function", fun = dnorm, args = list(mean = mean_val, sd = sd_val), fill = fill_color, alpha = 0.3) +
    geom_vline(aes(xintercept = mean_val), color = "red", linetype = "dashed") +
    annotate("text", x = mean_val, y = 0.02, label = paste("Mean =", round(mean_val, 2)), color = "red") +
    geom_vline(aes(xintercept = median(data[[variable]], na.rm = TRUE)), color = "black", linetype = "dashed") +
    annotate("text", x = median(data[[variable]], na.rm = TRUE), y = 0.015, label = paste("Median =", round(median(data[[variable]], na.rm = TRUE), 2)), color = "black") +
    labs(title = title, x = x_label, y = "Density") +
    theme_minimal()
}

# 4. Plot the bell curve for log-income
bell_curve_log_income <- plot_bell_curve(clean_data, "log_income", mean_log_income, sd_log_income, "yellow", "yellow", "Bell Curve for Log Income", "Log(Income)")
print(bell_curve_log_income)

# 5. Central Tendency and Standard Deviation for Education and Prestige
mean_prestige <- mean(clean_data$prestige, na.rm = TRUE)
median_prestige <- median(clean_data$prestige, na.rm = TRUE)
sd_prestige <- sd(clean_data$prestige, na.rm = TRUE)

mean_education <- mean(clean_data$education, na.rm = TRUE)
median_education <- median(clean_data$education, na.rm = TRUE)
sd_education <- sd(clean_data$education, na.rm = TRUE)

# 6. Plot bell curves for Education and Prestige
bell_curve_education <- plot_bell_curve(clean_data, "education", mean_education, sd_education, "pink", "pink", "Bell Curve for Education", "Years of Education")
bell_curve_prestige <- plot_bell_curve(clean_data, "prestige", mean_prestige, sd_prestige, "blue", "blue", "Bell Curve for Prestige", "Prestige Score")

print(bell_curve_education)
print(bell_curve_prestige)

# 7. ANOVA to test differences in prestige by occupation type
anova_result <- aov(prestige ~ type, data = clean_data)
summary_anova <- summary(anova_result)
print(summary_anova)

# 8. Shapiro-Wilk test for normality on prestige
shapiro_test_prestige <- shapiro.test(clean_data$prestige)

# Shapiro-Wilk test for normality on education
shapiro_test_education <- shapiro.test(clean_data$education)

# Print the Shapiro-Wilk test results
print(shapiro_test_prestige)
print(shapiro_test_education)

# 9. Pearson correlation test (assumes normality)
correlation_test_pearson <- cor.test(clean_data$prestige, clean_data$education, method = "pearson", use = "complete.obs")

# Print the result of Pearson's correlation
print(correlation_test_pearson)

# 10. Spearman correlation test (for non-normal distributions)
correlation_test_spearman <- cor.test(clean_data$prestige, clean_data$education, method = "spearman", use = "complete.obs")

# Print the result of Spearman's correlation
print(correlation_test_spearman)

# 11. Scatterplot of Education vs Prestige with Linear Regression Line
ggplot(clean_data, aes(x = education, y = prestige)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatterplot of Prestige vs Education", x = "Education", y = "Prestige") +
  theme_minimal()
