# Week 1 - Data Cleaning and Preliminary Analysis with R
# Titanic passenger dataset

packages <- c("readr", "dplyr", "tidyr", "ggplot2", "corrplot")
for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

df <- read_csv("data/titanic.csv", show_col_types = FALSE)

# 1. Initial inspection
print(dim(df))
print(names(df))
str(df)
summary(df)

# 2. Missing values
missing <- sapply(df, function(x) sum(is.na(x)))
missing_pct <- round(100 * missing / nrow(df), 2)
missing_report <- data.frame(variable = names(missing), missing = as.integer(missing),
                              percent = missing_pct)
print(missing_report[missing_report$missing > 0, ])

# 3. Duplicate check
cat("Duplicate rows:", sum(duplicated(df)), "\n")

# 4. Cleaning
mode_value <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

clean <- df %>%
  mutate(
    age = ifelse(is.na(age), median(age, na.rm = TRUE), age),
    embarked = ifelse(is.na(embarked), mode_value(embarked), embarked)
  ) %>%
  select(-deck) %>%
  distinct()

# 5. Outlier detection using IQR
outlier_count <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  sum(x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr), na.rm = TRUE)
}
cat("Age IQR outliers:", outlier_count(clean$age), "\n")
cat("Fare IQR outliers:", outlier_count(clean$fare), "\n")

# 6. Normalization
clean <- clean %>%
  mutate(
    Age_Z = as.numeric(scale(age)),
    Fare_Z = as.numeric(scale(fare)),
    Sex_Encoded = ifelse(sex == "male", 1, 0),
    Embarked_C = ifelse(embarked == "C", 1, 0),
    Embarked_Q = ifelse(embarked == "Q", 1, 0),
    Embarked_S = ifelse(embarked == "S", 1, 0)
  )

# 7. Preliminary descriptive analysis
cat("Overall survival rate:", round(mean(clean$survived) * 100, 2), "%\n")
print(clean %>% group_by(sex) %>% summarise(
  passengers = n(),
  survivors = sum(survived),
  survival_rate = round(mean(survived) * 100, 2)
))

num_df <- clean %>% select(survived, pclass, age, sibsp, parch, fare)
cor_mat <- cor(num_df, use = "pairwise.complete.obs")
print(round(cor_mat, 3))

# 8. Visualizations
p1 <- ggplot(missing_report[missing_report$missing > 0, ],
             aes(x = reorder(variable, missing), y = missing)) +
  geom_col() + coord_flip() +
  labs(title = "Missing Values by Variable", x = NULL, y = "Missing count") +
  theme_minimal()
ggsave("outputs/missing_values.png", p1, width = 7, height = 4, dpi = 150)

surv_sex <- clean %>% group_by(sex) %>% summarise(rate = mean(survived) * 100)
p2 <- ggplot(surv_sex, aes(x = sex, y = rate)) +
  geom_col() + ylim(0, 100) +
  labs(title = "Survival Rate by Sex", x = "Sex", y = "Survival rate (%)") +
  theme_minimal()
ggsave("outputs/survival_by_sex.png", p2, width = 7, height = 4, dpi = 150)

png("outputs/correlation_heatmap.png", width = 900, height = 700, res = 120)
corrplot(cor_mat, method = "color", type = "upper", addCoef.col = "black", tl.col = "black")
dev.off()

write_csv(clean, "outputs/titanic_cleaned.csv")
cat("Analysis complete. Outputs saved in outputs/.\n")
