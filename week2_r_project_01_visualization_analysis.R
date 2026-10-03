# Week 2 Task: Data Visualization and Insight Communication using R
# Dataset: Titanic (publicly available dataset)
# Main library: ggplot2

# install.packages(c("tidyverse", "scales"))  # run once if needed
library(tidyverse)
library(scales)

# 1. Load data
file_path <- "data/titanic.csv"
titanic <- read.csv(file_path, stringsAsFactors = FALSE)

# 2. Basic preparation
# Use median age and most frequent embarkation port for visualization completeness.
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)
mode_embarked <- names(sort(table(titanic$Embarked), decreasing = TRUE))[1]
titanic$Embarked[is.na(titanic$Embarked)] <- mode_embarked

titanic$Sex <- factor(titanic$Sex)
titanic$Pclass <- factor(titanic$Pclass, levels = c(1, 2, 3))

# 3. Bar chart: survival rate by sex
survival_sex <- titanic %>%
  group_by(Sex) %>%
  summarise(Survival_Rate = mean(Survived), .groups = "drop")

ggplot(survival_sex, aes(x = Sex, y = Survival_Rate)) +
  geom_col() +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(title = "Survival Rate by Sex", x = "Sex", y = "Survival Rate") +
  theme_minimal()

# 4. Scatter plot: age vs fare by passenger class
ggplot(titanic, aes(x = Age, y = Fare, color = Pclass)) +
  geom_point(alpha = 0.5) +
  labs(title = "Age vs Fare by Passenger Class", x = "Age", y = "Fare", color = "Passenger Class") +
  theme_minimal()

# 5. Histogram: age distribution
ggplot(titanic, aes(x = Age)) +
  geom_histogram(bins = 20, color = "black") +
  labs(title = "Distribution of Passenger Age", x = "Age", y = "Number of Passengers") +
  theme_minimal()

# 6. Line chart: survival rate by age group
age_breaks <- c(0, 10, 20, 30, 40, 50, 60, 70, 80)
titanic$AgeGroup <- cut(titanic$Age, breaks = age_breaks, include.lowest = TRUE,
                        labels = c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80"))

age_survival <- titanic %>%
  group_by(AgeGroup) %>%
  summarise(Survival_Rate = mean(Survived), .groups = "drop")

ggplot(age_survival, aes(x = AgeGroup, y = Survival_Rate, group = 1)) +
  geom_line() +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(title = "Survival Rate Across Age Groups", x = "Age Group", y = "Survival Rate") +
  theme_minimal()

# 7. Boxplot: fare by class
ggplot(titanic, aes(x = Pclass, y = Fare)) +
  geom_boxplot() +
  labs(title = "Fare Distribution by Passenger Class", x = "Passenger Class", y = "Fare") +
  theme_minimal()

# 8. Stacked bar: survival counts by class and sex
ggplot(titanic, aes(x = interaction(Pclass, Sex), fill = factor(Survived))) +
  geom_bar() +
  labs(title = "Survival Counts by Class and Sex",
       x = "Passenger Class and Sex", fill = "Survival (0=No, 1=Yes)") +
  theme_minimal()

# 9. Save examples programmatically (optional)
ggsave("outputs/01_survival_by_sex.png", width = 7, height = 4.5, dpi = 150)

# End of script
