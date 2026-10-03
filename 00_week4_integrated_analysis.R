# Week 4 - Comprehensive Data Analysis Integration
# This script provides a single reproducible entry point for the project.
# Run the Week 1-3 scripts first, or use the individual scripts in scripts/.

library(readr)
library(dplyr)
library(ggplot2)
library(caret)
library(pROC)

df <- read_csv("data/titanic.csv", show_col_types = FALSE)

# Overview
str(df)
summary(df)
colSums(is.na(df))

# Cleaning example
df_clean <- df %>%
  mutate(
    survived = factor(survived, levels=c(0,1), labels=c("No","Yes")),
    pclass = factor(pclass),
    sex = factor(sex),
    embarked = factor(embarked)
  )

# Visualization
ggplot(df_clean, aes(x=sex, fill=survived)) +
  geom_bar(position="fill") +
  labs(title="Survival Proportion by Sex", y="Proportion") +
  theme_minimal()

# Predictive modeling with 5-fold cross-validation
set.seed(42)
ctrl <- trainControl(method="cv", number=5, classProbs=TRUE,
                     savePredictions="final",
                     summaryFunction=twoClassSummary)

df_clean$survived <- relevel(df_clean$survived, ref="Yes")

model <- train(
  survived ~ pclass + sex + age + sibsp + parch + fare + embarked,
  data=df_clean, method="glm", family=binomial, metric="ROC",
  trControl=ctrl,
  preProcess=c("medianImpute","center","scale")
)

print(model)
confusionMatrix(model$pred$pred, model$pred$obs, positive="Yes")
