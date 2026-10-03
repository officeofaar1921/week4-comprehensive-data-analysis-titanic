# Key descriptive statistics used in the report
library(tidyverse)

titanic <- read.csv("data/titanic.csv", stringsAsFactors = FALSE)
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)
titanic$Embarked[is.na(titanic$Embarked)] <- names(sort(table(titanic$Embarked), decreasing = TRUE))[1]

cat("Rows:", nrow(titanic), "\n")
cat("Columns:", ncol(titanic), "\n")
cat("Overall survival rate:", mean(titanic$Survived), "\n")
cat("Female survival rate:", mean(titanic$Survived[titanic$Sex == 'female']), "\n")
cat("Male survival rate:", mean(titanic$Survived[titanic$Sex == 'male']), "\n")
cat("Median age:", median(titanic$Age), "\n")
cat("Median fare:", median(titanic$Fare), "\n")
