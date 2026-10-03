# Week 3 - Supporting visualizations
library(readr)
library(ggplot2)
df <- read_csv("data/titanic.csv", show_col_types=FALSE)

ggplot(df,aes(x=factor(survived),y=age)) +
  geom_boxplot(na.rm=TRUE) +
  labs(x="Survival (0=No, 1=Yes)",y="Age",
       title="Age Distribution by Survival Group") +
  theme_minimal()

ggplot(df,aes(x=sex,fill=factor(survived))) +
  geom_bar(position="fill") +
  labs(x="Sex",y="Proportion",fill="Survived",
       title="Survival Proportion by Sex") +
  theme_minimal()
