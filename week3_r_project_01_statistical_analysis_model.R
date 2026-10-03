# Week 3 - Statistical Analysis and Predictive Modeling using R
library(readr)
library(dplyr)
library(caret)
library(pROC)
library(car)

df <- read_csv("data/titanic.csv", show_col_types = FALSE) %>%
  mutate(
    survived = factor(survived, levels=c(0,1), labels=c("No","Yes")),
    pclass=factor(pclass), sex=factor(sex), embarked=factor(embarked)
  )

str(df); summary(df)

# Hypothesis testing
chisq.test(table(df$sex, df$survived))
t.test(age ~ survived, data=df)
shapiro.test(na.omit(df$age))
cor.test(df$age,df$fare,method="pearson",use="complete.obs")
cor.test(df$age,df$fare,method="spearman",exact=FALSE)

# 5-fold stratified cross-validation
set.seed(42)
ctrl <- trainControl(method="cv",number=5,classProbs=TRUE,
                     savePredictions="final",summaryFunction=twoClassSummary)
df$survived <- relevel(df$survived, ref="Yes")

model <- train(
  survived ~ pclass + sex + age + sibsp + parch + fare + embarked,
  data=df, method="glm", family=binomial, metric="ROC",
  trControl=ctrl, preProcess=c("medianImpute","center","scale")
)
print(model); print(model$results)

pred <- model$pred
confusionMatrix(pred$pred,pred$obs,positive="Yes")
roc_obj <- roc(pred$obs,pred$Yes,levels=c("No","Yes"),direction="<")
plot(roc_obj); auc(roc_obj)

# Final diagnostics
final_glm <- glm(survived ~ pclass+sex+age+sibsp+parch+fare+embarked,
                 data=df,family=binomial,na.action=na.exclude)
summary(final_glm)
par(mfrow=c(2,2)); plot(final_glm)
vif(final_glm)
exp(coef(final_glm))
