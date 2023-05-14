# heart-attack-analysis

# Import libraries
library(xtable)
library(ggplot2)
library(skimr)
library(moments)
library(visdat)
library(GGally)
library(corrplot)
library(caret)
library(MASS)
library(pROC)
library(pracma)
library(randomForest)
library(dplyr)



set.seed(17)


# Import dataset

# Online from github repo
#df_test = read.csv('https://raw.githubusercontent.com/Giovo17/heart-attack-analysis/main/data/heart_attack_test.csv')
#df_train = read.csv('https://raw.githubusercontent.com/Giovo17/heart-attack-analysis/main/data/heart_attack_train.csv')

# Local from disk
setwd("~/Documents/University/Data\ Science/1°\ Year\ (2022-2023)/Statistical\ Learning\ (1)/Exam\ -\ Statistical\ Learning/report/heart-attack-analysis")
df_train = read.csv("data/heart_attack_train.csv")
df_test = read.csv("data/heart_attack_test.csv")

# Rename rows
row.names(df_train) = df_train$X
row.names(df_test) = df_test$X
df_train = subset(df_train, select = -X )
df_test = subset(df_test, select = -X )


head(df_train)
print(xtable::xtable(head(df_train), type="latex", digits=2))

head(df_test)
print(xtable::xtable(head(df_test), type="latex", digits=2))



str(df_train)
str(df_test)



df_train$sex = as.factor(df_train$sex)
df_train$cp = as.factor(df_train$cp)
df_train$fbs = as.factor(df_train$fbs)
df_train$restecg = as.factor(df_train$restecg)
df_train$exang = as.factor(df_train$exang)
df_train$slope = as.factor(df_train$slope)
df_train$ca = as.factor(df_train$ca)
df_train$thal = as.factor(df_train$thal)
df_train$target = as.factor(df_train$target)



levels(df_train$sex)
levels(df_train$cp)
levels(df_train$fbs)
levels(df_train$restecg)
levels(df_train$exang)
levels(df_train$slope)
levels(df_train$ca)
levels(df_train$thal)
levels(df_train$target)




### -------------------------------------------------------------------------------------------------------
### 1. Exploratory data analysis of the train dataset ###



# Check for duplicated rows
nrow(df_train)
nrow(unique(df_train))
# Removed duplicated rows
df_train = unique(df_train)


skimr::skim(df_train)
print(xtable::xtable(skimr::skim(df_train), type="latex", digits=2))





# UNIVARIATE

# Age
jpeg(file="../LateX_project/images/chapter1/age_histogram.jpeg", width=6, height=6, units='in', res=200)

ggplot(df_train, aes(x=age)) +
  geom_histogram(aes(y=after_stat(density)), binwidth=3, fill="#6b9bc3", color="#6b9bc3", alpha=0.7, position='identity') +
  theme(plot.title=element_text(size=15)) +
  ylab("density") + 
  theme_minimal() +
  theme(text = element_text(size = 20))

dev.off()


jpeg(file="../LateX_project/images/chapter1/age_histogram_hue.jpeg", width=6, height=6, units='in', res=200)

ggplot(df_train, aes(x=age, fill=target)) +
  geom_density(aes(y=after_stat(density)), color="#6b9bc3", alpha=0.7, position='identity') +
  theme(plot.title=element_text(size=15)) +
  scale_fill_manual(values=c("green", "orange")) + 
  ylab("density") + 
  labs(fill="") + 
  theme_minimal() +
  theme(text = element_text(size = 20), legend.position="none")


dev.off()



# Resting blood pressure
jpeg(file="../LateX_project/images/chapter1/trestbps_histogram.jpeg", width=6, height=6, units='in', res=200)

ggplot(df_train, aes(x=trestbps)) +
  geom_histogram(aes(y=after_stat(density)), binwidth=9, fill="#6b9bc3", color="#6b9bc3", alpha=0.7, position = 'identity') +
  geom_vline(aes(xintercept = 130), colour="red") +
  geom_rug(aes(x=trestbps, y = NULL)) +
  theme(plot.title=element_text(size=15)) +
  ylab("density") + 
  theme_minimal() +
  theme(text = element_text(size = 20))

dev.off()


jpeg(file="../LateX_project/images/chapter1/trestbps_histogram_hue.jpeg", width=6, height=6, units='in', res=200)

ggplot(df_train, aes(x=trestbps, fill=target)) + 
  geom_density(aes(y=after_stat(density)), color="#6b9bc3", alpha=0.7, position='identity') +
  geom_vline(aes(xintercept = 130), colour="red") + 
  geom_rug(aes(x=trestbps, y = NULL)) + 
  scale_fill_manual(values=c("green", "orange")) + 
  ylab("density") + 
  labs(fill="") + 
  theme_minimal() + 
  theme(text = element_text(size = 20), legend.position="none")

dev.off()

print(moments::skewness(df_train$trestbps))


# Cholesterol
jpeg(file="../LateX_project/images/chapter1/cholesterol_histogram.jpeg", width=6, height=6, units='in', res=200)

ggplot(df_train, aes(x=chol)) +
  geom_histogram(aes(y=after_stat(density)), binwidth=20, fill="#6b9bc3", color="#6b9bc3", alpha=0.7, position = 'identity') +
  geom_vline(aes(xintercept = 240), colour="red") +
  geom_rug(aes(x=chol, y = NULL)) +
  theme(plot.title=element_text(size=15)) +
  ylab("density") + 
  theme_minimal() +
  theme(text = element_text(size = 20))

dev.off()


jpeg(file="../LateX_project/images/chapter1/cholesterol_histogram_hue.jpeg", width=6, height=6, units='in', res=200)

ggplot(df_train, aes(x=chol, fill=target)) +
  geom_density(aes(y=after_stat(density)), color="#6b9bc3", alpha=0.7, position='identity') +
  geom_vline(aes(xintercept = 240), colour="red") +
  geom_rug(aes(x=chol, y = NULL)) +
  scale_fill_manual(values=c("green", "orange")) +
  ylab("density") + 
  labs(fill="") +
  theme_minimal() +
  theme(text = element_text(size = 20), legend.position="none")

dev.off()


jpeg(file="../LateX_project/images/chapter1/cholesterol_boxplot.jpeg", width=6, height=6, units='in', res=200)

ggplot(df_train, aes(y=chol)) + 
  geom_boxplot(fill="#6b9bc3", alpha=0.7) + 
  theme(text = element_text(size = 20),
    axis.text=element_text(size=6.5), 
    axis.title=element_text(size=14, face="bold")
  ) +
  theme_minimal()

dev.off()

print(boxplot.stats(df_train$chol)$out)


# Maximum heart rate achieved
jpeg(file="../LateX_project/images/chapter1/thalach_histogram.jpeg", width=6, height=6, units='in', res=200)

ggplot(df_train, aes(x=thalach)) +
  geom_histogram(aes(y=after_stat(density)), binwidth=10, fill="#6b9bc3", color="#6b9bc3", alpha=0.7, position = 'identity') +
  geom_rug(aes(x=thalach, y = NULL)) +
  theme(plot.title=element_text(size=15)) +
  ylab("density") + 
  theme_minimal() +
  theme(text = element_text(size = 20))

dev.off()


jpeg(file="../LateX_project/images/chapter1/thalach_histogram_hue.jpeg", width=6, height=6, units='in', res=200)

ggplot(df_train, aes(x=thalach, fill=target)) +
  geom_density(aes(y=after_stat(density)), color="#6b9bc3", alpha=0.7, position='identity') +
  geom_rug(aes(x=thalach, y = NULL)) +
  scale_fill_manual(values=c("green", "orange")) +
  ylab("density") + 
  labs(fill="") +
  theme_minimal() +
  theme(text = element_text(size = 20), legend.position="none")

dev.off()

print(moments::skewness(df_train$thalach))

df_train_healthy <- df_train[df_train$target == 0, ]
df_train_sick <- df_train[df_train$target == 1, ]

print(mean(df_train_healthy$thalach))
print(moments::skewness(df_train_healthy$thalach))

print(mean(df_train_sick$thalach))
print(moments::skewness(df_train_sick$thalach))




# ST depression induced by exercise relative to rest
jpeg(file="../LateX_project/images/chapter1/oldpeak_histogram.jpeg", width=6, height=6, units='in', res=200)

ggplot(df_train, aes(x=oldpeak)) +
  geom_histogram(aes(y=after_stat(density)), binwidth=0.4, fill="#6b9bc3", color="#6b9bc3", alpha=0.7, position = 'identity') +
  geom_rug(aes(x=oldpeak, y = NULL)) +
  theme(plot.title=element_text(size=15)) +
  ylab("density") + 
  theme_minimal() +
  theme(text = element_text(size = 20))

dev.off()


jpeg(file="../LateX_project/images/chapter1/oldpeak_histogram_hue.jpeg", width=6, height=6, units='in', res=200)

ggplot(df_train, aes(x=oldpeak, fill=target)) +
  geom_density(aes(y=after_stat(density)), color="#6b9bc3", alpha=0.7, position='identity') +
  geom_rug(aes(x=oldpeak, y = NULL)) +
  scale_fill_manual(values=c("green", "orange")) +
  ylab("density") + 
  labs(fill="") +
  theme_minimal() +
  theme(text = element_text(size = 20), legend.position="none")

dev.off()

print(moments::skewness(df_train$oldpeak))

table(df_train$oldpeak)



# Target

df_train$target = factor(df_train$target, levels=c(0,1), labels=c("healthy", "sick"))


df_pie_target <- df_train %>% 
  group_by(target) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = paste(n, " (", scales::percent(perc), ")", sep=""))


jpeg(file="../LateX_project/images/chapter1/target_pie.jpeg", width=6, height=6, units='in', res=200)

ggplot(df_pie_target, aes(x = "", y = perc, fill = target)) +
  geom_col(color = "black") +
  geom_label(aes(label = labels), color = c("black", "black"),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Target")) +
  scale_fill_manual(values=c("green", "orange")) +
  coord_polar(theta = "y") + 
  theme_void() +
  theme(text = element_text(size = 20))

dev.off()

df_train$target = factor(df_train$target, levels=c("healthy", "sick"), labels=c(0, 1))



# Sex

df_train$sex = factor(df_train$sex, levels=c(0,1), labels=c("female", "male"))


df_pie_sex <- df_train %>% 
  group_by(sex) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = paste(n, " (", scales::percent(perc), ")", sep=""))


jpeg(file="../LateX_project/images/chapter1/sex_pie.jpeg", width=6, height=6, units='in', res=200)

ggplot(df_pie_sex, aes(x = "", y = perc, fill = sex)) +
  geom_col(color = "black") +
  geom_label(aes(label = labels), color = c("black", "black"),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Sex")) +
  scale_fill_manual(values=c("green", "orange")) +
  coord_polar(theta = "y") + 
  theme_void() +
  theme(text = element_text(size = 20))

dev.off()

df_train$sex = factor(df_train$sex, levels=c("female", "male"), labels=c(0,1))


# Number of major vessels
jpeg(file="../LateX_project/images/chapter1/ca_barplot.jpeg", width=6, height=6, units='in', res=200)

ggplot(df_train, aes(x=ca)) +
  geom_bar(color="#6b9bc3", fill=rgb(0.1,0.4,0.5,0.7) ) + 
  geom_text(stat='count', aes(label=paste(after_stat(count), " (", round(after_stat(count)/nrow(df_train)*100, digits=2), "%)", sep="")), vjust=2, size=5) +
  xlab("ca") + 
  theme_minimal() +
  theme(text = element_text(size = 20)) 

dev.off()

head(df_train[df_train$ca == 4, ])
print(xtable::xtable(head(df_train[df_train$ca == 4, ]), type="latex", digits=2))

df_train[df_train$ca == 4, 'ca'] = c(3,3,3)


# Thalassemia
jpeg(file="../LateX_project/images/chapter1/thal_barplot.jpeg", width=6, height=6, units='in', res=200)

ggplot(df_train, aes(x=thal)) +
  geom_bar(color="#6b9bc3", fill=rgb(0.1,0.4,0.5,0.7) ) + 
  geom_text(stat='count', aes(label=paste(after_stat(count), " (", round(after_stat(count)/nrow(df_train)*100, digits=2), "%)", sep="")), vjust=2, size=5) +
  xlab("thal") + 
  theme_minimal() +
  theme(text = element_text(size = 20))

dev.off()

head(df_train[df_train$thal == 0, ])
print(xtable::xtable(head(df_train[df_train$thal == 0, ]), type="latex", digits=2))

df_train[df_train$thal == 0, 'thal'] = c(1,1)



# MULTIVARIATE

jpeg(file="../LateX_project/images/chapter1/pairplot_target.jpeg", width=6, height=6, units='in', res=200)

GGally::ggpairs(df_train, columns = c(1,4,5,8,10), ggplot2::aes(colour=target), 
                upper = list(continuous = "points"), diag = list(continuous = "densityDiag"))

dev.off()


GGally::ggpairs(df_train, columns = c(1,4,5,8,10), ggplot2::aes(colour=sex), 
                upper = list(continuous = "points"), diag = list(continuous = "barDiag"))

GGally::ggpairs(df_train, columns = c(1,4,5,8,10), ggplot2::aes(colour=fbs), 
                upper = list(continuous = "points"), diag = list(continuous = "barDiag"))

GGally::ggpairs(df_train, columns = c(1,4,5,8,10), ggplot2::aes(colour=cp), 
                upper = list(continuous = "points"), diag = list(continuous = "barDiag"))

GGally::ggpairs(df_train, columns = c(1,4,5,8,10), ggplot2::aes(colour=slope), 
                upper = list(continuous = "points"), diag = list(continuous = "barDiag"))


jpeg(file="../LateX_project/images/chapter1/corplot.jpeg", width=6, height=6, units='in', res=200)

corrplot::corrplot.mixed(cor(df_train[,c(1,4,5,8,10)]), upper = "ellipse")

dev.off()









### -------------------------------------------------------------------------------------------------------
### 2. Data modeling ###

### 2a. logistic regression ###


# Dataset partition
trainIndex = caret::createDataPartition(df_train$target, p = 0.8, list = FALSE, times = 1)

df_validation = df_train[-trainIndex,]
df_training = df_train[trainIndex,]



# Using all variables
model_glm_1 = glm(data=df_training, target~., family=binomial)

summary(model_glm_1)
print(xtable::xtable(summary(model_glm_1), type="latex", digits=2))

model_glm_1$aic


glm_valid_1 = predict.glm(model_glm_1, newdata=df_validation, type="response")


glm_valid_1[glm_valid_1 >= 0.5] = 1
glm_valid_1[glm_valid_1 < 0.5] = 0


df_validation = data.frame(cbind(df_validation, predicted_class = glm_valid_1))


validation_metrics = function(target, prediction) {
  
  cm = caret::confusionMatrix(factor(prediction), factor(target), dnn = c("predicted", "true_values"))
  plt_cm = as.data.frame(cm$table)
  
  cf_graphs = ggplot(data=plt_cm, aes(x=predicted, y=true_values, fill=Freq)) +
    geom_tile() + geom_text(aes(label=Freq)) +
    scale_fill_gradient(low="white", high="#6b9bc3") +
    labs(x = "True values", y = "Predicted") +
    theme_minimal() +
    theme(text = element_text(size = 20))
  
  graphs = list(cf_graphs)
  
  cm = as.matrix(table(Actual=target, Predicted=prediction))
  accuracy = (cm[2,2]+cm[1,1]) / (cm[2,2]+cm[1,1]+cm[1,2]+cm[2,1])   # (tp+tn)/(tp+tn+fp+fn)
  error_rate = 1 - accuracy
  specificity = cm[1,1] / (cm[1,2]+cm[1,1])   # tn/(fp+tn)
  sensitivity = cm[2,2] / (cm[1,2]+cm[2,2])   # tp/(tp+fp)
  auc = pROC::auc(target, prediction)
  
  metrics = data.frame(accuracy, error_rate, specificity, sensitivity, auc)
  colnames(metrics) = c("Accuracy", "Error rate", "Specificity", "Sensitivity", "AUC")
  print(xtable::xtable(metrics, type="latex", digits=2))
  
  return(graphs)
  
}


model = "glm_all_05"
graphs = validation_metrics(df_validation$target, df_validation$predicted_class)

jpeg(file=paste("../LateX_project/images/chapter2/confusion_matrix_", model, ".jpeg", sep=""), width=6, height=6, units='in', res=200)
graphs[1]
dev.off()


roc_curve = pROC::roc(df_validation$target, df_validation$predicted_class)

jpeg(file=paste("../LateX_project/images/chapter2/roc_curve_", model, ".jpeg", sep=""), width=6, height=6, units='in', res=200)
ggroc(roc_curve, colour = '#6b9bc3', size = 2) +
  theme_minimal() +
  theme(text = element_text(size = 20))
dev.off()


print(pROC::coords(roc_curve, "best", ret = "threshold"))


# Stepwise logistic regression
MASS::stepAIC(model_glm_1, direction="both")




df_validation = subset(df_validation, select = -predicted_class)

# Using only sex + cp + chol + thalach + exang + slope + ca + thal
model_glm_2 = glm(data=df_training, target~sex + cp + chol + thalach + exang + slope + ca + thal, family=binomial)

summary(model_glm_2)
print(xtable::xtable(summary(model_glm_2), type="latex", digits=2))

glm_valid_2 = predict.glm(model_glm_2, newdata=df_validation, type="response")


glm_valid_2[glm_valid_2 >= 0.5] = 1
glm_valid_2[glm_valid_2 < 0.5] = 0


df_validation = data.frame(cbind(df_validation, predicted_class = glm_valid_2))



model = "glm_thirdbest_05"
graphs = validation_metrics(df_validation$target, df_validation$predicted_class)

jpeg(file=paste("../LateX_project/images/chapter2/confusion_matrix_", model, ".jpeg", sep=""), width=6, height=6, units='in', res=200)
graphs[1]
dev.off()


roc_curve = pROC::roc(df_validation$target, df_validation$predicted_class)

jpeg(file=paste("../LateX_project/images/chapter2/roc_curve_", model, ".jpeg", sep=""), width=6, height=6, units='in', res=200)
ggroc(roc_curve, colour = '#6b9bc3', size = 2) +
  theme_minimal() +
  theme(text = element_text(size = 20))
dev.off()



df_validation = subset(df_validation, select = -predicted_class)



### 2b. random forests ###

mtry = pracma::ceil(sqrt(ncol(df_train)-1))

model_rf = randomForest::randomForest(target ~ ., data=df_training, mtry=mtry, importance=TRUE)

print(model_rf)
print(xtable::xtable(model_rf$confusion, type="latex", digits=2))


rf_valid = predict(model_rf, df_validation)

rf_valid = as.data.frame(rf_valid)
df_validation = merge(df_validation, rf_valid, by="row.names")
df_validation$target = as.numeric(df_validation$target)
df_validation$rf_valid = as.numeric(df_validation$rf_valid)
df_validation$target = plyr::mapvalues(df_validation$target, c(2, 1), c(1, 0))
df_validation$rf_valid = plyr::mapvalues(df_validation$rf_valid, c(2, 1), c(1, 0))


model = "rf"
graphs = validation_metrics(df_validation$target, df_validation$rf_valid)


jpeg(file=paste("../LateX_project/images/chapter2/confusion_matrix_", model, ".jpeg", sep=""), width=6, height=6, units='in', res=200)
graphs[1]
dev.off()


roc_curve = pROC::roc(as.numeric(df_validation$target), as.numeric(df_validation$rf_valid))

jpeg(file=paste("../LateX_project/images/chapter2/roc_curve_", model, ".jpeg", sep=""), width=6, height=6, units='in', res=200)
ggroc(roc_curve, colour = '#6b9bc3', size = 2) +
  theme_minimal() +
  theme(text = element_text(size = 20))
dev.off()


# Feature importance

importance(model_rf)

jpeg(file="../LateX_project/images/chapter2/variable_importance_rf.jpeg", width=10, height=6, units='in', res=200)
varImpPlot(model_rf, main = "Importance of each variable")
dev.off()







### 2c. neural networks ###










### -------------------------------------------------------------------------------------------------------
### 3. Results ###









