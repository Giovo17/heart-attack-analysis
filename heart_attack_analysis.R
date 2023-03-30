# heart-attack-analysis

# Import libraries
library(xtable)


set.seed(17)


# Import dataset

# Online from github repo
#df_test = read.csv('https://raw.githubusercontent.com/Giovo17/heart-attack-analysis/main/data/heart_attack_test.csv')
#df_train = read.csv('https://raw.githubusercontent.com/Giovo17/heart-attack-analysis/main/data/heart_attack_train.csv')

# Local from disk
setwd("~/Documents/University/Data\ Science/1°\ Year\ (2022-2023)/Statistical\ Learning\ (1)/Exam\ -\ Statistical\ Learning/report/heart-attack-analysis/data")
df_train = read.csv("heart_attack_train.csv")
df_test = read.csv("heart_attack_test.csv")

head(df_train)
print(xtable::xtable(head(df_train), type="latex", digits=5))

head(df_test)
print(xtable::xtable(head(df_test), type="latex", digits=5))
