# Code to fit the model which creates p(HPV | HNC, covariates)
# Trying not to use variables which are innapropriate for the eventual goal of creating p(HNC | HPV, covariates) such as cancer site, histology, and survival

# Clear the workspace
rm(list = ls())

# Load in packages
library(BART)
library(tidyverse)
library(randomForest)
library(ROCR)

# Load in data - terrible name for csv file
d = read.csv('model_datav4.csv',
             stringsAsFactors = TRUE)

# Remove first column - ID variable
# Also remove inappropriate columns
x = d %>%
  select(-PatientID,
         -Site,
         -Histology,
         -TotalNumberOfMalignantTumors,
         -VitalStatus,
         -TotalNumberOfBenignTumors) %>%
  mutate(HPVStatus = as.integer(HPVStatus)-1) %>%
  bartModelMatrix

# Set seed for repeatability
set.seed(123)

# Create training and test set
samp = sample(nrow(x),0.75*nrow(x))
train  = x[samp,]
test = x[-samp,]

# Fit a model to the full data
model = BART::pbart(x.train = train[,-ncol(train)],
                    y.train = as.logical(train[,ncol(train)]))
training = apply(predict(model, newdata = train[,-ncol(train)])$prob.test, 2, 'mean')
testing = apply(predict(model, newdata = test[,-ncol(test)])$prob.test, 2, 'mean')

# Look at ROC curve on test data
pred = prediction(testing, test[,'HPVStatus'])
perf = performance(pred,"tpr","fpr")
#plot(perf)
plot(perf, col = 'red')

auc_ROCR = performance(pred, measure = "auc")
auc_ROCR@y.values[[1]] # 0.6820848

# Think only those first few are really useful - try re-fitting?
x2 = d %>%
  select("HPVStatus",
          "AgeAtDiagnosis",
          "MaritalStatusAtDiagnosis",
          "Race",
          "Sex",
          "Insurance",
          "CurrentSmokerPercentage") %>%
  mutate(HPVStatus = as.integer(HPVStatus)-1) %>%
  bartModelMatrix

train2 = x2[samp, ]
test2 = x2[-samp, ]

model2 = BART::pbart(x.train = train2[,-1],
                     y.train = train2[,1])
saveRDS(model2, file = 'bart_model_reduced_20210326.rds')
model2 = readRDS(file = 'bart_model_reduced_20210326.rds')

# Get full posterior
testing2 = apply(predict(model2, newdata = test2[,-1])$prob.test, 2, 'mean')

# Get AUC again
pred2 = prediction(testing2, test2[,"HPVStatus"])
perf2 = performance(pred2,"tpr","fpr")
plot(perf2)

auc_ROCR2 = performance(pred2, measure = "auc")
auc_ROCR2@y.values[[1]] # 0.6576552

