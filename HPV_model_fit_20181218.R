# Code to fit the model which creates p(HPV | HNC, covariates)
# Trying not to use variables which are innapropriate for the eventual goal of creating p(HNC | HPV, covariates) such as cancer site, histology, and survival

# Clear the workspace
rm(list = ls())

# Load in packages
dyn.load('/Library/Java/JavaVirtualMachines/openjdk-13.0.1.jdk/Contents/Home/lib/server/libjvm.dylib')
options(java.parameters = "-Xmx16g")
auf::packages('tidyverse', 'randomForest', 'bartMachine', 'ROCR')

# Load in data - terrible name for csv file
d = read.csv('model_datav4.csv')

# Remove first column - ID variable
# Also remove inappropriate columns
x = d %>%
  select(-PatientID,
         -Site,
         -Histology,
         -TotalNumberOfMalignantTumors,
         -VitalStatus,
         -TotalNumberOfBenignTumors)

# Set seed for repeatability
set.seed(123)

# Create training and test set
samp = sample(nrow(x),0.75*nrow(x))
train  = x[samp,]
test = x[-samp,]

# Fit a model
# model = randomForest(HPVStatus ~ ., data = train)
# stop()
# saveRDS(model, file = 'rf_model_20191218.rds')
# model = readRDS(file = 'rf_model_20191218.rds')
# set_bart_machine_num_cores(parallel::detectCores())
# model = bartMachine(X = train[,-ncol(train)],
#                     y = train[,ncol(train)])
# training = predict(model,new_data = train[,-ncol(train)],"prob")
# testing = predict(model,new_data = test[,-ncol(test)],"prob")

# Look at ROC curve on test data
pred = prediction(testing[,2], test$HPVStatus)
perf = performance(pred,"tpr","fpr")
#plot(perf)
plot(perf, col = 'red')

auc_ROCR = performance(pred, measure = "auc")
auc_ROCR@y.values[[1]] # 0.6657831

# Look at variable importance
importance(model)
varImpPlot(model)
o = order(importance(model), decreasing = TRUE)
head(importance(model)[o,])

# Think only those first 3 are really useful - try re-fitting?
# select = 3
# choose_cols = names(importance(model)[o[1:select],])
choose_cols = c("AgeAtDiagnosis", "MaritalStatusAtDiagnosis", "Race", "Sex", "Insurance", "CurrentSmokerPercentage")
train2 = x[samp, c('HPVStatus', choose_cols)]
test2 = x[-samp, c('HPVStatus', choose_cols)]

# Re-fit
model2 = randomForest(HPVStatus ~ ., data = train2)
saveRDS(model2, file = 'rf_model_reduced_20181018.rds')
model2 = readRDS(file = 'rf_model_reduced_20181018.rds')
stop()
model2 = bartMachine(X = train2[,-1],
                    y = train2[,1],
                    serialize = TRUE)
saveRDS(model2, file = 'bart_model_reduced_20191218.rds')
stop()
model2 = readRDS(file = 'bart_model_reduced_20191218.rds')

# Check convergence:
plot_convergence_diagnostics(model2)

# Look at variable importance
investigate_var_importance(model2)

# training2 = predict(model2,train2[,-1],"prob")
testing2 = predict(model2,test2[,-1],"prob")

# Get predictions and some performance metrics
oos_perf = bart_predict_for_test_data(model2, test2[,-1], test2[,1])
# Get full posterior
full_post = bart_machine_get_posterior(model2, test2[,-1])

# Get AUC again
pred2 = prediction(1-testing2, test2$HPVStatus)
perf2 = performance(pred2,"tpr","fpr")
plot(perf2)

auc_ROCR2 = performance(pred2, measure = "auc")
auc_ROCR2@y.values[[1]] # 0.6651991

