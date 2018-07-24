#### clear the console area
cat("\014")
### remove all stored variables
rm(list = ls(all=TRUE))
list.files()

path = "/Users/snehan/Desktop/PHD_Hackathon"
setwd(path)

### Load external files.
#source("UserDefinedFunctions.R")
#source("LogisticModel.R")


#############################################
# USER DEFINED FUNCTIONS.
#############################################

ns_checkTotalMissingValues = function(dataSet) {
  print(sum(is.na(dataSet)))
}

ns_checkColumnwiseMissingValues = function(dataSet) {
  sapply(dataSet, function(x) sum(is.na(x)))
  #print(table(is.na(dataSet)))
}

ns_showEachColumnFactorProportions = function(dataSet) {
  columnList = colnames(dataSet)
  sapply(columnList, function(x) table(dataSet[x]))
}

ns_dataStatistics = function(dataSet) {
  print('----------------- dimensions-----------------------')
  dim(dataSet)
  print('----------------- summary-----------------------')
  summary(dataSet)
  print('----------------- structure-----------------------')
  str(dataSet)
  print('----------------- missing values-----------------------')
  ns_checkTotalMissingValues(dataSet) ## 3160 missing values
  print('----------------- column wise missing values-----------------------')
  ns_checkColumnwiseMissingValues(dataSet) 
}


#############################################
# USER DEFINED FUNCTIONS ---------- END.
#############################################

split.data = function(data, p = 0.75, s = 666) {
  set.seed(s)
  index = sample(1:dim(data)[1])
  train = data[index[1:floor(dim(data)[1] * p)], ]
  test = data[index[((ceiling(dim(data)[1] * p)) + 1):dim(data)[1]], ]
  return(list(train = train, test = test)) 
}

#############################################
# Save to csv file
#############################################
saveToCSV = function(probabilityForTest, fileName, testData, firstCol) {
  file1 = data.frame(ID = firstCol)
  file1["y"] = probabilityForTest
  write.csv(file1, fileName, row.names = FALSE)
}


##########################
# Generalized Linear regression function. (Logistic Regression)
##########################
logisticRegressionModel = function(dataSet, targetVariable) {
  glmFit = glm(formula = targetVariable ~ ., data = dataSet, family = binomial)
  return(glmFit)
}

##########################
# VIF
##########################
library(car)

checkVarianceInflationFactor = function(model) {
  vif(model)
}

##########################
# Stepwise regression function.
##########################
stepwiseRegression = function(model) {
  stepwiseModel = stepAIC(model)
  return(stepwiseModel)
}

##########################
# Predict on Train/Test with model. return probabilities
# Get a list of predictions (probability scores) using the predict() function
# Use the argument 'type = "response"' in the predict function to get a list of predictions between 0 and 1
##########################
predictProbability = function(model, dataSet) {
  probPredict = predict(model, dataSet, type = "response")
  return(probPredict)
}

##########################
# Assessing performance with ROC curve
# Using the ROCR package create a "prediction()" object
##########################
library(ROCR)

rocrPerformance = function(trainedModel, testData, targetVariable) {
  
  # 1. Prepare Probability Matrix. requires -> trainedModel, testData without y
  # or get probabilities.
  # 2. ROCR prediction
  # 3. Prepare ROCR performance object for ROC curve (tpr, fpr) ans AUC
  # The performance() function from the ROCR package helps us extract metrics such as True positive rate, False positive rate etc. from the prediction object, we created above.
  # Two measures (y-axis = tpr, x-axis = fpr) are extracted
  # 4. plot ROCR curve.
  
  # 1.
  train.model.predict.probs = predictProbability(trainedModel, testData)
  # 2.
  train.model.probability.rocr = prediction(train.model.predict.probs, testData[targetVariable])
  # 3.
  train.model.performance = performance(train.model.probability.rocr, "tpr", "fpr")
  train.model.auc.perf = performance(train.model.probability.rocr, measure = "auc", x.measure = "cutoff")
  # 4.
  plot(train.model.performance, col = 2, colorize = TRUE, 
       main = paste("AUC", train.model.auc.perf@y.values))
  auc <- train.model.auc.perf@y.values[[1]]
  print(auc)
  return(list(train.rocr.perf = train.model.performance, auc = auc)) 
  #return(train.model.performance)
}

choosingCutOffValues = function(train.rocr.perf) {
  # For different threshold values identifying the tpr and fpr
  cutoffs <- data.frame(cut= train.rocr.perf@alpha.values[[1]], fpr= train.rocr.perf@x.values[[1]], 
                        tpr=train.rocr.perf@y.values[[1]])
  
  # Sorting the data frame in the decreasing order based on tpr
  cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
  
  # Plotting the true positive rate and false negative rate based based on the cutoff       
  # increasing from 0.1-1
  plot(train.rocr.perf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
}






########################
# Saturday..
########################

machinery.original.train = read.csv('Train.csv')

train.additional = read.csv("Train_AdditionalData.csv")
str(train.additional)
#table(train.additional$TestA, train.additional$TestB)

addTestedAB_toDataframe = function(t.additional, t.original) {
  intersectionOnTestAB = intersect(t.additional$TestA, t.additional$TestB)
  tAB.out = sapply(1:length(t.original$ID), function(x) ifelse(t.original$ID[x] %in% intersectionOnTestAB, "1", "0"))
  tAB.df = data.frame(matrix(tAB.out, nrow=length(tAB.out), byrow=T),stringsAsFactors=FALSE)
  t.original["tested"] = tAB.df$matrix.tAB.out..nrow...length.tAB.out...byrow...T.
  return(t.original)
}
machine.train = machinery.original.train
dim(machine.train)  #### 3156 x 22
str(machine.train)
summary(machine.train)

### Append tested report data.
machine.train = addTestedAB_toDataframe(train.additional, machine.train)
table(machine.train$tested)
ns_checkTotalMissingValues(machine.train) ## 3160 missing values

ns_checkColumnwiseMissingValues(machine.train) 
### except ID and y doesnot have na's.
columns.mach = colnames(machine.train)
print(columns.mach)
ns_showEachColumnFactorProportions(machine.train)
table(machine.train$Number.of.Cylinders)
machine.train$ID = NULL

##############################
# Data Preprocessing. 
#############################


##############################
# PP - 1
#############################
library(DMwR)
######### no. of cylinders and central imputation.
names(machine.train)
ccc = c("Number.of.Cylinders", "Valve.Type", "Fuel.Type", "Compression.ratio" , "Cylinder.deactivation" , "Direct.injection", "piston.type", "Max..Torque", "Crankshaft.Design", "tested")
ns_preProcessingSaturday1 = function(data.set) {
  # Number.of.Cylinders has only 4,6,8 => can be converted to factors.
  data.set = data.set[, !(names(data.set) %in% ccc), drop = FALSE]
  #data.set$Peak.Power = NULL
  #data.set$Liner.Design. = NULL
  #data.set$material.grade = NULL
  #data.set$Lubrication = NULL
  #data.set$Bearing.Vendor = NULL
  #data.set$main.bearing.type = NULL
  data.set$tested = as.factor(data.set$tested)
  ns_dataStatistics(data.set)
  dataSet = knnImputation(data.set, k = 5)
  ns_dataStatistics(dataSet)
  dataSet$Number.of.Cylinders = as.character(dataSet$Number.of.Cylinders)
  dataSet$Number.of.Cylinders = ifelse(dataSet$Number.of.Cylinders == '4', "Four Cyl",
                                             ifelse(dataSet$Number.of.Cylinders == '6', "Six cyl",
                                                    ifelse(dataSet$Number.of.Cylinders == '8', "Eight cyl", "NewType Cyl")))
  dataSet$Number.of.Cylinders = as.factor(dataSet$Number.of.Cylinders)
  
  table(dataSet$Number.of.Cylinders)
  
  #d1 = centralImputation(dataSet)
  #d1 = knnImputation(dataSet, k=5)
  #knnImputation(dataSet, k = 5, scale = T, meth = "mode")
  #ns_dataStatistics(d1)
  #knn.impute(dataSet, k = 10, cat.var = 1:ncol(dataSet), to.impute = 1:nrow(dataSet), using = 1:nrow(dataSet))
  
  return(dataSet)
}
machine.train = ns_preProcessingSaturday1(machine.train)
#knn.impute(machine.train, k = 10, cat.var = 1:ncol(machine.train), to.impute = 1:nrow(machine.train), using = 1:nrow(machine.train))
ns_dataStatistics(machine.train)

##############################
# PP - 1 ---- ------ END.
#############################


convertAllCategoricalVariablesToDummies = function(dataSet, categoricalFeatures) {
  dummies = dummyVars(~., dataSet[categoricalFeatures])
  cat_hot = predict(dummies, dataSet[categoricalFeatures])
  print(cat_hot)
  return(cat_hot)
}
getCategoricalFeatures = function(featureClasses) {
  catFeatures = names(featureClasses[featureClasses == "factor"])
  print(catFeatures)
  return(catFeatures)
}
getTypeOfAllAttributes = function(dataSet) {
  featuresType = sapply(names(dataSet), function(x) { class(dataSet[[x]]) })
  return(featuresType)
}


#######################
# pp - 2
#######################
#machine.train = centralImputation(machine.train)
ns_preProcessingSaturday2 = function(dataSet) {
  attTypes = getTypeOfAllAttributes(dataSet)
  catFeat = getCategoricalFeatures(attTypes)
  dum = convertAllCategoricalVariablesToDummies(dataSet, catFeat)
  
  return(dum)
}
convertToDummies = function(dataSet) {
  dummiesTest <- dummyVars( ~ ., data = dataSet, levelsOnly = FALSE)
  dum_data <- as.data.frame(predict(dummiesTest, newdata = dataSet))
  return(dum_data)
}
tr1.dum = convertToDummies(machine.train[,-which(names(machine.train) == "y")])
#d1 = ns_preProcessingSaturday2(machine.train)
tr1.dum["y"] = machine.train$y
machine.train = tr1.dum
#######################
# pp - 2  ---- ----- end
#######################



#machine.train$ID = NULL

#### fail = 0, pass = 1

machine.train$y = as.character(machine.train$y) 
machine.train$y = ifelse(machine.train$y == 'fail', 0, 1)
machine.train$y = as.factor(machine.train$y)

#machine.train$tested = as.factor(machine.train$tested)
summary(machine.train)

#############
# All missing values are for categorical variables.
## https://link.springer.com/article/10.1007/s11222-016-9635-4
## http://juliejosse.com/publications/

### install package for imputing categorical variable.

#install.packages("missMDA")
#library(missMDA)
### material.grade
#table(machine.train$material.grade)
#sum(is.na(machine.train$material.grade))
#nb = estim_ncpMCA(machine.train, ncp.max = 5)
#nb = 4
### time consuming -> use nb = 4
#machine.train.impu = MIMCA(machine.train, nboot = 10, ncp = nb)

#missForest
missForestImputation = function(dataSet) {
  install.packages("missForest")
  library(missForest)
  library(foreach)
  library(itertools)
  library(iterators)
  
  mach.train.impu = missForest(machine.train)
  mach.train.impu$ximp
  mach.train.impu$OOBerror
  return(mach.train.impu)
}

################################
# Visualizations.
################################
plot(machine.train$Number.of.Cylinders, xlab = "# cylinders")
plot(machine.train$material.grade, xlab = "machine grades")
plot(machine.train$Lubrication, xlab = "Lubrication")

plot(machine.train$Valve.Type, xlab = "valve type")
plot(machine.train$Bearing.Vendor, xlab = "Bearing.Vendor")
plot(machine.train$Fuel.Type, xlab = "Fuel.Type") ## left only two

plot(machine.train$Compression.ratio, xlab = "Compression.ratio")
plot(machine.train$cam.arrangement, xlab = "cam.arrangement") ## right
plot(machine.train$Cylinder.arragement, xlab = "Cylinder.arragement") ## left


library(dplyr)
library(ROCR)
library(caret)
library(e1071)

####### split data into train and validation set.

allSet = split.data(machine.train)
train.data = allSet$train
validation.data = allSet$test
dim(train.data)
dim(validation.data)
summary(train.data)


################################
# Build Logistic Reg Model.
################################

train.ids = train.data$ID
valida.ids = validation.data$ID
valid.y = validation.data$y

train.data$ID = NULL
validation.data$ID = NULL

attach(train.data)
str(train.data)
summary(train.data)
glm.model = glm(train.data$y ~ ., data = train.data, family = "binomial")
glm.model  #### AIC = 2551, 1698 after adding testA and testB, knn: 1625
summary(glm.model)
step.glm.model = stepwiseRegression(glm.model)
step.glm.model #### AIC = 2535, 1668 after adding testA and testB, knn: 1601
summary(step.glm.model)
checkVarianceInflationFactor(step.glm.model)

cutoff = 0.3
#### predict on validation set
probability.validation = predictProbability(step.glm.model, validation.data)
prob.train = predictProbability(step.glm.model, train.data)

rocr.output = rocrPerformance(step.glm.model, validation.data, "y")
# AUC = 82.81, 91.4 after adding testA and testB

choosingCutOffValues(rocr.output$train.rocr.perf)
cutoff = 0.2

######### prediction on validation data.
pred.classes.train = ifelse(prob.train > cutoff, "1", "0")
pred.classes.validation = ifelse(probability.validation > cutoff, "1", "0")



############## confusion matrix.
library(caret)
confusionMatrix(pred.classes.validation, valid.y, positive = "1")
######### Accuracy = 65.27
######### Sensitivity : 0.9365         
######### Specificity : 0.3917

#### after adding testA and testB
## Accuracy : 0.8352 ,Sensitivity : 0.9815, Specificity : 0.7007


#### save to csv file
test.data = read.csv("Test.csv", header = TRUE)
test.additional = read.csv("Test_AdditionalData.csv")
test.data = addTestedAB_toDataframe(test.additional, test.data)
test.data$tested = as.factor(test.data$tested)
test.ID = test.data[, "ID"]
test.data$ID = NULL
str(test.data)
#### test preprocessing.

##############################
# Test Data Preprocessing.
#############################
################
# Number.of.Cylinders has only 4,6,8 => can be converted to factors.

#test.data$Number.of.Cylinders = as.character(test.data$Number.of.Cylinders)
#test.data$Number.of.Cylinders = ifelse(test.data$Number.of.Cylinders == '4', "Four Cyl",
#                                           ifelse(test.data$Number.of.Cylinders == '6', "Six cyl",
#                                                 ifelse(test.data$Number.of.Cylinders == '8', "Eight cyl", "NewType Cyl")))
#test.data$Number.of.Cylinders = as.factor(test.data$Number.of.Cylinders)

#summary(test.data)
#test.data = centralImputation(test.data)
#ns_dataStatistics(test.data)

### PP-1
test.data = ns_preProcessingSaturday1(test.data)
ns_dataStatistics(test.data)
table(test.data$Number.of.Cylinders)
## pp -2
test.data = ns_preProcessingSaturday2(test.data)

probability.testdata = predict(step.glm.model, test.data, type = 'response')
predict.classes.test = ifelse(probability.testdata > cutoff, "pass", "fail")

saveToCSV(predict.classes.test, "san_sample_submission_TestAB_Logistic.csv", test.data, test.ID)

###############################################################################################
# Decision Tree
###############################################################################################
## 1.
## CART -> uses GINI Index -> Binary split
library(rpart)
### grow tree.
rpart.model = rpart(train.data$y ~ ., data = train.data, method = "class")
summary(rpart.model)
print(rpart.model)
predict.train.rpart = predict(rpart.model, newdata = train.data, type = "vector")
predict.train.rpart.classes = ifelse(predict.train.rpart > 1, "1", "0")
unique(predict.train.rpart)
predict.validation.rpart = predict(rpart.model, newdata = validation.data, type = "vector")
predict.rpart.validation.classes = ifelse(predict.validation.rpart > 1, "1", "0")

confusionMatrix(predict.train.rpart.classes, train.data$y, positive = "1")
confusionMatrix(predict.rpart.validation.classes, validation.data$y, positive = "1")

probability.rpart.test = predict(rpart.model, test.data, type = 'prob')
head(probability.rpart.test)
predict.classes.test.rapart = ifelse(probability.rpart.test[,1] > probability.rpart.test[,2], "fail", "pass")
head(predict.classes.test.rapart)
saveToCSV(predict.classes.test.rapart, "san_sample_submission_DT_rpart.csv", test.data, test.ID)


### 2
library(caret)
buildRPart = function(costParameter) {
  rpart.cp.model = rpart(train.data$y ~ ., data = train.data, method = "class", control = rpart.control(cp = costParameter))
  summary(rpart.cp.model)
  print(rpart.cp.model)
  plot(rpart.cp.model)
  predict.cp.train.rpart = predict(rpart.cp.model, newdata = train.data, type = "vector")
  predict.cp.train.rpart.classes = ifelse(predict.cp.train.rpart > 1, "1", "0")
  predict.cp.validation.rpart = predict(rpart.cp.model, newdata = validation.data, type = "vector")
  predict.cp.rpart.validation.classes = ifelse(predict.cp.validation.rpart > 1, "1", "0")
  
  print(confusionMatrix(predict.cp.train.rpart.classes, train.data$y, positive = "1"))
  print(confusionMatrix(predict.cp.rpart.validation.classes, validation.data$y, positive = "1"))
}
rpart.cp.model = buildRPart(0.08)
buildRPART_one = function(cp) {
  tc = trainControl("cv",5)
  rpart.grid = expand.grid(.cp=cp) # 0.2
  rpart.cp.model = train(machine.train$y ~ ., data=machine.train, method="rpart",trControl=tc,tuneGrid=rpart.grid)
}
rpart.cp.m1 = buildRPART_one(0.08)

###### second way.

## 3.
library(rpart)
library(rpart.plot)
model_dt = rpart(train.data$y ~ . , train.data)
dt.preds_dt = predict(model_dt, validation.data)
preds_tree = ifelse(dt.preds_dt[, 1] > dt.preds_dt[, 2], "0", "1")
confusionMatrix(preds_tree, validation.data$y,positive="1")
rpart.plot(model_dt)

# grow tree 
ctrl = trainControl(method="repeatedcv",repeats =2)
rpart.grid = expand.grid(.cp=seq(0.01,.2,.01))
model.rpart = train(y~.,data=train.data,method='rpart',trControl=ctrl,tuneGrid=rpart.grid,minsplit=10,xval=10,metric="Accuracy")
model.rpart
#fit <- rpart(X1 ~ ., data = train_datanew1,method="class")
#preds_r <- predict(fit, type = 'prob')
### cp = 0.08
#model.rpart = rpart(train.data$y ~ ., data = train.data, method = "class", control = rpart.control(cp = 0.08))

preds.rp.train = predict(model.rpart,train.data)
preds.rp.valid = predict(model.rpart,validation.data)
confusionMatrix(preds.rp.train, train.data$y,positive="1")
confusionMatrix(preds.rp.valid, validation.data$y,positive="1")
#######
#rpart: 
#  cp:
#  Accuracy    : 0.8631,0.8657
#  Sensitivity : 0.8744,0.8677   
#  Specificity : 0.8505,0.8637
######


###############################################################################################
# Random Forest.
###############################################################################################

library(randomForest)
set.seed(1738)
x.rf = train.data[, names(train.data) != "y"]
dim(x.rf)
y.rf = train.data[, "y"]
str(y.rf)
### standardize

#prePro.rf = preProcess(x.rf)
#rf.std.train = predict(prePro.rf, x.rf, center = TRUE, scale = TRUE)
#rf.std.validation = predict(prePro.rf, validation.data[, names(validation.data) != "y"])


rf.model = randomForest(x.rf, y = y.rf , ntree = 300, importance = TRUE)
summary(rf.model)
names(rf.model)
rf.model$importance
varImpPlot(rf.model)
plot(rf.model)

rf.predict.valid = predict(rf.model, validation.data)
rf.probPred.train = predict(rf.model, train.data, type = 'prob')
head(rf.probPred.train)
rf.class.pred.train = ifelse(rf.probPred.train[,1]>rf.probPred.train[,2],"0", "1")
rf.prob.valid = predict(rf.model, validation.data, type = 'prob')
rf.class.valid = ifelse(rf.prob.valid[,1]>rf.prob.valid[,2],"0", "1")
rf.prd.train = predict(rf.model, train.data)

confusionMatrix(rf.prd.train, train.data$y)
#### acc: 90, Sensitivity : 0.8729  Specificity : 0.9256
confusionMatrix(rf.predict.valid, validation.data$y)
#### acc: 0.8669, Sensitivity : 0.8564  Specificity : 0.8783

rf.imp = importance(rf.model)#, type=1 )
importance(rf.model)
varImp(rf.model)
impOrder = order(-rf.model$importance)
impOrder


### Accuracy Estimated by Cross-validation

rf.tr.x = machine.train[, names(machine.train) != "y"]
rf.tr.y = machine.train[, "y"]
rf.model.forcv = randomForest(rf.tr.x, y = rf.tr.y, ntree = 200, importance = T)
summary(rf.model.forcv)
rf.model.forcv
varImpPlot(rf.model.forcv)
plot(rf.model.forcv)
fit.control = trainControl( method = "LOOCV" ) 
rf.loocv = train(rf.tr.x, y = rf.tr.y, method="rf", ntree=200 , tuneGrid=data.frame( mtry=10 ) , trControl=fit.control )

par(mfrow=c(1,2))
rf.imp1 = as.data.frame(rf.model.forcv$importance)
rf.imp1$features = rownames(rf.imp1)
rf.imp_sorted <- arrange( rf.imp1  , desc(MeanDecreaseAccuracy)  )
barplot(rf.imp_sorted$MeanDecreaseAccuracy, ylab="Mean Decrease in Accuracy (Variable Importance)", main="RF Classification Variable Importance Distribution")



################
# XG BOOSTING.
library(vegan)
library(dummies)
library(xgboost) 

data.xg = machinery.original.train
x.xg = machine.train[, names(machine.train) != "y"]
y.xg = machine.train[,"y"]
data.xg$ID = NULL
data.xg = convertToDummies(x.xg)

############ 3
table(y.xg)
data.xg["y"] = y.xg
str(data.xg)
#data.xg$y = ifelse(data.xg$y == "pass", "1", "0")
table(data.xg$y)

str(data.xg)
############ 3
library(DMwR)
library(plyr)
library(dplyr)
xgb.ctrl <- trainControl(method = "cv", number = 10, search='random')

set.seed(1233)
xgb.tune <-train(y~.,
                 data = data.xg,
                 method="xgbTree",
                 trControl=xgb.ctrl,
                 tuneLength=15)
summary(xgb.tune)

xg.final= predict(xgb.tune, newdata= dum_testfin)
table(XgB_final)
pred2=XgB_final
pred2= ifelse(pred2=="0","fail","pass")








#########################
# SVM
#########################

### Spliting Data train and test #####
library("caret")
set.seed(1234)
train_rows <- createDataPartition(traindata_final1$y, p = 0.80, list = F)
train_data <- traindata_final1[train_rows, ]
test_data1 <- traindata_final1[-train_rows, ]

########### cross validation #############
trainctrl.svm <- trainControl(method = "repeatedcv", repeats = 3, number = 3,
                          search='random',
                          allowParallel=T)
library(permute)
library(ggplot2)
svm.model <- train(y ~ .,data = data.xg,trControl = trainctrl.svm,method = "svmRadial",metric = "Accuracy",tuneLength=30)

svm.model






########################
# maxtorque and peak power(drop)
# linear design -> drop

# Mosaic Plot Example
library(vcd)
str(machine.train)
mosaic(machine.train, shade = T, legend = T)

boxplot(machine.train$material.grade, data = machine.train)
library(ggplot2)
ggplot(data=machine.train, aes(x=Number.of.Cylinders, y=Lubrication)) +
  geom_point() +
  labs(title="Machinery Data", x="Number.of.Cylinders", y="Lubrication")

ggplot(data=machine.train, aes(x=machine.train$Valve.Type, y=machine.train$Fuel.Type)) +
  geom_point() +
  labs(title="Machinery Data", x="Valve.Type", y="Fuel.Type")


summary(glm.model)

machine.train$material.grade
machine.train$Lubrication
machine.train$Bearing.Vendor
table(machine.train$main.bearing.type)
table(machine.train$displacement)
table(machine.train$Max..Torque)
table(machine.train$cam.arrangement)
