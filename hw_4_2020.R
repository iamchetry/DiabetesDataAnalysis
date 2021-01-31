#setwd('../iamchetry/Documents/UB_files/506/hw_4/')

#install.packages("klaR")
#install.packages("MASS")

#----------------- Loading Libraries -------------------

library(klaR)
library(MASS)
library(caret)
library(ISLR)
library(pROC)


#--------------------------------- 1st Question -------------------------------------

load('Diabetes.RData')
data_ = Diabetes
attach(data_)
head(data_)
names(data_)
table(group)

#Pairwise Scatter plots
col = c("blue", "darkgreen", "red")[group]
pch = c(16,15,17)[group]
plot(data_[,1:5], col=col, pch=pch, main='Pairwise Scatter Plots')

set.seed(2)
t = createDataPartition(group, p=0.7, list = FALSE)
train_ = na.omit(data_[t, ])
test_ = na.omit(data_[-t, ])

y_true_train = as.numeric(train_$group)-1
y_true_test = as.numeric(test_$group)-1

lda_ = lda(group~., data=as.data.frame(train_))
print(summary(lda_))

train_preds = predict(lda_, newdata = as.data.frame(train_))
y_pred_train = as.numeric(train_preds$class)-1

test_preds = predict(lda_, newdata = as.data.frame(test_))
y_pred_test = as.numeric(test_preds$class)-1

tab_train = table(y_pred_train, y_true_train)
tab_test = table(y_pred_test, y_true_test)

#--------- Confusion Matrix to determine Accuracy ---------
conf_train = confusionMatrix(tab_train) 
conf_test = confusionMatrix(tab_test)

train_error = 1 - round(conf_train$overall['Accuracy'], 4) # Training Error
test_error = 1 - round(conf_test$overall['Accuracy'], 4) # Testing Error

print(train_error)
print(test_error)

#-------------------------------------------

qda_ = qda(group~., data=as.data.frame(train_))

train_preds = predict(qda_, newdata = as.data.frame(train_))
y_pred_train = as.numeric(train_preds$class)-1

test_preds = predict(qda_, newdata = as.data.frame(test_))
y_pred_test = as.numeric(test_preds$class)-1

tab_train = table(y_pred_train, y_true_train)
tab_test = table(y_pred_test, y_true_test)

#--------- Confusion Matrix to determine Accuracy ---------
conf_train = confusionMatrix(tab_train) 
conf_test = confusionMatrix(tab_test)

train_error = 1 - round(conf_train$overall['Accuracy'], 4) # Training Error
test_error = 1 - round(conf_test$overall['Accuracy'], 4) # Testing Error

print(train_error)
print(test_error)

#------------------------------------------------

test_ = data.frame(relwt = c(1.86),
                   glufast = c(184),
                   glutest = c(68),
                   instest = c(122),
                   sspg = c(544),
                   group = c('x')
)

y_lda = predict(lda_, newdata = test_)
y_qda = predict(qda_, newdata = test_)


#--------------------------------- 2nd Question -------------------------------------

head(Weekly)
attach(Weekly)
table(Direction)
table(Year)

temp_data = Weekly
temp_data$lag1_diff = temp_data$Today-temp_data$Lag1
temp_data$lag2_diff = temp_data$Today-temp_data$Lag2
temp_data$lag3_diff = temp_data$Today-temp_data$Lag3
temp_data$lag4_diff = temp_data$Today-temp_data$Lag4
temp_data$lag5_diff = temp_data$Today-temp_data$Lag5

par(mfrow = c(3,2))
densityplot(~lag1_diff | Direction, data = temp_data,
            auto.key = list(space = "right"), groups = Direction,
            main='Today-Lag1 vs Direction') 
densityplot(~lag2_diff | Direction, data = temp_data,
            auto.key = list(space = "right"), groups = Direction,
            main='Today-Lag2 vs Direction')
densityplot(~lag3_diff | Direction, data = temp_data,
            auto.key = list(space = "right"), groups = Direction,
            main='Today-Lag3 vs Direction') 
densityplot(~lag4_diff | Direction, data = temp_data,
            auto.key = list(space = "right"), groups = Direction,
            main='Today-Lag4 vs Direction') 
densityplot(~lag5_diff | Direction, data = temp_data,
            auto.key = list(space = "right"), groups = Direction,
            main='Today-Lag5 vs Direction')
plot(Volume~Year, data = Weekly)

# GLM
logreg = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly,
             family='binomial')
print(summary(logreg))
train_preds = predict(logreg, newdata = Weekly, type='response')

#Finding optimal threshold to calculate decision boundary
train_actual = as.factor(as.numeric(Weekly$Direction)-1)
analysis = roc(response=train_actual, predictor=train_preds)
e = cbind(analysis$thresholds, analysis$sensitivities+analysis$specificities)
opt_t = subset(e,e[,2]==max(e[,2]))[,1]

train_preds = ifelse(train_preds >= opt_t, '1', '0') # Train Prediction

tab_train = table(train_preds, train_actual)

#--------- Confusion Matrix to determine Accuracy ---------
conf_train = confusionMatrix(tab_train) 
print(conf_train)

train_error = 1 - round(conf_train$overall['Accuracy'], 4) # Training Error

print(train_error)

#Part D
train_ = subset(Weekly, Year>=1990 & Year<=2008)
test_ = subset(Weekly, Year>=2009 & Year<=2010)

logreg = glm(Direction~Lag2, data=train_, family='binomial')
print(summary(logreg))
train_preds = predict(logreg, newdata = train_, type='response')
test_preds = predict(logreg, newdata = test_, type='response')

#Finding optimal threshold to calculate decision boundary
train_actual = as.factor(as.numeric(train_$Direction)-1)
analysis = roc(response=train_actual, predictor=train_preds)
e = cbind(analysis$thresholds, analysis$sensitivities+analysis$specificities)
opt_t = subset(e,e[,2]==max(e[,2]))[,1]

train_preds = ifelse(train_preds >= opt_t, '1', '0') # Train Prediction
test_preds = ifelse(test_preds >= opt_t, '1', '0') # Test Prediction

test_actual = as.factor(as.numeric(test_$Direction)-1)
tab_train = table(train_preds, train_actual)
tab_test = table(test_preds, test_actual)

#--------- Confusion Matrix to determine Accuracy ---------
conf_train = confusionMatrix(tab_train) 
conf_test = confusionMatrix(tab_test) 

train_error = 1 - round(conf_train$overall['Accuracy'], 4) # Training Error
test_error = 1 - round(conf_test$overall['Accuracy'], 4) # Testing Error

print(train_error)
print(test_error)
print(conf_test)


# LDA

y_true_train = as.numeric(train_$Direction)-1
y_true_test = as.numeric(test_$Direction)-1

lda_ = lda(Direction~Lag2, data=train_)

train_preds = predict(lda_, newdata = train_)
y_pred_train = as.numeric(train_preds$class)-1

test_preds = predict(lda_, newdata = test_)
y_pred_test = as.numeric(test_preds$class)-1

tab_train = table(y_pred_train, y_true_train)
tab_test = table(y_pred_test, y_true_test)

#--------- Confusion Matrix to determine Accuracy ---------
conf_train = confusionMatrix(tab_train) 
conf_test = confusionMatrix(tab_test)

train_error = 1 - round(conf_train$overall['Accuracy'], 4) # Training Error
test_error = 1 - round(conf_test$overall['Accuracy'], 4) # Testing Error

print(train_error)
print(test_error)
print(conf_test)

# KNN
require(class)
KNN_train = knn(train_['Lag2'], train_['Lag2'], train_$Direction, 1) # Train Prediction
KNN_test = knn(train_['Lag2'], test_['Lag2'], train_$Direction, 1) # Test Prediction

train_predicted = as.factor(KNN_train)
test_predicted = as.factor(KNN_test)

train_actual = as.factor(train_$Direction)
test_actual = as.factor(test_$Direction)

tab_train = table(train_predicted, train_actual)
tab_test = table(test_predicted, test_actual)

#-------- Confusion Matrix for Accuracy ---------
conf_train = confusionMatrix(tab_train)
conf_test = confusionMatrix(tab_test)

train_error = 1 - round(conf_train$overall['Accuracy'], 4)
test_error = 1 - round(conf_test$overall['Accuracy'], 4)

print(train_error)
print(test_error)
print(conf_test)










