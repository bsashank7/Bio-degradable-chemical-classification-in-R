library(e1071)
library(caret)
library(rpart)
library(randomForest)
library(party)
df<- read.csv(file.choose(),sep = ';',header = F)
df

####### splitting of data 
sindex<-sample(2,nrow(df),prob=c(0.7,0.3),replace = T)
train_data<-df[sindex==1,]
test_data<-df[sindex==2,]



###### knn #######
trcon<-trainControl(method='repeatedcv',
                    number=10,
                    repeats=10,
                    classProbs = T,
                    summaryFunction = twoClassSummary)

knn_fit<-train(V42~., data=train_data ,
               method='knn',
               tuneLength=20,
               trControl=trcon,
               preProc= c('center','scale'),
               tuneGrid = expand.grid(k=c(1:70)))

knn_fit ## k= 21, ROC=92, sens= 86, spec= 84

pred_train<-predict(knn_fit,newdata = train_data)
pred_test<-predict(knn_fit,newdata = test_data)

conf_train<-table(observed=train_data$V42,predicted=pred_train)
conf_test<-table(observed=test_data$V42,predicted=pred_test)

accuracy_train<-sum(diag(conf_train))/sum(conf_train)
accuracy_test<-sum(diag(conf_test))/sum(conf_test)

accuracy_test;accuracy_train # 81,86

precision(conf_train) # 88
precision(conf_test)  # 80
recall(conf_train)    # 92
recall(conf_test)     # 90



####### decision tree ####

dtmodel<- rpart(V42~.,data=train_data,
                method= 'class',
                parms=list(split='information'))

pred_train<- predict(dtmodel,newdata= train_data,type = 'class')
pred_test<- predict(dtmodel,newdata= test_data,type = 'class')

conf_train<-table(observed=train_data$V42,predicted=pred_train)
conf_test<-table(observed=test_data$V42,predicted=pred_test)

accuracy_train<-sum(diag(conf_train))/sum(conf_train)
accuracy_test<-sum(diag(conf_test))/sum(conf_test)

accuracy_test;accuracy_train # 81,90

     # 86


####### random forest ######
train_data$V42<-as.factor(train_data$V42)
test_data$V42<-as.factor(test_data$V42)

rf <- randomForest(V42~., data= train_data,
                   importance=T,ntree = 300)
pred_train<- predict(rf,newdata= train_data,type = 'class')
pred_test<- predict(rf,newdata= test_data,type = 'class')

conf_train<-table(observed=train_data$V42,predicted=pred_train)
conf_test<-table(observed=test_data$V42,predicted=pred_test)

accu_train<-sum(diag(conf_train))/sum(conf_train)
accu_test<-sum(diag(conf_test))/sum(conf_test)

accu_train;accu_test # 100,86

precision(conf_train) # 1
precision(conf_test)  # 90
recall(conf_train)    # 1
recall(conf_test)     # 88

## tuning rf ##
tune_rf=tuneRF(train_data[,-42],train_data$V42,
               stepFactor = 1,plot = T,
               ntreeTry = 300,trace = T,improve = 0.05)

tune_rf # mtry = 6 


control <- trainControl(method="repeatedcv", number=10, repeats=5, search="grid")
set.seed(111)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(V42~., data=train_data, method="rf",tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
pred_train<- predict(rf_gridsearch,newdata= train_data)
pred_test<- predict(rf_gridsearch,newdata= test_data)

conf_train<-table(observed=train_data$V42,predicted=pred_train)
conf_test<-table(observed=test_data$V42,predicted=pred_test)

accu_train<-sum(diag(conf_train))/sum(conf_train)
accu_test<-sum(diag(conf_test))/sum(conf_test)

accu_train;accu_test

########## svm ######
trcon<-trainControl(method='repeatedcv',
                    number=5,
                    repeats=5)

svm_cv<- train(V42~., data=train_data,
               method='svmRadial',
               tuneGrid=expand.grid(C=seq(0,5,length=20),sigma=10^(-7:1)),
               preProc=c('center','scale'),
               trControl=trcon)

pred_train<- predict(svm_cv,newdata= train_data)
pred_test<- predict(svm_cv,newdata= test_data)

conf_train<-table(observed=train_data$V42,predicted=pred_train)
conf_test<-table(observed=test_data$V42,predicted=pred_test)

accu_train<-sum(diag(conf_train))/sum(conf_train)
accu_test<-sum(diag(conf_test))/sum(conf_test)

accu_train;accu_test # 96,84

precision(conf_train) # 98
precision(conf_test)  # 87
recall(conf_train)    # 97
recall(conf_test)     # 88


####### logistic regression ######

preproc2 <- preProcess(train_data[,-42], method=c("range"))

train_data_sc <- predict(preproc2, train_data[,-42])
test_data_sc<-predict(preproc2, test_data[,-42])

train_data_sc$V42<-train_data$V42
test_data_sc$V42<-test_data$V42

logimodel <- glm(V42~. , data=train_data_sc,family = binomial(link = 'logit'))


pred_train<- predict(logimodel,newdata= train_data_sc,type = 'response')
pred_test<- predict(logimodel,newdata= test_data_sc,type = 'response')

summary(logimodel)

pred_train_class <- ifelse(pred_train > 0.7, "RB", "NRB")
pred_test_class <- ifelse(pred_test > 0.7, "RB", "NRB")

conf_train<-table(observed=train_data$V42,predicted=pred_train_class)
conf_test<-table(observed=test_data$V42,predicted=pred_test_class)

accu_train<-sum(diag(conf_train))/sum(conf_train)
accu_test<-sum(diag(conf_test))/sum(conf_test)

accu_train;accu_test # 89,83

precision(conf_train) # 97
precision(conf_test)  # 89
recall(conf_train)    # 88
recall(conf_test)     # 85

