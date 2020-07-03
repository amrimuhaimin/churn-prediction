#Import data and check structur of data
data <- read.csv('TelcoChurn.csv', header=T, sep=",", stringsAsFactors = T)
str(data)
data$Churn

#missing values and balance check
library(ggplot2)
plot(is.na(data$TotalCharges), ylab='Missing Status', type="p", pch=20, cex=1.5, col="dark red") #missing values visualize
ggplot(data, aes(x=Churn)) + geom_bar(aes(y=(..count..)/sum(..count..)), fill=c("dark blue",'dark red')) +
  ylim(c(0,1)) + ylab("Percentage Of Churn") + theme_classic()
balance.value <- prop.test(sum(as.numeric(data$Churn)-1), nrow(data) , 0.5) #balance test
print(balance.value)

#Exploratory data analysis
p.function <- function(df,xaxis,yaxis){
  yaxis <- grep(yaxis, names(df))
  xaxis <- grep(xaxis, names(df))
  ggplot(data, aes(as.factor(df[,yaxis]))) + 
    geom_bar(aes(fill=df[,xaxis]), position='fill', width=0.25) +
    ylab("Percentage") + xlab(paste(names(df)[yaxis])) +
    guides(fill=guide_legend(title="Churn Status")) +
    ggtitle(paste(names(df)[yaxis])) + theme_bw()
}
p.gender <- p.function(data, "Churn", "gender")
p.scitizen <- p.function(data, "Churn", "SeniorCitizen")
p.partner <- p.function(data, "Churn", "Partner")
p.dependents <- p.function(data, "Churn", "Dependents")
p.phoneservice <- p.function(data, "Churn", "PhoneService")
p.multiplelines <- p.function(data, "Churn", "MultipleLines")
p.internetservice <- p.function(data, "Churn", "InternetService")
p.onlinesecurity <- p.function(data, "Churn", "OnlineSecurity")
p.onlinebackup <- p.function(data, "Churn", "OnlineBackup")
p.devicep <- p.function(data, "Churn", "DeviceProtection")
p.techsup <- p.function(data, "Churn", "TechSupport")
p.streamingtv <- p.function(data, "Churn", "StreamingTV")
p.streamingmovies <- p.function(data, "Churn", "StreamingMovies")
p.contract <- p.function(data, "Churn", "Contract")
p.paperless <- p.function(data, "Churn", "PaperlessBilling")
p.payment <- p.function(data, "Churn", "PaymentMethod")
library(cowplot)
plot_grid(p.gender, p.scitizen, p.partner, p.dependents, 
          p.phoneservice, p.multiplelines, p.internetservice, p.onlinesecurity,
          nrow=4, ncol=2)
plot_grid(p.onlinebackup, p.devicep, p.techsup, p.streamingtv, 
          p.streamingmovies, p.contract, p.paperless, p.payment,
          nrow=4, ncol=2)
plot_grid(ggplot(data, aes(y = tenure, x= "", fill = Churn)) + geom_boxplot() +
            theme_bw() + xlab(" ") + ylab("Tenure (Months)"),
          ggplot(data, aes(x= tenure)) + geom_histogram(color='black', aes(fill= Churn)) +
            theme_bw() + xlab("Tenure") + ylab("Freq"),
          ggplot(data, aes(y = MonthlyCharges, x="", fill=Churn)) + geom_boxplot() +
            theme_bw() + xlab(" ") + ylab("Monthly Charges ($)"),
          ggplot(data, aes(x= MonthlyCharges)) + geom_histogram(color='black', aes(fill= Churn)) +
            theme_bw() + xlab("Monthly charges") + ylab("Freq"),
          ggplot(data, aes(y=TotalCharges, x="", fill=Churn)) + geom_boxplot(na.rm=T) +
            theme_bw() + xlab(" ") + ylab("Total Charges ($)"),
          ggplot(data, aes(x= TotalCharges)) + geom_histogram(color='black', aes(fill= Churn), na.rm=T) +
            theme_bw() + xlab("Total charges") + ylab("Freq"),
          nrow = 3, ncol = 2)

#pre processing
#split into train and test
data <- data[,-1]
data$SeniorCitizen <- as.factor(data$SeniorCitizen)
set.seed(112)
ind.split <- sample(2, nrow(data), replace=T, prob=c(0.7, 0.3))
testdata <- data[ind.split==2,]
trainval <- data[ind.split==1,]
trainval$TotalCharges[is.na(trainval$TotalCharges)] = median(trainval$TotalCharges, na.rm=T)
testdata$TotalCharges[is.na(testdata$TotalCharges)] = median(testdata$TotalCharges, na.rm=T)

#feature selection
library(caret)
backward.fs <- MASS::stepAIC(glm(Churn~., trainval, family=binomial(link="logit")),direction="both")
backward.fs;predictors(backward.fs)
trainval.fs <- trainval[predictors(backward.fs)]
test.fs <- testdata[predictors(backward.fs)]
test.fs$Churn <- testdata$Churn
trainval.fs$Churn <- trainval$Churn

#tune parameter
#random forest
control_rf <- trainControl(method="cv", number=5, search = 'grid')
tunegrid_rf <- expand.grid(.mtry=c(sqrt(ncol(trainval.fs))))
modellist <- list()
for (ntree in c(1000, 1500, 2000)) {
  set.seed(123)
  fit <- train(Churn~., data=trainval.fs, method="rf", metric="Accuracy", tuneGrid=tunegrid_rf,
               trControl=control_rf, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results to get best parameter
results_rf <- resamples(modellist)
summary(results_rf)
dotplot(results_rf)

#knn
set.seed(400)
ctrl_knn <- trainControl(method="cv",number = 5)
knnFit <- train(Churn ~ ., data = trainval.fs, method = "knn", trControl = ctrl_knn, preProcess = c("center","scale"), tuneLength = 20)
print(knnFit)
plot(knnFit)

#rpart
ctrl_tree <- trainControl(method='cv',number=5,classProbs=T,summaryFunction=multiClassSummary)
cartfit <- train(Churn~., data=trainval.fs, method='rpart', trControl=ctrl_tree, tuneLength=20)
cartfit


#model evaluation part 1
eval.test <- function(train, test, fml){
  if(sum(is.na(train$TotalCharges)) > 0){
    train$TotalCharges[is.na(train$TotalCharges)] = median(train$TotalCharges, na.rm=T)  
  } else {
    train <- train
  }
  if(sum(is.na(test$TotalCharges)) > 0){
    test$TotalCharges[is.na(test$TotalCharges)] = median(test$TotalCharges, na.rm=T)  
  } else {
    test <- test
  }
  modellog <- glm(fml, train, family=binomial(link="logit"))
  modelnb <- naivebayes::naive_bayes(fml, train)
  modelknn <- caret::knn3Train(sapply(train[,-(grep("Churn",names(train)))], as.numeric),
                               sapply(test[,-(grep("Churn",names(test)))], as.numeric),
                               cl=train[,grep("Churn", names(train))], k=31, prob=T)
  modeltree <- rpart::rpart(fml, train, method='class',cp=0.001108647)
  modelrf <- randomForest::randomForest(fml, train, ntree=500)
  labellog <- ifelse(predict(modellog, test[,-(grep("Churn", names(test)))], type='response') < 0.5, "No", "Yes")
  labelnb <- predict(modelnb, test[,-(grep("Churn", names(test)))], type="class")
  labelknn <- as.factor(modelknn)
  labeltree <- predict(modeltree, test[,-(grep("Churn", names(test)))], type='class')
  labelrf <- predict(modelrf, test[,-(grep("Churn", names(test)))], type="class")
  evallog <- confusionMatrix(as.factor(labellog), test$Churn, positive="Yes")
  evalnb <- confusionMatrix(labelnb, test$Churn, positive="Yes")
  evalknn <- confusionMatrix(labelknn, test$Churn, positive="Yes")
  evaltree <- confusionMatrix(labeltree, test$Churn, positive="Yes")
  evalrf <- confusionMatrix(labelrf, test$Churn, positive="Yes")
  predlog <- predict(modellog, test[,-(grep("Churn", names(test)))], type='response');predlog <- prediction(predlog, test$Churn)
  prednb <- predict(modelnb, test[,-(grep("Churn", names(test)))], type='prob');prednb <- prediction(prednb[,2], test$Churn)
  predknn <- prediction(attributes(modelknn)$prob[,2], test$Churn)
  predtree <- predict(modeltree, test[,-(grep("Churn", names(test)))], type="prob");predtree <- prediction(predtree[,2], test$Churn)
  predrf <- predict(modelrf, test[,-(grep("Churn", names(test)))], type="prob");predrf <- prediction(predrf[,2], test$Churn)
  auclog <- performance(predlog, 'auc')@y.values[[1]]
  aucnb <- performance(prednb, 'auc')@y.values[[1]]
  aucknn <- performance(predknn, 'auc')@y.values[[1]]
  auctree <- performance(predtree, 'auc')@y.values[[1]]
  aucrf <- performance(predrf, 'auc')@y.values[[1]]
  return(list(logistic=list(evallog,auclog), naive_bayes=list(evalnb,aucnb),
              knn=list(evalknn,aucknn), dctree=list(evaltree,auctree), randomforest=list(evalrf,aucrf)))
}
eval1 <- eval.test(trainval.fs,test.fs,formula(Churn~.))
eval1

#oversampling and feature engineering
trainset <- data.frame(lapply(trainval, function(x) {gsub("No internet service", "No", x)})) # no internet service > no
trainset <- data.frame(lapply(trainset, function(x) {gsub("No phone service", "No", x)})) # no phone service > no
trainset$tenure <- as.numeric(trainset$tenure);trainset$MonthlyCharges <- as.numeric(trainset$MonthlyCharges)
trainset$TotalCharges <- as.numeric(trainset$TotalCharges)
testset <- data.frame(lapply(testdata, function(x) {gsub("No internet service", "No", x)})) # no internet service > no
testset <- data.frame(lapply(testset, function(x) {gsub("No phone service", "No", x)})) # no phone service > no
testset$tenure <- as.numeric(testset$tenure);testset$MonthlyCharges <- as.numeric(testset$MonthlyCharges)
testset$TotalCharges <- as.numeric(testset$TotalCharges)
trainset <- dplyr::mutate(trainset, tenure_bin=tenure)
trainset$tenure_bin[trainset$tenure_bin >= 0 & trainset$tenure_bin <= 12] <- '0-1 year'
trainset$tenure_bin[trainset$tenure_bin > 12 & trainset$tenure_bin <= 24] <- '1-2 years'
trainset$tenure_bin[trainset$tenure_bin > 24 & trainset$tenure_bin <= 36] <- '2-3 years'
trainset$tenure_bin[trainset$tenure_bin > 36 & trainset$tenure_bin <= 48] <- '3-4 years'
trainset$tenure_bin[trainset$tenure_bin > 48 & trainset$tenure_bin <= 60] <- '4-5 years'
trainset$tenure_bin[trainset$tenure_bin > 60 & trainset$tenure_bin <= 73] <- '5-6 years'
trainset$tenure_bin <- as.factor(trainset$tenure_bin)
MonthlyCharges.f <- ifelse(trainset$MonthlyCharges > median(trainset$MonthlyCharges), "1","2")
TotalCharges.f <- ifelse(trainset$TotalCharges > median(trainset$TotalCharges), "1","2")
trainset$TotalCharges <- TotalCharges.f
trainset$MonthlyCharges <- MonthlyCharges.f
trainset$TotalCharges <- as.factor(trainset$TotalCharges); trainset$MonthlyCharges <- as.factor(trainset$MonthlyCharges)
trainset <- trainset[,-5]
testset <- dplyr::mutate(testset, tenure_bin=tenure)
testset$tenure_bin[testset$tenure_bin >= 0 & testset$tenure_bin <= 12] <- '0-1 year'
testset$tenure_bin[testset$tenure_bin > 12 & testset$tenure_bin <= 24] <- '1-2 years'
testset$tenure_bin[testset$tenure_bin > 24 & testset$tenure_bin <= 36] <- '2-3 years'
testset$tenure_bin[testset$tenure_bin > 36 & testset$tenure_bin <= 48] <- '3-4 years'
testset$tenure_bin[testset$tenure_bin > 48 & testset$tenure_bin <= 60] <- '4-5 years'
testset$tenure_bin[testset$tenure_bin > 60 & testset$tenure_bin <= 73] <- '5-6 years'
testset$tenure_bin <- as.factor(testset$tenure_bin)
MonthlyCharges.f <- ifelse(testset$MonthlyCharges > median(testset$MonthlyCharges), "1","2")
TotalCharges.f <- ifelse(testset$TotalCharges > median(testset$TotalCharges), "1","2")
testset$TotalCharges <- TotalCharges.f
testset$MonthlyCharges <- MonthlyCharges.f
testset$TotalCharges <- as.factor(testset$TotalCharges); testset$MonthlyCharges <- as.factor(testset$MonthlyCharges)
testset <- testset[,-5]
str(trainset);str(testset)
trainset[names(trainset)] <- lapply(trainset[names(trainset)],factor)
testset[names(testset)] <- lapply(testset[names(testset)],factor)

#oversampling
library(UBL)
summary(is.na(trainset))
perc <- summary(trainset$Churn)[1]/summary(trainset$Churn)[2]
trainset.bl <- SmoteClassif(Churn~., trainset, C.perc=list("No"=1, "Yes"=perc), dist="HVDM", k=10)

#reselect feature
backward2.fs <- MASS::stepAIC(glm(Churn~., trainset.bl, family=binomial(link=logit)), direction='both')
trainset.fs <- trainset.bl[predictors(backward2.fs)]
trainset.fs$Churn <- trainset.bl$Churn
testset.fs <- testset[predictors(backward2.fs)]
testset.fs$Churn <- testset$Churn
backward2.fs;predictors(backward2.fs)

#retune the parameter
#random forest
control_rf <- trainControl(method="cv", number=5, search = 'grid')
tunegrid_rf <- expand.grid(.mtry=c(sqrt(ncol(trainval.fs))))
modellist <- list()
for (ntree in c(1000, 1500, 2000)) {
  set.seed(123)
  fit <- train(Churn~., data=trainset.fs, method="rf", metric="Accuracy", tuneGrid=tunegrid_rf,
               trControl=control_rf, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results_rf <- resamples(modellist)
summary(results_rf)
dotplot(results_rf)

#knn
set.seed(400)
ctrl_knn <- trainControl(method="cv",number = 5)
knnFit <- train(Churn ~ ., data = trainset.fs, method = "knn", trControl = ctrl_knn, preProcess = c("center","scale"), tuneLength = 20)
print(knnFit)
plot(knnFit)

#rpart
ctrl_tree <- trainControl(method='cv',number=5,classProbs=T,summaryFunction=multiClassSummary)
cartfit <- train(Churn~., data=trainset.fs, method='rpart', trControl=ctrl_tree, tuneLength=20)
cartfit

#model eval 2
eval2 <- eval.test(trainset.fs, testset.fs, formula(Churn~.))
eval2
