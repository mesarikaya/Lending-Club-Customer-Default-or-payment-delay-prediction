library(caret)
library(memoise)
library(caTools)
library(randomForest)
library(gbm)
library(plyr)
library(ggplot2)
library(doParallel)
library(data.table)
library(lattice)
library(lubridate)
library(xgboost)

# Decide the data size
# m: the data size

# Set a seed for consistency
set.seed(100)


# Create train, test and validation data
segment_data<-function(file,m){
    force(file)
    force(m)  
    file<-subset(file,!(file$loan_status=="Current" | file$loan_status=="Does not meet the credit policy. Status:Charged Off" | file$loan_status=="Does not meet the credit policy. Status:Fully Paid" | file$loan_status==""))
    # Set the data to size m
    datasize<<-m
    data<-sample(1:nrow(file), m)
    data<-as.data.frame(file[data,])
    maindata<<-generic_preprocess(data)
    data<-maindata
  
    # Use 70% for train and valid, 30% for test from the selected data
    inTrain_valid <- sample(1:nrow(data), floor(dim(data)[1]*0.7))

    #Set the train and validation data separately
    Train_valid<-data[inTrain_valid,]
    inTrain<-sample(1:nrow(Train_valid), floor(dim(Train_valid)[1]*0.7))
    
    # Assign the data to each segment
    Train<-Train_valid[inTrain,]
    
    Valid<-Train_valid[-inTrain,]
    # Remove the single factor columns
    Test <- data[-inTrain_valid,]
    
    # Create a list to retrun the files
    out<-list(Train,Valid,Test)
    
    return(out)
 
}

generic_preprocess<-function(datasets){
  force(datasets)
  datasets<-as.data.frame(datasets)
  
  exclude_names<-c("id","member_id","out_prncp","out_prncp_inv","url","desc")
  for (features in exclude_names){
    datasets<-datasets[-which(colnames(datasets)==features)]
  }

  # datasets<-datasets[-c("out_prncp","out_prncp_inv ")]
  x<-datasets
  datasets<-unique(x)
  
  # Remove the single factor columns
  datasets<-datasets[, sapply(datasets, function(col) length(unique(col))) > 1]

  # correct int_rate to integer
  datasets$int_rate<-gsub(" ","", datasets$int_rate)
  datasets$int_rate<-gsub("%","", datasets$int_rate)
  datasets$int_rate<-as.numeric(datasets$int_rate)
  
  # correct revol_util to integer
  datasets$revol_util<-gsub(" ","", datasets$revol_util)
  datasets$revol_util<-gsub("%","", datasets$revol_util)  
  datasets$revol_util<-as.numeric(datasets$revol_util)
  
  
  # #Convert Dates into numeric numbers
  datasets$issue_d <- mdy(datasets$issue_d)
  datasets$issue_d<-as.numeric(datasets$issue_d)
  datasets$earliest_cr_line <- mdy(datasets$earliest_cr_line)
  datasets$earliest_cr_line<-as.numeric(datasets$earliest_cr_line)
  datasets$last_pymnt_d <- mdy(datasets$last_pymnt_d)
  datasets$last_pymnt_d<-as.numeric(datasets$last_pymnt_d)
  datasets$next_pymnt_d <- mdy(datasets$next_pymnt_d)
  datasets$next_pymnt_d<-as.numeric(datasets$next_pymnt_d)
  datasets$last_credit_pull_d <- mdy(datasets$last_credit_pull_d)
  datasets$last_credit_pull_d<-as.numeric(datasets$last_credit_pull_d)
  
  datasets[is.na(datasets)]<- -1000
 
   for (columns in colnames(datasets)){
    if (is.character(datasets[,columns])& !columns=="loan_status") {
      datasets[,columns]<-factor(datasets[,columns])
    }
  }
  
  out<-as.data.frame(datasets)
  return(out)
  
}



# Preprocess data
preprocess<-function(datasets){
  force(datasets)

  for (k in 1:length(datasets)){
    # Set the data and process "NA" values
    datasets[[k]]<-as.data.frame(datasets[[k]])
    if (k>1) {  
      b<-colnames(datasets[[k]]) %in% colnames(datasets[[1]])
      datasets[[k]]<-datasets[[k]][,which(b==TRUE)]
      datasets[[k]]<-generic_preprocess(datasets[[k]])
      # datasets[[k]][is.na(datasets[[k]])]<- -1000
    }
    else{
      datasets[[k]]<-datasets[[k]][-1]
      datasets[[k]]<-datasets[[k]][-1]
      # Remove columns that should not be used for prediction
      datasets[[k]]<-datasets[[k]][-34]
      datasets[[k]]<-datasets[[k]][-34]
      datasets[[k]]<-datasets[[k]][-34]
      datasets[[k]]<-datasets[[k]][-8]
      datasets[[k]]<-generic_preprocess(datasets[[k]])
    }
  }
  
  # Set the lists to data frame for train, validation and test sets
  #Convert all 'chr' columns to factors
  #data<-as.data.frame(apply(data,2,convert_factor) )
  train<-as.data.frame(datasets[[1]])
  valid<-as.data.frame(datasets[[2]])
  test<-as.data.frame(datasets[[3]])
  # return the data sets
  out<-list(train,valid,test)
  
  return(out)
}

# convert_factor<-function(x) {
#   force(x)
# 
#     if(!is.character(x)==TRUE){ 
#         x<-factor(x)
#        # x<-as.numeric(x)
#     }
#     #x<- factor(x)
#   return(x)
#     
# }

# Create Learning curve to see if there exists bias or variance with selected model

model_run<-function(sets,method,...){
 
  force(sets)
  force(method)
  traindata<<-sets[[1]]
  validdata<<-sets[[2]]
  testdata<<-sets[[3]]
  

  
  for (columns in colnames(traindata)){
    if (is.factor(maindata[columns])){
      levels(traindata[columns])<-union(levels(traindata[columns]), levels(maindata[columns]))
      levels(validdata[columns])<-union(levels(validdata[columns]), levels(maindata[columns]))
      levels(testdata[columns])<-union(levels(testdata[columns]), levels(maindata[columns]))
    }
  }
  print(str(traindata))
  print(str(validdata))
  print(str(testdata))
  # std_levels(traindata,validdata)
  # std_levels(validdata,testdata)
  train_fit<-train(loan_status~., data = traindata ,method = method,...)
  #set levels same for character columns
  #levelchar<-apply(traindata,function(x) )
  valid_fit<-predict(train_fit,validdata)
  test_fit<-predict(train_fit,testdata)
  print(train_fit)
  out<-list(train_fit,valid_fit,test_fit)
  return(out)
  
}

# std_levels<-function(train,valid){
#   
#   if (is.factor(valid)){
#     # loops through factors and standardizes the levels
#     for (f in 1:length(names(train))) {
#         levels(valid[,f]) = union(levels(valid[,f]),levels(train[,f]))
#     }
#   }
# }

general_call_err<-function(act,pred){
  force(pred)
  force(act)
  conf_matrix<-createConfusionMatrix(act, pred)
  err_rate<-(sum(conf_matrix)-sum(diag(conf_matrix)))/sum(conf_matrix)*!00
  return(err_rate)
}

calc_err_rate<-function(model){
  force(model)
  train_model<-model[[1]]
  valid_pred<-model[[2]]

  #ncol2<-ncol(valid_model$finalModel$confusion)
  
  
  
  #valid_model_confusion<-valid_model$finalModel$confusion[,-ncol2,drop=FALSE]
  ncol<-ncol(train_model$finalModel$confusion)
  train_model_confusion<-train_model$finalModel$confusion[,-length(train_model$finalModel$confusion)]
  train_err<-sum(train_model_confusion)-sum(diag(train_model_confusion))
  train_err_rate<-((train_err/sum(train_model_confusion)))*100
  train_err_rate<-as.data.frame(train_err_rate)
  
  valid_model_confusion<-createConfusionMatrix(validdata$loan_status, valid_pred)
  valid_err<-sum(valid_model_confusion)-sum(diag(valid_model_confusion))
  valid_err_rate<-((valid_err/sum(valid_model_confusion)))*100
  #valid_err_rate<-general_call_err(validdata$loanstatus,valid_pred)
  valid_err_rate<-as.data.frame(valid_err_rate)

  names(train_err_rate)<-names(valid_err_rate)

  train_err_rate$errortype<-"training"
  train_err_rate$datasize<-datasize
  valid_err_rate$errortype<-"validation"
  valid_err_rate$datasize<-datasize
  
  out<-rbind(train_err_rate,valid_err_rate)
  out<-as.data.frame(out)
  return(out)
}


createConfusionMatrix <- function(act, pred) {
  # You've mentioned that neither actual nor predicted may give a complete
  # picture of the available classes, hence:
  numClasses <- max(length(levels(act)), length(levels(pred)))
  # Sort predicted and actual as it simplifies what's next. You can make this
  # faster by storing `order(act)` in a temporary variable.
  pred <- pred[order(act)]
  act  <- act[order(act)]
  result<-sapply(split(pred, act), tabulate, nbins=numClasses)
  return(result)
}

z<-0
general_call<-function(file,datasize,method,...) {
  force(file)
  force(datasize)
  force(method)
  
  result<-calc_err_rate(model_run(preprocess(segment_data(file,datasize)),method=method,...))
  z<<-z+1 
  print(z)
  return(result)

}



create_learning_curve<- function (result){
  force(result)
  
      for (i in length(result)){
        if (i==1) {
          result[[i]]<-as.data.table(result[[i]])
        }
        else{
          result[[i]]<-as.data.table(rbind(result[[i-1]],result[[i]]))
        }
      } 

  result<-result[[length(result)]]

  result<-as.data.frame(result,stringsAsFactors=TRUE)
  
#   plot(result$error ~ result$data_size, data=result, groups=factor(result$error_type))
#          scales = list(x = list(at = 1:4, labels = levels(trans.factor))))
  g<-ggplot(data=result,aes(x= datasize, y=valid_err_rate,linetype=factor(errortype)))+
  geom_line(size=1,aes(color=factor(errortype)))+ 
  labs(title ="Learning Curve", x = "Training set size", y = "Accuracy Error")+
  theme(plot.title = element_text(size=30))+
  theme_bw()
  return(g)
   
}

run_model<-function(file){
  cl <- makeCluster(6)
  registerDoParallel(cl)
  finalmodel<-lapply(seq(200000,200000,1000),function(x) general_call(file=file,datasize=x,method="rf",ntree=5000,mtry=10) )
  stopCluster(cl)
  return(finalmodel)
}