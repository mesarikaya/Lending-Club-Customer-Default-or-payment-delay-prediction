# this code aims to create three types of functions

# 1. To preprocess the past data
# 2. To preprocess the current data in line with the past data
# 3. To run a random forest algorithm to see if it is possible to predict the loan status

# Load relevant libraries
library(caret)
library(caTools)
library(randomForest)
library(gbm)
library(plyr)
library(dplyr)
library(ggplot2)
library(doParallel)
library(data.table)
library(lattice)
library(lubridate)
library(xgboost)
library(e1071)
library(doSNOW)

# Decide the data size
# m: the data size

# Create train, test and validation data
segment_data<-function(file,m){
  force(file)
  force(m)  
  #file<-subset(file,!(file$loan_status=="Current" | file$loan_status=="Does not meet the credit policy. Status:Charged Off" | file$loan_status=="Does not meet the credit policy. Status:Fully Paid" | file$loan_status==""))
  # Set the data to size m and create data frame from it
  datasize<<-m
  data<-sample(1:nrow(file), m)
  data<-as.data.frame(file[data,])
  data$payment_gap<-data$loan_amnt-data$total_pymnt
  
  #Call the preprocess function to arrange the data
  data<-generic_preprocess(data)
  
  
  # Use 70% for train and valid, 30% for test from the selected data
  inTrain_valid <- sample(1:nrow(data), floor(dim(data)[1]*0.7))
  
  # Set the train and validation data separately
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
  
  today <- Sys.Date()
  today<-as.numeric(mdy(format(today, "%b-%Y")))
  
  #Exclude names that are irrelevant or too biased in preiction 
  exclude_names<-c("id","next_pymnt_d","member_id","earliest_cr_line","revol_util","funded_amnt", "funded_amnt_inv", "out_prncp","out_prncp_inv","url","desc","total_rec_prncp","collection_recovery_fee","recoveries","emp_title","title","zip_code")
  for (features in exclude_names){
    datasets<-datasets[-which(colnames(datasets)==features)]
  }
  check<<-datasets$loan_status
  # datasets<-datasets[-c("out_prncp","out_prncp_inv ")]

  # Remove the single factor columns
  datasets<-datasets[, sapply(datasets, function(col) length(unique(col))) > 1]
  
  # correct int_rate to integer
  datasets$int_rate<-gsub(" ","", datasets$int_rate)
  datasets$int_rate<-gsub("%","", datasets$int_rate)
  datasets$int_rate<-as.numeric(datasets$int_rate)
  
  #Convert important dates into numeric numbers
  datasets$issue_d <- as.numeric((today-as.numeric(mdy(datasets$issue_d))))
  datasets$last_pymnt_d <- as.numeric((today-as.numeric(mdy(datasets$last_pymnt_d))))
  datasets$last_credit_pull_d <- as.numeric((today-as.numeric(mdy(datasets$last_credit_pull_d))))

  
  # Take out highly correlated variables and rearrange the dataset
  datasets_rest<-datasets[,!sapply(datasets,function(x) is.numeric(x))]
  datasets_num<-datasets[,sapply(datasets,function(x) is.numeric(x))]
  descrCor <-  cor(datasets_num[,sapply(datasets_num,function(x) is.numeric(x))],use="pairwise.complete.obs")
  highlyCorDescr <- findCorrelation(descrCor, cutoff = .90,exact=FALSE,names=TRUE)
  highlyCorDescr<-highlyCorDescr[is.na(highlyCorDescr)==FALSE]
  highlyCorDescr<-highlyCorDescr[highlyCorDescr!=c("loan_amnt","loan_status") ]
  
  if (is.na(highlyCorDescr[1])==FALSE){
    datasets_num <- datasets_num[,-which(colnames(datasets_num) %in% highlyCorDescr)]
  }
  datasets<-as.data.frame(cbind(datasets_rest,datasets_num),stringsAsFactors=FALSE )
  
  # Convert all NAs to -10000
  datasets[is.na(datasets)]<- -1000000
  
  #Set all the character columns to factors
  for (columns in colnames(datasets)){
    if (is.character(datasets[,columns])) {
      # !columns=="loan_status"
      datasets[,columns]<-factor(datasets[,columns])
    }
  }
  
  out<-datasets
  return(out)
  
}

# This is a function to preprocess the data based on the train dataset so that they have the same structure
currentdata_preprocess<-function(traindataset,datasets){
  force(datasets)
  datasets<-as.data.frame(datasets)
  
  today <- Sys.Date()
  today<-as.numeric(mdy(format(today, "%b-%Y")))
  includelist<-colnames(traindataset)
  datasets<-datasets[,which(colnames(datasets) %in% includelist)]
 
  # correct int_rate to integer
  datasets$int_rate<-gsub(" ","", datasets$int_rate)
  datasets$int_rate<-gsub("%","", datasets$int_rate)
  datasets$int_rate<-as.numeric(datasets$int_rate)
  
  #Convert Dates into numeric numbers
  datasets$issue_d <- as.numeric((today-as.numeric(mdy(datasets$issue_d))))
  datasets$last_pymnt_d <- as.numeric((today-as.numeric(mdy(datasets$last_pymnt_d))))
  datasets$last_credit_pull_d <- as.numeric((today-as.numeric(mdy(datasets$last_credit_pull_d))))
  
  # Convert all NAs to -1
  datasets[is.na(datasets)]<- -1000000
  
  for (columns in colnames(datasets)){
    if (is.character(datasets[,columns])) {
      # !columns=="loan_status"
      datasets[,columns]<-factor(datasets[,columns])
    }
  }
 
  #return the dataset
  out<-datasets
  return(out)
  
}


# This is the method that runs the machine learning algorithm
model_run<-function(sets,cdata,method,...){
  # Set a seed for consistency
  set.seed(100)
  force(sets)
  force(method)
  force(cdata)
  #cdata<-as.data.frame(cdata,stringsAsFactors=TRUE)
 
   traindata<-sets[[1]]
   validdata<-sets[[2]]
   testdata<-sets[[3]]
  
   
  # set the levels of the trained dataset inline with the big scope data so that 
  # there will not be issues when we need to predict the current data
  for (columns in colnames(traindata)){
    
    if (!is.numeric(cdata[,columns])){
      levels(traindata[,columns])<-union(levels(traindata[,columns]), levels(cdata[,columns]))
      levels(validdata[,columns])<-union(levels(validdata[,columns]), levels(cdata[,columns]))
      levels(testdata[,columns])<-union(levels(testdata[,columns]), levels(cdata[,columns]))
    }
  }
  
  # Run in parallel processeor
  cl <- makeCluster(detectCores()-2)
  registerDoSNOW(cl)
  registerDoParallel(cl)
  
  #set predictor and outcome names to speed up the algorithm run
  predictornames<-colnames(traindata[,-which(colnames(traindata)=="loan_status")])
  outcomename<-which(colnames(traindata)=="loan_status")
  outcome<-traindata[,outcomename,drop=FALSE]
  
  #Run the algorithm
  train_fit<-train(x=traindata[,predictornames],y=traindata[,outcomename],method = method,...)
  #train_fit<-randomForest(traindata[,predictornames],traindata[,"loan_status"],...)
  
  #Finalize the parallel runs and close the clusters
  registerDoSEQ()
  #on.exit(stopCluster(cl))
  stopCluster(cl)
  
  #set levels same for character columns
  #levelchar<-apply(traindata,function(x) )
  valid_fit<-predict(train_fit,validdata)
  test_fit<-predict(train_fit,testdata)
  print(train_fit)
  out<-list(train_fit,valid_fit,test_fit)
  return(out)
  
}

# Below given functions are not used in the general model that call the download, preprocesses the data and run algorithm
# They were only designed to find the right amount of data that converges that is why they are commented out
# general_call_err<-function(act,pred){
#   force(pred)
#   force(act)
#   conf_matrix<-createConfusionMatrix(act, pred)
#   err_rate<-(sum(conf_matrix)-sum(diag(conf_matrix)))/sum(conf_matrix)*100
#   return(err_rate)
# }
# 
# calc_err_rate<-function(model){
#   force(model)
#   train_model<-model[[1]]
#   valid_pred<-model[[2]]
# 
#   ncol<-ncol(train_model$finalModel$confusion)
#   train_model_confusion<-train_model$finalModel$confusion[,-length(train_model$finalModel$confusion)]
#   train_err<-sum(train_model_confusion)-sum(diag(train_model_confusion))
#   train_err_rate<-((train_err/sum(train_model_confusion)))*100
#   train_err_rate<-as.data.frame(train_err_rate)
#   
#   valid_model_confusion<-createConfusionMatrix(validdata$loan_status, valid_pred)
#   valid_err<-sum(valid_model_confusion)-sum(diag(valid_model_confusion))
#   valid_err_rate<-((valid_err/sum(valid_model_confusion)))*100
#   #valid_err_rate<-general_call_err(validdata$loanstatus,valid_pred)
#   valid_err_rate<-as.data.frame(valid_err_rate)
#   
#   names(train_err_rate)<-names(valid_err_rate)
#   
#   train_err_rate$errortype<-"training"
#   train_err_rate$datasize<-datasize
#   valid_err_rate$errortype<-"validation"
#   valid_err_rate$datasize<-datasize
#   
#   out<-rbind(train_err_rate,valid_err_rate)
#   out<-as.data.frame(out)
#   return(out)
# }
# 
# 
# createConfusionMatrix <- function(act, pred) {
#   # You've mentioned that neither actual nor predicted may give a complete
#   # picture of the available classes, hence:
#   numClasses <- max(length(levels(act)), length(levels(pred)))
#   print(numClasses)
#   # Sort predicted and actual as it simplifies what's next. You can make this
#   # faster by storing `order(act)` in a temporary variable.
#   pred <- pred[order(act)]
#   act  <- act[order(act)]
#   result<-sapply(split(pred, act), tabulate, nbins=numClasses)
#   return(result)
# }
# 
# z<-0
# general_call<-function(file,datasize,method,...) {
#   force(file)
#   force(datasize)
#   force(method)
#   
#   result<-calc_err_rate(model_run(segment_data(file,datasize),method=method,...))
#   z<<-z+1 
#   print(z)
#   return(result)
#   
# }
# 
# 
# 
# create_learning_curve<- function (result){
#   force(result)
#   
#   for (t in seq(length(result))){
#     if (t==1) {
#       result[[t]]<-as.data.table(result[[t]])
#       
#     }
#     else{
#       result[[t]]<-as.data.table(rbind(result[[t-1]],result[[t]]))
#       
#     }
#   } 
#   
#   result<-result[[length(result)]]
#   
#   result<-as.data.frame(result,stringsAsFactors=TRUE)
#   
#   
#   #   plot(result$error ~ result$data_size, data=result, groups=factor(result$error_type))
#   #          scales = list(x = list(at = 1:4, labels = levels(trans.factor))))
#   g<-ggplot(data=result,aes(x= datasize, y=valid_err_rate,linetype=factor(errortype)))+
#     geom_line(size=1,aes(color=factor(errortype)))+ 
#     labs(title ="Learning Curve", x = "Training set size", y = "Accuracy Error")+
#     theme(plot.title = element_text(size=30))+
#     theme_bw()
#   return(g)
#   
# }
# 
# # apply_pca<- function(x){
# #   x<-prcomp(x,center=FALSE,scale=FALSE)
# #   x<-as.data.frame(x$x,)
# #   return(x)
# # }
# 
# run_model<-function(file){
#   finalmodel<-lapply(seq(1000,10000,100),function(x) general_call(file=file,datasize=x,method="rf") )
#   return(finalmodel)
# }