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
    data<-file[data,]
    
    #Convert all 'chr' columns to factors
    data<-as.data.frame(apply(data,2,convert_factor) )

  
    # Use 70% for train and valid, 30% for test from the selected data
    inTrain_valid <- sample(1:nrow(data), floor(dim(data)[1]*0.7))

    #Set the train and validation data separately
    Train_valid<-data[inTrain_valid,]
    inTrain<-sample(1:nrow(Train_valid), floor(dim(Train_valid)[1]*0.7))
    
    # Assign the data to each segment
    Train<-Train_valid[inTrain,]
    Valid<-Train_valid[-inTrain,]
    Test <- data[-inTrain_valid,]
    
    # Create a list to retrun the files
    out<-list(Train,Valid,Test)
    
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
      datasets[[k]][is.na(datasets[[k]])]<- -1000
    }
    else{    
      datasets[[k]]<-datasets[[k]][-1]
      datasets[[k]]<-datasets[[k]][-1]
      # datasets[[k]]<-datasets[[k]][-c("out_prncp","out_prncp_inv ")]
      datasets[[k]][is.na(datasets[[k]])]<- -1000
      x<-datasets[[k]]
      datasets[[k]]<-unique(x)
      
      # Remove the single factor columns
      datasets[[k]]<-datasets[[k]][, sapply(datasets[[k]], function(col) length(unique(col))) > 1]
      datasets[[k]]<-datasets[[k]][-34]
      datasets[[k]]<-datasets[[k]][-34]
      datasets[[k]]<-datasets[[k]][-34]
    }
      # correct int_rate to integer
      datasets[[k]]$int_rate<-gsub(" ","", datasets[[k]]$int_rate)
      datasets[[k]]$int_rate<-gsub("%","", datasets[[k]]$int_rate)
      datasets[[k]]$int_rate<-as.numeric(datasets[[k]]$int_rate)
      
      
      # Convert Grade into numeric factors
      datasets[[k]]$grade<-factor(datasets[[k]]$grade)
      datasets[[k]]$grade<-as.numeric( datasets[[k]]$grade)
      
      # Convert Purpose into numeric factors
      datasets[[k]]$purpose<-factor(datasets[[k]]$purpose)
      datasets[[k]]$purpose<- as.numeric(datasets[[k]]$purpose)
      
      # Convert loan status into numeric factors
      datasets[[k]]$loan_status<-factor(datasets[[k]]$loan_status)
      datasets[[k]]$loan_status<-as.numeric(datasets[[k]]$loan_status)
      datasets[[k]]$loan_status<-factor(datasets[[k]]$loan_status)

  }
  
  # Set the lists to data frame for train, validation and test sets
  train<-as.data.frame(datasets[[1]])
  valid<-as.data.frame(datasets[[2]])
  test<-as.data.frame(datasets[[3]])
  
  # return the data sets
  out<-list(train,valid,test)
  
  return(out)
}

convert_factor<-function(x) {
  force(x)

    if(is.character(x)){ 
      x<-factor(x)
      as.numeric(x)
    }
    #x<- factor(x)
    
}

# Create Learning curve to see if there exists bias or variance with selected model

model_run<-function(sets,method,...){
 
  force(sets)
  force(method)
  traindata<<-sets[[1]]
  validdata<<-sets[[2]]
  
  train_fit<-train(loan_status~., data = traindata ,method = method,...)
  valid_fit<-predict(train_fit,validdata)
  print(train_fit)
  out<-list(train_fit,valid_fit)
  return(out)
  
}

calc_err_rate<-function(model){
  force(model)
  train_model<-model[[1]]
  valid_pred<-model[[2]]
  
  
  ncol<-ncol(train_model$finalModel$confusion)
  #ncol2<-ncol(valid_model$finalModel$confusion)
  
  train_model_confusion<-createConfusionMatrix(traindata$loan_status, valid_pred)
  valid_model_confusion<-createConfusionMatrix(validdata$loan_status, valid_pred)
  #valid_model_confusion<-valid_model$finalModel$confusion[,-ncol2,drop=FALSE]
   
  train_err<-sum(train_model_confusion)-sum(diag(train_model_confusion))
  valid_err<-sum(valid_model_confusion)-sum(diag(valid_model_confusion))

  train_err_rate<-((train_err/sum(train_model_confusion)))*100
  train_err_rate<-as.data.frame(train_err_rate)
  valid_err_rate<-((valid_err/sum(valid_model_confusion)))*100
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
  cl <- makeCluster(4)
  registerDoParallel(cl)
  finalmodel<-lapply(seq(1000,20000,1000),function(x) general_call(file=file,datasize=x,method="rf") )
  stopCluster(cl)
  return(finalmodel)
}