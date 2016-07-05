# Load required packages to load the scraped required data files
library (RCurl)
library (XML)
library(lubridate)


# Load required packages to run the machine learning algorithm
library(caret)
library(memoise)
library(caTools)
library(randomForest)
library(gbm)
library(dplyr)
library(plyr)
library(ggplot2)
library(doParallel)
library(data.table)
library(lattice)
library(lubridate)
library(xgboost)
library(e1071)


#Call the required data files
address<-"https://www.lendingclub.com/info/download-data.action"
files<-filenames(address)
loanfiles<-open_and_merge_csv(find_csv())

# Split the Past data and Current data. The analysis are done based on past data.
pastdata<-subset(loanfiles[[1]],!(loanfiles[[1]]$loan_status=="Current" | loanfiles[[1]]$loan_status=="Does not meet the credit policy. Status:Charged Off" | loanfiles[[1]]$loan_status=="Does not meet the credit policy. Status:Fully Paid" | loanfiles[[1]]$loan_status==""))
currentdata<-subset(loanfiles[[1]],loanfiles[[1]]$loan_status=="Current")
currentdata$payment_gap<-currentdata$loan_amnt-currentdata$total_pymnt

# Assign all the credits other than "Fully Paid" to unhealthy "Risky Credit" status
pastdata$loan_status<-sapply(pastdata$loan_status,function(x) {if(x=="Fully Paid"){"Healthy Credit"}else{"Risky Credit"}})
currentdata$loan_status<-sapply(currentdata$loan_status,function(x) {if(x=="Fully Paid"){"Healthy Credit"}else{"Risky Credit"}})


#Create train,test and validation sets for the data
segmented_pastdata<-segment_data(pastdata,m=200000)

#Create preprocessing to current data
segmented_currentdata<-as.data.frame(currentdata_preprocess(segmented_pastdata[[1]],currentdata))

#Run the model with selected small subset of important variables to enable easy run of Shiny model
picklist<-c( "term","sub_grade","int_rate","annual_inc","issue_d","last_pymnt_d","last_pymnt_amnt",
             "avg_cur_bal","pub_rec_bankruptcies","payment_gap","loan_status")

#Make a smaller subset of past and current data to use in the machine learning algorithm
system.time(model_data<-lapply(seq(length(segmented_pastdata)), function(x) segmented_pastdata[[x]][,which(colnames(segmented_pastdata[[x]]) %in% picklist ),drop=FALSE]))

#Use the full data for current portfolio analysis
#Run the machine learning algorithm for past data and use this information to cretae a customer pre screening tool
customerscreeningmodel<-model_run(sets=model_data,cdata=segmented_currentdata,method="rf",ntrees=150,importance=TRUE)

#Save the shiny model
save(customerscreeningmodel, file = "RF_model_for_customer_Screening_m.rda")

#Run the machine learning algorithm for past data and use this information to evaluate current customer base
system.time(currentcustomermodel<-model_run(sets=segmented_pastdata,cdata=segmented_currentdata,method="rf",ntrees=150,importance=TRUE))

#Save the full model
save(currentcustomermodel, file = "RF_model_for_current_customer_Screening_m.rda")

# Predict the current customer base and save
current_data_pred<-as.data.frame(predict(currentcustomermodel[[1]],segmented_currentdata))
current_data_pred$remainderDebt<-segmented_currentdata$payment_gap
current_data_pred$loan_amnt<-segmented_currentdata$loan_amnt
colnames(current_data_pred)<-c("loan_pred","RemainingDebt")
loan_results<-as.data.frame(tapply(current_data_pred$RemainingDebt,current_data_pred$loan_pred,function(x){sum(as.numeric(x),na.rm=TRUE)}),col.names="Total Remaining Debt")
colnames(loan_results)<-"Total_Remaining_Debt"
loan_results$loan_status<-rownames(loan_results)
loan_results$percentage<-loan_results$Total_Remaining_Debt/sum(loan_results$Total_Remaining_Debt,na.rm=TRUE)
save(loan_results, file = "currentdatafits.rda")
