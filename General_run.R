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

#Create train,test and validation sets for the data
segmented_pastdata<-segment_data(pastdata,m=100000)

#Picked Features to run the model
# picklist<-c("int_rate","loan_amnt","grade","sub_grade","tot_hi_cred_lim", "term", "total_bal_ex_mort", "annual_inc",
#             "addr_state", "total_pymnt", "total_pymnt_inv", "funded_amnt", "funded_amnt_inv", "last_pymnt_amnt",
#             "issue_d", "total_rec_int", "total_rec_late_fee", "next_pymnt_d", "tot_cur_bal", "installment", "loan_status")
# 

picklist<-c("int_rate","loan_amnt","sub_grade","tot_hi_cred_lim", "total_bal_ex_mort", "annual_inc",
            "tot_cur_bal", "installment", "loan_status")

#Make a smaller subset of past and current data to use in the machine learning algorithm
system.time(model_data<-lapply(seq(length(segmented_pastdata)), function(x) segmented_pastdata[[x]][,which(colnames(segmented_pastdata[[x]]) %in% picklist ),drop=FALSE]))
str(model_data)

#Run the machine learning algorithm for past data and use this information to cretae a customer pre screening tool
customerscreeningmodel<-model_run(model_data,"rf",ntrees=150)

#Save the model
save(customerscreeningmodel, file = "RF_model_for_customer_Screening_m_300000.rda")

#Run the machine learning algorithm for past data and use this information to evaluate current customer base
system.time(currentcustomermodel<-model_run(segmented_pastdata,"rf",ntrees=150))

#Save the model
save(currentcustomermodel, file = "RF_model_for_current_customer_Screening_m_300000.rda")
