# Goal: To retrieve the csv files from a specified location

library(memoise)


find_csv<- memoise(function(location="./Data/"){
  force(location)
  
  # Search available .csv files  
  loan_csv_files<-list.files(path=location,pattern='^LoanStats3.*csv$',full.names=TRUE)
  reject_csv_files<-list.files(path=location,pattern='^RejectStats[A-Z].*csv$',full.names=TRUE)
  out<-list(loan_csv_files,reject_csv_files)
  return(out)
  
})


open_and_merge_csv<- memoise(function(files){
  force(files)
  
  if (!(length(files[[1]])>0)) {
    stop("There is no found .csv file." ,call=FALSE)
  }   
  
  
  for (j in 1:2){
    for (i in seq_along(files[[j]])){
      tmp<-read.csv(files[[j]][[i]], skip=1, stringsAsFactors=FALSE, na.strings="NA",blank.lines.skip = TRUE)
      
      if (i==1){
        merged_file<-tmp
      }
      else {
        merged_file<-rbind(merged_file,tmp)
      }
    }
    tmp<-NULL

    if(j==1) {
      loanfiles<-as.data.frame(merged_file)
    }
    else{
      rejectfiles<-as.data.frame(merged_file)
    }
    merged_file<-NULL
    
  }
  
  out<-list(loanfiles,rejectfiles)
  return(out)
})
