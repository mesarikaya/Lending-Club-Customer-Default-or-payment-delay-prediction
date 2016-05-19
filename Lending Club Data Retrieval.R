## Model Purpose: To automate data retrieval from Lending Club website

## Automation steps:
## 1. ACTIVATE relavant libraries (RCurl and XML)
## 2. SCRAPE the Lending club web page: "https://www.lendingclub.com/info/download-data.action" to see what files are available
## 3. UNLIST (with filenames function) the retrieved list that has web root link and the file names
## 4. DOWNLOAD the zip files via call from filenames function
## 5. UNZIP the zip files via call from filenames function


# Call Rcurl and XML libraries in order to be able to scrape web data

library (RCurl)
library (XML)

# Create Function to call the relevant webpage
# Parameter: 1) HTML address, 2) download link

scrape<-function(htmladdress) {
  # force retrieval of the internet address at the call  
  force(htmladdress)
  
  # Check if the requested web adress is entered as character
  if (!is.character(htmladdress)) {
    stop("Don't know how to handle this type. Please enter a character vector value." ,call=FALSE)
  }
  else{
    # Connect the website
    webpage<-getURL(htmladdress)
    webpage<-readLines(conn <- textConnection(webpage));close(conn)
    
    # Parse the website
    pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
    
    # Check the available files - "Loan" and "Reject" and download address prefix
    loan_filenames<-xpathSApply(pagetree, "//*[@id='loanStatsFileNamesJS']" , xmlValue)
    rejected_loans_filenames<-xpathSApply(pagetree, "//*[@id='rejectedLoanStatsFileNamesJS']" , xmlValue)
    url_prefix<-xpathSApply(pagetree, "//*[@id='urlPublicPrefix']" , xmlValue)
    
    # Return two outputs for files - (Loan, Download address prefix)
    output<-list(loan_filenames,rejected_loans_filenames,url_prefix) 
    return(output)

  }
  
}

# Key function to call to scrape the website and start scraping, download and extraction process of csv files

filenames<-function(htmladdress) {
  
  force(htmladdress)
  # Call scrape function to retrieve the loan files
  files<-scrape(htmladdress)
  file_names<-unlist(strsplit(unlist(files)[-3],"[|]"))
  #Retrieve the website prefix
  download_prefix<-unlist(files)[3]
  #For all files create a download link
  links<-lapply(file_names, function(x)  paste0(download_prefix,x))
  #For all files call the download function
  lapply(links,function(x) download_files(x))
}

#Downloads the files from a provided link
i<-0
download_files<-function(links,...){
  force(links)
  i<<-i+1
  
  # Check if the file already exists in the specified folder
  if (!file.exists(paste0("./Data/",basename(links)))){
    # If the file does not exist start download process for the file
    download.file(links,paste0("./Data/",basename(links)),...)
    # Call unzip_files function to start extraction
    unzip_files(paste0("./Data/",basename(links)))
  }
  else{
      print(paste0("File ", i, " Exists"))
  } 
 # data <- read.table(unz(temp,filename=file_names[i],encoding = getOption("encoding")))

}

# Extract the files to the specified folder
unzip_files<-function(zipfiles,...){
  force(zipfiles)
  print(gsub(".zip","",zipfiles))
  unzip(zipfiles,exdir="./Data",)
}
