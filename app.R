
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(caret)
library(lubridate)
library(ETAS)

#"C:/Users/Sarikaya.me/Desktop/Transfer files/Transfer files/Rf_model_m_10000.rda"
load("C:/Users/MustafaErgin/Desktop/R Studies/R develop tools course/R Portfolio/Financial Industry/Lending Club/Lending Club Data Analysis/RF_model_for_customer_Screening_m_300000.rda")
shinymodel<-customerscreeningmodel[[1]]

grade_levels<-levels(shinymodel$trainingData$grade)
sub_grade_levels<-levels(shinymodel$trainingData$sub_grade)
# term_levels<-shinymodel$xlevels$term
state_levels<-levels(shinymodel$trainingData$addr_state)
# issue_date_levels<-levels(as.factor(currentdata$issue_d))# Convert this to gap between today and the issue date
# last_payment_date_levels<-levels(as.factor(currentdata$last_pymnt_d))# Convert this to number
# next_payment_date_levels<-levels(as.factor(currentdata$next_pymnt_d))# Convert this to number
today <- Sys.Date()
today<-as.numeric(mdy(format(today, "%b-%Y")))

# Define UI for application that draws a histogram
ui <-shinyUI(fluidPage(
  
  
  # titlePanel("INPUTS"),
  
  # tags$head(
  #   tags$style(type="text/css", "select { max-width: 2000px; }"),
  #   tags$style(type="text/css", ".span4 { max-width: 250px; }"),
  #   tags$style(type="text/css", ".well { max-width: 250px; }")
  # )
  fluidRow(
    tabsetPanel(
      tabPanel(h2("Customer Screening"),
    splitLayout(cellWidths = c("50%", "50%"),fluidRow(sidebarPanel(h3("Personal Info"),width = 6,
                                            # selectizeInput("Grade", "Credit Grade:", choices=grade_levels,
                                            #                options = list(create = TRUE),width=200),
                                            sliderInput("Loan_Amount","Loan amount ($):",
                                                        min = 1000,
                                                        max = 1000000,
                                                        value = 1000,step=1000,width=1000),
                                            
                                            selectizeInput("SubGrade", "Credit Sub Grade:", choices=sub_grade_levels,
                                                           options = list(create = TRUE),width=1000),
                                            
                                            sliderInput("Annual_Income", "Annual Income ($):", value=1000,
                                                         min=0, max=1000000, step=1000,width=51000),
                                            
                                            sliderInput("Credit_limit", "Total credit limit ($):", value=1000,
                                                         min=0, max=1000000, step=1000,width=1000)
                                            
                                            # selectizeInput("State", "State:", choices=state_levels,
                                            #                options = list(create = TRUE),width=200)
                                            
                                            ),
  
                sidebarPanel(h3("Requested Credit Inputs"),offset=1,width = 6,
                             
                             
                             numericInput("Interest_rate", "Interest Rate (%):",
                                          min = 0,
                                          max = 200,
                                          value = 5,step=0.25,width=1000),
                             sliderInput("Installment", "Monthly payment amount ($):",
                                          min = 0,
                                          max = 100000,
                                          value = 100,step=100,width=1000),
                             sliderInput("Total_curent_balance", "Current balance amount ($):", value=1000,
                                         min=0, max=1000000, step=1000,width=1000),
                             sliderInput("Total_bal_excl_mortgages", "Credit balance exc. mortgages ($):", value=1000,
                                         min=0, max=1000000, step=1000,width=1000)
                             # submitButton("Submit")      
                )
        ),
        mainPanel(h2("Expected Outcome:"),h3(textOutput("Expectation"))
                  # tabPanel("Plot", plotOutput("distPlot")), 
                  # tabPanel("Table", tableOutput("table"))
        ))),
        
        tabPanel(h2("Current Account Trends"))
        
        ))
  
))
    
   





#Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  

  
  output$Expectation<-renderText({
    newdata<-NULL
    newdata$loan_amnt<-as.numeric(input$Loan_Amount)
    # newdata$funded_amnt<-as.numeric(input$Funded_amount)
    # newdata$funded_amnt_inv<-as.numeric(input$Funded_amount_inv)
    # newdata$term<-as.factor(input$Term)
    newdata$int_rate<-as.numeric(input$Interest_rate)
    newdata$installment<-as.numeric(input$Installment)
    # newdata$grade<-as.factor(input$Grade)
    newdata$sub_grade<-as.factor(input$SubGrade)
    newdata$annual_inc<-as.numeric(input$Annual_Income)
    # newdata$issue_d<-as.numeric((today-as.numeric(mdy(input$Issue_Date))))
    # newdata$addr_state<-as.factor(input$State)
    # newdata$total_pymnt<-as.numeric(input$Total_payment)
    # newdata$total_pymnt_inv<-as.numeric(input$Total_payment_inv)
    # newdata$total_rec_int<-as.numeric(input$Total_interest_paid)
    # newdata$total_rec_late_fee<-as.numeric(input$Total_late_int)
    # newdata$last_pymnt_d<-as.numeric(mdy(input$Last_payment_date))
    # newdata$last_pymnt_amnt<-as.numeric(input$Last_payment_amount)
    # newdata$next_pymnt_d<-as.numeric(mdy(input$Next_payment_date))
    newdata$tot_cur_bal<-as.numeric(input$Total_curent_balance)
    newdata$tot_hi_cred_lim<-as.numeric(input$Credit_limit)
    newdata$total_bal_ex_mort<-as.numeric(input$Total_bal_excl_mortgages)
    
    levels(newdata$grade)<-levels(shinymodel$trainingData$grade)
    levels(newdata$sub_grade)<-levels(shinymodel$trainingData$sub_grade)
    # levels(newdata$addr_state)<-levels(shinymodel$trainingData$addr_state)
    
    
    newdata<-as.data.frame(newdata)
    str(newdata)
    #newdata
    out<-as.character(predict(shinymodel,newdata))
   })
}) 

#   print(predict(newdata))
#   print(input$a)
# })
# output$distPlot <- renderPlot({
#    # generate bins based on input$bins from ui.R
#    x    <- faithful[, 2] 
#    bins <- seq(min(x), max(x), length.out = input$bins + 1)
#    
#    # draw the histogram with the specified number of bins
#    hist(x, breaks = bins, col = 'darkgray', border = 'white')
# })

# Run the application 
shinyApp(ui = ui, server = server)

