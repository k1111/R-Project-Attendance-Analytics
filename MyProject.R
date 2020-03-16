## install packages
library(shiny)
library(shinydashboard)
library(readxl)
library(sqldf)

header <- dashboardHeader()
sidebar <- dashboardSidebar()
body <- dashboardBody()
skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- "red"


dashboardPage(header, sidebar, body, skin = skin)


sidebar <- dashboardSidebar(
## the below command is required for displaying tabItem "dashboard"  
  sidebarMenu(
    menuItem("Batch Wise", tabName = "dashboard-1", icon = icon("theater-masks")),
    menuItem("Registration Wise", tabName = "dashboard-2", icon = icon("user-graduate"))
 )  
)  ## closure of sidebar


header <- dashboardHeader(
  title = "Attendance-Analysis"
)  ## closure of header


######################################### load Excel Files

## With Sheet name and data types
## attendance_detail <- read_excel("D:/attendance detail.xlsx", sheet = "Attndnc_dtls", col_types = c("text", "text", "text", "numeric", "text", "text", "text", "text", "text", "date", "date", "text", "numeric", "numeric", "text"))

## Without Sheet name and data types
attendance_detail <- read_excel("D:/attendance detail.xlsx")

mydata = sqldf("select rgstrtn_cd, btch_cd, cntct_nmbr, stts from attendance_detail")


######################################### code for body


body <- dashboardBody(
  tabItems(


## ui for first page starts here

    tabItem("dashboard-1", 
            
            fluidRow(            
              box(
                title = "Would you like to View the Status of your Batch?",
                width = 12, solidHeader = TRUE, background = "olive",
                selectInput("dropdownBatchCode", "If Yes then Choose Batch Code from the DropDown",
                            choices = attendance_detail$btch_cd,
                            selected = NULL  
                )
              )            
            ), ## closure for fluid Row  


          fluidRow(
               ## Displaying Table
            box(
              title = "Status of the Batch",
              width = 12, solidHeader = TRUE, background = "olive",
              tableOutput(outputId = 'myBatchTable')
              )
            ) ## closure for fluid Row

    ),  ## closure of tabsItem
    
## ui for first page ends here

    
## ui for second page starts here
    
    tabItem("dashboard-2", 
            
            fluidRow(            
              box(
                title = "Choose Registration Number",
                width = 12, solidHeader = TRUE, status = "primary",
                selectInput("dropdownmyReg", "Reg. No.",
                            choices = attendance_detail$rgstrtn_cd,
                            selected = NULL  
                )
              )            
            ), ## closure for fluid Row  
            
            
            fluidRow(
              box(
                title = "Choose Registration Number",
                width = 12, solidHeader = TRUE, status = "primary"
              ),
              column(
                width = 6,
                verbatimTextOutput("printMe")
              ),
              column( 
                width = 6,
                plotOutput("plot1")
              )
              
            ), ## closure for fluid Row  
            
            fluidRow(
              ## Displaying Table
              box(
                title = "Status of the Student",
                width = 12, solidHeader = TRUE, status = "primary",
                tableOutput(outputId = 'myRegTable')
              )
            ) ## closure for fluid Row
            
    )
    
## ui for second page ends here
    
  )  ## closure of tabItem
)  ## closure of body


ui <- dashboardPage(header, sidebar, body, skin = skin)

######################################### code for server

server <- function(input, output) {


## Logic for First Page starts here

## reading value from dropdown menu
myBatchCode <- reactive({
    ## this will read input from dropdown
    myBatchCode = input$dropdownBatchCode    
})
  

## data for the table
output$myBatchTable <- renderTable({ 
if(input$dropdownBatchCode=="") { "Select Batch Code First"
} else { 
wholedata = data.frame(matrix(nrow = 0, ncol = 6))
names(wholedata) = c("RegistrationNo", "TotalSessions", "Present", "Absent", "Percentage", "Remark")


## for loop will go over each record
for(i in mydata[which(input$dropdownBatchCode==mydata$btch_cd),1] ) {


## logic for calculations
att_present = sum((i == mydata$rgstrtn_cd) & (mydata$stts=='P'))
att_total = sum((i == mydata$rgstrtn_cd) & (unique(mydata$cntct_nmbr)))
att_absent = att_total - att_present
att_percent = as.numeric(att_present) / as.numeric(att_total) * 100
att_percent_r = round(att_percent, 2)
att_ratio=c(Present=att_present, Absent=att_absent)

   
wholedata[i,1] = i
wholedata[i,2] = att_total
wholedata[i,3] = att_present
wholedata[i,4] = att_absent
wholedata[i,5] = att_percent_r

if(att_percent_r<50 || (tail(mydata$stts, 3)=="A")) {
wholedata[i,6] = "Student MIGHT become a dropout"
} else {
  wholedata[i,6] = NA
}
}

## As of now, sqldf doesn't support right join or full join hence we will use merge() instead
## k = sqldf("select btch_cd from myData full join wholedata on mydata.rgstrtn_cd=wholedata.RegistrationNo")

k = merge(wholedata, mydata, by.x="RegistrationNo", by.y="rgstrtn_cd")

somedata = sqldf("select DISTINCT btch_cd, RegistrationNo, TotalSessions, Present, Absent, Percentage, Remark from k")

  
subset(somedata, somedata$btch_cd == input$dropdownBatchCode) }
})

## Logic for First Page ends here


## Logic for Second Page starts here

## reading value from dropdown menu
myReg <- reactive({
  ## this will read input from dropdown
  myReg = input$dropdownmyReg    
})


output$printMe <- renderPrint({
  if(input$dropdownmyReg=="") { "Select Registration Number First"
  } else { 
    myRegNo = input$dropdownmyReg
    att_present = sum((attendance_detail$rgstrtn_cd==myRegNo) & (attendance_detail$stts=='P'))
    att_total = sum((attendance_detail$rgstrtn_cd==myRegNo))
    att_absent = att_total - att_present
    att_percent = as.numeric(att_present) / as.numeric(att_total) * 100
    att_percent_r = round(att_percent, 2)
    att_ratio=c(Present=att_present, Absent=att_absent)
    
    ##mystatus = if(myRegNo==registration$rgstrtn_cd) unique(registration$stdnt_stts) else NULL
    ##cat("\n",  "\n",  "\n", "Status is ", mystatus)
    cat("Total Sessions Attended", att_present, "\n", "\n",  "\n", "Total Sessions Conducted", att_total, "\n", "\n",  "\n",
        "Total Sessions Missed", att_absent, "\n",  "\n",  "\n", "Percentage", att_percent_r)
  }
})

output$plot1 <- renderPlot({
  if(input$dropdownmyReg=="") { "Select Registration Number First"
  } else { 
    myRegNo = input$dropdownmyReg
    att_present = sum((attendance_detail$rgstrtn_cd==myRegNo) & (attendance_detail$stts=='P'))
    att_total = sum((attendance_detail$rgstrtn_cd==myRegNo))
    att_absent = att_total - att_present
    att_percent = as.numeric(att_present) / as.numeric(att_total) * 100
    att_percent_r = round(att_percent, 2)
    att_ratio=c(Present=att_present, Absent=att_absent)
    pie(att_ratio, col=c("Green", "Red"), radius=0.8)
  }
})


## data for the table
output$myRegTable <- renderTable({ 
  if(input$dropdownmyReg=="") { "Select Registration Number First"
  } else { 
    
    
    att_present = sum((mydata$rgstrtn_cd==input$dropdownmyReg) & (mydata$stts=='P'))
    
    att_total = sum((mydata$rgstrtn_cd==input$dropdownmyReg))
    
    att_absent = att_total - att_present
    
    att_percent = as.numeric(att_present) / as.numeric(att_total) * 100
    att_percent_r = round(att_percent, 2)
    
    att_ratio=c(Present=att_present, Absent=att_absent)
    ##    pie(att_ratio, col=c("Green", "Red"), main="Attendance Analysis", radius=1)
    
    subset(mydata, mydata$rgstrtn_cd == input$dropdownmyReg) }
})


## Logic for Second Page ends here


print("App is Ready")
    
}  ## closure of server


shinyApp(ui, server)