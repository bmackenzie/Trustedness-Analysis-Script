library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("cerulean"),
  titlePanel("Trustedness Analysis Application"),
  h4("Instructions"),
  tags$ol(
    tags$li(tags$a(href="https://trustradius.lightning.force.com/lightning/r/Report/00O0V000005Z35wUAC/view?queryScope=userFolders", "Use this SalesForce report"), "change the filters to match the product you are analyzing, make sure you export as a CSV with details only. "), 
    tags$li("click the browse item below and select the CSV from the report"), 
    tags$li("A checklist of campaigns will appear, select the campaigns you are analyzing for trustedness"),
    tags$li("A message will appear telling you how many reviews are in the sample.  You need at least 20 to mark the sample as trusted"),
    tags$li("Two graphs will generate, both compare the selected campaigns to all trusted reviews, the first graph by percent, and the second by quantity"),
    tags$li("Determine if the blue sample distribution is in line with the red trusted distribution, or represents a reasonable deviation (10% or so)"),
    tags$li("If the data looks good, open the csv, save the TRids (first column) of only the ratings and reviews to be marked trusted. Send to Erik Hlavarty, and cc Meghan and the CSM. Tell him it's ratings and reviews that need to be marked as trusted"),
    tags$li("If the data doesn't look good, message Megan to communicate with the vendor, and let the CSM know")
  ),
  fileInput(
    inputId="data", 
    label="select the Excel file to be analyzed", 
    multiple=FALSE, 
    accept=".csv", 
    width = "40%", 
    buttonLabel="browse...", 
    placeholder = "No File Selected"
    ),
  
  uiOutput("moreControls"),
  
  textOutput("numpoints"),
  
  plotOutput("bar"),
  plotOutput("bar2")
  )

server <- function(input, output) {
  
  #Declare Functions
  percentage <- function(count, len){
    percent <- count / len
    return(percent)
  }
  
  #Function for summarizing ratings with counts
  counts <- function(reviews) {
    summary <- data.frame(one = numeric(0), two = numeric(0), three = numeric(0), four = numeric(0), five = numeric(0), six = numeric(0), seven = numeric(0), eight = numeric(0), nine = numeric(0), ten = numeric(0))
    summary
    
    
    #Declare variables to hold rating count
    one <- 0
    two <- 0
    three <- 0
    four <- 0
    five <- 0
    six <- 0
    seven <- 0
    eight <- 0
    nine <- 0
    ten <- 0
    total <- 0
    len <- nrow(reviews)
    
    for(i in 1:nrow(reviews)) {
      print(i) 
    }
    
    for(i in 1:nrow(reviews)) {
      rating <- (reviews[i,])
      
      if (rating == 1){
        one <- one + 1
      } else if (rating == 2){
        two <- two + 1
      } else if (rating == 3){
        three <- three + 1
      }else if (rating == 4){
        four <- four + 1
      }else if (rating == 5){
        five <- five + 1
      }else if (rating == 6){
        six <- six + 1
      }else if (rating == 7){
        seven <- seven + 1
      }else if (rating == 8){
        eight <- eight + 1
      }else if (rating == 9){
        nine <- nine + 1
      }else if (rating == 10){
        ten <- ten + 1
      }
    }
    
    
    #append percentages to campaignSummary dataframe
    summary <- rbind(summary, list(one = one, two = two, three = three, four = four, five = five, six = six, seven = seven, eight = eight, nine = nine, ten = ten))
    
    return(summary)
  }
  
  #Function for summarizing ratings with percents
  summarize <- function(reviews) {
    summary <- data.frame(one = numeric(0), two = numeric(0), three = numeric(0), four = numeric(0), five = numeric(0), six = numeric(0), seven = numeric(0), eight = numeric(0), nine = numeric(0), ten = numeric(0))
    summary
    
    
    #Declare variables to hold rating count
    one <- 0
    two <- 0
    three <- 0
    four <- 0
    five <- 0
    six <- 0
    seven <- 0
    eight <- 0
    nine <- 0
    ten <- 0
    total <- 0
    len <- nrow(reviews)
    
    for(i in 1:nrow(reviews)) {
      print(i) 
    }
    
    for(i in 1:nrow(reviews)) {
      rating <- (reviews[i,])
      
      if (rating == 1){
        one <- one + 1
      } else if (rating == 2){
        two <- two + 1
      } else if (rating == 3){
        three <- three + 1
      }else if (rating == 4){
        four <- four + 1
      }else if (rating == 5){
        five <- five + 1
      }else if (rating == 6){
        six <- six + 1
      }else if (rating == 7){
        seven <- seven + 1
      }else if (rating == 8){
        eight <- eight + 1
      }else if (rating == 9){
        nine <- nine + 1
      }else if (rating == 10){
        ten <- ten + 1
      }
    }
    
    #Convert counts to percentage
    oneper <- percentage(one, len)
    twoper <- percentage(two, len)
    threeper <- percentage(three, len)
    fourper <- percentage(four, len)
    fiveper <- percentage(five, len)
    sixper <- percentage(six, len)
    sevenper <- percentage(seven, len)
    eightper <- percentage(eight, len)
    nineper <- percentage(nine, len)
    tenper <- percentage(ten, len)
    
    #append percentages to campaignSummary dataframe
    summary <- rbind(summary, list(one = oneper, two = twoper, three = threeper, four = fourper, five = fiveper, six = sixper, seven = sevenper, eight = eightper, nine = nineper, ten = tenper))
    
    return(summary)
  }
  
  #Outputs
  #Generate campaign list
  output$moreControls <- renderUI({
    if(is.null(input$data$datapath))     return(NULL) 
    data <- read.csv(input$data$datapath)
    campaigns <- subset(data, select = c("Review.Campaign.ID"))
    campaigns <- unique(campaigns)
    tagList(
      checkboxGroupInput(
        inputId="campaigns",
        label = "Check off the campaigns to be analyzed",
        choices = campaigns[[1]],
      )
    )
  })
  
  #Generate count of sample
  output$numpoints <- renderText ({
    if(is.null(input$campaigns))     return(NULL) 
    campaigns <- input$campaigns
    data <- read.csv(input$data$datapath)
    trueData <- subset(data, Trusted==TRUE, select=c("Rating"))
    sample <- subset(data, Review.Campaign.ID %in% campaigns, select=c("Rating"))
    paste("Number of reviews in sample: ", toString(nrow(sample)), sep = " ")
  })

    
  #Output Graph percent
  output$bar <- renderPlot ({
    if(is.null(input$campaigns))     return(NULL) 
    campaigns <- input$campaigns

    data <- read.csv(input$data$datapath)
    trueData <- subset(data, Trusted==TRUE, select=c("Rating"))
    sample <- subset(data, Review.Campaign.ID %in% campaigns, select=c("Rating"))
    
    trueSummary <- summarize(trueData)
    
    sampleSummary <- summarize(sample)
    
    plotSummary <- data.matrix(rbind(trueSummary, sampleSummary))
    
    barplot(plotSummary, beside=T, main= "Trusted Reviews vs Sample Reviews by percent", xlab= "Rating", ylab= "Frequency",
            col=c("red", "blue"))
    
    legend("top", legend=c("Trusted", "Sample"), col=c("red", "blue"), lty=1:1, cex=1)
  })
  
  #Output Graph count
  output$bar2 <- renderPlot ({
    if(is.null(input$campaigns))     return(NULL) 
    campaigns <- input$campaigns
    
    data <- read.csv(input$data$datapath)
    trueData <- subset(data, Trusted==TRUE, select=c("Rating"))
    sample <- subset(data, Review.Campaign.ID %in% campaigns, select=c("Rating"))
    
    trueSummary <- counts(trueData)
    
    sampleSummary <- counts(sample)
    
    plotSummary <- data.matrix(rbind(trueSummary, sampleSummary))
    
    barplot(plotSummary, beside=T, main= "Trusted Reviews vs Sample Reviews by count", xlab= "Rating", ylab= "Frequency",
            col=c("red", "blue"))
    
    legend("top", legend=c("Trusted", "Sample"), col=c("red", "blue"), lty=1:1, cex=1)
  })  

  
}

shinyApp(ui = ui, server = server)



#access input data with input$id

