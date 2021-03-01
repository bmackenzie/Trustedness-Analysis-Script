library(shiny)
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


#access input data with input$id

