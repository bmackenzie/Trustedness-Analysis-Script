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