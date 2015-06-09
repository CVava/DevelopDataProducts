library(shiny)

# ui.R
shinyUI(bootstrapPage(

  headerPanel("Demand for Medical Care by the Elderly"),
  sidebarPanel(
    radioButtons("gender", "Gender:",
             c("Female" = "female", 
               "Male" = "male")),
    radioButtons("hStatus", "Health Status:",
             c("Excellent" = "excellent",
               "Average" = "average",
			   "Poor" = "poor")),
    checkboxInput("insurance", "Private Insurance", 0),
    sliderInput("nChronic", 
	  "Number of Chronic Conditions", min = 0, max = 8, value = 0, step = 1),
    sliderInput("nSchool", 
	  "Education (School Years)", min = 0, max = 18, value = 0, step = 1),
    numericInput(inputId="hVisits", 
	  label = "Number of Hospital Visits", 0, min=0, max=10, step=1)
  ),
  mainPanel(
    h3('Number of Office Visits'),
    h4(verbatimTextOutput('ofp')),
    h3('Data Set Visualization'),
    plotOutput('plot1'),
    h4('Author: CVava'),
    h4('Date: June 9, 2015')
  )
))
