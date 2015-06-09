# server.R
library(shiny)
  load("DebTrivedi.rda")
  TrainSet <- DebTrivedi[, c(1, 6:8, 13, 15, 18)]         # select variables
  fitP <- glm(ofp ~ ., data = TrainSet, family = poisson) # regression

  clog <- function(x) log(x + 0.5)
  cfac <- function(x, breaks = NULL) {
    if(is.null(breaks)) breaks <- unique(quantile(x, 0:10/10))
    x <- cut(x, breaks, include.lowest = TRUE, right = FALSE)
    levels(x) <- paste(breaks[-length(breaks)], ifelse(diff(breaks) > 1,
    c(paste("-", breaks[-c(1, length(breaks))] - 1, sep = ""), "+"), ""),
    sep = "")
    return(x)
  }

shinyServer(function(input, output) {

  output$ofp <- renderText({
    if(input$insurance == 1) insurance <- "yes"
	else insurance <- "no"

    Xc <- data.frame(hosp=as.numeric(input$hVisits), 
	  health=input$hStatus, 
	  numchron=as.numeric(input$nChronic), 
	  gender=as.factor(input$gender), 
	  school=input$nSchool, 
	  privins=as.factor(insurance))

    Po <- data.frame(predict.glm(fitP, newdata = Xc, se.fit = TRUE))
	avg <- format(Po$fit, digits=2)
	Cc <- c(Po$fit - qnorm(0.95)*Po$se.fit, Po$fit + qnorm(0.95)*Po$se.fit)
	rng <- format(Cc, digits=2)
	  
    paste('Estimated Value: ', avg, '\nRange: ', rng[1], ' to ', rng[2])
  })

  output$plot1 <- renderPlot({
    par( mfrow = c( 2, 3 ) )
    plot(clog(ofp) ~ health, data = TrainSet, varwidth = TRUE, ylab='Log(Office Visits)', xlab='Health Status')
    plot(clog(ofp) ~ cfac(numchron), data = TrainSet, ylab='Log(Office Visits)', xlab='Chronic Conditions')
    plot(clog(ofp) ~ privins, data = TrainSet, varwidth = TRUE, ylab='Log(Office Visits)', xlab='Private Insurance')
    plot(clog(ofp) ~ cfac(hosp, c(0:2, 8)), data = TrainSet, ylab='Log(Office Visits)', xlab='Hospital Visits')
    plot(clog(ofp) ~ gender, data = TrainSet, varwidth = TRUE, ylab='Log(Office Visits)', xlab='Gender')
    plot(cfac(ofp, c(0:2, 4, 6, 10, 100)) ~ school, data = TrainSet, breaks = 9, ylab='Log(Office Visits)', xlab='Years of Education')
  })

})
