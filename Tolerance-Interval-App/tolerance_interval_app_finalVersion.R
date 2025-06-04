library(shiny)
library(shinyWidgets)
library(tidyverse)
library(greekLetters)
library(dplyr) 
library(ggplot2)
library(markdown)
library(rsconnect)


#################### USER INTERFACE ####################
ui <- fluidPage(style = "color: black;",
                tags$head(
                  tags$style(HTML(".shiny-output-error-warning {
                    color: orange;
                  }")
                  )
                ),
                tags$head(
                  tags$style(HTML(".shiny-output-error-danger {
                    color: red;
                  }")
                  )
                ),
                withMathJax(),
                fluidRow(style = "background-color: #7569a7; font: Arial; color: #FFFFFF",
                         column(width = 3, img(src = "STAT Mark A Color.png", height = "100", width = "300")),
                         column(width = 4, offset = 1, h1(strong(HTML("<center>Tolerance Interval Limit Calculator<center>"))))
                         ),
                setBackgroundColor(color = "#EDEBF3;"),
                navbarPage("",
                           tabPanel(h4("Introduction"),
                                    column(width = 8, offset = 2,
                                           includeMarkdown("Introduction.Rmd")
                                           )
                           ),
                           tabPanel(h4("Definitions/Formulas"),
                                    fluidRow(
                                      column(width = 8, offset = 2, 
                                             includeMarkdown("Def&FormulaPart1.Rmd")
                                           )
                                    ),
                                    fluidRow(
                                      column(width = 6, offset = 4,
                                             img(src = "tolerance_interval_limit.png"))
                                    ),
                                    fluidRow(
                                      column(width = 8, offset = 2,
                                             includeMarkdown("Def&FormulaPart2.Rmd")
                                             )
                                    )
                           ),
                           tabPanel(h4("Noparametric TI Calculator"),
                                    fluidRow(
                                      column(4,
                                             dropdown(label = "Instructions Dropdown",
                                                      h4("Directions:"),
                                                      br(),
                                                      h5("Use either the sliders or boxes to set desired confidence level, population proportion, 
                                                permissable overshoot, and risk. The minimum sample size required and tolerance limit is automatically updated to the right."),
                                                      br(),
                                                      h5("Note that the boxes can potentially be unweildy if one tries to manually type in the amount they want. To get around this,
                                                there are arrows on the right side of the text box which will either increase or decrease the input by one tick mark on the corresponding slider.")
                                             ),
                                             br(),
                                             )
                                    ),
                                    fluidRow(
                                      column(4, offset = 1,
                                             sliderInput("confidence_nonP", "Confidence (1-\\(\\boldsymbol{\\alpha}\\)): ", value = 0.9, min = 0.01, max = 0.99, step = 0.01),
                                             numericInput("confidenceBox_nonP", "", value = 0.9, min= 0.01, max = 0.99, step = 0.01, width = "100px"), 
                                             sliderInput("proportion_nonP", "Proprtion (P): ", value = 0.9, min = 0.01, max = 0.99, step = 0.01),
                                             numericInput("proportionBox_nonP", "", value = 0.9, min = 0.01, max = 0.99, step = 0.01, width = "100px"),
                                             sliderInput("error_nonP", paste0("Permissable Overshoot (", greeks("epsilon"), "): "), value = 0.1, min = 0.01, max = 0.99, step = 0.01),
                                             numericInput("errorBox_nonP", "", value = 0.1, min = 0.01, max = 0.99, step = 0.01, width = "100px"),
                                             sliderInput("risk_nonP", "Risk (\\(\\boldsymbol{\\alpha^*}\\))", value = 0.01, min = 0.01, max = 0.99, step = 0.01),
                                             numericInput("riskBox_nonP", "", value = 0.01, min = 0.01, max = 0.99, step = 0.01, width = "100px")
                                      ),
                                      column(4, offset = 1,
                                             selectInput("limit_type", "LIMIT TYPE: ", choices = c("Upper Limit", "Lower Limit", "2-Sided Limit")
                                             ),
                                             uiOutput("output_nonP")
                                      )
                                    )
                           ),
                           tabPanel(h4("Normal TI Calculator"),
                                    fluidRow(
                                      column(4,
                                             dropdown(label = "Instructions Dropdown",
                                                      h4("Directions"),
                                                      br(),
                                                      h5("First select tolerance limit type (i.e. 1-sided or 2-sided). Then use either the sliders or boxes to set desired confidence level, population proportion, 
                                                        permissable overshoot, and risk; the minimum sample size required (assuming the population is normal) is then updated on the right. Then one has the option
                                                        to proceed to the tolerance limit calculation, in which case it will appear on the right along with a graphical representation."),
                                                      br(),
                                                      h5("Note that when when switching to the 1-sided TI type to the 2-sided TI type or vice versa, it is necessary to first refresh the page. Otherwise if this is not done, 
                                                         the calculations will be based off the initial inputs given from the previous TI type, not the currently set inputs."),
                                                      br(),
                                                      h5("Note also that the boxes can potentially be unweildy if one tries to manually type in the amount they want. To get around this,
                                                        there are arrows on the right side of the text box which will either increase or decrease the input by one tick mark on the corresponding slider.")
                                             ),
                                             br(),
                                             selectInput("TI_type", "TI TYPE", choices = c("Select TI Type", "1-Sided", "2-Sided"), selected = NULL
                                             ),
                                             conditionalPanel(
                                               condition = 'input.TI_type == "1-Sided"',
                                               sliderInput("confidence_norm", "Confidence (1-\\(\\boldsymbol{\\alpha}\\)): ", value = 0.9, min = 0.01, max = 0.99, step = 0.01),
                                               numericInput("confidenceBox_norm", "", value = 0.9, min= 0.01, max = 0.99, step = 0.01, width = "100px"), 
                                               sliderInput("proportion_norm", "Proprtion (P): ", value = 0.9, min = 0.01, max = 0.99, step = 0.01),
                                               numericInput("proportionBox_norm", "", value = 0.9, min = 0.01, max = 0.99, step = 0.01, width = "100px"),
                                               sliderInput("error_norm1sided", paste0("Permissable Overshoot (", greeks("epsilon"), "): "), value = 0.099, min = 0.001, max = 0.989, step = 0.001),
                                               numericInput("errorBox_norm1sided", "", value = 0.099, min = 0.001, max = 0.989, step = 0.001, width = "100px"),
                                               sliderInput("risk_norm1sided", "Risk (\\(\\boldsymbol{\\alpha^*}\\))", value = 0.01, min = 0.01, max = 0.99, step = 0.01),
                                               numericInput("riskBox_norm1sided", "", value = 0.01, min = 0.01, max = 0.99, step = 0.01, width = "100px")
                                             ),
                                             conditionalPanel(
                                               condition = 'input.TI_type == "2-Sided"',
                                               sliderInput("confidence_norm", "Confidence (1-\\(\\boldsymbol{\\alpha}\\)): ", value = 0.9, min = 0.01, max = 0.99, step = 0.01),
                                               numericInput("confidenceBox_norm", "", value = 0.9, min= 0.01, max = 0.99, step = 0.01, width = "100px"), 
                                               sliderInput("proportion_norm", "Proprtion (P): ", value = 0.9, min = 0.01, max = 0.99, step = 0.01),
                                               numericInput("proportionBox_norm", "", value = 0.9, min = 0.01, max = 0.99, step = 0.01, width = "100px"),
                                               sliderInput("error_norm2sided", paste0("Permissable Overshoot (", greeks("epsilon"), "): "), value = 0.099, min = 0.001, max = 0.989, step = 0.001),
                                               numericInput("errorBox_norm2sided", "", value = 0.099, min = 0.001, max = 0.989, step = 0.001, width = "100px"),
                                               sliderInput("risk_norm2sided", "Risk (\\(\\boldsymbol{\\alpha^*}\\))", value = 0.01, min = 0.01, max = 0.99, step = 0.01),
                                               numericInput("riskBox_norm2sided", "", value = 0.01, min = 0.01, max = 0.99, step = 0.01, width = "100px")
                                             )
                                      ),
                                      column(4,
                                             conditionalPanel(
                                               condition = 'input.TI_type == "1-Sided"',
                                               uiOutput("sample_size_norm1sided"),
                                               selectInput("calculate", "", choices = c("PROCEED TO TI LIMIT CALCULATION", "Yes", "No"), selected = NULL
                                               ),
                                               conditionalPanel(
                                                 condition = 'input.calculate == "Yes"',
                                                 numericInput("sample_mean", "Sample Mean (\\(\\boldsymbol{\\bar{x}}\\)): ", value = 0),
                                                 numericInput("sample_std_dev", "Sample Standard Deviation (\\(s\\)): ", value = 1)
                                               ),
                                               conditionalPanel(
                                                 condition = 'input.calculate == "No"'
                                               )
                                             ),
                                             conditionalPanel(
                                               condition = 'input.TI_type == "2-Sided"',
                                               uiOutput("sample_size_norm2sided"),
                                               selectInput("calculate", "", choices = c("PROCEED TO TI LIMIT CALCULATION", "Yes", "No"), selected = NULL
                                               ),
                                               conditionalPanel(
                                                 condition = 'input.calculate == "Yes"',
                                                 numericInput("sample_mean", "Sample Mean (\\(\\boldsymbol{\\bar{x}}\\)): ", value = 0),
                                                 numericInput("sample_std_dev", "Sample Standard Deviation (\\(s\\)): ", value = 1)
                                               ),
                                               conditionalPanel(
                                                 condition = 'input.calculate == "No"'
                                               )
                                             )
                                      ),
                                      column(4,
                                             conditionalPanel(
                                               condition = 'input.TI_type == "1-Sided" && input.calculate == "Yes"',
                                               selectInput("n_limit_type", "LIMIT TYPE: ", choices = c("Upper Limit", "Lower Limit")
                                               ),
                                               conditionalPanel(
                                                 condition = 'input.n_limit_type == "Upper Limit"',
                                                 uiOutput("upperLimit_norm"),
                                                 plotOutput("upperLimit_normPlot")
                                               ),
                                               conditionalPanel(
                                                 condition = 'input.n_limit_type == "Lower Limit"',
                                                 uiOutput("lowerLimit_norm"),
                                                 plotOutput("lowerLimit_normPlot")
                                               )
                                             ),
                                             conditionalPanel(
                                               condition = 'input.TI_type == "2-Sided" && input.calculate == "Yes"',
                                               uiOutput("twoSidedLimit_norm"),
                                               plotOutput("twoSidedLimit_normPlot")
                                             )
                                      )
                                    )
                           )
                )
                
                
)


#################### SERVER SIDE ####################
server <- function(input, output) {
  
  #################### NONPARAMETRIC REACTIVE VARIABLES ####################
  confidence_nonP <- reactive({
    input$confidence_nonP
  })
  
  observeEvent(input$confidenceBox_nonP, {
    if ((input$confidenceBox_nonP != input$confidence_nonP) & (is.na(input$confidenceBox_nonP) == FALSE)) {
      updateSliderInput(
        inputId = "confidence_nonP",
        value = input$confidenceBox_nonP
      )
    }
  })
  
  observeEvent(input$confidence_nonP, {
    if ((input$confidence_nonP != input$confidenceBox_nonP) | (is.na(input$confidenceBox_nonP) == TRUE)) {
      updateNumericInput(
        inputId = "confidenceBox_nonP",
        value = input$confidence_nonP
      )
    }
  })
  
  proportion_nonP <- reactive({
    input$proportion_nonP
  })
  
  observeEvent(input$proportionBox_nonP, {
    if ((input$proportionBox_nonP != input$proportion_nonP) & (is.na(input$proportionBox_nonP) == FALSE)) {
      updateSliderInput(
        inputId = "proportion_nonP",
        value = input$proportionBox_nonP
      )
    }
  })
  
  observeEvent(input$proportion_nonP, {
    if ((input$proportionBox_nonP != input$proportion_nonP) | (is.na(input$proportionBox_nonP) == TRUE)) {
      updateNumericInput(
        inputId = "proportionBox_nonP",
        value = input$proportion_nonP
      )
    }
  })
  
  error_nonP <- reactive({
    input$error_nonP
  })
  
  errorBox_nonP <- reactive({
    input$errorBox_nonP
  })
  
  observeEvent(input$proportion_nonP, {
    updateSliderInput(
      inputId = "error_nonP",
      value = 1-input$proportion_nonP,
      max = 1-input$proportion_nonP
    )
  })
  
  observeEvent(input$errorBox_nonP, {
    if ((input$errorBox_nonP != input$error_nonP) & (is.na(input$errorBox_nonP) == FALSE)) {
      updateSliderInput(
        inputId = "error_nonP",
        value = input$errorBox_nonP
      )
    }
  })
  
  observeEvent(input$error_nonP, { 
    if ((input$errorBox_nonP != input$error_nonP) | (is.na(input$errorBox_nonP) == TRUE)) {
      updateNumericInput(
        inputId = "errorBox_nonP",
        value = input$error_nonP
      )
    }
  })
  
  risk_nonP <- reactive({
    input$risk_nonP
  })
  
  observeEvent(input$riskBox_nonP, {
    if ((input$riskBox_nonP != input$risk_nonP) & (is.na(input$riskBox_nonP) == FALSE)) {
      updateSliderInput(
        inputId = "risk_nonP",
        value = input$riskBox_nonP
      )
    }
  })
  
  observeEvent(input$risk_nonP, {
    if ((input$riskBox_nonP != input$risk_nonP) | (is.na(input$riskBox_nonP) == TRUE)) {
      updateNumericInput(
        inputId = "riskBox_nonP",
        value = input$risk_nonP
      )
    }
  })
  
  #################### NORMAL REACTIVE VARIABLES ####################
  
  confidence_norm <- reactive({
    input$confidence_norm
  })
  
  observeEvent(input$confidenceBox_norm, {
    if ((input$confidenceBox_norm != input$confidence_norm) & (is.na(input$confidenceBox_norm) == FALSE)) {
      updateSliderInput(
        inputId = "confidence_norm",
        value = input$confidenceBox_norm
      )
    }
  })
  
  observeEvent(input$confidence_norm, {
    if ((input$confidence_norm != input$confidenceBox_norm) | (is.na(input$confidenceBox_norm) == TRUE)) {
      updateNumericInput(
        inputId = "confidenceBox_norm",
        value = input$confidence_norm
      )
    }
  })
  
  proportion_norm <- reactive({
    input$proportion_norm
  })
  
  observeEvent(input$proportionBox_norm, {
    if ((input$proportionBox_norm != input$proportion_norm) & (is.na(input$proportionBox_norm) == FALSE)) {
      updateSliderInput(
        inputId = "proportion_norm",
        value = input$proportionBox_norm
      )
    }
  })
  
  observeEvent(input$proportion_norm, {
    if ((input$proportionBox_norm != input$proportion_norm) | (is.na(input$proportionBox_norm) == TRUE)) {
      updateNumericInput(
        inputId = "proportionBox_norm",
        value = input$proportion_norm
      )
    }
  })
  
  error_norm1sided <- reactive({
    input$error_norm1sided
  })
  
  errorBox_norm1sided <- reactive({
    input$errorBox_norm1sided
  })
  
  observeEvent(input$proportion_norm, {
    updateSliderInput(
      inputId = "error_norm1sided",
      value = 1-input$proportion_norm-0.001,
      max = 1-input$proportion_norm-0.001
    )
  })
  
  observeEvent(input$errorBox_norm1sided, {
    if ((input$errorBox_norm1sided != input$error_norm1sided) & (is.na(input$errorBox_norm1sided) == FALSE)) {
      updateSliderInput(
        inputId = "error_norm1sided",
        value = input$errorBox_norm1sided
      )
    }
  })
  
  observeEvent(input$error_norm1sided, { 
    if ((input$errorBox_norm1sided != input$error_norm1sided) | (is.na(input$errorBox_norm1sided) == TRUE)) {
      updateNumericInput(
        inputId = "errorBox_norm1sided",
        value = input$error_norm1sided
      )
    }
  })
  
  error_norm2sided <- reactive({
    input$error_norm2sided
  })
  
  errorBox_norm2sided <- reactive({
    input$errorBox_norm2sided
  })
  
  observeEvent(input$proportion_norm, {
    updateSliderInput(
      inputId = "error_norm2sided",
      value = 1-input$proportion_norm-0.001,
      max = 1-input$proportion_norm-0.001
    )
  })
  
  observeEvent(input$errorBox_norm2sided, {
    if ((input$errorBox_norm2sided != input$error_norm2sided) & (is.na(input$errorBox_norm2sided) == FALSE)) {
      updateSliderInput(
        inputId = "error_norm2sided",
        value = input$errorBox_norm2sided
      )
    }
  })
  
  observeEvent(input$error_norm2sided, { 
    if ((input$errorBox_norm2sided != input$error_norm2sided) | (is.na(input$errorBox_norm2sided) == TRUE)) {
      updateNumericInput(
        inputId = "errorBox_norm2sided",
        value = input$error_norm2sided
      )
    }
  })
  
  risk_norm1sided <- reactive({
    input$risk_norm1sided
  })
  
  observeEvent(input$riskBox_norm1sided, {
    if ((input$riskBox_norm1sided != input$risk_norm1sided) & (is.na(input$riskBox_norm1sided) == FALSE)) {
      updateSliderInput(
        inputId = "risk_norm1sided",
        value = input$riskBox_norm1sided
      )
    }
  })
  
  observeEvent(input$risk_norm1sided, {
    if ((input$riskBox_norm1sided != input$risk_norm1sided) | (is.na(input$riskBox_norm1sided) == TRUE)) {
      updateNumericInput(
        inputId = "riskBox_norm1sided",
        value = input$risk_norm1sided
      )
    }
  })
  
  risk_norm2sided <- reactive({
    input$risk_norm2sided
  })
  
  observeEvent(input$riskBox_norm2sided, {
    if ((input$riskBox_norm2sided != input$risk_norm2sided) & (is.na(input$riskBox_norm2sided) == FALSE)) {
      updateSliderInput(
        inputId = "risk_norm2sided",
        value = input$riskBox_norm2sided
      )
    }
  })
  
  observeEvent(input$risk_norm2sided, {
    if ((input$riskBox_norm2sided != input$risk_norm2sided) | (is.na(input$riskBox_norm2sided) == TRUE)) {
      updateNumericInput(
        inputId = "riskBox_norm2sided",
        value = input$risk_norm2sided
      )
    }
  })
  
  #################### SAMPLE SIZE/STD DEV ####################
  
  sample_mean <- reactive({
    input$sample_mean
  })
  
  sample_std_dev <- reactive({
    input$sample_std_dev
  })
  
  #################### SAMPLE SIZE ALGORITHMS ####################
  number_of_successes_and_runs <- function(c,p,e,r) {
    start_num = 1000000 
    
    runs <- start_num
    largest_unconvergent <- 0
    smallest_convergent <- 0
    converged <- FALSE
    
    while (converged == FALSE) {
      K <- qbinom(c, runs, p) 
      
      if ((pbinom(K, runs, p, lower.tail = FALSE) <= c) & (pbinom(K, runs, p + e) >= r)) {    #i.e. P(X >= K) < C in (P,C)*100% tolerance interval and P(X <= K) > R in (P+E,C)*100% tolerance interval
        largest_unconvergent <- runs 
      } 
      else { 
        smallest_convergent <- runs 
      }
      
      difference <- largest_unconvergent - smallest_convergent
      
      if (difference > 0) {
        runs <- 10*runs 
      }
      else if (difference < -1) {
        runs <- floor((smallest_convergent - largest_unconvergent)/2 + largest_unconvergent)
      }
      else {
        N <- smallest_convergent 
        R <- ceiling((N-K+2)/2)
        S <- floor((N+K)/2)
        converged <- TRUE 
      }  
    }   
    return (c(N, K, N - K + 1,R,S)) #(sample size, upper 1-sided limit, lower 1-sided limit, upper 2-sided limit, lower 2-sided limit)
  }
  
  sample_size_norm1sided <- function(c,p,e,r) {
    start_num = 1000000
    z_p <- qnorm(p,0,1)
    z_pe <- qnorm(p+e,0,1)
    
    runs <- start_num
    largest_unconvergent <- 0
    smallest_convergent <- 0
    converged <- FALSE
    
    while (converged == FALSE) {
      if (qt(c,runs-1,z_p*sqrt(runs)) > qt(r,runs-1,z_pe*sqrt(runs))) {
        largest_unconvergent <- runs
      } 
      else { 
        smallest_convergent <- runs
      }
      
      difference <- largest_unconvergent - smallest_convergent
      
      if (difference > 0) {
        runs <- 10*runs 
      }
      else if (difference < -2) {
        runs <- ceiling((smallest_convergent - largest_unconvergent)/2 + largest_unconvergent)
      }
      else {
        N <- smallest_convergent
        converged <- TRUE 
      }  
    }   
    return (N)
  }
  
  sample_size_norm2sided <- function(c,p,e,r) {
    start_num = 1000000
    z_p <- qnorm((1+p)/2,0,1)
    z_pe <- qnorm((1+p+e)/2,0,1)
    
    runs <- start_num
    largest_unconvergent <- 0
    smallest_convergent <- 0
    converged <- FALSE
    
    while (converged == FALSE) {
      if ((qchisq(c,runs-1)/qchisq(r,runs-1)) > (z_pe/z_p)^2) {
        largest_unconvergent <- runs
      } 
      else { 
        smallest_convergent <- runs
      }
      
      difference <- largest_unconvergent - smallest_convergent
      
      if (difference > 0) {
        runs <- 10*runs 
      }
      else if (difference < -2) {
        runs <- ceiling((smallest_convergent - largest_unconvergent)/2 + largest_unconvergent)
      }
      else {
        N <- smallest_convergent
        converged <- TRUE 
      }  
    }   
    return (N)
  }
  
  #################### FUNCTIONS FOR (NORMAL) TOLERANCE INTERVAL THRESHOLDS ####################
  a <- function(n,c) {
    z_c <- qnorm(1-c,0,1)
    return (1-((z_c)^2/(2*n-2))) 
  }
  
  b <- function(n,c,p) {
    z_c <- qnorm(1-c,0,1)
    z_p <- qnorm(p,0,1)
    return (z_p^2 - ((z_c)^2/n))
  }
  
  k1 <- function(n,c,p) {
    z_p <- qnorm(p,0,1)
    return ((z_p + sqrt((z_p)^2-a(n,c)*b(n,c,p)))/a(n,c))
  }
  
  k2 <- function(n,c,p) {
    z_p <- qnorm((1-p)/2,0,1)
    return (z_p*sqrt(((n-1)*(1+(1/n)))/qchisq(c,n-1)))
  }
  
  #################### OUTPUTS ####################
  output$output_nonP <- renderUI({
    if (input$limit_type == "Upper Limit") {
      fluidRow(
        column(12, offset = 1,
               h4("MINIMUM SAMPLE SIZE REQUIRED: ", number_of_successes_and_runs(confidence_nonP(),proportion_nonP(),error_nonP(),risk_nonP())[1]),
               br(),
               h4("UPPER LIMIT DATA POINT: THE ORDER ", number_of_successes_and_runs(confidence_nonP(),proportion_nonP(),error_nonP(),risk_nonP())[2], " STATISTIC"),
               br(),
               h5("Given a confidence of ", confidence_nonP(), ", a proportion of ", proportion_nonP(), ", a permissable overshoot amount of ", error_nonP(), ", and a risk of ", risk_nonP(), 
                  ", the upper tolerance interval limit is given by the order ", number_of_successes_and_runs(confidence_nonP(),proportion_nonP(),error_nonP(),risk_nonP())[2], " statistic out of a sample of size ", number_of_successes_and_runs(confidence_nonP(),proportion_nonP(),error_nonP(),risk_nonP())[1], ".")
        )
      )
    }
    else if (input$limit_type == "Lower Limit") {
      fluidRow(
        column(12, offset = 1,
               h4("MINIMUM SAMPLE SIZE REQUIRED: ", number_of_successes_and_runs(confidence_nonP(),proportion_nonP(),error_nonP(),risk_nonP())[1]),
               br(),
               h4("UPPER LIMIT DATA POINT: THE ORDER ", number_of_successes_and_runs(confidence_nonP(),proportion_nonP(),error_nonP(),risk_nonP())[3], " STATISTIC"),
               br(),
               h5("Given a confidence of ", confidence_nonP(), ", a proportion of ", proportion_nonP(), ", a permissable overshoot amount of ", error_nonP(), ", and a risk of ", risk_nonP(), 
                  ", the lower tolerance interval limit is given by the order ", number_of_successes_and_runs(confidence_nonP(),proportion_nonP(),error_nonP(),risk_nonP())[3], " statistic out of a sample of size ", number_of_successes_and_runs(confidence_nonP(),proportion_nonP(),error_nonP(),risk_nonP())[1], ".")
        )
      )
    }
    else {
      fluidRow(
        column(12, offset = 1,
               h4("MINIMUM SAMPLE SIZE REQUIRED: ", number_of_successes_and_runs(confidence_nonP(),proportion_nonP(),error_nonP(),risk_nonP())[1]),
               br(),
               h4("UPPER LIMIT DATA POINT: THE ORDER ", number_of_successes_and_runs(confidence_nonP(),proportion_nonP(),error_nonP(),risk_nonP())[5], " STATISTIC"),
               h4("LOWER LIMIT DATA POINT: THE ORDER ", number_of_successes_and_runs(confidence_nonP(),proportion_nonP(),error_nonP(),risk_nonP())[4], " STATISTIC"),
               br(),
               h5("Given a confidence of ", confidence_nonP(), ", a proportion of ", proportion_nonP(), ", a permissable overshoot amount of ", error_nonP(), ", and a risk of ", risk_nonP(), 
                  ", the 2-sided tolerance interval limits are given by the order ", number_of_successes_and_runs(confidence_nonP(),proportion_nonP(),error_nonP(),risk_nonP())[4], "and ", number_of_successes_and_runs(confidence_nonP(),proportion_nonP(),error_nonP(),risk_nonP())[5],  
                  " statistics out of a sample size of", number_of_successes_and_runs(confidence_nonP(),proportion_nonP(),error_nonP(),risk_nonP())[1], ".")
        )
      )
    } 
  })
  
  output$sample_size_norm1sided <- renderUI({
    column(12,
           h4("MINIMUM SAMPLE SIZE REQUIRED: ", sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided())),
           br(),
           h5("Given a confidence of ", confidence_norm(), ", a proportion of ", proportion_norm(), ", a permissable overshoot amount of ", error_norm1sided(), ", and a risk of ", risk_norm1sided(), 
              ", the minimum sample size required for a 1-sided tolerance interval is ",  sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()), ".")
    )
  })
  
  output$sample_size_norm2sided <- renderUI({
    column(12,
           h4("MINIMUM SAMPLE SIZE REQUIRED: ", sample_size_norm2sided(confidence_norm(),proportion_norm(),error_norm2sided(),risk_norm2sided())),
           br(),
           h5("Given a confidence of ", confidence_norm(), ", a proportion of ", proportion_norm(), ", a permissable overshoot amount of ", error_norm2sided(), ", and a risk of ", risk_norm2sided(), 
              ", the minimum sample size required for a 2-sided tolerance interval is ",  sample_size_norm2sided(confidence_norm(),proportion_norm(),error_norm2sided(),risk_norm2sided()), ".")
    )
  })
  
  output$upperLimit_norm <- renderUI({
    if (is.na(sample_mean()) | is.na(sample_std_dev()) == TRUE) {
      validate('')
    }
    else if (is.na(k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm())) == 'TRUE') {
      validate("This combination of inputs yields mathematically invalid calculations. It is recommended that you increase confidence or reduce risk.", errorClass = "danger")
      req(k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm()) >= -k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm()))
      req(sample_std_dev() >= 0)
    }
    else if (k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm()) < -k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm())) {
      validate("This combination of levels inputs unreliable limit calculations. We recommend that you reduce the risk level.", errorClass = "warning")
    }
    else if (sample_std_dev() < 0) {
      validate("Sample standard deviation cannot be negative.", errorClass = "warning")
    }
    else {
      column(12, 
             h4("UPPER TOLERANCE INTERVAL LIMIT: ", round(sample_mean() + k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm())*sample_std_dev(), digits = 3)),
             br(),
             h5("Given a confidence of ", confidence_norm(), ", a proportion of ", proportion_norm(), ", a permissable overshoot amount of ", error_norm1sided(), ", and a risk of ", risk_norm1sided(), 
                ", a minimum sample size of ",  sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()), ", a sample mean of ", sample_mean(), ", and sample standard deviation of ", sample_std_dev(), 
                ", the upper tolerance interval limit is given by ", round(sample_mean() + k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm())*sample_std_dev(), digits = 3))
      )
    }
  })
  
  output$upperLimit_normPlot <- renderPlot({
    if (is.na(sample_mean()) | is.na(sample_std_dev()) == TRUE) {
      validate('')
    }
    else if (is.na(k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm())) == 'TRUE') {
      validate('')
    }
    else if (k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm()) < -k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm())) {
      validate('')
    }
    else if (sample_std_dev() < 0) {
      validate('')
    }
    else {
      x <- seq(sample_mean()-3*sample_std_dev(), sample_mean()+3*sample_std_dev(), 0.01*sample_std_dev())
      y <- dnorm(x,sample_mean(), sample_std_dev())
      
      x_shaded <- seq(sample_mean()-3.5*sample_std_dev(), sample_mean() + k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm())*sample_std_dev(), 0.01*sample_std_dev())
      y_shaded <- c(dnorm(x_shaded,sample_mean(),sample_std_dev()), 0)
      x_shaded <- c(x_shaded, sample_mean() + k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm())*sample_std_dev())
      
      ggplot() + 
        geom_line(aes(x, y)) + 
        scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) + 
        geom_polygon(data = data.frame(x=x_shaded, y=y_shaded), aes(x_shaded, y_shaded), fill = "deepskyblue", alpha = 0.5) + 
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()
        ) 
    }
  })
  
  output$lowerLimit_norm <- renderUI({
    if (is.na(sample_mean()) | is.na(sample_std_dev()) == TRUE) {
      validate('')
    }
    else if (is.na(k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm())) == 'TRUE') {
      validate("This combination of inputs yields invalid calculations. Try increasing confidence and/or proportion, or reducing risk.", errorClass = "danger")
      req(k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm()) >= -k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm()))
      req(sample_std_dev() >= 0)
    }
    else if (k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm()) < -k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm())) {
      validate("This combination of levels inputs unreliable limit calculations. We recommend that you reduce the risk level.", errorClass = "warning")
    }
    else if (sample_std_dev() < 0) {
      validate("Sample standard deviation cannot be negative.", errorClass = "warning")
    }
    else {
      column(12, 
             h4("LOWER TOLERANCE INTERVAL LIMIT: ", round(sample_mean() - k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm())*sample_std_dev(), digits = 3)),
             br(),
             h5("Given a confidence of ", confidence_norm(), ", a proportion of ", proportion_norm(), ", a permissable overshoot amount of ", error_norm1sided(), ", and a risk of ", risk_norm1sided(), 
                ", a minimum sample size of ",  sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()), ", a sample mean of ", sample_mean(), ", and sample standard deviation of ", sample_std_dev(), 
                ", the upper tolerance interval limit is given by ", round(sample_mean() - k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm())*sample_std_dev(), digits = 3))
      )
    }
  })
  
  output$lowerLimit_normPlot <- renderPlot({
    if (is.na(sample_mean()) | is.na(sample_std_dev()) == TRUE) {
      validate('')
    }
    else if (is.na(k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm())) == 'TRUE') {
      validate('')
    }
    else if (k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm()) < -k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm())) {
      validate('')
    }
    else if (sample_std_dev() < 0) {
      validate('')
    }
    else {
      x <- seq(sample_mean()-3*sample_std_dev(), sample_mean()+3*sample_std_dev(), 0.01*sample_std_dev())
      y <- dnorm(x,sample_mean(), sample_std_dev())
      
      x_shaded <- seq(sample_mean() - k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm())*sample_std_dev(), sample_mean()+3.5*sample_std_dev(), 0.01*sample_std_dev())
      y_shaded <- c(dnorm(x_shaded,sample_mean(), sample_std_dev()), 0)
      x_shaded <- c(x_shaded, sample_mean() - k1(sample_size_norm1sided(confidence_norm(),proportion_norm(),error_norm1sided(),risk_norm1sided()),confidence_norm(),proportion_norm())*sample_std_dev())
      
      ggplot() + 
        geom_line(aes(x, y)) + 
        scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) + 
        geom_polygon(data = data.frame(x=x_shaded, y=y_shaded), aes(x_shaded, y_shaded), fill = "deepskyblue", alpha = 0.5) + 
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()
        )  
    }
  })
  
  output$twoSidedLimit_norm <- renderUI({
    if (is.na(sample_mean()) | is.na(sample_std_dev()) == TRUE) {
      validate('')
    }
    else if (is.na(k2(sample_size_norm2sided(confidence_norm(),proportion_norm(),error_norm2sided(),risk_norm2sided()),confidence_norm(),proportion_norm())) == 'TRUE') {
      validate("This combination of levels yields invalid calculations. Try increasing confidence and/or proportion, or reducing risk.", errorClass = "danger")
      req(k2(sample_size_norm2sided(confidence_norm(),proportion_norm(),error_norm2sided(),risk_norm2sided()),confidence_norm(),proportion_norm()) <= -k2(sample_size_norm2sided(confidence_norm(),proportion_norm(),error_norm2sided(),risk_norm2sided()),confidence_norm(),proportion_norm()))
      req(sample_std_dev() >= 0)
    }
    else if (k2(sample_size_norm2sided(confidence_norm(),proportion_norm(),error_norm2sided(),risk_norm2sided()),confidence_norm(),proportion_norm()) > -k2(sample_size_norm2sided(confidence_norm(),proportion_norm(),error_norm2sided(),risk_norm2sided()),confidence_norm(),proportion_norm())) {
      validate("This combination of levels inputs unreliable limit calculations. We recommend that you reduce the risk level.", errorClass = "warning")
    }
    else if (sample_std_dev() < 0) {
      validate("Sample standard deviation cannot be negative.", errorClass = "warning")
    }
    else {
      column(12, 
             h4("2-SIDED TOLERANCE INTERVAL LIMITS: ", round(sample_mean() - k2(sample_size_norm2sided(confidence_norm(),proportion_norm(),error_norm2sided(),risk_norm2sided()),confidence_norm(),proportion_norm())*sample_std_dev(), digits = 3), " AND ", round(sample_mean() + k2(sample_size_norm2sided(confidence_norm(),proportion_norm(),error_norm2sided(),risk_norm2sided()),confidence_norm(),proportion_norm())*sample_std_dev(), digits = 3)),   
             br(),
             h5("Given a confidence of ", confidence_norm(), ", a proportion of ", proportion_norm(), ", a permissable overshoot amount of ", error_norm2sided(), ", and a risk of ", risk_norm2sided(), 
                ", a minimum sample size of ",  sample_size_norm2sided(confidence_norm(),proportion_norm(),error_norm2sided(),risk_norm2sided()), ", a sample mean of ", sample_mean(), ", and sample standard deviation of ", sample_std_dev(), 
                ", the two-sided tolerance interval limits are given by ", round(sample_mean() - k2(sample_size_norm2sided(confidence_norm(),proportion_norm(),error_norm2sided(),risk_norm2sided()),confidence_norm(),proportion_norm())*sample_std_dev(), digits = 3), 
                " and ", round(sample_mean() + k2(sample_size_norm2sided(confidence_norm(),proportion_norm(),error_norm2sided(),risk_norm2sided()),confidence_norm(),proportion_norm())*sample_std_dev(), digits = 3), "(respectively).")
      )
    }
  })
  
  output$twoSidedLimit_normPlot <- renderPlot({
    if (is.na(sample_mean()) | is.na(sample_std_dev()) == TRUE) {
      validate('')
    }
    else if (is.na(k2(sample_size_norm2sided(confidence_norm(),proportion_norm(),error_norm2sided(),risk_norm2sided()),confidence_norm(),proportion_norm())) == 'TRUE') {
      validate('')
    }
    else if (k2(sample_size_norm2sided(confidence_norm(),proportion_norm(),error_norm2sided(),risk_norm2sided()),confidence_norm(),proportion_norm()) > -k2(sample_size_norm2sided(confidence_norm(),proportion_norm(),error_norm2sided(),risk_norm2sided()),confidence_norm(),proportion_norm())) {
      validate('')
    }
    else if (sample_std_dev() < 0) {
      validate('')
    }
    else {
      x <- seq(sample_mean()-3*sample_std_dev(), sample_mean()+3*sample_std_dev(), 0.01*sample_std_dev())
      y <- dnorm(x,sample_mean(),sample_std_dev())
      
      x_shaded <- seq(sample_mean() + k2(sample_size_norm2sided(confidence_norm(),proportion_norm(),error_norm2sided(),risk_norm2sided()),confidence_norm(),proportion_norm())*sample_std_dev(), sample_mean() - k2(sample_size_norm2sided(confidence_norm(),proportion_norm(),error_norm2sided(),risk_norm2sided()),confidence_norm(),proportion_norm())*sample_std_dev(), 0.01*sample_std_dev())
      y_shaded <- c(0, dnorm(x_shaded,sample_mean(),sample_std_dev()), 0)
      x_shaded <- c(sample_mean() + k2(sample_size_norm2sided(confidence_norm(),proportion_norm(),error_norm2sided(),risk_norm2sided()),confidence_norm(),proportion_norm())*sample_std_dev(), x_shaded, sample_mean() - k2(sample_size_norm2sided(confidence_norm(),proportion_norm(),error_norm2sided(),risk_norm2sided()),confidence_norm(),proportion_norm())*sample_std_dev())
      
      ggplot() + 
        geom_line(aes(x, y)) + 
        scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) + 
        geom_polygon(data = data.frame(x=x_shaded, y=y_shaded), aes(x_shaded, y_shaded), fill = "deepskyblue", alpha = 0.5) + 
        theme(axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank()
        )
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
