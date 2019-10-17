shinyServer(
  pageWithSidebar(
    headerPanel("Stock Prediction Shiny App"),
    
    
    sidebarPanel(
      selectInput("Stocks", "Please Select The Companies",
                  choices = c("AMWD (American Woodmark Corporation)", "AMWD Trend Line", "AMWD Return Estimation", "SFIX (Stitch Fix Inc.)", "FMI (Foundation Medicine Inc.)", "AMBA (Ambarella Inc.)", "CVNA (Carvana Co.)", "NTNX (Nutanix Inc.)", "ESV (ENSCO PLC)", "SM (SM Energy Co.)", "QEP (QEP Resources Inc.)", "VIRT (Virtu Financial Inc.)"),
                  selected = "AMWD Trend Line"),
      
      
      dateRangeInput(inputId = "daterange", label = "Date range",
                     
                     start = Sys.Date() - 250, end = Sys.Date()),
      
      checkboxInput(inputId = "smoothline", label = strong("Stocks Trend Line"), value = FALSE),
      
      
      
      conditionalPanel(condition = "input.Stocks == 'AMWD (American Woodmark Corporation)' ",
                       selectInput("Method", "Please Select The Method",
                                   choices = c("Bootstrap Prediction", "MCMC Prediction"))),
      conditionalPanel(condition = "input.smoothline == true", 
                       sliderInput(inputId = "s", label = "Trend: ",
                                   min = 0.01, max = 1, value = 0.67, step = 0.01,
                                   animate = animationOptions(interval = 100)))
      
    ),           
    
    
    mainPanel(plotOutput(outputId = "lineplot", height = "300px"),
              textOutput(outputId = "textcontent"))
    
  )
)






















