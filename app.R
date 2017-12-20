#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
library(fscaret)
library(DT)

regChoices <- funcRegPred
classChoices <- funcClassPred

#' busyIndicator from https://github.com/AnalytixWare/ShinySky/blob/master/R/busy-indicator.r by  xiaodaigh
#' 
#' A busy indicator
#' 
#' @param text The text to show
#' @param img An anitmated gif
#' @param wait The amount of time to wait before showing the busy indicator. The
#'   default is 1000 which is 1 second.
#'   
#' @export
busyIndicator <- function(text = "Calculation in progress..",img = "ajax-loader.gif", wait=1000) {
  tagList(
    singleton(tags$head(
      tags$link(rel="stylesheet", type="text/css",href="busyIndicator.css")
    ))
    ,div(class="shinysky-busy-indicator",p(text),img(src=img))
    ,tags$script(sprintf(
      "	setInterval(function(){
      if ($('html').hasClass('shiny-busy')) {
      setTimeout(function() {
      if ($('html').hasClass('shiny-busy')) {
      $('div.shinysky-busy-indicator').show()
      }
      }, %d)  		    
      } else {
      $('div.shinysky-busy-indicator').hide()
      }
},100)
      ",wait)
    )
  )	
  }





# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  headerPanel("fscaret shiny ui"),

  ## START of custom CSS for 3 column layout (used below for mechanics filter options)
  tags$head(
    tags$style(HTML("

     .multicol {

       -webkit-column-count: 3; /* Chrome, Safari, Opera */

       -moz-column-count: 3; /* Firefox */

       column-count: 3;

     }

   "))
    
  ),
  
  ## END of custom CSS for 3 column layout (used below for mechanics filter options)
  

    
  tabsetPanel(id="tabs",
              tabPanel("Home",fluid=TRUE,
                       titlePanel(h2("Introduction")),
                       mainPanel(
                          p("This is an User Interface (ui) application which implements the automated feature selection
                                 provided by the 'fscaret' package of R-environment. Please visit the page ",
                            a("https://cran.r-project.org/package=fscaret",href="https://cran.r-project.org/package=fscaret",
                                                " for more information on fscaret.",align="justified")),
                          p("In general the fscaret is a wrapper module. It uses the engine of the caret package to build models and to get the
                              variable rankings. When models are build the algorithm draws variable importance from the models (directly or indirectly).
                              To compare the raw results the scaling process is introduced. Moreover, developed models are used to get prediction errors
                              (RMSE and MSE). Finally the output is produced as a variable importance ranking.",align="justified"),
                          p("In summary the whole feature ranking procedure can be divided into six steps:"),
                          tags$ol(
                            tags$li("Provide the input data sets and settings."),
                            tags$li("Build the models."),
                            tags$li("Draw out the variable rankings from the models."),
                            tags$li("Calculate the generalization errorfor each model."),
                            tags$li("Scale the variable rankings according to the generalization error."),
                            tags$li("Summarize the the results in table.")
                              ),
                          p("The overview of the concept for feature selection based on fscaret is presented on the image below."),
                          tags$img(src="fscaret_idea.png",width="621px",height="360px")
                        )
              ),
              
              
    tabPanel("Upload data",fluid=TRUE,
        
    
  # App title ----
  titlePanel("Uploading Data Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file_train", "Choose TRAINING CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),

      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Horizontal line ----
      tags$hr(),
      
      
      # Input: Select a file ----
      fileInput("file_test", "Choose TEST CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      hr(),
      helpText("To actiavate next tabs please select both training and testing data sets.")
      
    ),
      
      # Show a plot of the generated distribution
      mainPanel(
        # Output: Data file ----
        tableOutput("contents_train"),
        tableOutput("contents_test")
        )
      )
    ),
  
    tabPanel("Options",fluid=TRUE,
             
             
               mainPanel(
                 fluidRow(
                   column(5,
                 
                          tags$h4(print('Parameters - regression')),
                          hr(),
                          checkboxInput("regPred",tags$b("Regression problem"),value=TRUE),

                          hr(),
                          #Parameters/Variables
                          radioButtons("installReqPckg1","Install all required packages before calculations?",
                                        list("TRUE","FALSE"), selected = "FALSE" ),
                          
                          radioButtons("preprocessData1","Pre-process data before training the models?",
                                       list("TRUE","FALSE"), selected = "FALSE" ),
                          radioButtons("withlabels1","Input data include header?",
                                       list("TRUE","FALSE"), selected = "TRUE" ),
                          radioButtons("impCalcMet1","Scale variable importence according to:",
                                       list("RMSE&MSE","RMSE","MSE"), selected = "RMSE&MSE" ),
                          numericInput("myTimeLimit1","Time limit for single model development (in seconds):",
                                       3600),
                          numericInput("nocores1", "Number of cores used", 4), # -1 value causes error!!
                          radioButtons("method1","Method passed to fitControl of caret package:",
                                       list("boot","boot_all","oob","cv","boot632","repeatedcv","LOOCV","LGOCV"), selected = "boot" ),
                          radioButtons("returnResamp1","Returned resampling method passed to fitControl of caret package:",
                                       list("all","final","none"), selected = "all" ),
                          radioButtons("missData1","Handling of missing data values:",
                                       list("delRow","delCol","meanCol","NULL"), selected = "NULL" ),
                          radioButtons("supressOutput1","Supress the output of modeling phase by caret functions:",
                                       list("TRUE","FALSE"), selected = "FALSE" ),
                          radioButtons("saveModel1","Trained models should be embedded in the result:",
                                       list("TRUE","FALSE"), selected = "FALSE" ),
                          hr(),
                          tags$h4(print('Models selection')),
                          checkboxInput("regAllNone","All/None",value = TRUE),
                          tags$div(class="multicol",checkboxGroupInput("regFuncSelect","Regression models",regChoices,selected = TRUE))

                          ),
                 
                         column(5,
                         
                         tags$h4(print('Parameters - classification')),
                         hr(),
                         checkboxInput("classPred",tags$b("Classification problem"),value=FALSE),
                         bsTooltip("classPred", "Please, check only one of Regression OR Classification checkbox.",
                                   "right", options = list(container = "body")),
                         hr(),
                         #Parameters/Variables
                         radioButtons("installReqPckg2","Install all required packages before calculations?",
                                      list("TRUE","FALSE"), selected = "FALSE" ),
                         radioButtons("preprocessData2","Pre-process data before training the models?",
                                      list("TRUE","FALSE"), selected = "FALSE" ),
                         radioButtons("withlabels2","Input data include header?",
                                      list("TRUE","FALSE"), selected = "TRUE" ),
                         numericInput("myTimeLimit2","Time limit for single model development (in seconds):",
                                      3600),
                         numericInput("nocores2", "Number of cores used", -1),
                         radioButtons("method2","Method passed to fitControl of caret package:",
                                      list("boot","boot_all","oob","cv","boot632","repeatedcv","LOOCV","LGOCV"), selected = "boot" ),
                         radioButtons("returnResamp2","Returned resampling method passed to fitControl of caret package:",
                                      list("all","final","none"), selected = "all" ),
                         radioButtons("missData2","Handling of missing data values:",
                                      list("delRow","delCol","meanCol","NULL"), selected = "NULL" ),
                         radioButtons("supressOutput2","Supress the output of modeling phase by caret functions:",
                                      list("TRUE","FALSE"), selected = "FALSE" ),
                         radioButtons("saveModel2","Trained models should be embedded in the result:",
                                      list("TRUE","FALSE"), selected = "FALSE" ),
                         hr(),
                         tags$h4(print('Models selection')),
                         checkboxInput("classAllNone","All/None",value = TRUE),
                         tags$div(class="multicol",checkboxGroupInput("classFuncSelect","Classification models",classChoices,selected = TRUE))
                         ),
                   
                   
                   
                   # Tool tips - regression options
                   bsTooltip("regPred", "Please, check only one of Regression OR Classification checkbox.",
                             "right", options = list(container = "body")),
                   bsTooltip("installReqPckg1", "Default value = FALSE",
                             "right", options = list(container = "body")),
                   bsTooltip("preprocessData1", "Default value = FALSE",
                             "right", options = list(container = "body")),
                   bsTooltip("withlabels1", "Default value = TRUE",
                             "right", options = list(container = "body")),
                   bsTooltip("impCalcMet1", "Default value = RMSE&MSE",
                             "right", options = list(container = "body")),
                   bsTooltip("myTimeLimit1", "Default value = RMSE&MSE",
                             "right", options = list(container = "body")),
                   bsTooltip("nocores1", "Default value = -1 (Use All Available Cores)",
                             "right", options = list(container = "body")),
                   bsTooltip("method1", "Default value = boot",
                             "right", options = list(container = "body")),
                   bsTooltip("returnResamp1", "Default value = all",
                             "right", options = list(container = "body")),
                   bsTooltip("missData1", "Default value = NULL",
                             "right", options = list(container = "body")),
                   bsTooltip("supressOutput1", "Default value = FALSE",
                             "right", options = list(container = "body")),
                   bsTooltip("saveModel1", "Default value = FALSE",
                             "right", options = list(container = "body")),
                   
                   # Tool tips - classification options
                   bsTooltip("classPred", "Please, check only one of Regression OR Classification checkbox.",
                             "right", options = list(container = "body")),
                   bsTooltip("installReqPckg2", "Default value = FALSE",
                             "right", options = list(container = "body")),
                   bsTooltip("preprocessData2", "Default value = FALSE",
                             "right", options = list(container = "body")),
                   bsTooltip("withlabels2", "Default value = TRUE",
                             "right", options = list(container = "body")),
                   bsTooltip("myTimeLimit2", "Default value = RMSE&MSE",
                             "right", options = list(container = "body")),
                   bsTooltip("nocores2", "Default value = -1 (Use All Available Cores)",
                             "right", options = list(container = "body")),
                   bsTooltip("method2", "Default value = boot",
                             "right", options = list(container = "body")),
                   bsTooltip("returnResamp2", "Default value = all",
                             "right", options = list(container = "body")),
                   bsTooltip("missData2", "Default value = NULL",
                             "right", options = list(container = "body")),
                   bsTooltip("supressOutput2", "Default value = FALSE",
                             "right", options = list(container = "body")),
                   bsTooltip("saveModel2", "Default value = FALSE",
                             "right", options = list(container = "body")),
                   

                         #checkboxInput('multicore', 'Use Multicore', FALSE),

                         
                      #   progressInit(),
                         column(2,
                         
                         br(),
                         br(),
                         
                         actionButton ("runlocal1", "Run computations")
                         
                         
                         )
                 )
               )
             ),
  
  
  tabPanel("Results",fluid=TRUE,

             mainPanel(
              
            
              fluidRow(tags$h3(textOutput("textRMSE"))),  
              fluidRow(p("")),
              busyIndicator(),
              column(width = 8, 
                      DT::dataTableOutput("resultsRMSE",
                                          width = "75%")),
              fluidRow(p("")),
              fluidRow(p("")),
              fluidRow(tags$h3(textOutput("textMSE"))),
              fluidRow(p("")),
              column(width = 8, 
                     DT::dataTableOutput("resultsMSE",
                                         width = "75%"))
              
               
              
             )
             
                        
  ),
  
  tabPanel("About",fluid=TRUE,
           
           sidebarLayout(
             sidebarPanel(
               
             ),
             mainPanel(
               
             )
           )
           
           
           
  ) 
  
  
  
  
                          
          )
  

  
  
  )
  


# Define server logic required to draw a histogram
server <- function(input,output,session) {
  
   
  output$contents_train <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file_train)
    
    df <- read.csv(input$file_train$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
   })
  
  output$contents_test <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file_test)
    
    df <- read.csv(input$file_test$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  observe({
    updateCheckboxGroupInput(
      session, 'regFuncSelect', choices = regChoices,
      selected = if (input$regAllNone) {regChoices}
    )
  })
  
  observe({
    updateCheckboxGroupInput(
      session, 'classFuncSelect', choices = classChoices,
      selected = if (input$classAllNone) {classChoices}
    )
  })
  
  observeEvent(input$regPred, {
    if(input$classPred==TRUE){
    updateCheckboxInput(session,"regPred",value=FALSE)
    }
  })
  
  observeEvent(input$classPred, {
    if(input$regPred==TRUE){
    updateCheckboxInput(session,"classPred",value=FALSE)
    }
    
  })
  

  

rv <- reactiveValues()
rv$data <- NULL
  
  observeEvent(input$runlocal1,{    ## will 'observe' the button press
    
    
      rv$data <- myFS()   ## store the data in the reactive value
      
      output$textRMSE <- renderText({
        
        paste("Variable importance ranking according to RMSE")
        
      })
      
      output$textMSE <- renderText({
        
        paste("Variable importance ranking according to MSE")
        
      })
      
      length(rv$data$VarImp$model)
      
      rv$data
    
  })
  
  output$resultsRMSE <- renderDataTable({
    ## The data has been stored in our rv, so can just return it here
        return(rv$data$VarImp$matrixVarImp.RMSE)
    })
    
  output$resultsMSE <- renderDataTable({
    ## The data has been stored in our rv, so can just return it here
    return(rv$data$VarImp$matrixVarImp.MSE)
  })
  
  
  
  # fscaret function call for regression problems
  myFS <- function(){
    
    trainDF <- read.csv(input$file_train$datapath,
                        header = input$header,
                        sep = input$sep,
                        quote = input$quote)
    
    testDF <- read.csv(input$file_test$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
    
    if(input$nocores1 < 0){
      
      input$nocores1 <- NULL
      
    }

    res <- fscaret(trainDF, testDF, myTimeLimit = input$myTimeLimit1, preprocessData=input$preprocessData1,
                   regPred=input$regPred,
                   Used.funcRegPred=c(input$regFuncSelect), with.labels=input$withlabels1,
                   supress.output=input$supressOutput1, no.cores=input$nocores1)
    res_tab_MSE <- res$VarImp$matrixVarImp.MSE
    res_tab_RMSE <- res$VarImp$matrixVarImp.RMSE
    
    return(res)
    
  }
  
    
  
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)

