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

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  headerPanel("fscaret shiny ui"),

  
  # Important! : JavaScript functionality to add the Tabs
  tags$head(tags$script(HTML("
                             /* In coherence with the original Shiny way, tab names are created with random numbers. 
                             To avoid duplicate IDs, we collect all generated IDs.  */
                             var hrefCollection = [];
                             
                             Shiny.addCustomMessageHandler('addTabToTabset', function(message){
                             var hrefCodes = [];
                             /* Getting the right tabsetPanel */
                             var tabsetTarget = document.getElementById(message.tabsetName);
                             
                             /* Iterating through all Panel elements */
                             for(var i = 0; i < message.titles.length; i++){
                             /* Creating 6-digit tab ID and check, whether it was already assigned. */
                             do {
                             hrefCodes[i] = Math.floor(Math.random()*100000);
                             } 
                             while(hrefCollection.indexOf(hrefCodes[i]) != -1);
                             hrefCollection = hrefCollection.concat(hrefCodes[i]);
                             
                             /* Creating node in the navigation bar */
                             var navNode = document.createElement('li');
                             var linkNode = document.createElement('a');
                             
                             linkNode.appendChild(document.createTextNode(message.titles[i]));
                             linkNode.setAttribute('data-toggle', 'tab');
                             linkNode.setAttribute('data-value', message.titles[i]);
                             linkNode.setAttribute('href', '#tab-' + hrefCodes[i]);
                             
                             navNode.appendChild(linkNode);
                             tabsetTarget.appendChild(navNode);
                             };
                             
                             /* Move the tabs content to where they are normally stored. Using timeout, because
                             it can take some 20-50 millis until the elements are created. */ 
                             setTimeout(function(){
                             var creationPool = document.getElementById('creationPool').childNodes;
                             var tabContainerTarget = document.getElementsByClassName('tab-content')[0];
                             
                             /* Again iterate through all Panels. */
                             for(var i = 0; i < creationPool.length; i++){
                             var tabContent = creationPool[i];
                             tabContent.setAttribute('id', 'tab-' + hrefCodes[i]);
                             
                             tabContainerTarget.appendChild(tabContent);
                             };
                             }, 100);
                             });
                             "))),
  # End Important
  
  
  
  
  
  
                       
  
  
  
    
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
                          bsTooltip("regPred", "If this box is checked the regression models are applied",
                                    "right", options = list(container = "body")),
                          hr(),
                          #Parameters/Variables
                          radioButtons("installReqPckg","Install all required packages before calculations?",
                                        list("TRUE","FALSE"), selected = "FALSE" ),
                          radioButtons("preprocessData","Pre-process data before training the models?",
                                       list("TRUE","FALSE"), selected = "FALSE" ),
                          radioButtons("with.labels","Input data include header?",
                                       list("TRUE","FALSE"), selected = "TRUE" ),
                          radioButtons("impCalcMet","Scale variable importence according to:",
                                       list("RMSE&MSE","RMSE","MSE"), selected = "RMSE&MSE" ),
                          numericInput("myTimeLimit","Time limit for single model development (in seconds):",
                                       3600),
                          numericInput("no.cores", "Number of cores used", -1),
                          numericInput("elitism_percent", "Percentage of Elitism", 20)
                          ),
                 
                         column(5,
                         
                         tags$h4(print('Parameters - classification')),
                         tags$h4(print('some text')),
                         hr(),
                         checkboxInput("classPred",tags$b("Classification problem"),value=FALSE),
                         numericInput("hid1", "Number of Nodes in Hidden layer 1:", 5),
                         numericInput("hid2", "Number of Nodes in Hidden layer 2:", 2),
                         numericInput("ens", "No. of Ensembles (Local):", 10),
                         numericInput("trials", "Number of Trials:", 5),
                         numericInput("iter_max", "Maximum number of Iterations:", 10)
                         ),

                         #checkboxInput('multicore', 'Use Multicore', FALSE),

                         
                      #   progressInit(),
                         column(2,
                         
                         br(),
                         br(),
                         
                         actionButton ("runlocal1", "Run regression"),
                         tags$br(),
                         actionButton ("runlocal2", "Run classification")
                         
                         )
                 )
               )
             ),
  
  
  tabPanel("Results",fluid=TRUE,
           sidebarLayout(
             sidebarPanel(
               
             ),
             mainPanel(
               
             )
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
server <- function(input, output) {
  
  
  
  # Important! : creationPool should be hidden to avoid elements flashing before they are moved.
  #              But hidden elements are ignored by shiny, unless this option below is set.
  output$creationPool <- renderUI({})
  outputOptions(output, "creationPool", suspendWhenHidden = FALSE)
  # End Important
  
  # Important! : This is the make-easy wrapper for adding new tabPanels.
  addTabToTabset <- function(Panels, tabsetName){
    titles <- lapply(Panels, function(Panel){return(Panel$attribs$title)})
    Panels <- lapply(Panels, function(Panel){Panel$attribs$title <- NULL; return(Panel)})
    
    output$creationPool <- renderUI({Panels})
    session$sendCustomMessage(type = "addTabToTabset", message = list(titles = titles, tabsetName = tabsetName))
  }
  # End Important 
  
  
  
  
  
   
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)

