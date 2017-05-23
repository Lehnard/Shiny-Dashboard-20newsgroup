#################################################################################
#                                                                               #
# This is the user-interface definition of a Shiny dashboard web application.   #
#  You can run the application by clicking 'Run App' above.                     #
#                                                                               #
#  Autores:     Maria Calvo                                                     #
#               David Grinan                                                    #
#               Jordi Aceiton                                                   #
#                                                                               #
#  Fecha:       22/05/2017                                                      #
#                                                                               #
#################################################################################

library(shinydashboard)

# The list of news gruop choices.
load("./data/newsGrpNames.RData")
newsGrps_list <- 1:length(newsGrpLabel)
names(newsGrps_list) <- newsGrpLabel
newsGrps_list <- as.list(newsGrps_list)


dashboardPage(
  
  # PAGE :
  skin = "green",   # One of "blue", "black", "purple", "green", "red", "yellow".
  
  # HEADER:
  dashboardHeader(
    title = "Newsgroup term analysis",
    titleWidth = 300
  ), #dashboardHeader
  
  # SLIDEBAR:
  dashboardSidebar(
    sidebarMenu(
      menuItem("Term-frecuency graphs", 
               tabName = "tfgraphs", 
               icon = icon("line-chart")),
      
      menuItem("Term-frecuency table", 
               tabName = "tftable", 
               icon = icon("th")),
      menuItem("Classify a text", 
               tabName = "predict", 
               icon = icon("cubes"))
    )#sidebarMenu
  ), #dashboardSidebar
  
  # BODY:
  dashboardBody(
    tabItems(
      
      # First tab content
      tabItem(tabName = "tfgraphs",
              
              fluidRow(
                
                box(title= "Bar diagram",
                    status= "success",  #header color.
                    width= 6,
                    height= NULL,  
                    solidHeader= TRUE, 
                    background= NULL, 
                    
                    # box output: bar diagram.
                    plotOutput("barDiagr")
                ), #box
                
                box(title= "Word cloud",
                    status= "success",  #header color.
                    width= 6,
                    height= NULL,  
                    solidHeader= TRUE, 
                    background= NULL, 
                    
                    # box output: bar diagram.
                    plotOutput("wcloud")
                ) #box
              ), #fluidRow
              
              fluidRow(
                
                box(title= NULL,
                    status= NULL,  #header color.
                    width= 6,
                    height= NULL,  
                    solidHeader= TRUE, 
                    background= "black", 
                    
                    # box title
                    h4("Bar diagram controls:"), br(),
                    
                    # Box output: selection menu.
                    selectInput(inputId= "newsGrp1", 
                                label = "Select news group:", 
                                choices = newsGrps_list, 
                                selected = 1),
                    
                    # Box output: bar diagram.
                    sliderInput(inputId= "barDiagrSlider", 
                                label= "Select the number of K-top words.",
                                max= 100, min= 1, value= 10),
                    
                    # Box output: centered action button.
                    div(
                      actionButton(inputId= "barDiagrButton", 
                                   label= "Update Bar diagram"),
                      style="float:left; position:relative; left:30%"
                    ) #div
                ), #box
                
                box(title= NULL,
                    status= NULL,  #header color.
                    width= 6,
                    height= NULL,  
                    solidHeader= TRUE, 
                    background= "black", 
                    
                    # box title
                    h4("Bar diagram controls:"), br(),
                    
                    # Box output: selection menu.
                    selectInput(inputId= "newsGrp2", 
                                label = "Select news group:", 
                                choices = newsGrps_list, 
                                selected = 1),
                    
                    # Box output: bar diagram.
                    sliderInput(inputId= "wcSlider", 
                                label= "Select the number of K-top words.",
                                max= 100, min= 1, value= 10),
                    
                    # Box output: centered action button.
                    div(
                      actionButton(inputId= "wcButton", 
                                   label= "Update Word cloud"),
                      style="float:left; position:relative; left:30%"
                    )#dif
                )#box
              ) #fluidRow
      ),#tabItem    
      
      # Second tab content
      tabItem(tabName =  "tftable",
              
              # Table frecuency controls
              fluidRow(
                
                column(1),
                
                box(title= NULL,
                    status= NULL,  #header color.
                    width= 10,
                    height= NULL,  
                    solidHeader= TRUE, 
                    background= "black", 
                    
                    # box title                 
                    h4("Table controls:"), br(),
                    
                    # Box output: table newsgroup selection menu.
                    selectInput(inputId= "newsGrp3", 
                                label = "Select news group:", 
                                choices = newsGrps_list, 
                                selected = 1),
                    
                    # Box output: frecuency selection in Table.
                    br(),h4("Select word frecuency interval:"), br(),
                    
                    numericInput("tableFreq1", label = "Maximum frecuency value",
                                 value = 2),
                    numericInput("tableFreq2", label = "Minimum frecuency value", 
                                 value = 1),
                    
                    # Box output: centered action button.
                    div(
                      actionButton(inputId= "tableButton", 
                                   label= "Update table"), 
                                   style= "float:left; position:relative; left:40%"
                    ) #div
                ) #box
              ), #fluidRow
              
              # term-frecuency Table
              fluidRow(
                box(title= "Term-Frecuency table",
                    status= "success",  #header color.
                    width= 12,
                    height= NULL,  
                    solidHeader= TRUE, 
                    background= NULL, 
                    
                    # box output: data table.
                    dataTableOutput("tfTable"),
                    
                    # box output: data table error
                    textOutput("tableErr")
                ) #box
              ) #fluidRow
      ), #tabItem
      tabItem(tabName = "predict",
              
              fluidRow(column(1),
                       
                       box(title= NULL,
                           status= NULL,  #header color.
                           width= 12,
                           height= NULL,  
                           solidHeader= TRUE, 
                           background= "black", 
                           
                           # box title
                           h4("Classify a text"), br(),
                           
                           # Upload file with file upload manager.
                           fileInput("file", 
                                     label = "Select a text file to classify:"),
                           accept= c(".txt"),
                           
                           fluidRow( 
                                   box(title= NULL,
                                       status= NULL,  #header color.
                                       width= 12,
                                       height= NULL,  
                                       solidHeader= TRUE, 
                                       background= NULL, 
                                   
                                       column(10, offset= 0, verbatimTextOutput("value")) )#column
                                   ),#box
                           
                           #text input
                           textAreaInput(inputId="writeText", 
                                         label= "Or write a text here:", 
                                         rows= 10),
                           
                          
                           fluidRow(    
                                    column(2, offset = 0,
                           
                                           actionButton(inputId= "classifyButton", 
                                                        label= "Classify")
                                           
                                           
                                    ), #column
                                    column(2, offset = 0,
                                           
                                           actionButton(inputId= "resetButton", 
                                                        label= "Reset")
                                    ) #column
                           ),#fluidRow
                           
                           fluidRow(
                                    column(8, offset= 5,  
                           
                                          h2(textOutput(outputId="resultado")),
                                          style="align-content: center; "
                                    ), #column
                                    
                                    
                                    
                                    box(title= NULL,
                                         status= NULL,  #header color.
                                         width= 12,
                                         height= NULL,  
                                         solidHeader= TRUE, 
                                         background= "black", 
                                         plotOutput("barDiagr2")
                                    )
                           )#fluidRow
                           
                       ) #box
                       
              )  #fluidRow
              
      )
      
      
      
    ) #tabItems
  ) #dashboardBody
) #dashboardpage()     

