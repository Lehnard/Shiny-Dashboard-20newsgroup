################################################################################
#
# This is the server logic of a Shiny dashboard web application. You can run the 
# application by clicking 'Run App' above.
#
################################################################################

library(tm, quietly= TRUE, verbose= FALSE)
library(data.table, quietly= TRUE, verbose= FALSE)
library(SnowballC, quietly= TRUE, verbose= FALSE)
library(wordcloud, quietly= TRUE, verbose= FALSE)

source("./scripts/wordFreq.R")
source("./scripts/tratayPredice.R")
# source("pocessText.R") # Activate only if the text process in wordFreq.R is on.

# The file names fot the newsgroups data sets.
filenames <- list.files("newsData", pattern= ".RData")

server <- function(input, output, session) {
        
        # An observer is like a reactive expression in that it can read reactive 
        # values and call reactive ex- pressions, and will automatically re-execute 
        # when those dependencies change. But unlike reactive expressions, it doesn't 
        # yield a result and can't be used as an input to other reactive expressions.
        observe({
                
                # Load the selected newsgroup data set.
                file <- c("newsData/", filenames[as.numeric(input$newsGrp3)]) 
                filepath <- paste(file, collapse= "")
                DT_name <- load(filepath)
                DT <- get(DT_name)
                
                dataTable <- wordFreq(DT) 
                
                updateNumericInput(session, inputId= "tableFreq1", label= NULL,
                                   value= max(dataTable$frecuency),
                                   max= max(dataTable$frecuency),
                                   min= 1)
                
                })
        
        observe({

                updateNumericInput(session, inputId= "tableFreq2", label= NULL,
                                   value= NULL,
                                   max= input$tableFreq1,
                                   min= 1)
                })
        
                
        # Bar diagram reaction to button action. 
        data1 <- eventReactive(input$barDiagrButton, {
                
                # Load the selected newsgroup data set.
                file <- c("newsData/", filenames[as.numeric(input$newsGrp1)]) 
                filepath <- paste(file, collapse= "")
                DT_name <- load(filepath)
                DT <- get(DT_name)
                
                wf <- wordFreq(DT)
                
                barData <- wf[1:input$barDiagrSlider, ]
        }) # event reactive end
        
        # Word cloud reaction to button action. 
        data2 <- eventReactive(input$wcButton, {
                
                # Load the selected newsgroup data set.
                file <- c("newsData/", filenames[as.numeric(input$newsGrp2)]) 
                filepath <- paste(file, collapse= "")
                DT_name <- load(filepath)
                DT <- get(DT_name)
                
                wf <- wordFreq(DT)
                
                wcData <- wf[1:input$wcSlider, ]
        }) # event reactive end
        
        
        # table reaction to button action. 
        data3 <- eventReactive(input$tableButton, {
               
                # Load the selected newsgroup data set.
                file <- c("newsData/", filenames[as.numeric(input$newsGrp3)]) 
                filepath <- paste(file, collapse= "")
                DT_name <- load(filepath)
                DT <- get(DT_name)

                dataTable <- wordFreq(DT) 
                
                # Subset dataTable according to the selected frecuencies.
                selectRows <- dataTable$frecuency >= input$tableFreq2 &
                dataTable$frecuency <= input$tableFreq1
                dataTable <- dataTable[selectRows , ]

        }) #event reactive end

        
        #file upload handler.
        observeEvent(input$file,{
                texto <- readLines(input$file$datapath)
                texto <- paste(texto, collapse=  "\n ")
                updateTextAreaInput(session, 
                                    inputId = "writeText" , 
                                    label = "", 
                                    value = texto)
                
                output$value <- renderPrint({ 
                        cat("file name: \t\t", input$file$name,"\n")
                        cat("file size(kb): \t\t", input$file$size," \n")
                        cat("file type: \t\t", input$file$type)
                })
                
        })
        
        # Reaction to file upload classify button. 
        data4 <- eventReactive( input$classifyButton, {
                        texto <- readLines(input$file$datapath)
                        texto <- paste(texto, collapse=  " ")
                 }) #event reactive end
        
        
        # Reaction to Reset button in Classify.
        observeEvent(input$resetButton,{
                                updateTextAreaInput(session, 
                                                    inputId = "writeText" , 
                                                    label = "", 
                                                    value = "") 
                
                                output$barDiagr2 <- renderPlot(
                                                        par(bg= "black"),
                                                        barplot(0) )             
                             
                                output$resultado <- renderText({""})
                                
                                output$value <- renderPrint({ 
                                                        cat("file name: \t\t", NULL ,"\n")
                                                        cat("file size(kb): \t\t", NULL ," \n")
                                                        cat("file type: \t\t", NULL)
                                                }) # renderPrint end
        }) # observeEvent end
         
        output$barDiagr <- renderPlot({ 
                                        #plot the histogram
                                        barplot(data1()$frecuency, 
                                        las= 2, 
                                        names.arg= data1()$word,
                                        col= "lightgreen",
                                        main= "Most frequent words",
                                        ylab= "Word frequencies")
                          }) # renderPlot end 
        
        output$wcloud <- renderPlot({ 
                                        #plot the wordcloud
                                        suppressWarnings(
                                        wordcloud(data2()$word, data2()$frecuency,
                                        random.order = FALSE,
                                        colors= brewer.pal(8,"Dark2"),
                                        scale= c(5, 1))  ) #supressWarings end
                         }) # renderPlot end.
        
        # input$tableFreq1 and input$TableFreq2 filter to correct values.
        observeEvent(input$tableButton, {
                                        if(input$tableFreq2 <= input$tableFreq1) {
                                                output$tfTable <- renderDataTable({ data3() })
                                        }else{
                                                 output$tfTable <- renderDataTable({})
                                                 output$tableErr <- renderText({ "Invalid frecuency Range."})
                                        } # if-else end
        }) # observeEvent end
        
        # initial value for output$value.
        output$value <- renderPrint({ 
                                        cat("file name: \t\t", NULL ,"\n")
                                        cat("file size(kb): \t\t", NULL ," \n")
                                        cat("file type: \t\t", NULL)
                        }) # renderPrint end
        
        # initial value for output$resultado
        output$resultado <- renderText({""})
        
        # Reaction when classify button is pressed.
        observeEvent(input$classifyButton, {
                                         
                                        texto <- input$writeText
                                        cosas <- tratayPredice(texto)
                                        grupo <- cosas$grupo
                                        grupos <- cosas$grupos
                                        
                                        probs <- cosas$prob
                                        colnames(probs) <- grupos[as.numeric(colnames(probs))]
                                        probs <- probs[, order(colnames(probs), decreasing = TRUE)]
                                        
                                        cat("\nTexto clasificado en: ", grupo, "\n")
                                        cat("\nclass(probs): ", class(probs), "\n")
                                        cat("\nProbabilidades: ", probs, "\n")
                
                                        output$barDiagr2 <-renderPlot({ 
                                                                par(mar= c(4,12,2,2))
                                                                barplot(probs*100,
                                                                names.arg= names(probs),
                                                                las= 2,
                                                                horiz= TRUE,
                                                                xlim= c(0,100),
                                                                col= "lightgreen")
                                        }) # renderPlot end
                
                                        output$resultado <- renderText({as.character(grupo)})
        }) #observeEvent end
        
       
}