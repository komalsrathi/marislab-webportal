library(shiny)
library(reshape2)
library(ggplot2)
library(Rgraphviz)
library(DT)
library(shinydashboard)
library(shinyIncubator)

# source functions
source('R/viewDataTable.R')
source('R/plotGeneScatter.R')
source('R/plotGeneBar.R')
source('R/themes.R')

options(shiny.maxRequestSize = 30*1024^2)

shinyServer(function(input, output, session){

  # set themes
  tbw <- themebw()
  
  # load data  
  datasetInput <-  reactive({
    file <- paste('data/','2016-04-27-CellLineSTARcounts_2pass_matrix.txt', sep = '')
    dat <- read.delim(file = file)
  })
  
  # output original data in nbrcltable1
  output$nbrcltable1 <- DT::renderDataTable({
    DT::datatable(datasetInput(),
                  extensions = c('TableTools', 'ColVis','Scroller'), 
                  options = list(dom = 'RMDCT<"clear">lfrtip', 
                                 searchHighlight = TRUE,
                                 initComplete = JS("function(settings, json) {",
                                                   "$(this.api().table().header()).css({'background-color': '#005ab3', 'color': '#fff'});",
                                                   "}"),
                                 tableTools = list(sSwfPath = '//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls_pdf.swf'),
                                 pageLength = 5,
                                 lengthMenu = list(c(5, 10, 15, 20, 25, -1), c('5', '10', '15', '20', '25', 'All')),
                                 scrollX = TRUE),
                  selection = 'single')
  })
  
  # update all select inputs with available gene names
  observe({
    dat <- datasetInput()
    n <- rownames(dat)
    updateSelectInput(session = session, inputId = "clggcselectInput1", choices = n)
    updateSelectInput(session = session, inputId = "clggcselectInput2", choices = n)
    updateSelectInput(session = session, inputId = "clgeselectInput1", choices = n)
    updateSelectInput(session = session, inputId = "clctselectInput1", choices = n)
    updateSelectInput(session = session, inputId = "clctselectInput2", choices = n)
    updateSelectInput(session = session, inputId = "pgehselectInput4", choices = n)
  })
  
  # output correlation plot in plot1
  output$clggcplot1 <- renderPlot({
    if(input$clggcsubmit1){
      isolate({
        dat <- datasetInput()
        gene1 <- as.character(input$clggcselectInput1)
        gene2 <- as.character(input$clggcselectInput2)
        plotGeneScatter(dat = dat, gene1 = `gene1`, gene2 = `gene2`, customtheme = tbw)
      })
    }
  })
  
  output$clgeplot1 <- renderPlot({
    if(input$clgesubmit1){
      isolate({
        dat <- datasetInput()
        gene1 <- as.character(input$clgeselectInput1)
        plotGeneBar(dat = dat, gene1 = gene1, customtheme = tbw)
      })
    }
  })
  
  # heatmap
  # read the genes in for creating heatmaps
  fileInput <- reactive({
    infile <- input$clhfileInput
    if(is.null(infile))
      return(NULL)
    read.table(file = infile$datapath, check.names = F, header = FALSE, stringsAsFactors = F)
  })
  
  # heatmap
  # update select input with the list of genes uploaded
  observe({
    dat <- fileInput()
    n <- nrow(dat)
    updateSelectInput(session = session, inputId = "clhselectInput1", choices = dat[,1])
    updateSelectInput(session = session, inputId = "clhselectInput2", choices = n, selected = n)
  })
  
}) # shinyServer ends
  
 