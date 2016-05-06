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
    file = paste('data/','2016-04-27-CellLineSTARcounts_2pass_matrix.txt',sep = '')
    dat = read.delim(file = file)
  })
  
  # output original data in tab1
  output$table <- DT::renderDataTable({
    viewDataTable(datasetInput())
  })
  
  # output correlation plot in plot1
  output$clggcplot1 <- renderPlot({
    if(input$clggcsubmit1){
      isolate({
        dat <- datasetInput()
        gene1 <- as.character(input$clggcgene1)
        gene2 <- as.character(input$clggcgene2)
        plotGeneScatter(dat = dat, gene1 = gene1, gene2 = gene2, customtheme = tbw)
      })
    }
  })
  
  output$clgeplot1 <- renderPlot({
    if(input$clgesubmit1){
      isolate({
        dat <- datasetInput()
        gene1 <- as.character(input$clgegene1)
        plotGeneBar(dat = dat, gene1 = gene1, customtheme = tbw)
      })
    }
  })

}) # shinyServer ends
  
 