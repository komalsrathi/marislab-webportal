# source functions
source('R/processRawData.R')
source('R/postProcessDataForHeatmap.R')
source('R/viewDataTable.R')
source('R/plotGeneScatter.R')
source('R/plotGeneBar.R')
source('R/themes.R')
source('R/dataCollect.R')
source('R/cellMutationTable.R')
source('R/plotGeneBarCNA.R')
source('R/plotGeneCNAvsRNA.R')
source('R/createTargets.R')
source('R/runLimma.R')
source('R/plotOncoPrint.R')

shinyServer(function(input, output, session){
  
  tbw <- themebw()
  newList <- processRawData()
  processedList <- postProcessDataForHeatmap(dataCNA = newList$dataCNA, 
                                             dataExpGeneName = newList$dataExpGeneName, 
                                             dataAnnBin = newList$dataAnnBin, 
                                             dataMutBin = newList$dataMutBin)
  
  
  # update projects selectInput
  observe({
    project <- read.csv(file = 'data/db.txt', stringsAsFactors = F)
    n <- project$Project
    updateSelectInput(session = session, inputId = "cldbselectInput1", choices = n)
  })
  
  # update datasetInput based on what project is selected
  datasetInput <- reactive({
    filename <- as.character(input$cldbselectInput1)
    project <- dataCollect(filename)
    dat <- read.delim(file = project, stringsAsFactors = FALSE)
  })

  # update the table only if submit button is clicked
  output$cldbtable1 <- DT::renderDataTable({
    if(input$cldbsubmit1==0){
      return()
    }
    withProgress(session = session, message = "Getting Data...", detail = "This may take a while...", {
      isolate({
        viewDataTable(datasetInput())
      })
    })
  })
  
  # update all select inputs with available gene names
  observe({
    if(input$cldbsubmit1==0){
      return()
    }
    dat <- datasetInput()
    n <- rownames(dat)
    updateSelectInput(session = session, inputId = "clgeselectInput1", choices = n)
    updateSelectInput(session = session, inputId = "clggcselectInput1", choices = n)
    updateSelectInput(session = session, inputId = "clggcselectInput2", choices = n)
    updateSelectInput(session = session, inputId = "clmselectInput1", choices = n)
    updateSelectInput(session = session, inputId = "clgcnselectInput1", choices = n)
    updateSelectInput(session = session, inputId = "clcvmselectInput1", choices = n)
  })
  
  # output correlation plot for selected genes
  output$clggcplot1 <- renderPlotly({
    if(input$clggcsubmit1==0){
      return()
    }
    isolate({
      dat <- datasetInput()
      gene1 <- as.character(input$clggcselectInput1)
      gene2 <- as.character(input$clggcselectInput2)
      logvalue <- input$clggccheckboxInput1
      plotGeneScatter(dat = dat, gene1 = `gene1`, gene2 = `gene2`, customtheme = tbw, log = logvalue)
    })
  })
  
  # output expression plot for selected gene
  output$clgeplot1 <- renderPlotly({
    if(input$clgesubmit1==0){
      return()
    }
    isolate({
      dat <- datasetInput()
      gene1 <- as.character(input$clgeselectInput1)
      logvalue <- input$clgecheckboxInput1
      sortby <- input$clgeselectInput2
      plotGeneBar(dat = dat, gene1 = gene1, customtheme = tbw, log = logvalue, sortby)
    })
  })
  
  output$clmtable1 <- renderDataTable({
    if(input$clmsubmit1==0){
      return()
    }
    isolate({
      gene <- as.character(input$clmselectInput1)
      viewDataTable(dat = cellMutationTable(gene))
    })
  })
  
  output$clgcnplot1 <- renderPlotly({
    if(input$clgcnsubmit1==0){
      return()
    }
    isolate({
      gene1 <- as.character(input$clgcnselectInput1)
      dat <- newList$dataCNA
      plotGeneBarCNA(dat = dat, gene1 = gene1, customtheme = tbw)
    })
  })
  
  output$clcvmplot1 <- renderPlotly({
    if(input$clcvmsubmit1==0){
      return()
    }
    isolate({
      gene1 <- as.character(input$clcvmselectInput1)
      plotGeneCNAvsRNA(mrna = newList$dataExpGeneName, cna = newList$dataCNA, gene1 = gene1, customtheme = tbw)
    })
  })
  
  # heatmap - read the genes in for creating heatmaps
  fileInput <- reactive({
    infile <- input$clhfileInput1
    if(is.null(infile))
      return(NULL)
    read.table(file = infile$datapath, check.names = F, header = FALSE, stringsAsFactors = F)
  })
  
  # heatmap - update select input with the list of genes uploaded
  output$clhplot1 <- renderPlot({
    if(input$clhsubmit1==0){
      return()
    }
    isolate({
      dat <- fileInput()
      genes <- dat[,1]
      plotOncoPrint(genes = genes, 
                    dataCNABin = processedList$dataCNABin, 
                    dataMutBin = processedList$dataMutBin, 
                    dataAnnBin = processedList$dataAnnBin, 
                    dataExpGeneNameBin = processedList$dataExpGeneNameBin, 
                    commonCellLines = processedList$commonCellLines)
      
    })
  })
  
  # cell line comparison table
  output$clcttable1 <- renderDataTable({
    if(input$clctsubmit1==0){
      return()
    }
    isolate({
      set1 <- as.character(input$clctselectInput1)
      set2 <- as.character(input$clctselectInput2)
      targets <- createTargets(set1, set2)
      pvalue <- as.numeric(input$clcttextInput1)
      dat <- runLimma(targets = targets, 
                      contrast = "SET1-SET2", 
                      pvalue = pvalue,
                      probeAnnot = newList$probeAnnot,
                      gs = F,
                      dataExp = newList$dataExp)
      viewDataTable(dat = dat)
    })
  })
  
}) # shinyServer ends