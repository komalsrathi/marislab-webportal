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
source('R/viewDataTable.fixedcols.R')
source('R/plotGeneBarPatientAnalysis.R')
source('R/plotBoxplotPatientAnalysis.R')
source('R/kapmChoose.R')

shinyServer(function(input, output, session){
  
  # num <- NULL
  tbw <- themebw()
  newList <- processRawData()
  processedList <- postProcessDataForHeatmap(dataCNA = newList$dataCNA, 
                                             dataExpGeneName = newList$dataExpGeneName, 
                                             dataAnnBin = newList$dataAnnBin, 
                                             dataMutBin = newList$dataMutBin)
  
  
  # # update projects selectInput
  # observe({
  #   project <- read.csv(file = 'data/db.txt', stringsAsFactors = F)
  #   n <- project$Project
  #   updateSelectInput(session = session, inputId = "cldbselectInput1", choices = n)
  # })
  # 
  # # update datasetInput based on what project is selected
  # datasetInput <- reactive({
  #   filename <- as.character(input$cldbselectInput1)
  #   project <- dataCollect(filename)
  #   dat <- read.delim(file = project, stringsAsFactors = FALSE)
  # })
  
  # update datasetInput based on what project is selected
  datasetInput <- reactive({
    filename <- as.character(input$cldbselectInput1)
    project <- paste('data/',filename,sep='')
    dat <- read.delim(file = project, stringsAsFactors = FALSE)
    # num <<- rownames(dat)
    # dat
  })

  # update the table only if submit button is clicked
  output$cldbtable1 <- DT::renderDataTable({
    if(input$cldbsubmit1 == 0){
      return()
    }
    isolate({
      viewDataTable(datasetInput())
    })
  })
  
  # update all select inputs with available gene names
  observe({
    if(input$cldbsubmit1 == 0){
      return()
    }
    dat <- datasetInput()
    num <- rownames(dat)
    updateSelectInput(session = session, inputId = "clgeselectInput1", choices = num)
    updateSelectInput(session = session, inputId = "clggcselectInput1", choices = num)
    updateSelectInput(session = session, inputId = "clggcselectInput2", choices = num)
    updateSelectInput(session = session, inputId = "clmselectInput1", choices = num)
    updateSelectInput(session = session, inputId = "clgcnselectInput1", choices = num)
    updateSelectInput(session = session, inputId = "clcvmselectInput1", choices = num)
    updateSelectInput(session = session, inputId = "pgehselectInput3", choices = num)
  })
  
  # output correlation plot for selected genes
  output$clggcplot1 <- renderPlotly({
    if(input$clggcsubmit1 == 0){
      return()
    }
    isolate({
      dat <- datasetInput()
      gene1 <- as.character(input$clggcselectInput1)
      gene2 <- as.character(input$clggcselectInput2)
      logvalue <- input$clggccheckboxInput1
      correlation <- input$clggcselectInput3
      plotGeneScatter(dat = dat, gene1 = `gene1`, gene2 = `gene2`, 
                      customtheme = tbw, log = logvalue, corr = correlation)
    })
  })
  
  # output expression plot for selected gene
  output$clgeplot1 <- renderPlotly({
    if(input$clgesubmit1 == 0){
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
    if(input$clmsubmit1 == 0){
      return()
    }
    isolate({
      gene <- as.character(input$clmselectInput1)
      viewDataTable.fixedcols(dat = cellMutationTable(gene))
    })
  })
  
  output$clgcnplot1 <- renderPlotly({
    if(input$clgcnsubmit1 == 0){
      return()
    }
    isolate({
      gene1 <- as.character(input$clgcnselectInput1)
      dat <- newList$dataCNA
      sortby <- input$clgcnselectInput2
      plotGeneBarCNA(dat = dat, gene1 = gene1, customtheme = tbw, sortby)
    })
  })
  
  output$clcvmplot1 <- renderPlotly({
    if(input$clcvmsubmit1 == 0){
      return()
    }
    isolate({
      gene1 <- as.character(input$clcvmselectInput1)
      correlation <- as.character(input$clcvmselectInput2)
      plotGeneCNAvsRNA(mrna = newList$dataExpGeneName, cna = newList$dataCNA, 
                       gene1 = gene1, customtheme = tbw, correlation = correlation)
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
    if(input$clhsubmit1 == 0){
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
    if(input$clctsubmit1 == 0){
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
      viewDataTable.fixedcols(dat = dat)
    })
  })
  
  # patient gene bar plot
  observe({
    if(input$pgehsubmit1 == 0){
      return()
    }
    
    # load dataset and get rownames
    load('data/allDataPatient3.RData')
    dataset <- input$pgehselectInput1
    myData <- paste(dataset,'_All',sep='')
    myData <- get(myData)
    num <- rownames(myData[[1]])
    updateSelectInput(session = session, inputId = "pgehselectInput3", choices = num)
  })

  # patient gene bar plot
  output$pgehplot1 <- renderPlotly({
    if(input$pgehsubmit2 == 0){
      return()
    }
    isolate({
      dataset <- input$pgehselectInput1
      sortby <- as.character(input$pgehcheckboxInput1)
      log <- as.character(input$pgehcheckboxInput2)
      density <- as.character(input$pgehcheckboxInput3)
      colorby <- as.character(input$pgehselectInput2)
      gene1 <- as.character(input$pgehselectInput3)
      plotGeneBarPatientAnalysis(gene1 = gene1, dataset = dataset, 
                                 sortby = sortby, log = log, density = density, 
                                 colorby = colorby)
    })
  })
  
  # patient box plot
  observe({
    if(input$pgebpsubmit1 == 0){
      return()
    }
    
    # load dataset and get rownames
    load('data/allDataPatient3.RData')
    dataset <- input$pgebpselectInput1
    myData <- paste(dataset,'_All',sep='')
    myData <- get(myData)
    num <- rownames(myData[[1]])
    updateSelectInput(session = session, inputId = "pgebpselectInput3", choices = num)
  })
  
  # patient box plot
  output$pgebpplot1 <- renderPlotly({
    if(input$pgebpsubmit2 == 0){
      return()
    }
    isolate({
      dataset <- input$pgebpselectInput1
      log <- input$pgebpcheckboxInput1
      colorby <- input$pgebpselectInput2
      gene1 <- input$pgebpselectInput3
      plotBoxplotPatientAnalysis(gene1 = gene1, colorby = colorby, dataset = dataset, log = log)
    })
  })
  
  # kaplan meier plot
  observe ({
    if(input$pkmsubmit1 == 0){
      return()
    }
    isolate({
      # load dataset and get rownames
      load('data/allDataPatient3.RData')
      dataset <- input$pkmselectInput1
      myData <- paste(dataset,'_All',sep='')
      myData <- get(myData)
      num <- rownames(myData[[1]])
      updateSelectInput(session = session, inputId = "pkmselectInput3", choices = num)
    })
  })
  
  # kaplan meier plot
  output$pkmplot1 <- renderPlotly({
    if(input$pkmsubmit2 == 0){
      return()
    }
    isolate({
      dataset <- as.character(input$pkmselectInput1)
      endpoint <- as.character(input$pkmselectInput2)
      genes <- as.character(input$pkmselectInput3)
      kapmChoose(dataset = dataset, genes = genes, endpoint = endpoint)
    })
  })
  
}) # shinyServer ends