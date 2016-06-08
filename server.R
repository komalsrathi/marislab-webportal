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
source('R/plotGeneScatterPatientData.R')
source('R/getCorrelationPatientAnalysis.R')
source('R/plotGeneBarCNAPatientAnalysis.R')
source('R/plotGeneCNAvsRNAPatientAnalysis.R')
source('R/boxPlotGeneSUTC.R')
source('R/boxPlotGeneHighSUTC.R')

shinyServer(function(input, output, session){
  
  tbw <- themebw()
  newList <- processRawData()
  processedList <- postProcessDataForHeatmap(dataCNA = newList$dataCNA, 
                                             dataExpGeneName = newList$dataExpGeneName, 
                                             dataAnnBin = newList$dataAnnBin, 
                                             dataMutBin = newList$dataMutBin)

  # update datasetInput based on what project is selected
  datasetInput <- reactive({
    dat <- as.character(input$cldbselectInput1)
    dat <- get(load(paste0('data/',dat)))
  })

  # update the table only if submit button is clicked
  output$cldbtable1 <- DT::renderDataTable({
    if(input$cldbsubmit1 == 0){
      return()
    }
    isolate({
      viewDataTable(dat = datasetInput())
    })
  })
  
  # update all select inputs with available gene names
  observe({
    if(input$cldbsubmit1 == 0){
      return()
    }
    dat <- as.character(input$cldbselectInput1)
    dat <- get(load(paste0('data/',dat)))
    num <- rownames(dat)
    #updateSelectInput(session = session, inputId = "clgcnselectInput1", choices = num)
    updateSelectInput(session = session, inputId = "clcvmselectInput1", choices = num)
    updateSelectInput(session = session, inputId = "pgehselectInput3", choices = num)
  })
  
  # output correlation plot for selected genes
  observe({
    if(input$clggcsubmit1 == 0){
      return()
    }
    dat <- as.character(input$clggcselectInput1)
    dat <- get(load(paste0('data/',dat)))
    num <- rownames(dat)
    updateSelectInput(session = session, inputId = "clggcselectInput2", choices = num)
    updateSelectInput(session = session, inputId = "clggcselectInput3", choices = num)
  })
  
  output$clggcplot1 <- renderPlotly({
    if(input$clggcsubmit2 == 0){
      return()
    }
    isolate({
      dat <- as.character(input$clggcselectInput1)
      dat <- get(load(paste0('data/',dat)))
      gene1 <- as.character(input$clggcselectInput2)
      gene2 <- as.character(input$clggcselectInput3)
      logvalue <- input$clggccheckboxInput1
      correlation <- input$clggcselectInput4
      plotGeneScatter(dat = dat, gene1 = `gene1`, gene2 = `gene2`, 
                      customtheme = tbw, log = logvalue, corr = correlation)
    })
  })
  
  # output expression plot for selected gene
  observe({
    if(input$clgesubmit1 == 0){
      return()
    }
    dat <- as.character(input$clgeselectInput1)
    dat <- get(load(paste0('data/',dat)))
    num <- rownames(dat)
    updateSelectInput(session = session, inputId = "clgeselectInput2", choices = num)
  })
  
  output$clgeplot1 <- renderPlotly({
    if(input$clgesubmit2 == 0){
      return()
    }
    isolate({
      dat <- as.character(input$clgeselectInput1)
      dat <- get(load(paste0('data/',dat)))
      gene1 <- as.character(input$clgeselectInput2)
      logvalue <- input$clgecheckboxInput1
      sortby <- input$clgeselectInput3
      plotGeneBar(dat = dat, gene1 = gene1, customtheme = tbw, log = logvalue, sortby)
    })
  })
  
  # mutation table
  observe({
    if(input$clmsubmit1 == 0){
      return()
    }
    isolate({
      dat <- as.character(input$clmselectInput1)
      dat <- paste0('data/',dat)
      dat <- read.delim(dat, stringsAsFactors = FALSE)
      num <- unique(as.character(dat$Gene))
      updateSelectInput(session = session, inputId = "clmselectInput2", choices = num)
    })
  })
  
  output$clmtable1 <- DT::renderDataTable({
    if(input$clmsubmit2 == 0){
      return()
    }
    isolate({
      dataset <- as.character(input$clmselectInput1)
      gene <- as.character(input$clmselectInput2)
      viewDataTable.fixedcols(dat = cellMutationTable(gene, dataset))
    })
  })
  
  # Gene CNA plot
  observe({
    if(input$clgcnsubmit1 == 0){
      return()
    }
    isolate({
      dat <- input$clgcnselectInput1
      dat <- get(load(paste0('data/',dat,'_cna.RData')))
      num <- rownames(dat)
      updateSelectInput(session = session, inputId = "clgcnselectInput2", choices = num)
    })
  })
  
  output$clgcnplot1 <- renderPlotly({
    if(input$clgcnsubmit2 == 0){
      return()
    }
    isolate({
      dat <- input$clgcnselectInput1
      dat <- get(load(paste0('data/',dat,'_cna.RData')))
      gene1 <- as.character(input$clgcnselectInput2)
      sortby <- input$clgcnselectInput3
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
  output$clcttable1 <- DT::renderDataTable({
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
    load('data/allDataPatient.RData')
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
    load('data/allDataPatient.RData')
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
      load('data/allDataPatient.RData')
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
  
  # patient gene scatter plot
  observe ({
    if(input$pggcsubmit1 == 0){
      return()
    }
    isolate({
      # load dataset and get rownames
      load('data/allDataPatient.RData')
      dataset <- input$pggcselectInput1
      myData <- paste(dataset,'_All',sep='')
      myData <- get(myData)
      num <- rownames(myData[[1]])
      updateSelectInput(session = session, inputId = "pggcselectInput3", choices = num)
      updateSelectInput(session = session, inputId = "pggcselectInput4", choices = num)
    })
  })
  
  # patient gene scatter plot
  output$pggcplot1 <- renderPlotly({
    if(input$pggcsubmit2 == 0){
      return()
    }
    isolate({
      dataset <- as.character(input$pggcselectInput1)
      log <- input$pggccheckboxInput1
      colorby <- input$pggcselectInput2
      gene1 <- as.character(input$pggcselectInput3)
      gene2 <- as.character(input$pggcselectInput4)
      correlation <- input$pggcselectInput5
      plotGeneScatterPatientData(gene1 = gene1, gene2 = gene2, 
                                 dataset = dataset, log = log, 
                                 colorby = colorby, correlation = correlation,
                                 customtheme = tbw)
    })
  })
  
  # patient most correlated genes
  observe({
    if(input$pmcgsubmit1 == 0){
      return()
    }
    isolate({
      # load dataset and get rownames
      load('data/allDataPatient.RData')
      dataset <- input$pmcgselectInput1
      myData <- paste(dataset,'_data',sep='')
      myData <- get(myData)
      num <- rownames(myData)
      n <- as.numeric(nrow(myData))
      updateSelectInput(session = session, inputId = "pmcgselectInput2", choices = num)
      updateTextInput(session = session, inputId = "pmcgtextInput1", value = n)
    })
  })
  
  # patient most correlated genes
  output$pmcgtable1 <- DT::renderDataTable({
    if(input$pmcgsubmit2 == 0){
      return()
    }
    isolate({
      dataset <- as.character(input$pmcgselectInput1)
      gene1 <- as.character(input$pmcgselectInput2)
      numRet <- as.numeric(input$pmcgtextInput1)
      cortab <- getCorrelationPatientAnalysis(gene1 = gene1, dataset = dataset, numRet = numRet)
      viewDataTable(dat = cortab)
    })
  })
  
  # patient gene bar CNA
  observe({
    if(input$pgcnsubmit1 == 0){
      return()
    }
    isolate({
      # load dataset and get rownames
      load('data/allDataPatient.RData')
      dataset <- input$pgcnselectInput1
      myData <- paste(dataset,'_cdata',sep='')
      myData <- get(myData)
      num <- rownames(myData)
      updateSelectInput(session = session, inputId = "pgcnselectInput2", choices = num)
    })
  })
  
  # patient gene bar CNA
  output$pgcnplot1 <- renderPlot({
    if(input$pgcnsubmit2 == 0){
      return()
    }
    isolate({
      dataset <- input$pgcnselectInput1
      sortby <- input$pgcncheckboxInput1
      log <- input$pgcncheckboxInput2
      gene1 <- as.character(input$pgcnselectInput2)
      plotGeneBarCNAPatientAnalysis(gene1 = gene1, dataset = dataset, log = log, sortby = sortby)
    })
  })
  
  # plot gene CNA vs RNA patient analysis
  observe({
    if(input$pgcvmsubmit1 == 0){
      return()
    }
    isolate({
      # load dataset and get rownames
      load('data/allDataPatient.RData')
      dataset <- input$pgcvmselectInput1
      myData <- paste(dataset,'_data',sep='')
      myData <- get(myData)
      mycData <- paste(dataset,'_cdata',sep = '')
      mycData <- get(mycData)
      num <- intersect(rownames(myData),rownames(mycData))
      updateSelectInput(session = session, inputId = "pgcvmselectInput2", choices = num)
    })
  })
  
  # plot gene CNA vs RNA patient analysis
  output$pgcvmplot1 <- renderPlotly({
    if(input$pgcvmsubmit2 == 0){
      return()
    }
    isolate({
      dataset <- input$pgcvmselectInput1
      gene1 <- as.character(input$pgcvmselectInput2)
      plotGeneCNAvsRNAPatientAnalysis(gene1 = gene1, dataset = dataset)
    })
  })
  
  # plot tumor vs normal RNA data
  observe({
    if(input$tvnbsubmit1==0){
      return()
    }
    load('data/TumNormData.RData')
    num <- as.character(intersect(tumData$gene_id,normData$Description))
    updateSelectInput(session = session, inputId = "tvnbselectInput2", choices = num)
  })
  
  # plot tumor vs normal RNA data
  output$tvnbplot1 <- renderPlotly({
    if(input$tvnbsubmit2==0){
      return()
    }
    isolate({
      gene1 <- input$tvnbselectInput2
      boxPlotGeneSUTC(gene1 = gene1)
    })
  })
  
  # plot tumor vs normal RNA data (high)
  observe({
    if(input$tvnbasubmit1==0){
      return()
    }
    load('data/TumNormData.RData')
    num <- as.character(intersect(tumData$gene_id,normData$Description))
    updateSelectInput(session = session, inputId = "tvnbaselectInput2", choices = num)
  })
  
  # plot tumor vs normal RNA data (high)
  output$tvnbaplot1 <- renderPlotly({
    if(input$tvnbasubmit2==0){
      return()
    }
    isolate({
      gene1 <- input$tvnbaselectInput2
      boxPlotGeneHighSUTC(gene1 = gene1)
    })
  })
  
  # compendia overexpression analysis
  output$aoatable1 <- DT::renderDataTable({
    if(input$aoasubmit1 == 0){
      return()
    }
    isolate({
      dat <- paste0('data/',input$aoaselectInput1,'.RData')
      dat <- get(load(dat))
      thres <- as.numeric(input$aoatextInput1)
      freq <- as.numeric(input$aoatextInput2)
      rank <- as.numeric(input$aoatextInput3)
      dat <- dat[which(dat$Threshold == thres),]
      if(freq !=0 ){
        dat <- dat[which(dat$Frequency > freq),]
      }
      if(rank != 10000){
        dat <- na.omit(dat)
        dat <- dat[dat$Rank < rank,]
      }
      viewDataTable(dat = dat)
    })
  })
  
  # compendia TM calls
  output$atpctable1 <- DT::renderDataTable({
    if(input$atpcsubmit1 == 0){
      return()
    }
    isolate({
      dat <- paste0('data/',input$atpcselectInput1,'.RData')
      dat <- get(load(dat))
      viewDataTable(dat = dat)
    })
  })
  
  # compendia diffexp analysis
  output$atndatable1 <- DT::renderDataTable({
    if(input$atndasubmit1 == 0){
      return()
    }
    isolate({
      dat <- paste0('data/',input$atndaselectInput1,'.RData')
      dat <- get(load(dat))
      logfc <- as.numeric(input$atndatextInput1)
      pval <- as.numeric(input$atndatextInput2)
      rank <- as.numeric(input$atndatextInput3)
      if (logfc != -10){
        dat <- dat[dat$LogFC > logfc,]
      }
      if (pval != 1){
        dat <- dat[dat$pValue< pval,]
      }
      if (rank != 10000){
        dat <- na.omit(dat)
        dat <- dat[dat$Rank < rank,]
      }
      viewDataTable(dat = dat)
    })
  })
  
  # list external and internal resources
  output$rdbitable1 <- DT::renderDataTable({
    isolate({
      dat <- read.csv('data/internal.txt')
      dat$Link <- paste0("<a href='",dat$Link,"' target='_blank'>",dat$Link,"</a>")
      return(dat)
    })
  }, escape = FALSE)
  
  output$rdbetable1 <- DT::renderDataTable({
    isolate({
      dat <- read.csv('data/external.txt')
      dat$Link <- paste0("<a href='",dat$Link,"' target='_blank'>",dat$Link,"</a>")
      return(dat)
    })
  }, escape = FALSE)
  
}) # shinyServer ends