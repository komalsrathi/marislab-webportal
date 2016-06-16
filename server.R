# source functions
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

  # update datasetInput based on what project is selected
  datasetInput <- reactive({
    dat <- as.character(input$cldbselectInput1)
    dat <- get(load(paste0('data/',dat,'.RData')))
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
  
  # output expression plot for selected gene
  observe({
    if(input$clgesubmit1 == 0){
      return()
    }
    dat <- as.character(input$clgeselectInput1)
    dat <- get(load(paste0('data/',dat,'.RData')))
    num <- rownames(dat)
    updateSelectizeInput(session = session, inputId = "clgeselectInput2", choices = num, server = TRUE)
  })
  
  output$clgeplot1 <- renderPlotly({
    if(input$clgesubmit2 == 0){
      return()
    }
    isolate({
      datatype <- as.character(input$clgeselectInput1)
      dat <- get(load(paste0('data/',datatype,'.RData')))
      gene1 <- as.character(input$clgeselectInput2)
      logvalue <- input$clgecheckboxInput1
      sortby <- input$clgeselectInput3
      plotGeneBar(datatype = datatype, dat = dat, gene1 = gene1, customtheme = tbw, log = logvalue, sortby)
    })
  })
  
  # output correlation plot for selected genes
  observe({
    if(input$clggcsubmit1 == 0){
      return()
    }
    dat <- as.character(input$clggcselectInput1)
    dat <- get(load(paste0('data/',dat,'.RData')))
    num <- rownames(dat)
    updateSelectizeInput(session = session, inputId = "clggcselectInput2", choices = num, server = TRUE)
    updateSelectizeInput(session = session, inputId = "clggcselectInput3", choices = num, server = TRUE)
  })
  
  output$clggcplot1 <- renderPlotly({
    if(input$clggcsubmit2 == 0){
      return()
    }
    isolate({
      datatype <- as.character(input$clggcselectInput1)
      dat <- get(load(paste0('data/',datatype,'.RData')))
      gene1 <- as.character(input$clggcselectInput2)
      gene2 <- as.character(input$clggcselectInput3)
      logvalue <- input$clggccheckboxInput1
      correlation <- input$clggcselectInput4
      plotGeneScatter(datatype = datatype, dat = dat, gene1 = gene1, gene2 = gene2, 
                      customtheme = tbw, log = logvalue, corr = correlation)
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
      updateSelectizeInput(session = session, inputId = "clmselectInput2", choices = num, server = TRUE)
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
      updateSelectizeInput(session = session, inputId = "clgcnselectInput2", choices = num, server = TRUE)
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
  
  # CNA vs mRNA plot
  observe({
    if(input$clcvmsubmit1 == 0){
      return()
    }
    isolate({
      dat <- input$clcvmselectInput1
      dat <- get(load(paste0('data/',dat,'_expgene.RData')))
      num <- rownames(dat)
      updateSelectizeInput(session = session, inputId = "clcvmselectInput2", choices = num, server = TRUE)
    })
  })
  
  output$clcvmplot1 <- renderPlotly({
    if(input$clcvmsubmit2 == 0){
      return()
    }
    isolate({
      datatype <- input$clcvmselectInput1
      mrna <- get(load(paste0('data/',datatype,'_expgene.RData')))
      cna <- get(load(paste0('data/',datatype,'_cna.RData')))
      gene1 <- as.character(input$clcvmselectInput2)
      correlation <- as.character(input$clcvmselectInput3)
      plotGeneCNAvsRNA(mrna = mrna, 
                       cna = cna, 
                       gene1 = gene1, customtheme = tbw, 
                       correlation = correlation,
                       datatype = datatype)
    })
  })
  
  # heatmap - read the genes in for creating heatmaps
  fileInput <- reactive({
    infile <- input$clhfileInput1
    if(is.null(infile))
      return(NULL)
    read.table(file = infile$datapath, check.names = F, header = FALSE, stringsAsFactors = F)
  })
  
  output$clhplot1 <- renderPlot({
    if(input$clhsubmit1 == 0){
      return()
    }
    isolate({
      data <- fileInput()
      genes <- data[,1]
      dat <- input$clhselectInput1
      dataCNABin <- get(load(paste0('data/',dat,'_heatmapcnabin.RData')))
      dataMutBin <- get(load(paste0('data/',dat,'_heatmapmutbin.RData')))
      dataAnnBin <- get(load(paste0('data/',dat,'_heatmapannbin.RData')))
      dataExpGeneNameBin <- get(load(paste0('data/',dat,'_heatmapexpgenebin.RData')))
      commonCellLines <- get(load(paste0('data/',dat,'_heatmapcommoncelllines.RData')))
      plotOncoPrint(genes = genes, 
                    dataCNABin = dataCNABin, 
                    dataMutBin = dataMutBin, 
                    dataAnnBin = dataAnnBin, 
                    dataExpGeneNameBin = dataExpGeneNameBin, 
                    commonCellLines = commonCellLines)
      
    })
  })
  
  # cell line comparison table
  output$clcttable1 <- DT::renderDataTable({
    if(input$clctsubmit1 == 0){
      return()
    }
    isolate({
      dat <- input$clctselectInput1
      probeAnnot <- get(load(paste0('data/', dat, '_probeannot.RData')))
      dataExp <- get(load(paste0('data/', dat, '_expprobe.RData')))
      set1 <- as.character(input$clctselectInput2)
      set2 <- as.character(input$clctselectInput3)
      targets <- createTargets(set1, set2)
      pvalue <- as.numeric(input$clcttextInput1)
      dat <- runLimma(targets = targets, 
                      contrast = "SET1-SET2", 
                      pvalue = pvalue,
                      probeAnnot = probeAnnot,
                      gs = F,
                      dataExp = dataExp)
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
    myData <- get(paste(dataset,'_data',sep=''))
    num <- rownames(myData)
    updateSelectizeInput(session = session, inputId = "pgehselectInput3", choices = num, server = TRUE)
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
                                 colorby = colorby,
                                 customtheme = tbw)
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
    myData <- get(paste(dataset,'_data',sep=''))
    num <- rownames(myData)
    updateSelectizeInput(session = session, inputId = "pgebpselectInput3", choices = num, server = TRUE)
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
      plotBoxplotPatientAnalysis(gene1 = gene1, colorby = colorby, 
                                 dataset = dataset, log = log,
                                 customtheme = tbw)
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
      #num <- rownames(myData)
      num <- rownames(myData[[1]])
      updateSelectizeInput(session = session, inputId = "pkmselectInput3", choices = num, server = TRUE)
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
      myData <- get(paste(dataset,'_data',sep=''))
      num <- rownames(myData)
      updateSelectizeInput(session = session, inputId = "pggcselectInput3", choices = num, server = TRUE)
      updateSelectizeInput(session = session, inputId = "pggcselectInput4", choices = num, server = TRUE)
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
      updateSelectizeInput(session = session, inputId = "pmcgselectInput2", choices = num, server = TRUE)
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
      updateSelectizeInput(session = session, inputId = "pgcnselectInput2", choices = num, server = TRUE)
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
      plotGeneBarCNAPatientAnalysis(gene1 = gene1, dataset = dataset, 
                                    log = log, sortby = sortby,
                                    customtheme = tbw)
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
      updateSelectizeInput(session = session, inputId = "pgcvmselectInput2", choices = num, server = TRUE)
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
      plotGeneCNAvsRNAPatientAnalysis(gene1 = gene1, dataset = dataset, customtheme = tbw)
    })
  })
  
  # plot tumor vs normal RNA data
  observe({
    if(input$tvnbsubmit1==0){
      return()
    }
    load('data/TumNormData.RData')
    num <- as.character(intersect(tumData$gene_id,normData$Description))
    updateSelectizeInput(session = session, inputId = "tvnbselectInput2", choices = num, server = TRUE)
  })
  
  # plot tumor vs normal RNA data
  output$tvnbplot1 <- renderPlotly({
    if(input$tvnbsubmit2==0){
      return()
    }
    isolate({
      gene1 <- input$tvnbselectInput2
      logby <- input$tvnbcheckboxInput1
      boxPlotGeneSUTC(gene1 = gene1, logby = logby)
    })
  })
  
  # plot tumor vs normal RNA data (high)
  observe({
    if(input$tvnbasubmit1==0){
      return()
    }
    load('data/TumNormData.RData')
    num <- as.character(intersect(tumData$gene_id,normData$Description))
    updateSelectizeInput(session = session, inputId = "tvnbaselectInput2", choices = num, server = TRUE)
  })
  
  # plot tumor vs normal RNA data (high)
  output$tvnbaplot1 <- renderPlotly({
    if(input$tvnbasubmit2==0){
      return()
    }
    isolate({
      gene1 <- input$tvnbaselectInput2
      logby <- input$tvnbacheckboxInput1
      boxPlotGeneHighSUTC(gene1 = gene1, logby = logby)
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
      dat <- as.character(input$atpcselectInput1)
      dat <- get(load(paste0('data/',dat,'.RData')))
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
  
  # datasummary plot
  dbplot <- reactive({
    dat <- get(load("data/data_summary.RData"))
    p <- plotly_build(ggplot(data = dat, aes(x = Source, y = Genes)) +
                        geom_bar(stat="identity",aes(color = Type)) +
                        theme_bw() + geom_text(aes(label = Samples, color = Type), size=5) +
                        theme_hc(bgcolor = "darkunica") +
                        scale_colour_hc("darkunica") +
                        theme(legend.position = "none",axis.title = element_blank(),
                              axis.text.y = element_blank(), axis.ticks = element_blank(),
                              panel.grid = element_blank()))
    p$layout$plot_bgcolor <- p$layout$paper_bgcolor
    return(p)
  })

  output$dbplot1 <- renderPlotly({
    dbplot()
  })
  
}) # shinyServer ends