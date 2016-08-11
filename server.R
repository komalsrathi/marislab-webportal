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
source('R/getTukeyHSDBoxplotPatientAnalysis.R')
source('R/kapmChoose.R')
source('R/plotGeneScatterPatientData.R')
source('R/getCorrelationPatientAnalysis.R')
source('R/plotGeneBarCNAPatientAnalysis.R')
source('R/plotGeneCNAvsRNAPatientAnalysis.R')
source('R/boxPlotGeneSUTC.R')
source('R/boxPlotGeneHighSUTC.R')
source('R/dbplot.R')
source('R/plotBoxplotTargetRNA.R')
source('R/plotGeneScatterTargetRNA.R')

# load datasets here
load('data/data_summary.RData')
load('data/TumNormData.RData')
load('data/allDataPatient.RData')
load('data/Microarray_RMA_HumanGene1.0ST_29cells_genes.RData')
load('data/Microarray_RMA_U133Plus2_17cells_CCLE_genes.RData')
load('data/Microarray_RMA_U133Plus2_29cells_genes.RData')
load('data/Microarray_RMA_HumanWG6v2_38cells_genes.RData')
load('data/STAR_FPKM_41cells_genes.RData')
load('data/kallisto_TPM_41cells_genes.RData')
load('data/STAR_FPKM_Target724_genes.RData')
load('data/kallisto_TPM_Target724_genes.RData')
load('data/STAR_FPKM_46cells_genes.RData')
load('data/ExomeCalls85K8_10_13.RData')
load('data/ExomeCallsCCLE.RData')
load('data/compAim3_DE.RData')
load('data/compAim3_OE.RData')
load('data/compAim3_TM.RData')
load('data/Target724_targetcode.RData')
# load('data/Cufflinks_FPKM_GSE49711_SEQC_genes.RData')
# load('data/Cufflinks_FPKM_GSE49711_SEQC_transcripts.RData')

shinyServer(function(input, output, session){
  
  tbw <- themebw()

  # cell line database
  output$cldbtable1 <- DT::renderDataTable({
    if(input$cldbsubmit1 == 0){
      return()
    }
    isolate({
      dat <- as.character(input$cldbselectInput1)
      dat <- get(dat)
      viewDataTable(dat = dat)
    })
  })
  
  # output expression plot for selected gene
  observe({
    if(input$clgesubmit1 == 0){
      return()
    }
    dat <- as.character(input$clgeselectInput1)
    dat <- get(dat)
    num <- rownames(dat)
    updateSelectizeInput(session = session, inputId = "clgeselectInput2", choices = num, server = TRUE)
  })
  
  output$clgeplot1 <- renderPlotly({
    if(input$clgesubmit2 == 0){
      return()
    }
    isolate({
      datatype <- as.character(input$clgeselectInput1)
      dat <- get(datatype)
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
    dat <- get(dat)
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
      dat <- get(datatype)
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
      dat <- get(dat)
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
      dataset <- get(dataset)
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
      dat <- get(paste0(dat,'_cna'))
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
      dat <- get(paste0(dat,'_cna'))
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
      dat <- get(paste0(dat,'_expgene'))
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
      mrna <- get(paste0(datatype,'_expgene'))
      cna <- get(paste0(datatype,'_cna'))
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
      dataCNABin <- get(paste0(dat,'_heatmapcnabin'))
      dataMutBin <- get(paste0(dat,'_heatmapmutbin'))
      dataAnnBin <- get(paste0(dat,'_heatmapannbin'))
      dataExpGeneNameBin <- get(paste0(dat,'_heatmapexpgenebin'))
      commonCellLines <- get(paste0(dat,'_heatmapcommoncelllines'))
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
      probeAnnot <- get(paste0(dat,'_probeannot'))
      dataExp <- get(paste0(dat,'_expprobe'))
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
      rownames(dat) <- NULL
      viewDataTable(dat = dat)
    })
  })
  
  # patient sample database
  output$psdbtable1 <- DT::renderDataTable({
    if(input$psdbsubmit1 == 0){
      return()
    }
    isolate({
      dataset <- as.character(input$psdbselectInput1)
      dat <- get(paste0(dataset,'_data'))
      viewDataTable(dat = dat)
    })
  })
  
  # patient gene bar plot
  observe({
    if(input$pgehsubmit1 == 0){
      return()
    }
    
    # load dataset and get rownames
    dataset <- input$pgehselectInput1
    myData <- get(paste0(dataset,'_data'))
    num <- rownames(myData)
    updateSelectizeInput(session = session, inputId = "pgehselectInput3", choices = num, server = TRUE)
    
    myData.pheno <- get(paste0(dataset,'_mData'))
    cols <- intersect(colnames(myData.pheno),c('MYCN','RISK','STAGE'))
    if(length(cols)==0)
    {
      cols <- 'None'
    }
    updateSelectizeInput(session = session, inputId = "pgehselectInput2", choices = cols, server = TRUE)
  })

  # patient gene bar plot
  output$pgehplot1 <- renderPlotly({
    if(input$pgehsubmit2 == 0){
      return()
    }
    isolate({
      dataset <- input$pgehselectInput1
      myDataExp <- get(paste0(dataset,'_data'))
      myDataAnn <- get(paste0(dataset,'_mData'))
      sortby <- as.character(input$pgehcheckboxInput1)
      log <- as.character(input$pgehcheckboxInput2)
      density <- as.character(input$pgehcheckboxInput3)
      colorby <- as.character(input$pgehselectInput2)
      gene1 <- as.character(input$pgehselectInput3)
      plotGeneBarPatientAnalysis(gene1 = gene1, 
                                 datatype = dataset,
                                 myDataExp = myDataExp,
                                 myDataAnn = myDataAnn,
                                 sortby = sortby, log = log, 
                                 density = density, colorby = colorby,
                                 customtheme = tbw)
    })
  })
  
  # patient box plot
  observe({
    if(input$pgebpsubmit1 == 0){
      return()
    }
    
    # load dataset and get rownames
    dataset <- input$pgebpselectInput1
    myData <- get(paste0(dataset,'_data'))
    num <- rownames(myData)
    updateSelectizeInput(session = session, inputId = "pgebpselectInput3", choices = num, server = TRUE)
    
    myData.pheno <- get(paste0(dataset,'_mData'))
    cols <- intersect(colnames(myData.pheno),c('MYCN','RISK','STAGE'))
    if(length(cols)==0)
    {
      cols <- 'None'
    }
    updateSelectizeInput(session = session, inputId = "pgebpselectInput2", choices = cols, server = TRUE)
  })
  
  # patient box plot
  output$pgebpplot1 <- renderPlotly({
    if(input$pgebpsubmit2 == 0){
      return()
    }
    isolate({
      dataset <- input$pgebpselectInput1
      myDataExp <- get(paste0(dataset,'_data'))
      myDataAnn <- get(paste0(dataset,'_mData'))
      log <- input$pgebpcheckboxInput1
      colorby <- input$pgebpselectInput2
      gene1 <- input$pgebpselectInput3
      plotBoxplotPatientAnalysis(gene1 = gene1, 
                                 datatype = dataset,
                                 colorby = colorby, 
                                 myDataExp = myDataExp, 
                                 myDataAnn = myDataAnn,
                                 log = log,
                                 customtheme = tbw)
    })
  })
  
  output$pgebtable1 <- renderDataTable({
    if(input$pgebpsubmit2 == 0){
      return()
    }
    isolate({
      dataset <- input$pgebpselectInput1
      myDataExp <- get(paste0(dataset,'_data'))
      myDataAnn <- get(paste0(dataset,'_mData'))
      log <- input$pgebpcheckboxInput1
      colorby <- input$pgebpselectInput2
      gene1 <- input$pgebpselectInput3
      dat <- getTukeyHSDBoxplotPatientAnalysis(gene1 = gene1, 
                                 datatype = dataset,
                                 colorby = colorby, 
                                 myDataExp = myDataExp, 
                                 myDataAnn = myDataAnn,
                                 log = log)
      viewDataTable(dat = dat)
    })
  })
  
  # kaplan meier plot
  observe ({
    if(input$pkmsubmit1 == 0){
      return()
    }
    isolate({
      # load dataset and get rownames
      dataset <- input$pkmselectInput1
      myData <- get(paste0(dataset,'_data'))
      num <- rownames(myData)
      updateSelectizeInput(session = session, inputId = "pkmselectInput3", choices = num, server = TRUE)
    })
  })
  
  # kaplan meier plot
  output$pkmplot1 <- renderPlotly({
    if(input$pkmsubmit2 == 0){
      return()
    }
    isolate({
      datatype <- as.character(input$pkmselectInput1)
      dataset <-  get(paste0(datatype,'_All'))
      endpoint <- as.character(input$pkmselectInput2)
      genes <- as.character(input$pkmselectInput3)
      kapmChoose(datatype = datatype, dataset = dataset, genes = genes, endpoint = endpoint)
    })
  })
  
  # patient gene scatter plot
  observe ({
    if(input$pggcsubmit1 == 0){
      return()
    }
    isolate({
      # load dataset and get rownames
      dataset <- input$pggcselectInput1
      myData <- get(paste0(dataset,'_data'))
      num <- rownames(myData)
      updateSelectizeInput(session = session, inputId = "pggcselectInput3", choices = num, server = TRUE)
      updateSelectizeInput(session = session, inputId = "pggcselectInput4", choices = num, server = TRUE)
      
      myData.pheno <- get(paste0(dataset,'_mData'))
      cols <- intersect(colnames(myData.pheno),c('MYCN','RISK','STAGE'))
      if(length(cols)==0)
      {
        cols <- 'None'
      }
      updateSelectizeInput(session = session, inputId = "pggcselectInput2", choices = cols, server = TRUE)
    })
  })
  
  # patient gene scatter plot
  output$pggcplot1 <- renderPlotly({
    if(input$pggcsubmit2 == 0){
      return()
    }
    isolate({
      dataset <- input$pggcselectInput1
      myDataExp <- get(paste0(dataset,'_data'))
      myDataAnn <- get(paste0(dataset,'_mData'))
      log <- input$pggccheckboxInput1
      colorby <- input$pggcselectInput2
      gene1 <- as.character(input$pggcselectInput3)
      gene2 <- as.character(input$pggcselectInput4)
      correlation <- input$pggcselectInput5
      plotGeneScatterPatientData(gene1 = gene1, gene2 = gene2, 
                                 datatype = dataset,
                                 myDataExp = myDataExp, myDataAnn = myDataAnn,
                                 log = log, colorby = colorby, correlation = correlation,
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
      dataset <- input$pmcgselectInput1
      myData <- get(paste0(dataset,'_data'))
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
      myData <- get(paste0(dataset,'_data'))
      gene1 <- as.character(input$pmcgselectInput2)
      numRet <- as.numeric(input$pmcgtextInput1)
      cortab <- getCorrelationPatientAnalysis(gene1 = gene1, myData = myData, numRet = numRet)
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
      dataset <- input$pgcnselectInput1
      myData <- get(paste0(dataset,'_cdata'))
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
      myData <- get(paste0(dataset,'_cdata'))
      sortby <- input$pgcncheckboxInput1
      log <- input$pgcncheckboxInput2
      gene1 <- as.character(input$pgcnselectInput2)
      plotGeneBarCNAPatientAnalysis(gene1 = gene1, myData = myData, 
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
      dataset <- input$pgcvmselectInput1
      myData <- get(paste0(dataset,'_data'))
      mycData <- get(paste0(dataset,'_cdata'))
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
      myData <- get(paste0(dataset,'_data'))
      mycData <- get(paste0(dataset,'_cdata'))
      gene1 <- as.character(input$pgcvmselectInput2)
      correlation <- input$pgcvmselectInput3
      plotGeneCNAvsRNAPatientAnalysis(gene1 = gene1, myData = myData, mycData = mycData, customtheme = tbw, correlation = correlation)
    })
  })
  
  # plot tumor vs normal RNA data
  observe({
    if(input$tvnbsubmit1==0){
      return()
    }
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
      boxPlotGeneSUTC(gene1 = gene1, logby = logby, 
                      tumData = tumData,
                      normData = normData, 
                      normDataAnnot = normDataAnnot)
    })
  })
  
  # plot tumor vs normal RNA data (high)
  observe({
    if(input$tvnbasubmit1==0){
      return()
    }
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
      boxPlotGeneHighSUTC(gene1 = gene1, logby = logby, 
                          tumData = tumData,
                          normData = normData, 
                          normDataAnnot = normDataAnnot)
    })
  })
  
  # target rnaseq boxplot
  observe({
    if(input$tgeboxsubmit1==0){
      return()
    }
    dat <- as.character(input$tgeboxselectInput1)
    dat <- get(dat)
    num <- rownames(dat)
    updateSelectizeInput(session = session, inputId = "tgeboxselectInput2", choices = num, server = TRUE)
  })
  
  output$tgeboxplot1 <- renderPlotly({
    if(input$tgeboxsubmit2 == 0){
      return()
    }
    isolate({
      datatype <- as.character(input$tgeboxselectInput1)
      dat <- get(datatype)
      gene1 <- as.character(input$tgeboxselectInput2)
      logvalue <- input$tgeboxcheckboxInput1
      colorby <- as.character(input$tgeboxselectInput3)
      plotBoxplotTargetRNA(gene1 = gene1, colorby = colorby, datatype = datatype, 
                           dat = dat, log = logvalue, customtheme = tbw, 
                           targetcode = Target724_targetcode)
    })
  })
  
  # target rnaseq dotplot
  observe({
    if(input$tgedotsubmit1 == 0){
      return()
    }
    dat <- as.character(input$tgedotselectInput1)
    dat <- get(dat)
    num <- rownames(dat)
    updateSelectizeInput(session = session, inputId = "tgedotselectInput2", choices = num, server = TRUE)
    updateSelectizeInput(session = session, inputId = "tgedotselectInput3", choices = num, server = TRUE)
  })
  
  output$tgedotplot1 <- renderPlotly({
    if(input$tgedotsubmit2 == 0){
      return()
    }
    isolate({
      datatype <- as.character(input$tgedotselectInput1)
      dat <- get(datatype)
      gene1 <- as.character(input$tgedotselectInput2)
      gene2 <- as.character(input$tgedotselectInput3)
      logvalue <- input$tgedotcheckboxInput1
      correlation <- input$tgedotselectInput4
      colorby <- as.character(input$tgedotselectInput5)
      plotGeneScatterTargetRNA(datatype = datatype, dat = dat, gene1 = gene1, gene2 = gene2, log = logvalue, customtheme = tbw, 
                               correlation = correlation, colorby = colorby, targetcode = Target724_targetcode)
    })
  })
  
  
  # compendia overexpression analysis
  output$aoatable1 <- DT::renderDataTable({
    if(input$aoasubmit1 == 0){
      return()
    }
    isolate({
      dat <- as.character(input$aoaselectInput1)
      dat <- get(dat)
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
      dat <- get(dat)
      viewDataTable(dat = dat)
    })
  })
  
  # compendia diffexp analysis
  output$atndatable1 <- DT::renderDataTable({
    if(input$atndasubmit1 == 0){
      return()
    }
    isolate({
      dat <- as.character(input$atndaselectInput1)
      dat <- get(dat)
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
  
  output$dbplot1 <- renderPlotly({
    dbplot(data_summary)
  })
  
}) # shinyServer ends