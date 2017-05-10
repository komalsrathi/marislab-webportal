#####################################################
# plot kaplan meier plot for set of genes per dataset 
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#####################################################

source('R/kmScan.R')
source('R/createSurvivalFrame.R')
source('R/qplot_survival.R')

# quick Function to do Zscore
standScore <- function(x)
{
  x <- (x-mean(x))/sd(x)
}

kapmPlot <- function(genes, myDataExp, myDataAnn, risk, createPlot=T, tVar, eVar)
{
  #Get metadata
  if(risk != "All"){
    tmpMeta <- myDataAnn[which(myDataAnn$RISK == risk),]
    myDataExp <- myDataExp[,which(colnames(myDataExp) %in% rownames(tmpMeta))]
  } else {
    tmpMeta <- myDataAnn
    myDataExp <- myDataExp
  }
  
  #Now pick a gene/genes &
  if(length(genes)==1)
  {
    myGene <- myDataExp[genes,]
    tmpMeta[,"Gene"] <- as.numeric(myGene)
    tmpMeta <- tmpMeta[order(tmpMeta[,"Gene"]),]
  }
  
  if(length(genes)>1)
  {
    myGene <- myDataExp[genes,]
    myGene <- apply(myGene, FUN=standScore, MARGIN=1)
    myGene <- apply(myGene, FUN=max, MARGIN=1)
    tmpMeta[,"Gene"] <- as.numeric(myGene)
    tmpMeta <- tmpMeta[order(tmpMeta[,"Gene"]),]
    genes <- paste(genes, collapse=" / ")
  }
  
  #Run scan
  tmpMetaScan <- tmpMeta
  tmpMetaScan <- tmpMetaScan[,as.character(c(tVar, eVar, "Gene"))]
  out <- kmScan(tmpMetaScan, tVar, eVar)
  
  #Sort DF and set it    
  tmpMetaScan[,"GeneBin"] <- 1
  tmpMetaScan[1:out[[1]],"GeneBin"] <- 0
  
  #time
  timeVar <- tmpMetaScan[,tVar]
  #event
  eventVar <- tmpMetaScan[,eVar]
  
  #createsurvival
  t.Surv <- Surv(timeVar, eventVar)
  t.survfit <- survfit(t.Surv~GeneBin, data=tmpMetaScan)
  
  #Change strata names
  myLowName <- paste("Low : n = ", t.survfit$n[[1]], sep="")
  myHighName <- paste("High : n = ", t.survfit$n[[2]], sep="")
  names(t.survfit$strata) <- c(myLowName, myHighName)
  t.survframe <- createSurvivalFrame(t.survfit)
  
  if(createPlot==T)
  {
    tmpTitle <- paste(genes," |", "\nP-val(Adj) :", format(out[2], scientific=T, digits=3), "(", format(out[3], scientific=T, digits=3), ")")
    myReturn <- qplot_survival(t.survframe, f.CI=F, myTitle=tmpTitle) + theme_bw() + 
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 14)) + xlab('\nTime') + ylab('Survival\n')
    #+scale_colour_manual(values=c("red", "blue") )  
  }
  
  if(createPlot==F)
  {
    myReturn <- c(genes, out[2], out[3])
  }
  
  myReturn
}



