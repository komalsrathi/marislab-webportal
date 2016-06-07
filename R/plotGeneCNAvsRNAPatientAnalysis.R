#############################################
# plot Gene CNA vs RNA for Patient data
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#############################################

# load patient sample set
load('data/allDataPatient.RData')

# plot scatter plot of 2 genes
plotGeneCNAvsRNAPatientAnalysis <- function(gene1, dataset)
{
  
  myData <- paste(dataset,'_data',sep='')
  myData <- get(myData)
  
  mycData <- paste(dataset,'_cdata',sep = '')
  mycData <- get(mycData)
  
  #Let's do other stuff
  intCL <- intersect(colnames(myData), colnames(mycData))
  tmpDataGC <- data.frame(t(rbind(myData[gene1,intCL], mycData[gene1,intCL])))
  colnames(tmpDataGC) <- c("mRNA", "CNA")
  
  #For title correlation and p-value
  tmpcor <- cor.test(tmpDataGC[,"mRNA"], tmpDataGC[,"CNA"])
  tmpcorp <- round(tmpcor$p.val,2)
  tmpcor <- round(tmpcor$estimate,2)
  myText <- paste("Cor=", tmpcor, " | P-Val=", tmpcorp, sep="")

  tmpDataGC[,"CL_NAME"] <- rownames(tmpDataGC)
  
  p <- ggplot(tmpDataGC, aes(x= mRNA, y= CNA)) + 
    geom_point() + geom_smooth(method="lm") + ggtitle(myText)
  
  p <- ggplotly(p)
  
  return(p)
}
