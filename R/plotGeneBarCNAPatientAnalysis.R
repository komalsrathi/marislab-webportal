#############################################
# Cell line copy number code for Patient data
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#############################################

# load patient sample set
load('data/allDataPatient3.RData')

# plot Copy Number Patient Data
plotGeneBarCNAPatientAnalysis <- function(gene1, dataset, logby, sortby)
{
  
  # get selected dataset
  myData <- paste(dataset,'_cdata',sep='')
  myData <- get(myData)
  
  myData$gene <- rownames(myData)
  myData.m <- melt(data = myData, id.vars = 'gene')
  myData.c <- dcast(data = myData.m, formula = variable~gene, value.var = 'value')
  
  if(sortby == TRUE)
  {
    myData.c <- myData.c[order(myData.c[,gene1]),]
    myData.c$variable <- factor(myData.c$variable, levels=myData.c$variable)
  }
  
  if(logby == TRUE)
  {
    myData.tmp <- myData.c[,-1]
    myData.tmp <- as.data.frame(log2(myData.tmp)-1)
    myData.tmp <- cbind(variable=myData.c$variable, myData.tmp)
    myData.c <- myData.tmp
  }
  colnames(myData.c)[1] <- 'Sample'
  
  p <- ggplot(myData.c, aes_string(x='Sample', y=gene1)) + geom_bar(stat="identity") + theme(axis.text.x  = element_blank()) + ggtitle(gene1)
  
  return(p)
}