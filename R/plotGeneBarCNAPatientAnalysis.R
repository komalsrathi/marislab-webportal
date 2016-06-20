#############################################
# Cell line copy number code for Patient data
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#############################################

# plot Copy Number Patient Data
plotGeneBarCNAPatientAnalysis <- function(gene1, myData, logby, sortby, customtheme)
{

  # get selected dataset

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
  
  # modify gene name, dashes present
  gene1.mut <- paste('`',gene1,'`',sep='')
  
  p <- ggplot(myData.c, aes_string(x='Sample', y=gene1.mut)) + 
    customtheme + geom_bar(stat="identity") + 
    theme(axis.text.x  = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 14),
          axis.title.y= element_text(size = 14),
          axis.title.x = element_text(size = 14),
          plot.title = element_text(size = 16)) + ggtitle(gene1) + 
    ylab("Copy Number\n") + xlab("\nSample")
  
  # p <- plotly_build(p)
  
  return(p)
}