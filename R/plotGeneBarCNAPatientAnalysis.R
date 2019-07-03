#############################################
# Cell line copy number code for Patient data
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#############################################

# plot Copy Number Patient Data
plotGeneBarCNAPatientAnalysis <- function(gene1, myData, sortby, logby, customtheme, phenotype, colorby)
{
  
  # get selected dataset
  myData <- myData[rownames(myData) %in% gene1,]
  myData$gene <- rownames(myData)
  myData.m <- melt(data = myData, id.vars = 'gene')
  myData.c <- dcast(data = myData.m, formula = variable~gene, value.var = 'value')
  
  # add phenotype data - MYCN status
  myData.c <- merge(myData.c, phenotype, by.x = 'variable', by.y = 'TARGET.USI', all.x = TRUE)
  
  if(sortby == "None") {
    myData.c <- myData.c[order(myData.c[,gene1]),]
    myData.c$variable <- factor(myData.c$variable, levels=myData.c$variable)
  } else {
    myData.c <- myData.c[order(myData.c[,sortby]),]
    myData.c$variable <- factor(myData.c$variable, levels=myData.c$variable)
  }
  
  if(logby == TRUE) {
    myData.c[,gene1] <- log2(myData.c[,gene1]-1)
  }
  colnames(myData.c)[1] <- 'Sample'
  
  # modify gene name, dashes present
  gene1.mut <- paste('`',gene1,'`',sep='')
  
  if(colorby == "None"){
    p <- ggplot(myData.c, aes_string(x='Sample', y = gene1.mut)) + 
      customtheme + geom_bar(stat="identity") + 
      theme(axis.text.x  = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(size = 16),
            axis.title.y= element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.title = element_text(size = 18)) + ggtitle(gene1) + 
      ylab("Copy Number\n") + xlab("\nSample")
  } else {
    p <- ggplot(myData.c, aes_string(x = 'Sample', y = gene1.mut, fill = colorby)) + 
      customtheme + geom_bar(stat="identity") + 
      theme(axis.text.x  = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(size = 16),
            axis.title.y= element_text(size = 16),
            axis.title.x = element_text(size = 16),
            plot.title = element_text(size = 18)) + ggtitle(gene1) + 
      ylab("Copy Number\n") + xlab("\nSample")
  }
  
  
  # p <- plotly_build(p)
  
  return(p)
}