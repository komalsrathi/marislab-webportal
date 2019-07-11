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
  myData.c <- merge(myData.c, phenotype, by.x = 'variable', by.y = 'row.names', all.x = TRUE)
  
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
  colnames(myData.c)[1] <- 'Patient_Sample'
  
  # modify gene name, dashes present
  gene1.mut <- paste('`',gene1,'`',sep='')
  
  if(colorby == "None"){
    p <- ggplot(myData.c, aes_string(x = 'Patient_Sample', y = gene1.mut)) + 
      geom_bar(stat = "identity", color = 'black', fill = 'gray', size = 0.2) + customtheme + xlab('') +
      ggtitle(gene1)
  } else {
    p <- ggplot(myData.c, aes_string(x = 'Patient_Sample', y = gene1.mut, fill = colorby)) + guides(fill = FALSE) + 
      geom_bar(stat = "identity", color = 'black', size = 0.2) + customtheme + xlab('') +
      ggtitle(gene1)
  }
  
  
  # p <- plotly_build(p)
  
  return(p)
}