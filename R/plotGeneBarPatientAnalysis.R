#############################################
# plot bar plot for a gene per patient sample
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#############################################

# function to plot bar charts
plotGeneBarPatientAnalysis <- function(gene1, myDataExp, myDataAnn, sortby, log, density, colorby, customtheme)
{

  # get expression and annotation of the selected dataset
  # modify dataframe
  myDataExp$gene <- rownames(myDataExp)
  myDataExp.m <- melt(data = myDataExp, id.vars = 'gene')
  myDataExp.c <- dcast(data = myDataExp.m, formula = variable~gene, value.var = 'value')
  colnames(myDataExp.c)[1] = "Sample"
  
  # sort by value
  if(sortby == TRUE){
    myDataExp.c$Sample <- reorder(myDataExp.c$Sample,myDataExp.c[,gene1])
  }
  
  # plot log values
  if(log==FALSE)
  {
    y.axis <- "RMA"
    myDataExp.tmp <- myDataExp.c[,-1]
    myDataExp.tmp <- as.data.frame(2^myDataExp.tmp)
    myDataExp.tmp <- cbind(Sample=myDataExp.c$Sample, myDataExp.tmp)
    myDataExp.c <- myDataExp.tmp
  }
  if(log==TRUE)
  {
    y.axis <- "log2(RMA)"
  }
  
  # add annotation data to expression set
  myDataExp.c <- merge(myDataExp.c, myDataAnn, by.x="Sample",by.y='row.names')
  
  # eliminate confusion between MYCN gene and status
  if(length(grep('MYCN',colnames(myDataExp.c)))>1)
  {
    coln <- grep("MYCN.x", colnames(myDataExp.c))
    colnames(myDataExp.c)[coln] <- 'MYCN'
    coln <- grep("MYCN.y", colnames(myDataExp.c))
    colnames(myDataExp.c)[coln] <- 'MYCNS'
    if(colorby == "MYCN")
    {
      colorby = "MYCNS"
    }
  }
  
  
  
  # modify gene name, dashes present
  gene1.mut <- paste('`',gene1,'`',sep='')
  
  if(colorby == "None")
  {
    if(density == FALSE)
    {
      p <- ggplot(myDataExp.c, aes_string(x='Sample', y=gene1.mut)) + 
        geom_bar(stat="identity") + customtheme + theme(axis.text.x  = element_text(angle=90)) + ggtitle(gene1)
    }
    if(density == TRUE)
    {
      p <- ggplot(myDataExp.c, aes_string(gene1.mut, fill=1)) + geom_density(alpha = 0.5) + customtheme + ggtitle(gene1) 
    }
  }
  if(colorby!="None")
  {
    if(density == FALSE)
    {
      p <- ggplot(myDataExp.c, aes_string(x='Sample', y=gene1.mut, fill=colorby)) + customtheme +
        geom_bar(stat="identity") + theme(axis.text.x  = element_text(angle=90)) + ggtitle(gene1)
    }
    if(density == TRUE)
    {
      p <- ggplot(myDataExp.c, aes_string(gene1.mut, fill=colorby)) + geom_density(alpha = 0.5) + customtheme + ggtitle(gene1) 
    }
  }
  p <- plotly_build(p)
  p$layout$yaxis$title <- y.axis
  return(p)
  
}