#############################################
# plot bar plot for a gene per patient sample
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#############################################

# function to plot bar charts
plotGeneBarPatientAnalysis <- function(datatype, gene1, myDataExp, myDataAnn, sortby, log, density, colorby, customtheme)
{

  # get expression and annotation of the selected dataset
  # modify dataframe
  myDataExp$gene <- rownames(myDataExp)
  myDataExp.m <- melt(data = myDataExp, id.vars = 'gene')
  myDataExp.c <- dcast(data = myDataExp.m, formula = variable~gene, value.var = 'value')
  colnames(myDataExp.c)[1] = "Patient_Sample"
  
  # plot log values
  if(length(grep('FPKM',datatype))==0)
  {
    if(log==FALSE)
    {
      y.axis <- "RMA"
      myDataExp.tmp <- myDataExp.c[,-1]
      myDataExp.tmp <- as.data.frame(2^myDataExp.tmp)
      myDataExp.tmp <- cbind(Patient_Sample=myDataExp.c$Patient_Sample, myDataExp.tmp)
      myDataExp.c <- myDataExp.tmp
    }
    if(log==TRUE)
    {
      y.axis <- "log2(RMA)"
    }
  }
  if(length(grep('FPKM',datatype))==1)
  {
    if(log==FALSE)
    {
      y.axis <- "FPKM"
    }
    if(log==TRUE)
    {
      y.axis <- "log2(FPKM)"
      myDataExp.tmp <- myDataExp.c[,-1]
      myDataExp.tmp <- as.data.frame(log2(myDataExp.tmp+1))
      myDataExp.tmp <- cbind(Patient_Sample=myDataExp.c$Patient_Sample, myDataExp.tmp)
      myDataExp.c <- myDataExp.tmp
    }
  }
  
  # add annotation data to expression set
  myDataExp.c <- merge(myDataExp.c, myDataAnn, by.x="Patient_Sample",by.y='row.names')
  
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
    if(sortby == "MYCN")
    {
      sortby = "MYCNS"
    }
  }
  
  # sort by value
  if(sortby == "Gene"){
    myDataExp.c$Patient_Sample <- reorder(myDataExp.c$Patient_Sample, myDataExp.c[,gene1])
  }
  if(sortby != "Gene" && sortby != "None"){
    myDataExp.c$Patient_Sample <- reorder(myDataExp.c$Patient_Sample, as.numeric(as.factor(myDataExp.c[,sortby])))
  }
  
  # modify gene name, dashes present
  gene1.mut <- paste('`',gene1,'`',sep='')
  
  if(colorby == "None")
  {
    if(density == FALSE)
    {
      p <- ggplot(myDataExp.c, aes_string(x='Patient_Sample', y=gene1.mut)) + 
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
      p <- ggplot(myDataExp.c, aes_string(x='Patient_Sample', y=gene1.mut, fill=colorby)) + customtheme +
        geom_bar(stat="identity") + theme(axis.text.x  = element_text(angle=90)) + ggtitle(gene1)
    }
    if(density == TRUE)
    {
      p <- ggplot(myDataExp.c, aes_string(gene1.mut, fill=colorby)) + geom_density(alpha = 0.5) + customtheme + ggtitle(gene1) 
    }
  }
  
  p <- plotly_build(p)
  p$x$layout$yaxis$title <- y.axis
  
  return(p)
  
}