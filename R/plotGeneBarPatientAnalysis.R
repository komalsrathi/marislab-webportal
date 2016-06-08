#############################################
# plot bar plot for a gene per patient sample
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#############################################

# load patient sample set
load('data/allDataPatient.RData')

# function to plot bar charts
plotGeneBarPatientAnalysis <- function(gene1, dataset, sortby, log, density, colorby)
{

  # get selected dataset
  myData <- paste(dataset,'_All',sep='')
  myData <- get(myData)
  
  # get expression and annotation of the selected dataset
  myDataExp <- myData[[1]]
  myDataAnn <- myData[[2]]
  
  # modify dataframe
  myDataExp$gene <- rownames(myDataExp)
  myDataExp.m <- melt(data = myDataExp, id.vars = 'gene')
  myDataExp.c <- dcast(data = myDataExp.m, formula = variable~gene, value.var = 'value')
  colnames(myDataExp.c)[1] = "Cell_Line"
  
  # sort by value
  if(sortby==TRUE){
    myDataExp.c$Cell_Line <- reorder(myDataExp.c$Cell_Line,myDataExp.c[,gene1])
  }
  
  # plot log values
  if(log==FALSE)
  {
    myDataExp.tmp <- myDataExp.c[,-1]
    myDataExp.tmp <- as.data.frame(2^myDataExp.tmp)
    myDataExp.tmp <- cbind(Cell_Line=myDataExp.c$Cell_Line, myDataExp.tmp)
    myDataExp.c <- myDataExp.tmp
  }
  
  # add annotation data to expression set
  myDataExp.c <- merge(myDataExp.c, myDataAnn, by.x="Cell_Line",by.y='row.names')
  
  # eliminate confusion between MYCN gene and status
  coln <- grep("MYCN.x", colnames(myDataExp.c))
  colnames(myDataExp.c)[coln] <- 'MYCN'
  coln <- grep("MYCN.y", colnames(myDataExp.c))
  colnames(myDataExp.c)[coln] <- 'MYCNS'
  if(colorby=="MYCN")
  {
    colorby = "MYCNS"
  }
  
  # modify gene name, dashes present
  gene1.mut <- paste('`',gene1,'`',sep='')
  
  if(colorby == "None")
  {
    if(density == FALSE)
    {
      p <- ggplot(myDataExp.c, aes_string(x='Cell_Line', y=gene1.mut)) + 
        geom_bar(stat="identity") + theme(axis.text.x  = element_text(angle=90)) + ggtitle(gene1)
    }
    if(density == TRUE)
    {
      p <- ggplot(myDataExp.c, aes_string(gene1.mut, fill=1)) + geom_density() + 
        theme(axis.text.x  = element_text(angle=90), legend.position = "none") + ggtitle(gene1) 
    }
  }
  if(colorby!="None")
  {
    if(density == FALSE)
    {
      p <- ggplot(myDataExp.c, aes_string(x='Cell_Line', y=gene1.mut, fill=colorby)) + 
        geom_bar(stat="identity") + theme(axis.text.x  = element_text(angle=90)) + ggtitle(gene1)
    }
    if(density == TRUE)
    {
      p <- ggplot(myDataExp.c, aes_string(gene1.mut, fill=colorby)) + geom_density() + 
        theme(axis.text.x  = element_text(angle=90), legend.position = "none") + ggtitle(gene1) 
    }
  }
  p <- ggplotly(p)
  return(p)
  
}