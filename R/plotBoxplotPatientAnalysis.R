#############################################
# plot box plot for a gene per dataset 
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#############################################

plotBoxplotPatientAnalysis <- function(gene1, colorby, dataset, log, customtheme)
{
  # get expression and annotation of the selected dataset
  myDataExp <- get(paste(dataset,'_data',sep=''))
  myDataAnn <- get(paste(dataset,'_mData',sep=''))
  
  # modify dataframe
  myDataExp$gene <- rownames(myDataExp)
  myDataExp.m <- melt(data = myDataExp, id.vars = 'gene')
  myDataExp.c <- dcast(data = myDataExp.m, formula = variable~gene, value.var = 'value')
  colnames(myDataExp.c)[1] = "Cell_Line"
  
  # plot log values
  if(log==FALSE)
  {
    y.axis <- "Raw Intensity"
    myDataExp.tmp <- myDataExp.c[,-1]
    myDataExp.tmp <- as.data.frame(2^myDataExp.tmp)
    myDataExp.tmp <- cbind(Cell_Line=myDataExp.c$Cell_Line, myDataExp.tmp)
    myDataExp.c <- myDataExp.tmp
  }
  if(log==TRUE)
  {
    y.axis <- "RMA"
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
  
  # change colorby to factor
  myDataExp.c[,colorby] <- as.factor(myDataExp.c[,colorby])
  
  if(length(levels(myDataExp.c[,colorby]))>1)
  {
    pval <- summary(aov(lm(myDataExp.c[,gene1]~myDataExp.c[,colorby])))[[1]][[5]][1]
    pval <- round(pval, 6)
    myText <- paste("Anova P-Val=", pval, sep="")
    p <- ggplot(myDataExp.c, aes_string(x=colorby, y=gene1.mut, fill=colorby)) + geom_boxplot() + customtheme + ggtitle(paste0(gene1,'\n',myText)) + theme(legend.position = "none")
  }
  if(length(levels(myDataExp.c[,colorby]))==1)
  {
    p <- ggplot(myDataExp.c, aes(x=colorby, y=gene1.mut, fill=colorby)) + customtheme + geom_boxplot() + theme(legend.position = "none")
  }
  p <- plotly_build(p)
  p$layout$yaxis$title <- y.axis
  return(p)
  
}