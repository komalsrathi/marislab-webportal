####################################
# plot bar plot for a gene
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
####################################

plotGeneBar <- function(datatype, dat, gene1, log, customtheme, sortby)
{
  
  # load initial dataset
  dat$gene <- rownames(dat)
  dat.m <- melt(data = dat, id.vars = 'gene')
  dat.c <- dcast(data = dat.m, formula = variable~gene, value.var = 'value')
  colnames(dat.c)[1] = "Cell_Line"
  
  # modify gene name, dashes present
  gene1.mut <- paste('`',gene1,'`',sep='')
  
  # datatype
  if(length(grep('FPKM',datatype))==1)
  {
    y.axis <- 'FPKM'
    if(log == FALSE)
    {
      y.axis <- y.axis
    }
    
    if(log == TRUE)
    {
      y.axis <- paste0('Log2','(',y.axis,')')
      dat.tmp <- dat.c[,-1]
      dat.tmp <- as.data.frame(log2(dat.tmp+1))
      dat.tmp <- cbind(Cell_Line=dat.c$Cell_Line, dat.tmp)
      dat.c <- dat.tmp
    }
  }
  
  if(length(grep('RMA',datatype))==1)
  {
    y.axis <- 'RMA'
    if(log == FALSE)
    {
      y.axis <- y.axis
      dat.tmp <- dat.c[,-1]
      dat.tmp <- as.data.frame(2^(dat.tmp))
      dat.tmp <- cbind(Cell_Line=dat.c$Cell_Line, dat.tmp)
      dat.c <- dat.tmp
    }
    
    if(log == TRUE)
    {
      y.axis <- paste0('Log2','(',y.axis,')')
    }
  }
  
  if(length(grep('TPM', datatype))==1)
  {
    y.axis <- 'TPM'
    if(log == FALSE)
    {
      y.axis <- y.axis
    }
    
    if(log == TRUE)
    {
      y.axis <- paste0('Log2','(',y.axis,')')
      dat.tmp <- dat.c[,-1]
      dat.tmp <- log2(dat.tmp+1)
      dat.tmp <- cbind(Cell_Line=dat.c$Cell_Line, dat.tmp)
      dat.c <- dat.tmp
    }
  }
  
  # sorting of bars
  if(sortby == "Value"){
    dat.c$Cell_Line <- reorder(dat.c$Cell_Line,dat.c[,gene1])
  }
  
  # ggplot 
  p <- ggplot(dat.c, aes_string(x='Cell_Line', y=gene1.mut, fill='Cell_Line')) + 
    geom_bar(stat="identity") + customtheme + theme(axis.text.x  = element_text(angle=45), plot.margin = unit(c(0.5, 0.5, 2, 0.5), "cm")) + 
    ggtitle(gene1)
  
  p <- plotly_build(p)
  p$layout$annotations[[1]]$text <- ""
  p$layout$yaxis$title <- y.axis
  
  return(p)
  
}
