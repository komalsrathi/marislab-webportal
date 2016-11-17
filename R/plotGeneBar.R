####################################
# plot bar plot for a gene
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
####################################

plotGeneBar <- function(datatype, dat, phenotype, gene1, log, customtheme, sortby, colorby)
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
      dat.tmp <- as.data.frame(apply(dat.c[,-1], MARGIN = 2, function(x) log2(x+1)))
      dat.tmp$Cell_Line <- dat.c$Cell_Line
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
      dat.tmp <- as.data.frame(apply(dat.c[,-1], MARGIN = 2, function(x) log2(x+1)))
      dat.tmp$Cell_Line <- dat.c$Cell_Line
      dat.c <- dat.tmp
    }
  }
  
  # add phenotype data - MYCN status
  dat.c <- merge(dat.c, phenotype, by.x = 'Cell_Line', by.y = 'CellLine', all.x = TRUE)
  
  # sorting of bars
  if(sortby == "CellLine"){
    dat.c$Cell_Line <- factor(x = dat.c$Cell_Line, levels = sort(as.character(dat.c$Cell_Line)))
  }
  if(sortby == "Gene"){
    dat.c$Cell_Line <- reorder(dat.c$Cell_Line, dat.c[,gene1])
  }
  if(sortby == "MYCN_Status"){
    dat.c$Cell_Line <- reorder(dat.c$Cell_Line, as.numeric(dat.c$MYCN_Status))
  }
  
  # ggplot 
  if(colorby != "None"){
    p <- ggplot(dat.c, aes_string(x='Cell_Line', y=gene1.mut, fill = colorby)) + guides(fill=FALSE) + 
      geom_bar(stat="identity") + customtheme + theme(axis.text.x  = element_text(angle=45), plot.margin = unit(c(0.5, 0.5, 2, 0.5), "cm")) + 
      ggtitle(gene1)
  }
  if(colorby == "None"){
    p <- ggplot(dat.c, aes_string(x='Cell_Line', y=gene1.mut)) + guides(fill=FALSE) + 
      geom_bar(stat="identity") + customtheme + theme(axis.text.x  = element_text(angle=45), plot.margin = unit(c(0.5, 0.5, 2, 0.5), "cm")) + 
      ggtitle(gene1)
  }
  
  p <- plotly_build(p)
  p$x$layout$yaxis$title <- y.axis
  
  return(p)
  
}
