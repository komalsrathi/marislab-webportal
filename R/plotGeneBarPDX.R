####################################
# plot bar plot for a gene
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
####################################

plotGeneBarPDX <- function(datatype, dat, gene1, log, customtheme, sortby)
{
  
  # load initial dataset
  dat$gene <- rownames(dat)
  dat.m <- melt(data = dat, id.vars = 'gene')
  dat.c <- dcast(data = dat.m, formula = variable~gene, value.var = 'value')
  colnames(dat.c)[1] = "PDX"
  
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
      dat.tmp$PDX <- dat.c$PDX
      dat.c <- dat.tmp
    }
  }
  
  # sorting of bars
  if(sortby == "PDX"){
    dat.c$PDX <- factor(x = dat.c$PDX, levels = sort(as.character(dat.c$PDX)))
  }
  if(sortby == "Gene"){
    dat.c$PDX <- reorder(dat.c$PDX, dat.c[,gene1])
  }
  
  # ggplot 
  p <- ggplot(dat.c, aes_string(x='PDX', y=gene1.mut)) +
    geom_bar(stat="identity") + customtheme + 
    theme(axis.text.x  = element_text(angle=45), 
          plot.margin = unit(c(0.5, 0.5, 2, 0.5), "cm")) + 
    ggtitle(gene1)
  
  p <- plotly_build(p)
  p$x$layout$yaxis$title <- y.axis
  
  return(p)
}
