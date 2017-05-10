####################################
# plot bar plot for a gene
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
####################################

plotGeneBarPDX <- function(datatype, dat, phenotype, gene1, log, customtheme, sortby, colorby)
{
  
  # load initial dataset
  dat <- dat[rownames(dat) %in% gene1,]
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
      dat.c[,gene1] <- log2(dat.c[,gene1]+1)
    }
  }
  
  # add phenotype data
  dat.c <- merge(dat.c, phenotype, by.x = 'PDX', by.y = 'row.names', all.x = TRUE)
  
  # sorting of bars
  if(sortby == "PDX"){
    dat.c$PDX <- factor(x = dat.c$PDX, levels = sort(as.character(dat.c$PDX)))
  }
  if(sortby == "Gene"){
    dat.c$PDX <- reorder(dat.c$PDX, dat.c[,gene1])
  }
  
  # ggplot 
  if(colorby != "None"){
    p <- ggplot(dat.c, aes_string(x='PDX', y=gene1.mut, fill = colorby)) +
      geom_bar(stat="identity") + customtheme + 
      theme(axis.text.x  = element_text(angle=45), 
            plot.margin = unit(c(0.5, 0.5, 2, 0.5), "cm")) + 
      ggtitle(gene1)
  } else {
    p <- ggplot(dat.c, aes_string(x='PDX', y=gene1.mut)) +
      geom_bar(stat="identity") + customtheme + 
      theme(axis.text.x  = element_text(angle=45), 
            plot.margin = unit(c(0.5, 0.5, 2, 0.5), "cm")) + 
      ggtitle(gene1)
  }
  
  
  p <- plotly_build(p)
  p$x$layout$yaxis$title <- y.axis
  
  return(p)
}
