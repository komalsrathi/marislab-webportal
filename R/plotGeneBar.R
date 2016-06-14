####################################
# plot bar plot for a gene
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
####################################

plotGeneBar <- function(dat, gene1, log, customtheme, sortby)
{
  
  # load initial dataset
  dat$gene <- rownames(dat)
  dat.m <- melt(data = dat, id.vars = 'gene')
  dat.c <- dcast(data = dat.m, formula = variable~gene, value.var = 'value')
  colnames(dat.c)[1] = "Cell_Line"
  
  # modify gene name, dashes present
  gene1.mut <- paste('`',gene1,'`',sep='')
  
  # plot log values?
  if(log == FALSE)
  {
    y.axis <- 'Value'
  }
  if(log==TRUE)
  {
    y.axis <- 'Log2(Value)'
    dat.tmp <- dat.c[,-1]
    dat.tmp <- log2(dat.tmp+1)
    dat.tmp <- cbind(Cell_Line=dat.c$Cell_Line, dat.tmp)
    dat.c <- dat.tmp
  }
  
  # sorting of bars
  if(sortby == "Value"){
    dat.c$Cell_Line <- reorder(dat.c$Cell_Line,dat.c[,gene1])
  }
  
  # ggplot 
  p <- ggplot(dat.c, aes_string(x='Cell_Line', y=gene1.mut, fill='Cell_Line')) + 
    geom_bar(stat="identity") + customtheme + theme(axis.text.x  = element_text(angle=90)) + 
    ggtitle(gene1)
  
  p <- plotly_build(p)
  p$layout$annotations[[1]]$text <- ""
  p$layout$yaxis$title <- y.axis

  return(p)
  
}
