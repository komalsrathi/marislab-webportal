####################################
# plot bar plot for a gene
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
####################################

plotGeneBar <- function(dat, gene1, log=T, customtheme)
{
  
  # load initial dataset
  dat$gene <- rownames(dat)
  dat.m <- melt(data = dat, id.vars = 'gene')
  dat.c <- dcast(data = dat.m, formula = variable~gene, value.var = 'value')
  colnames(dat.c)[1] = "Cell_Line"
  
  # plot log values?
  if(log==F)
  {
    dat.c <- 2^dat.c
  }
  
  # plot 
  p <- ggplot(dat.c, aes_string(x='Cell_Line', y=gene1, fill='Cell_Line')) + 
    geom_bar(stat="identity") + customtheme + theme(axis.text.x  = element_text(angle=90)) + 
    ggtitle(gene1) + ylab('Expression Value\n')
  
  return(p)
  
}
