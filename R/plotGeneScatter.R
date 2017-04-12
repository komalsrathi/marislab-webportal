####################################
# plot scatter plot of 2 genes
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
####################################

# plotGeneScatter begins
plotGeneScatter <- function(datatype, dat, gene1, gene2, log, customtheme, correlation, colorby, phenotype){
  
  # load initial dataset
  dat <- dat[rownames(dat) %in% c(gene1,gene2),]
  dat$gene <- rownames(dat)
  dat.m <- melt(data = dat, id.vars = 'gene')
  dat.c <- dcast(data = dat.m, formula = variable~gene, value.var = 'value')
  colnames(dat.c)[1] = "Cell_Line"
  
  # compute correlation
  cor <- cor.test(dat.c[,gene1], dat.c[,gene2], method = correlation)
  if(cor$p.value==0){
    cor.pval <- '< 2.2e-16'
  }
  if(cor$p.value>0){
    cor.pval <- format(cor$p.value, scientific = T)
  }
  if(cor$estimate==1){
    cor.est <- 1
  }
  if(cor$estimate!=1){
    cor.est <- format(cor$estimate, scientific = T)
  }
  cor.title <- paste("Cor = ", cor.est, " | P-Val = ", cor.pval, sep="")
  
  # modify gene name, dashes present
  gene1.mut <- paste('`',gene1,'`',sep = '')
  gene2.mut <- paste('`',gene2,'`',sep = '')
  
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
      dat.c[,c(gene1,gene2)] <- log2(dat.c[,c(gene1,gene2)]+1)
    }
  }
  
  if(length(grep('RMA',datatype))==1)
  {
    y.axis <- 'RMA'
    if(log == FALSE)
    {
      y.axis <- y.axis
      dat.c[,c(gene1,gene2)] <- 2^(dat.c[,c(gene1,gene2)])
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
      dat.c[,c(gene1,gene2)] <- log2(dat.c[,c(gene1,gene2)]+1)
    }
  }
  
  # merge phenotype data
  dat.c <- merge(dat.c, phenotype, by.x = 'Cell_Line', by.y = 'CellLine', all.x = TRUE)
  
  if(colorby != "None"){
    p <- ggplot(data = dat.c, aes_string(x = gene1.mut, y = gene2.mut, color = colorby, label = 'Cell_Line')) + 
      geom_point() + geom_smooth(method = lm) + customtheme + ggtitle(label = cor.title)
  }
  if(colorby == "None"){
    p <- ggplot(data = dat.c, aes_string(x = gene1.mut, y = gene2.mut, label = 'Cell_Line')) + 
      geom_point() + geom_smooth(method = lm) + customtheme + ggtitle(label = cor.title)
  }
  
  p <- plotly_build(p)
  
  p$x$layout$yaxis$title <- paste0(gene2,' (', y.axis,')')
  p$x$layout$xaxis$title <- paste0(gene1,' (', y.axis,')')
  
  return(p)
} # plotGeneScatter ends