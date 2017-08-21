#####################################
# plots for pdx-celllines comparisons
# Authors: Komal Rathi
# Organization: DBHi, CHOP
#####################################

# plotGeneScatter begins
plotCelllinesPdxComparisons <- function(dat, gene1, log, customtheme, correlation, colorby){
  
  # load initial dataset
  dat <- dat[dat$gene_symbol %in% gene1,]
  
  # # compute correlation
  cor <- cor.test(dat[,'PDX'], dat[,'CellLines'], method = correlation)
  if(is.na(cor$p.value)){
    cor.pval <- NA
  } else if(cor$p.value==0){
    cor.pval <- '< 2.2e-16'
  } else if(cor$p.value>0){
    cor.pval <- format(cor$p.value, scientific = T, digits = 3)
  } 
  
  if(is.na(cor$estimate)){
    cor.est <- NA
  } else if(cor$estimate==1){
    cor.est <- 1
  } else if(cor$estimate!=1){
    cor.est <- format(cor$estimate, scientific = T, digits = 3)
  }
  cor.title <- paste("Gene = ",gene1," | Cor = ", cor.est, " | P-Val = ", cor.pval, sep="")
  
  # modify gene name, dashes present
  gene1.mut <- paste('`',gene1,'`',sep = '')
  
  # datatype
  y.axis <- 'FPKM'
  if(log == FALSE) {
    y.axis <- y.axis
  } else {
    y.axis <- paste0('Log2','(',y.axis,')')
    dat[,c('PDX','CellLines')] <- log2(dat[,c('PDX','CellLines')]+1)
  }
  bar.axis <- y.axis
  x.axis <- paste0('PDX (', y.axis,')')
  y.axis <- paste0('Cell-Lines (', y.axis,')')
  
  dat.m <- melt(dat, variable.name = 'group', value.name = 'FPKM')
  
  # plot
  if(colorby != "None"){
    p <- ggplot(data = dat, aes_string(x = 'PDX', y = 'CellLines', color = colorby, label = 'names')) + 
      geom_point() + geom_smooth(method = lm) + customtheme + ggtitle(cor.title)
    
    q <- ggplot(data = dat.m, aes_string(x = 'names', y = 'FPKM', fill = 'group', label = colorby)) + 
      geom_bar(stat = 'identity', position = 'dodge')  + xlab(label = '') + themebw() + 
      theme(axis.text.x  = element_text(angle=45), plot.margin = unit(c(0.5, 0.5, 2, 0.5), "cm")) +
      guides(color = FALSE)
  } else {
    p <- ggplot(data = dat, aes_string(x = 'PDX', y = 'CellLines', label = 'names')) + 
      geom_point() + geom_smooth(method = lm) + customtheme + ggtitle(cor.title)
    
    q <- ggplot(data = dat.m, aes_string(x = 'names', y = 'FPKM', fill = 'group')) + 
      geom_bar(stat = 'identity', position = 'dodge')  + xlab(label = '') + themebw() + 
      theme(axis.text.x  = element_text(angle=45), plot.margin = unit(c(0.5, 0.5, 2, 0.5), "cm")) + 
      guides(color = FALSE)
  }
  
  p <- plotly_build(p)
  q <- plotly_build(q)
  
  newList <- list(p,q)
  return(newList)
} # plotCelllinesPdxComparisons ends