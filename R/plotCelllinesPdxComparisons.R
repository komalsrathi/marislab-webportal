#####################################
# plots for pdx-celllines comparisons
# Authors: Komal Rathi
# Organization: DBHi, CHOP
#####################################

# plotGeneScatter begins
plotCelllinesPdxComparisons <- function(dat, gene1, log, theme_scatter, theme_bar, correlation, colorby){
  
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
  if(colorby == "None"){
    dat.m$colorby <- dat.m[,'MYCN_Status']
  } else {
    dat.m$colorby <- dat.m[,colorby]
  }
  dat.m <- dat.m[with(dat.m, order(dplyr::desc(colorby), FPKM, Model)),]
  dat.m$Model <- as.character(dat.m$Model)
  dat.m$Model <- factor(dat.m$Model, levels = unique(as.character(dat.m$Model)))
  dat.m$Label <- paste0(dat.m$group ,'\nMYCN: ',dat.m$MYCN_Status, '\nALK: ',dat.m$ALK_Status, '\nTP53: ',dat.m$TP53_Status)
  
  # plot
  if(colorby != "None"){
    p <- ggplot(data = dat, aes_string(x = 'PDX', y = 'CellLines', color = colorby, label = 'Model')) + 
      geom_point(size = 3) + geom_smooth(method = lm, se = FALSE, linetype = 'dashed') + theme_scatter + ggtitle(cor.title)
    
    q <- ggplot(data = dat.m, aes_string(x = 'Model', y = 'FPKM', label = 'Label', fill = 'colorby', color = 'group')) + 
      geom_bar(stat = 'identity', position = 'dodge', size = 0.2) + xlab('') + theme_bar +
      guides(color = FALSE, label = FALSE, fill = FALSE) + ggtitle(paste0('Gene = ',gene1)) + 
      scale_color_manual(values = c("black","black"))
  } else {
    p <- ggplot(data = dat, aes_string(x = 'PDX', y = 'CellLines', label = 'Model')) + 
      geom_point(size = 3) + geom_smooth(method = lm, se = FALSE, linetype = 'dashed') + theme_scatter + ggtitle(cor.title)
    
    q <- ggplot(data = dat.m, aes_string(x = 'Model', y = 'FPKM', fill = 'group', label = 'Label')) + 
      geom_bar(stat = 'identity', position = 'dodge', color = 'black', size = 0.2)  + xlab('') + theme_bar + 
      guides(color = FALSE) + ggtitle(paste0('Gene = ',gene1))
  }
  
  p <- plotly_build(p)
  q <- plotly_build(q) %>% layout(showlegend = FALSE)
  
  newList <- list(p,q)
  return(newList)
} # plotCelllinesPdxComparisons ends