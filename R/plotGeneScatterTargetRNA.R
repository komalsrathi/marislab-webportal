#############################################
# plot scatter plot of 2 genes for Target RNA
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
############################################

# plotGeneScatterTargetRNA begins
plotGeneScatterTargetRNA <- function(datatype, dat, gene1, gene2, log, customtheme, correlation, colorby, targetcode){
  
  # load initial dataset
  dat <- dat[rownames(dat) %in% c(gene1, gene2), grep(paste(colorby, collapse = "|"), colnames(dat))]
  dat$gene <- rownames(dat)
  dat.m <- melt(data = dat, id.vars = 'gene')
  dat.c <- dcast(data = dat.m, formula = variable~gene, value.var = 'value')
  colnames(dat.c)[1] = "Sample"
  
  # compute correlation
  cor <- cor.test(dat.c[,gene1], dat.c[,gene2], method = correlation)
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
  cor.title <- paste("Cor = ", cor.est, " | P-Val = ", cor.pval, sep="")
  
  # modify gene name, dashes present
  gene1.mut <- paste('`',gene1,'`',sep = '')
  gene2.mut <- paste('`',gene2,'`',sep = '')
  
  # datatype
  if(length(grep('FPKM',datatype))==1) {
    y.axis <- 'FPKM'
    if(log == FALSE)
    {
      y.axis <- y.axis
    } else {
      y.axis <- paste0('Log2','(',y.axis,')')
      dat.c[,c(gene1,gene2)] <- log2(dat.c[,c(gene1,gene2)]+1)
    }
  } else if(length(grep('TPM', datatype))==1) {
    y.axis <- 'TPM'
    if(log == FALSE)
    {
      y.axis <- y.axis
    } else {
      y.axis <- paste0('Log2','(',y.axis,')')
      dat.c[,c(gene1,gene2)] <- log2(dat.c[,c(gene1,gene2)]+1)
    }
  }
  
  dat.c$Code <- gsub('(TARGET-[0-9]{2}-).*','\\1',dat.c$Sample)
  targetcode[] <- lapply(targetcode, as.character)
  dat.c <- merge(dat.c, targetcode, by = "Code")
  dat.c$Tumor <- as.factor(dat.c$Tumor)
  colorby = "Tumor"
  
  # if(length(levels(dat.c[,colorby]))>1){
  #   p <- ggplot(data = dat.c, aes_string(x = gene1.mut, y = gene2.mut, color = "Tumor", label = "Sample")) + 
  #     geom_point() + geom_smooth(method = lm) + customtheme + ggtitle(label = cor.title) +
  #     theme(axis.text.x = element_text(size = 12),
  #           axis.text.y = element_text(size = 12),
  #           legend.text = element_text(size = 10),
  #           legend.title = element_text(size = 12))
  # }
  # if(length(levels(dat.c[,colorby]))==1){
  #   p <- ggplot(data = dat.c, aes_string(x = gene1.mut, y = gene2.mut, label = "Sample")) + 
  #     geom_point() + geom_smooth(method = lm) + customtheme + ggtitle(label = cor.title) + 
  #     theme(axis.text.x = element_text(size = 12),
  #           axis.text.y = element_text(size = 12),
  #           legend.text = element_text(size = 12))
  # }
  
  # colorby tumor
  if(length(levels(dat.c[,colorby])) > 1){
    # correlations <- plyr::ddply(.data = dat.c, .variables = colorby, .fun = function(x) getCorr(dat = x, gene1 = gene1, gene2 = gene2, correlation = correlation))
    # print(correlations)
    p <- ggplot(data = dat.c, aes_string(x = gene1.mut, y = gene2.mut, fill = "Tumor", label = 'Sample')) + 
      geom_point(size = 3, shape = 21, colour = 'black', stroke = 0.2) + customtheme + ggtitle(cor.title)
  } else {
    # correlations <- data.frame(Cor = cor.est, Pval = cor.pval)
    p <- ggplot(data = dat.c, aes_string(x = gene1.mut, y = gene2.mut, label = 'Sample')) + 
      geom_point(size = 3, shape = 21, colour = 'black', stroke = 0.2, fill = "gray") + customtheme + ggtitle(cor.title)
  }
  
  p <- plotly_build(p)
  p$x$layout$yaxis$title <- paste0(gene2,' (', y.axis,')')
  p$x$layout$xaxis$title <- paste0(gene1,' (', y.axis,')')
  
  return(p)
} 