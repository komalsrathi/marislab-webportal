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
    if(log == FALSE) {
      y.axis <- y.axis
    } else {
      y.axis <- paste0('Log2','(',y.axis,')')
      if(gene1 == gene2){
        dat.c[,c(gene1)] <- log2(dat.c[,c(gene1)] + 1)
      } else {
        dat.c[,c(gene1,gene2)] <- log2(dat.c[,c(gene1,gene2)]+1)
      }
    }
  } else if(length(grep('RMA',datatype))==1) {
    y.axis <- 'RMA'
    if(log == FALSE) {
      y.axis <- y.axis
      if(gene1 == gene2){
        dat.c[,c(gene1)] <- 2^(dat.c[,c(gene1)])
      } else {
        dat.c[,c(gene1,gene2)] <- 2^(dat.c[,c(gene1,gene2)])
      }
    } else {
      y.axis <- paste0('Log2','(',y.axis,')')
    }
  } else if(length(grep('TPM', datatype))==1) {
    y.axis <- 'TPM'
    if(log == FALSE) {
      y.axis <- y.axis
    } else {
      y.axis <- paste0('Log2','(',y.axis,')')
      if(gene1 == gene2){
        dat.c[,c(gene1)] <- log2(dat.c[,c(gene1)] + 1)
      } else {
        dat.c[,c(gene1,gene2)] <- log2(dat.c[,c(gene1,gene2)]+1)
      }
    }
  }
  
  # merge phenotype data
  dat.c <- merge(dat.c, phenotype, by.x = 'Cell_Line', by.y = 'CellLine', all.x = TRUE)

  # get correlations
  if(colorby != "None"){
    correlations <- plyr::ddply(.data = dat.c, .variables = colorby, .fun = function(x) getCorr(dat = x, gene1 = gene1, gene2 = gene2, correlation = correlation))
    p <- ggplot(data = dat.c, aes_string(x = gene1.mut, y = gene2.mut, fill = colorby, label = 'Cell_Line')) + 
      geom_point(size = 3, shape = 21, colour = 'black', stroke = 0.2) + customtheme + ggtitle(cor.title)
    # p <- p + geom_smooth(method = lm, se = FALSE, linetype = 'dashed', size = 0.5)
  } else {
    correlations <- data.frame(Cor = cor.est, Pval = cor.pval)
    p <- ggplot(data = dat.c, aes_string(x = gene1.mut, y = gene2.mut, label = 'Cell_Line')) + 
      geom_point(size = 3, shape = 21, colour = 'black', stroke = 0.2, fill = "gray") + customtheme + ggtitle(cor.title)
    # p <- p + geom_smooth(method = lm, se = FALSE, linetype = 'dashed', size = 0.5)
  }

  p <- plotly_build(p)
  
  p$x$layout$yaxis$title <- paste0(gene2,' (', y.axis,')')
  p$x$layout$xaxis$title <- paste0(gene1,' (', y.axis,')')
  
  newList <- list(p, correlations)
  return(newList)
} # plotGeneScatter ends