####################################
# plot scatter plot of 2 genes
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
####################################

# plot scatter plot of 2 genes##################
plotGeneScatterPatientData <- function(datatype, gene1, gene2, myDataExp, myDataAnn, log, colorby, correlation, customtheme)
{
  
  # get expression and annotation of the selected dataset
  myDataExp <- myDataExp[rownames(myDataExp) %in% c(gene1, gene2),]
  myDataExp$gene <- rownames(myDataExp)
  myDataExp.m <- melt(data = myDataExp, id.vars = 'gene')
  myDataExp.c <- dcast(data = myDataExp.m, formula = variable~gene, value.var = 'value')
  colnames(myDataExp.c)[1] = "Sample"
  
  #For title correlation and p-value
  cor <- cor.test(myDataExp.c[,gene1], myDataExp.c[,gene2], method = correlation)
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
  
  # plot log values? 
  if(length(grep('FPKM',datatype))==0) {
    if(log==FALSE) {
      y.axis <- "RMA"
      if(gene1 == gene2){
        myDataExp.c[,c(gene1)] <- 2^(myDataExp.c[,c(gene1)])
      } else {
        myDataExp.c[,c(gene1,gene2)] <- 2^(myDataExp.c[,c(gene1,gene2)])
      }
    } else {
      y.axis <- "log2(RMA)"
    }
  } else {
    if(log==FALSE)
    {
      y.axis <- "FPKM"
    } else {
      y.axis <- "log2(FPKM)"
      if(gene1 == gene2){
        myDataExp.c[,c(gene1)] <- log2(myDataExp.c[,c(gene1)]+1)
      } else {
        myDataExp.c[,c(gene1,gene2)] <- log2(myDataExp.c[,c(gene1,gene2)]+1)
      }
    }
  }
  
  # add annotation data to expression set
  myDataExp.c <- merge(myDataExp.c, myDataAnn, by.x = "Sample", by.y = 'row.names')
  
  # eliminate confusion between MYCN gene and status
  if(length(grep('MYCN',colnames(myDataExp.c)))>1) {
    coln <- grep("MYCN.x", colnames(myDataExp.c))
    colnames(myDataExp.c)[coln] <- 'MYCN'
    coln <- grep("MYCN.y", colnames(myDataExp.c))
    colnames(myDataExp.c)[coln] <- 'MYCNS'
    if(colorby=="MYCN")
    {
      colorby = "MYCNS"
    }
  }
  
  if(colorby != "None"){
    correlations <- plyr::ddply(.data = myDataExp.c, .variables = colorby, .fun = function(x) getCorr(dat = x, gene1 = gene1, gene2 = gene2, correlation = correlation))
    p <- ggplot(data = myDataExp.c, aes_string(x = gene1.mut, y = gene2.mut, fill = colorby, label = 'Sample')) + 
      geom_point(size = 3, shape = 21, colour = 'black', stroke = 0.2) + customtheme + ggtitle(cor.title)
  } else {
    correlations <- data.frame(Cor = cor.est, Pval = cor.pval)
    p <- ggplot(data = myDataExp.c, aes_string(x = gene1.mut, y = gene2.mut, label = 'Sample')) + 
      geom_point(size = 3, shape = 21, colour = 'black', stroke = 0.2, fill = "gray") + customtheme + ggtitle(cor.title)
  }
  
  p <- plotly_build(p)
  p$x$layout$yaxis$title <- paste0(gene2,' (', y.axis,')')
  p$x$layout$xaxis$title <- paste0(gene1,' (', y.axis,')')
  
  newList <- list(p, correlations)
  return(newList)
} 