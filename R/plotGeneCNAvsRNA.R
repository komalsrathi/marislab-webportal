#####################################
# mRNA/CNA correlation for a gene
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#####################################

plotGeneCNAvsRNA <- function(mrna, cna, gene1, customtheme, correlation, datatype, phenotype, colorby)
{
  
  #Let's do other stuff
  intCL <- intersect(colnames(mrna), colnames(cna))
  tmpDataGC <- data.frame(t(rbind(mrna[gene1,intCL], cna[gene1,intCL])))
  colnames(tmpDataGC) <- c("mRNA", "CNA")
  
  # compute correlation
  cor <- cor.test(tmpDataGC[,"mRNA"], tmpDataGC[,"CNA"], method = correlation)
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
  
  tmpDataGC[,"Cell_Line"] <- rownames(tmpDataGC)
  
  # datatype
  if(length(grep('FPKM',datatype))==1) {
    y.axis <- 'FPKM'
  } else if(length(grep('RMA',datatype))==1) {
    y.axis <- 'RMA'
  }
  
  # merge
  tmpDataGC <- merge(tmpDataGC, phenotype, by.x = 'Cell_Line', by.y = 'CellLine', all.x = TRUE)
  
  if(colorby != "None"){
    correlations <- plyr::ddply(.data = tmpDataGC, .variables = colorby, .fun = function(x) getCorr(dat = x, gene1 = 'mRNA', gene2 = 'CNA', correlation = correlation))
    p <- ggplot(data = tmpDataGC, aes_string(x = 'mRNA', y = 'CNA', fill = colorby, label = 'Cell_Line')) + 
      geom_point(size = 3, shape = 21, colour = 'black', stroke = 0.2) + customtheme + ggtitle(cor.title)
    # p <- p + geom_smooth(method = lm, se = FALSE, linetype = 'dashed', size = 0.5)
  } else if(colorby == "None"){
    correlations <- data.frame(Cor = cor.est, Pval = cor.pval)
    p <- ggplot(data = tmpDataGC, aes_string(x = 'mRNA', y = 'CNA', label = 'Cell_Line')) + 
      geom_point(size = 3, shape = 21, colour = 'black', stroke = 0.2, fill = "gray") + customtheme + ggtitle(cor.title) 
    # p <- p + geom_smooth(method = lm, se = FALSE, linetype = 'dashed', size = 0.5)
  } 
  
  p <- plotly_build(p)
  p$x$layout$xaxis$title <- paste0("mRNA"," (",y.axis,")")
  p$x$layout$yaxis$title <- paste0("CNA"," (Copy Number)")
  
  newList <- list(p, correlations)
  return(newList)
  # return(p)
  
}