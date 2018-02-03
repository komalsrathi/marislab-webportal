####################################
# plot scatter plot of 2 genes
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
####################################

# myDataExp <- get(load('data/PPTC_FPKM_hg19_Wheeler_subtracted_data.RData'))
# gene1 <- "GPC2"
# gene2 <- "MYCN"
# log <- FALSE
# tumor <- c('Neuroblastoma', 'Ependymoma', 'Medulloblastoma')
# myDataAnn <- get(load('data/PPTC_FPKM_hg19_Wheeler_subtracted_mData.RData'))
# correlation <- 'pearson'

# plot scatter plot of 2 genes ##################
plotGeneScatterPPTC <- function(gene1, gene2, myDataExp, myDataAnn, log, tumor, correlation, customtheme, colorby)
{
  
  # get expression and annotation of the selected dataset
  # if(length(tumor) == 1 && tumor == "none"){
  #   colorby <- "None"
  # } else if(length(tumor) >= 1 && tumor != "none"){
  #   myDataAnn <- myDataAnn[which(myDataAnn$CANCER %in% tumor),]
  #   colorby <- 'CANCER'
  # }  
  if(length(tumor) >= 1 && tumor != "none"){
    myDataAnn <- myDataAnn[which(myDataAnn$CANCER %in% tumor),]
  }  
  myDataExp <- myDataExp[rownames(myDataExp) %in% c(gene1, gene2),colnames(myDataExp) %in% rownames(myDataAnn)]
  myDataExp$gene <- rownames(myDataExp)
  myDataExp.m <- melt(data = myDataExp, id.vars = 'gene')
  myDataExp.c <- dcast(data = myDataExp.m, formula = variable~gene, value.var = 'value')
  colnames(myDataExp.c)[1] = "SAMPLE_ID"
  
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
  if(log == FALSE) {
    y.axis <- "FPKM"
  } else {
    y.axis <- "log2(FPKM)"
    myDataExp.c[,c(gene1,gene2)] <- log2(myDataExp.c[,c(gene1,gene2)]+1)
  }
  
  # add annotation data to expression set
  myDataExp.c <- merge(myDataExp.c, myDataAnn, by = "SAMPLE_ID")
  
  # colorby tumor
  if(colorby != "None"){
    correlations <- plyr::ddply(.data = myDataExp.c, .variables = colorby, .fun = function(x) getCorr(dat = x, gene1 = gene1, gene2 = gene2, correlation = correlation))
    p <- ggplot(data = myDataExp.c, aes_string(x = gene1.mut, y = gene2.mut, label = 'SAMPLE_ID', color = colorby)) + 
      geom_point() + geom_smooth(method = lm) + customtheme + ggtitle(cor.title)
  } else {
    correlations <- data.frame(Cor = cor.est, Pval = cor.pval)
    p <- ggplot(data = myDataExp.c, aes_string(x = gene1.mut, y = gene2.mut, label = 'SAMPLE_ID')) + 
      geom_point() + geom_smooth(method = lm) + customtheme + ggtitle(cor.title)
  }
  
  p <- plotly_build(p)
  p$x$layout$yaxis$title <- paste0(gene2,' (', y.axis,')')
  p$x$layout$xaxis$title <- paste0(gene1,' (', y.axis,')')
  print(correlations)
  newList <- list(p, correlations)
  return(newList)
} 