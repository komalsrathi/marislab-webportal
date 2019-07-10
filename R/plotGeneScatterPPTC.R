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
source('R/getCorr.R')

# plot scatter plot of 2 genes ##################
plotGeneScatterPPTC <- function(gene1, gene2, myDataExp, myDataAnn, log, tumor, correlation, customtheme, colorby)
{
  
  # get expression and annotation of the selected dataset
  # if(length(tumor) == 1 && tumor == "none"){
  #   colorby <- "None"
  # } else if(length(tumor) >= 1 && tumor != "none"){
  #   myDataAnn <- myDataAnn[which(myDataAnn$CANCER_TYPE %in% tumor),]
  #   colorby <- 'CANCER_TYPE'
  # }  
  if(length(tumor) >= 1 && tumor != "none"){
    myDataAnn <- myDataAnn[which(myDataAnn$CANCER_TYPE %in% tumor),]
  }  
  myDataExp <- myDataExp[rownames(myDataExp) %in% c(gene1, gene2),colnames(myDataExp) %in% rownames(myDataAnn)]
  myDataExp$gene <- rownames(myDataExp)
  myDataExp.m <- melt(data = myDataExp, id.vars = 'gene')
  myDataExp.c <- dcast(data = myDataExp.m, formula = variable~gene, value.var = 'value')
  colnames(myDataExp.c)[1] = "MODEL"
  
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
    if(gene1 == gene2){
      myDataExp.c[,c(gene1)] <- log2(myDataExp.c[,c(gene1)] + 1)
    } else {
      myDataExp.c[,c(gene1,gene2)] <- log2(myDataExp.c[,c(gene1,gene2)]+1)
    }
  }
  
  # add annotation data to expression set
  myDataExp.c <- merge(myDataExp.c, myDataAnn, by = "MODEL")
  
  # colorby tumor
  if(colorby != "None"){
    correlations <- plyr::ddply(.data = myDataExp.c, .variables = colorby, .fun = function(x) getCorr(dat = x, gene1 = gene1, gene2 = gene2, correlation = correlation))
    print(correlations)
    p <- ggplot(data = myDataExp.c, aes_string(x = gene1.mut, y = gene2.mut, fill = colorby, label = 'MODEL')) + 
      geom_point(size = 3, shape = 21, colour = 'black', stroke = 0.2) + customtheme + ggtitle(cor.title)
    # p <- p + geom_smooth(method = lm, se = FALSE, linetype = 'dashed', size = 0.5)
  } else {
    correlations <- data.frame(Cor = cor.est, Pval = cor.pval)
    p <- ggplot(data = myDataExp.c, aes_string(x = gene1.mut, y = gene2.mut, label = 'MODEL')) + 
      geom_point(size = 3, shape = 21, colour = 'black', stroke = 0.2, fill = "gray") + customtheme + ggtitle(cor.title)
    # p <- p + geom_smooth(method = lm, se = FALSE, linetype = 'dashed', size = 0.5)
  }
  
  p <- plotly_build(p)
  p$x$layout$yaxis$title <- paste0(gene2,' (', y.axis,')')
  p$x$layout$xaxis$title <- paste0(gene1,' (', y.axis,')')
  print(correlations)
  newList <- list(p, correlations)
  return(newList)
} 