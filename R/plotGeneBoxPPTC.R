#######################################################
# plot box plot for a gene per dataset in Target RNASeq
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
######################################################

# myDataExp <- get(load('data/PPTC_FPKM_hg19_Wheeler_subtracted_data.RData'))
# gene1 <- "GPC2"
# log <- FALSE
# tumor <- c('Neuroblastoma', 'ALL', 'Brain_Medulloblastoma')
# tumor <- c('Neuroblastoma', 'ALL')
# tumor <- 'Neuroblastoma'
# myDataAnn <- get(load('data/PPTC_FPKM_hg19_Wheeler_subtracted_mData.RData'))

plotGeneBoxPPTC <- function(gene1, tumor, myDataExp, myDataAnn, log, customtheme)
{
  myDataAnn <- myDataAnn[which(myDataAnn$tumor_subtype %in% tumor),]
  colorby <- 'tumor_subtype'
  
  myDataExp <- myDataExp[rownames(myDataExp) %in% gene1,colnames(myDataExp) %in% rownames(myDataAnn)]
  myDataExp$gene <- rownames(myDataExp)
  myDataExp.m <- melt(data = myDataExp, id.vars = 'gene')
  myDataExp.c <- dcast(data = myDataExp.m, formula = variable~gene, value.var = 'value')
  colnames(myDataExp.c)[1] = "Sample"
  
  # plot log values? 
  if(log == FALSE) {
    y.axis <- "FPKM"
  } else {
    y.axis <- "log2(FPKM)"
    myDataExp.c[,gene1] <- log2(myDataExp.c[,gene1]+1)
  }
  
  # modify gene name, dashes present
  gene1.mut <- paste('`',gene1,'`',sep='')
  
  # add annotation data to expression set
  myDataExp.c <- merge(myDataExp.c, myDataAnn, by.x = "Sample", by.y = 'row.names')

  if(length(tumor) > 2) {
    anovaRes <- aov(lm(myDataExp.c[,gene1]~myDataExp.c[,colorby]))
    dat.tukey <- TukeyHSD(anovaRes)[[1]]
    p <- ggplot(myDataExp.c, aes_string(x=colorby, y=gene1.mut, fill=colorby)) + 
      geom_boxplot() + customtheme + 
      ggtitle(gene1) + 
      theme(axis.text.y = element_text(size = 12), 
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12), 
            legend.position = "none")
  } else if(length(tumor) == 2) {
    anovaRes <- aov(lm(myDataExp.c[,gene1]~myDataExp.c[,colorby]))
    dat.tukey <- data.frame()
    pval <- summary(anovaRes)[[1]][[5]][1]
    pval <- signif(pval, 6)
    myText <- paste("Anova P-Val=", pval, sep="")
    p <- ggplot(myDataExp.c, aes_string(x=colorby, y=gene1.mut, fill=colorby)) + 
      geom_boxplot() + customtheme + 
      ggtitle(paste0(gene1,'\n',myText)) + 
      theme(axis.text.y = element_text(size = 12), 
            axis.text.x = element_text(size = 12), 
            legend.position = "none")
  } else if(length(tumor) == 1) {
    p <- ggplot(myDataExp.c, aes_string(x=colorby, y=gene1.mut, fill=colorby)) + 
      customtheme + geom_boxplot() + 
      ggtitle(gene1) +
      theme(axis.text.y = element_text(size = 12), 
            axis.text.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            legend.position = "none")
    dat.tukey <- data.frame()
  }
  p <- plotly_build(p)
  p$x$layout$yaxis$title <- y.axis
  p$x$layout$xaxis$title <- ""
  
  newList <- list(p, dat.tukey)
  return(newList)
}
