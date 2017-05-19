####################################
# code to plot rna-seq RPKM data
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
###################################

boxPlotGeneSUTC <- function(gene1, logby, tumData, normData, normDataAnnot)
{
  # format data
  normData <- normData[rownames(normData) == gene1,]
  tumData <- tumData[rownames(tumData)== gene1,]
  normData <- melt(normData)
  tumData <- melt(tumData)
  
  normData <- merge(normData, normDataAnnot[,c('SAMPID','SMTSD')], by.x = 'variable', by.y = 'SAMPID')
  tumData$SMTSD <- 'TARGET NBL'
  
  tmpDat <- rbind(tumData, normData)
  colnames(tmpDat) <- c('ID','FPKM','Tissue')
  
  if(logby == TRUE) {
    tmpDat$FPKM <- log2(tmpDat$FPKM)
    y.axis <- "Log2(FPKM)"
  } else {
    y.axis <- "FPKM"
  }
  
  tmpDat$Tissue <- as.factor(tmpDat$Tissue)
  tmpDat$Tissue <- relevel(tmpDat$Tissue, ref = 'TARGET NBL')
  
  p <- ggplot(tmpDat, aes(Tissue, FPKM, fill = Tissue)) + 
    geom_boxplot() + 
    ggtitle(gene1) + theme_bw() + 
    theme(axis.text.x = element_text(size = 12, angle = 60, hjust = 0),
          axis.text.y = element_text(size = 12),
          plot.margin = unit(c(1, 1, 7, 1), "cm"),
          legend.position = "none")
  
  p <- plotly_build(p)
  
  p$x$layout$yaxis$title <- y.axis
  p$x$layout$xaxis$title <- ""

  return(p)
}