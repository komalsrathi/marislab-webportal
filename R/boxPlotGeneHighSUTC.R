####################################
# code to plot rna-seq RPKM data
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
###################################

load('data/TumNormData.RData')

boxPlotGeneHighSUTC <- function(gene1, logby)
{
  
  # format data
  rownames(tumData) <- tumData[,1]
  tumData <- tumData[-1]
  rownames(normDataAnnot) <- normDataAnnot[,1]
  rownames(normDataAnnot) <- gsub("-", ".", rownames(normDataAnnot))
  normDataAnnot <- normDataAnnot[(colnames(normData)[3:1643]),]
  
  tmpTum <- tumData[gene1,]
  tmpTum <- data.frame(t(tmpTum), "Cancer")
  colnames(tmpTum) <- c("FPKM", "Tissue")
  
  tmpNorm <- normData[normData[,2]==gene1,3:1643]
  tmpNorm <- t(tmpNorm)
  tmpNorm <- data.frame(tmpNorm, as.character(normDataAnnot[,"SMTS"]))
  colnames(tmpNorm) <- c("FPKM", "Tissue")
  
  tmpDat <- rbind(tmpTum, tmpNorm)
  
  if(logby == TRUE)
  {
    tmpDat$FPKM <- log2(tmpDat$FPKM)
    y.axis <- "Log2(FPKM)"
  }
  if(logby == FALSE)
  {
    y.axis <- "FPKM"
  }
  
  p <- ggplot(tmpDat, aes(Tissue, FPKM, fill = Tissue)) + 
    geom_boxplot() + ggtitle(gene1) + theme_bw() + 
    theme(axis.text.x=element_text(angle = -75, hjust = 0)) + 
    theme(plot.margin = unit(c(1, 1, 3, 1), "cm"))
  
  p <- plotly_build(p)
  
  p$layout$yaxis$title <- y.axis
  p$layout$annotations[[1]]$text <- ""
  
  return(p)
}
