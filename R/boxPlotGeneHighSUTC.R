####################################
# code to plot rna-seq RPKM data
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
###################################

boxPlotGeneHighSUTC <- function(gene1, logby, tumData, normData, normDataAnnot)
{
  
  # format data
  rownames(tumData) <- tumData[,1]
  tumData <- tumData[-1]
  rownames(normDataAnnot) <- normDataAnnot[,1]
  rownames(normDataAnnot) <- gsub("-", ".", rownames(normDataAnnot))
  normDataAnnot <- normDataAnnot[(colnames(normData)[3:1643]),]
  
  tmpTum <- tumData[gene1,]
  tmpTum <- data.frame(t(tmpTum), "TARGET NBL")
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
    theme(axis.text.x = element_text(size = 12, angle = 60, hjust = 0),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          plot.margin = unit(c(1, 1, 3, 1), "cm"),
          legend.position = "none")
  
  p <- plotly_build(p)
  
  p$x$layout$yaxis$title <- y.axis
  p$x$layout$xaxis$title <- ""

  return(p)
}
