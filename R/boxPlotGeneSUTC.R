####################################
# code to plot rna-seq RPKM data
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
###################################

load('data/TumNormData.RData')

boxPlotGeneSUTC <- function(gene1)
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
  tmpNorm <- data.frame(tmpNorm, as.character(normDataAnnot[,"SMTSD"]))
  colnames(tmpNorm) <- c("FPKM", "Tissue")
  
  tmpDat <- rbind(tmpTum, tmpNorm)
  
  p <- ggplot(tmpDat, aes(as.factor(Tissue), FPKM, fill= Tissue)) + 
    geom_boxplot() + 
    ggtitle(gene1) + theme_bw() + xlab("Sample") + 
    theme(axis.text.x=element_text(angle = -75, hjust = 0)) + 
    theme(legend.position = "none")
  
  p <- ggplotly(p)
  
  return(p)
}