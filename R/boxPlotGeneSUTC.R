####################################
# code to plot rna-seq RPKM data
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
###################################

boxPlotGeneSUTC <- function(gene1, logby, tumData, normData, normDataAnnot)
{
  # format data
  normData <- normData[rownames(normData) == gene1,]
  normData <- melt(normData)
  normData <- merge(normData, normDataAnnot[,c('SAMPID','SMTSD')], by.x = 'variable', by.y = 'SAMPID')
  
  tumData <- tumData[rownames(tumData) == gene1,]
  tumData <- melt(tumData)
  tumData$SMTSD <- 'TARGET NBL'
  
  FetalAdrenalGland_FPKM_hg38_data <- FetalAdrenalGland_FPKM_hg38_data[rownames(FetalAdrenalGland_FPKM_hg38_data) == gene1,]
  if(nrow(FetalAdrenalGland_FPKM_hg38_data) > 0){
    FetalAdrenalGland_FPKM_hg38_data <- melt(FetalAdrenalGland_FPKM_hg38_data)
    FetalAdrenalGland_FPKM_hg38_data$SMTSD <- 'Fetal Adrenal Gland'
    tmpDat <- rbind(tumData, normData, FetalAdrenalGland_FPKM_hg38_data)
  } else {
    tmpDat <- rbind(tumData, normData)
  }
  
  colnames(tmpDat) <- c('ID','FPKM','Tissue')
  
  if(logby == TRUE) {
    tmpDat$FPKM <- log2(tmpDat$FPKM)
    y.axis <- "Log2(FPKM)"
  } else {
    y.axis <- "FPKM"
  }
  
  tmpDat$Tissue <- as.factor(tmpDat$Tissue)
  tmpDat$Tissue <- relevel(tmpDat$Tissue, ref = 'TARGET NBL')
  
  p <- p <- ggplot(tmpDat, aes(x=Tissue, y=FPKM)) + 
    stat_boxplot(geom ='errorbar', width = 0.2) +
    geom_boxplot(lwd = 0.5, fatten = 0.7, outlier.shape = 1, width = 0.5, outlier.size = 1, aes(fill = Tissue)) +
    geom_jitter(width = 0.1, pch = 21, stroke = 0.2, aes(fill = Tissue)) + customtheme + 
    ggtitle(gene1) + theme(legend.position = "none") + 
    stat_compare_means(method = "anova", label.x.npc = "center", label.y.npc = "top", color = "red")
  
  p <- plotly_build(p)
  
  p$x$layout$yaxis$title <- y.axis
  p$x$layout$xaxis$title <- ""

  # remove outliers
  p$x$data[1:length(levels(tmpDat[,'Tissue'])) + 1] <- lapply(p$x$data[1:length(levels(tmpDat[,'Tissue'])) + 1], FUN = function(x){
    x$marker = list(opacity = 0)
    return(x)
  })
  
  return(p)
}