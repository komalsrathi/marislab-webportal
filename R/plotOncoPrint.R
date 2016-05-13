###############################
# code for creating heatmaps
# Author: Pichai Raman
# Organization: DBHi, CHOP
###############################

# Plot it
# 0 is no amp/del over/under exp or mut
# 1 is del or under exp or deleterious mutation
# 2 is amp or over exp of activating mutation
library(grid)
library(ggplot2)

plotOncoPrint <- function(genes, dataCNABin, dataMutBin, dataAnnBin, dataExpGeneNameBin, commonCellLines)
{
  
  CNA_genesTmp <- genes[grep("_CNA", genes)]
  MUT_genesTmp <- genes[grep("_MUT", genes)]
  ANN_genesTmp <- genes[grep("_ANN", genes)]
  
  CNA_genes <- gsub("_CNA", "", CNA_genesTmp)
  MUT_genes <- gsub("_MUT", "", MUT_genesTmp)
  ANN_genes <- gsub("_ANN", "", ANN_genesTmp)
  
  # Pull out relevant Genes
  dataTmp <- rbind(dataCNABin[CNA_genes,commonCellLines], dataMutBin[MUT_genes, commonCellLines], dataAnnBin[ANN_genes, commonCellLines])
  rownames(dataTmp) <- c(CNA_genesTmp, MUT_genesTmp, ANN_genesTmp)
  
  # hclust
  dataTmp <- t(dataTmp)
  dataTmp <- data.frame(dataTmp)
  dataTmp <- dataTmp[rev(hclust(dist(dataTmp))$order),]
  dataTmp[,"ID"] <- rownames(dataTmp)
  dataTmp <- data.frame(dataTmp)
  dataTmp[,"ID"] <- factor(dataTmp[,"ID"], levels=as.character(dataTmp[,"ID"]))
  datam <- melt(dataTmp,id.var=c("ID"))
  datam[,"value"] <- as.character(datam[,"value"])
  datam[,"value"] <- gsub(.5,"3-Mut", datam[,"value"])
  datam[,"value"] <- gsub(0,"0-None", datam[,"value"])
  datam[,"value"] <- gsub(1,"1-Del", datam[,"value"])
  datam[,"value"] <- gsub(2,"2-Amp", datam[,"value"])
  
  theme_change <- theme(
    plot.background = element_blank(),
    plot.margin=unit(c(1,1,1,1),"cm"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust=.0005),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
  p <- ggplot(datam, aes(ID, variable)) + geom_tile(aes(fill = value))
  p <- p+geom_tile(colour="white", fill=NA, size=1.2)
  p <- p +scale_fill_manual(values=c("grey", "red", "blue", "yellow"))+theme_change;
  
  
  # Okay now do expression
  EXP_genesTmp <- genes[grep("_EXP", genes)]
  EXP_genes <- gsub("_EXP", "", EXP_genesTmp)
  
  dataTmpExp <- dataExpGeneNameBin[EXP_genes,commonCellLines]
  dataTmpExp <- data.frame(t(dataTmpExp))
  dataTmpExp[,"ID"] <- rownames(dataTmpExp)
  dataTmpExp <- data.frame(dataTmpExp)
  dataTmpExp[,"ID"] <- factor(dataTmpExp[,"ID"], levels=as.character(dataTmp[,"ID"]))
  datamExp <- melt(dataTmpExp,id.var=c("ID"))
  
  p2 <- ggplot(datamExp, aes(ID, variable)) + geom_tile(aes(fill = value))
  p2 <- p2+geom_tile(colour="white", fill=NA, size=1.2)
  p2 <- p2+scale_fill_gradient2(low="yellow", mid="grey", high="red")+theme_change;
  
  gA <- ggplot_gtable(ggplot_build(p))
  gB <- ggplot_gtable(ggplot_build(p2))
  
  # Set the widths
  gA$widths <- gB$widths
  
  # Arrange the two charts.
  # The legend boxes are centered
  grid.newpage()
  output <- grid.arrange(gA, gB, nrow = 2)
  
}