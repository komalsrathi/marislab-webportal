##################################################
# code for post processing data to create heatmaps
# Author: Pichai Raman
# Organization: DBHi, CHOP
##################################################

# oncoprints on cell lines
# Currently includes
# -Expression Z score +1/-1 cutoff
# -Copy Number 4 is amp and 1 is del
# -Mutation

postProcessDataForHeatmap <- function(dataCNA, dataExpGeneName, dataAnnBin, dataMutBin){
  # Must create trinary copy number and expression matrices
  # Let's start with Copy Number, set amplification as >4 and deletion as <1
  dataCNABin <- as.matrix(dataCNA)
  dataCNABin[dataCNABin<4&dataCNABin>1] <- 0;
  dataCNABin[dataCNABin<=1&dataCNABin>0] <- 1;
  dataCNABin[dataCNABin>=4] <- 2;
  dataCNABin <- as.data.frame(dataCNABin)
  
  # Now let's do expression
  dataExpGeneNameBin <- dataExpGeneName;
  myZ <- function(x) {out <- (x-mean(x))/sd(x)}
  dataExpGeneNameBin <-t(apply(dataExpGeneNameBin, FUN=myZ, MARGIN=1))
  dataExpGeneNameBin <- as.data.frame(dataExpGeneNameBin)
  
  # Finally let's do mutation, oh wait its already done for us....
  
  # Now annotation
  dataAnnBin <- data.frame(t(dataAnnBin))
  dataMutBin <- as.data.frame(dataMutBin)
  
  commonCellLines <- intersect(colnames(dataExpGeneNameBin), colnames(dataCNABin))
  commonCellLines <- intersect(commonCellLines, colnames(dataMutBin))
  
  newList <- list("dataCNABin" = dataCNABin, "dataMutBin" = dataMutBin, "dataAnnBin" = dataAnnBin, "dataExpGeneNameBin" = dataExpGeneNameBin, "commonCellLines" = commonCellLines)
  return(newList)
}
