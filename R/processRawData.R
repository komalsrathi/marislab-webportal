###############################
# code for processing raw data
# Author: Pichai Raman
# Organization: DBHi, CHOP
###############################

# data processing for cell line data
library(sqldf)
source('R/parseGeneInfo.R')

processRawData <- function(){
  # read mutation data
  dataMutBin <- read.delim('data/MUT_DATA_MATRIX.txt', row.names = 1)
  dataAnnBin <- read.delim('data/CELL_LINE_ANN.txt', row.names = 1)
  
  # read Maris Expression data
  load('data/alldata.RData')
  rownames(dataExp) <- dataExp[,1]
  dataExp <- dataExp[-1]
  
  # single CNA per gene and format
  dataCNA <- unique(dataCNA)
  maxCNA <- apply(dataCNA[2:dim(dataCNA)[2]], FUN=max, MARGIN=1)
  dataCNA[,"MAX"] <- maxCNA
  dataCNA <- dataCNA[order(-dataCNA[,"MAX"]),]
  dataCNA <- dataCNA[!duplicated(dataCNA[,1]),]
  rownames(dataCNA) <- dataCNA[,1]
  dataCNA <- dataCNA[-1]
  dataCNA <- dataCNA[-grep("MAX", colnames(dataCNA))]
  
  # annotation from HuGene Chip
  myAnnot <- myAnnot[,c(1,2,8)]
  
  tmpParse <- data.frame(t(apply(myAnnot, FUN=parseGeneInfo, MARGIN=1)))
  colnames(tmpParse) <- c("ID", "cDNA", "GeneName", "GeneDesc", "Pos", "GeneID")
  tmpParse <- unique(tmpParse)
  tmpParse[,"ID"] <- as.character(tmpParse[,"ID"])
  
  # hold onto this for later
  probeAnnot <- tmpParse
  
  # First transform into genes
  tmpDataForSet <- dataExp
  tmpDataForSet <- data.frame(tmpDataForSet)
  
  tmpDataForSet$Mean <- apply(tmpDataForSet, FUN=mean, MARGIN=1) 
  tmpDataForSet$ID <- rownames(tmpDataForSet)
  
  tmpDataForSet <- merge(tmpDataForSet, probeAnnot[,c("ID", "GeneName")], by.x="ID", by.y="ID")
  tmpDataForSet <- na.omit(tmpDataForSet)
  
  # mas probe per gene
  maxProbe <- data.frame(rownames(tmpDataForSet), tmpDataForSet[,c("Mean", "GeneName")])
  colnames(maxProbe)[1] <- "probe"
  maxProbeAgg <- sqldf("select GeneName, probe, max(Mean) from maxProbe group by GeneName")
  maxProbeAgg <- maxProbeAgg[-1,]
  
  tmpDataForSet <- tmpDataForSet[as.character(maxProbeAgg[,"probe"]),]
  rownames(tmpDataForSet) <- tmpDataForSet[,"GeneName"]
  tmpDataForSet <- tmpDataForSet[1:30]
  tmpDataForSet <- tmpDataForSet[-1]
  tmpDataForSet <- as.matrix(tmpDataForSet)
  
  # hold on to this for later
  dataExpGeneName <- as.data.frame(tmpDataForSet)
  
  newList <- list("dataExp" = dataExp, "dataExpGeneName" = dataExpGeneName, 
                  "dataCNA" = dataCNA, "dataMutBin" = dataMutBin, 
                  "dataAnnBin" = dataAnnBin, "probeAnnot" = probeAnnot)
  return(newList)
}