################################
# get mutation table for a gene
# Authors: Pichai Raman
# Organization: DBHi, CHOP
################################

library(reshape2)

cellMutationTable <- function(gene, dataset)
{
  data <- dataset
  tmpData <- data[data[,"Gene"]==gene,]
  
  ## this is only for formatting initial data to 52 columns
  # colstoKeep <- colnames(tmpData)[c(1:12, 42:79)]
  # tmpData2 <- melt(tmpData, id=colstoKeep)
  # 
  # # remove nulls
  # remove <- as.numeric(lapply(tmpData2[,"value"], FUN=nchar))!=1
  # tmpData2 <- tmpData2[remove,]
  # colnames(tmpData2)[51:52] <- c("Cell Line", "Genotype")
  # return(tmpData2)
  ## this is only for formatting
  
  return(tmpData)
}