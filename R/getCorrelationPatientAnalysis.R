#############################################
# get most correlated genes for selected gene
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#############################################

# load patient sample set
load('data/allDataPatient.RData')

myCorTest <- function(x,y)
{
  out <- cor.test(x,y)
  out <- c(out$estimate, out$p.value)
  return(out)
}

getCorrelationPatientAnalysis <- function(gene1, dataset, numRet)
{

  # get selected dataset
  myData <- paste(dataset,'_data',sep='')
  myData <- get(myData)
  
  # get gene of interest's values
  myGene <- as.matrix(myData)[gene1,]

  # calculate correlation
  output <- apply(as.matrix(myData), FUN=myCorTest, MARGIN=1, y=myGene)
  output <- data.frame(t(output))
  output[,"Q_Value"] <- p.adjust(output[,2], method="BH")
  output <- output[order(-abs(output[,1])),]
  colnames(output)[1:2] <- c("Cor", "P.Value")
  output <- cbind(rownames(output), output)
  colnames(output)[1] <- "Gene"
  rownames(output) <- NULL
  output <- output[1:numRet,]
  return(output)
}