#######################################
# Run comparisons on sets of cell lines
# Author: Pichai Raman
# Organization: DBHi, CHOP
#######################################

library(limma)
# function to run Limma comparison
# supply targets file and comparison
runLimma <- function(targets, contrast, gs, pvalue, probeAnnot, dataExp)
{ 
  
  targetsDF <- targets
  targets <- targets[,1]
  
  # running limma in gene or geneset space
  if(gs==F)
  {
    myData <- dataExp
  }
  if(gs==T)
  {
    myData <- GeneSetExprsMat
  }
  
  # Main code
  design <- model.matrix(~0+targets)
  colnames(design) <- gsub("targets", "", colnames(design))
  dataExpTmp <- myData[, rownames(targetsDF)]
  fit <- lmFit(dataExpTmp, design)
  
  contrast.matrix <- eval(as.call(c(as.symbol("makeContrasts"),
                                    as.list(contrast),
                                    levels=list(design))))
  
  fit2 <- contrasts.fit(fit, contrast.matrix)
  fit2 <- eBayes(fit2)
  outputAll <- topTable(fit2, number = Inf)
  outputAll$ID <- rownames(outputAll)
  
  # Add gene annotation
  if(gs==F)
  {
    outputAll <- merge(outputAll, probeAnnot, by.x="ID", by.y="ID")
  }
  
  # Sort according to p-value
  outputAll <- outputAll[order(outputAll[,"P.Value"]),]
  outputAll <- unique(outputAll)
  
  # okay get rid of dups
  dups <- row.names(outputAll[duplicated(outputAll[,1], fromLast=T),])
  outputAll <- outputAll[setdiff(row.names(outputAll), dups),]
  outputAll <- outputAll[,c("ID", "GeneName", "GeneDesc", "Pos", "logFC", "P.Value", "adj.P.Val")]
  outputAll <- outputAll[outputAll[,"P.Value"]<pvalue,]
  
  return(outputAll)
  
}