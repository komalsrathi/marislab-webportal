#############################################
# plot Gene CNA vs RNA for Patient data
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#############################################

# plot scatter plot of 2 genes
plotGeneCNAvsRNAPatientAnalysis <- function(gene1, myData, mycData, customtheme)
{
  
  #Let's do other stuff
  intCL <- intersect(colnames(myData), colnames(mycData))
  tmpDataGC <- data.frame(t(rbind(myData[gene1,intCL], mycData[gene1,intCL])))
  colnames(tmpDataGC) <- c("mRNA", "CNA")
  
  #For title correlation and p-value
  tmpcor <- cor.test(tmpDataGC[,"mRNA"], tmpDataGC[,"CNA"])
  tmpcorp <- round(tmpcor$p.val,2)
  tmpcor <- round(tmpcor$estimate,2)
  myText <- paste("Cor=", tmpcor, " | P-Val=", tmpcorp, sep="")

  tmpDataGC[,"CL_NAME"] <- rownames(tmpDataGC)
  
  p <- ggplot(tmpDataGC, aes(x = mRNA, y = CNA)) + customtheme + 
    geom_point() + geom_smooth(method="lm") + ggtitle(myText)
  
  p <- plotly_build(p)
  p$layout$xaxis$title <- paste0("mRNA"," (RMA)")
  p$layout$yaxis$title <- paste0("CNA"," (Copy Number)")
  
  return(p)
}
