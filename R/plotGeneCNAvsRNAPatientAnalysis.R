#############################################
# plot Gene CNA vs RNA for Patient data
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#############################################

# plot scatter plot of 2 genes
plotGeneCNAvsRNAPatientAnalysis <- function(gene1, myData, mycData, customtheme, correlation)
{
  
  # get common cell lines
  intCL <- intersect(colnames(myData), colnames(mycData))
  tmpDataGC <- data.frame(t(rbind(myData[gene1,intCL], mycData[gene1,intCL])))
  colnames(tmpDataGC) <- c("mRNA", "CNA")
  
  # for title correlation and p-value
  tmpcor <- cor.test(tmpDataGC[,"mRNA"], tmpDataGC[,"CNA"], method = correlation)
  if(tmpcor$p.value==0){
    tmpcorp <- '< 2.2e-16'
  }
  if(tmpcor$p.value>0){
    tmpcorp <- format(tmpcor$p.value, scientific = T, digits = 3)
  }
  if(tmpcor$estimate==1){
    tmpcore <- 1
  }
  if(tmpcor$estimate!=1){
    tmpcore <- format(tmpcor$estimate, scientific = T, digits = 3)
  }
  cor.title <- paste("Cor=", tmpcore, " | P-Val=", tmpcorp, sep="")

  tmpDataGC[,"CL_NAME"] <- rownames(tmpDataGC)
  
  p <- ggplot(data = tmpDataGC, aes(x = mRNA, y = CNA)) + 
    geom_point(size = 3, shape = 21, colour = 'black', stroke = 0.2, fill = "gray") + customtheme + ggtitle(cor.title)
  
  p <- plotly_build(p)
  p$x$layout$xaxis$title <- paste0("mRNA"," (RMA)")
  p$x$layout$yaxis$title <- paste0("CNA"," (Copy Number)")
  
  return(p)
}
