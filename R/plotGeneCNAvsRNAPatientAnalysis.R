#############################################
# plot Gene CNA vs RNA for Patient data
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#############################################

# plot scatter plot of 2 genes
plotGeneCNAvsRNAPatientAnalysis <- function(gene1, myData, mycData, customtheme, correlation)
{
  
  #Let's do other stuff
  intCL <- intersect(colnames(myData), colnames(mycData))
  tmpDataGC <- data.frame(t(rbind(myData[gene1,intCL], mycData[gene1,intCL])))
  colnames(tmpDataGC) <- c("mRNA", "CNA")
  
  #For title correlation and p-value
  tmpcor <- cor.test(tmpDataGC[,"mRNA"], tmpDataGC[,"CNA"], method = correlation)
  if(tmpcor$p.value==0){
    tmpcorp <- '< 2.2e-16'
  }
  if(tmpcor$p.value>0){
    tmpcorp <- format(tmpcor$p.value, scientific = T)
  }
  if(tmpcor$estimate==1){
    tmpcore <- 1
  }
  if(tmpcor$estimate!=1){
    tmpcore <- format(tmpcor$estimate, scientific = T)
  }
  myText <- paste("Cor=", tmpcore, " | P-Val=", tmpcorp, sep="")

  tmpDataGC[,"CL_NAME"] <- rownames(tmpDataGC)
  
  p <- ggplot(tmpDataGC, aes(x = mRNA, y = CNA)) + customtheme + 
    geom_point() + geom_smooth(method="lm") + ggtitle(myText)
  
  p <- plotly_build(p)
  p$x$layout$xaxis$title <- paste0("mRNA"," (RMA)")
  p$x$layout$yaxis$title <- paste0("CNA"," (Copy Number)")
  #p$x$data[[1]]$marker$line$color <- "rgb(220,20,60)"
  #p$x$data[[1]]$marker$color <- "rgb(220,20,60)"
  
  return(p)
}
