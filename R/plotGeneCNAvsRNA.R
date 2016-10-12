#####################################
# mRNA/CNA correlation for a gene
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#####################################

plotGeneCNAvsRNA <- function(mrna, cna, gene1, customtheme, correlation, datatype, phenotype, colorby)
{
  
  #Let's do other stuff
  intCL <- intersect(colnames(mrna), colnames(cna))
  tmpDataGC <- data.frame(t(rbind(mrna[gene1,intCL], cna[gene1,intCL])))
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
  
  tmpDataGC[,"Cell_Line"] <- rownames(tmpDataGC)
  
  # datatype
  if(length(grep('FPKM',datatype))==1)
  {
    y.axis <- 'FPKM'
  }
  if(length(grep('RMA',datatype))==1)
  {
    y.axis <- 'RMA'
  }
  
  # merge
  tmpDataGC <- merge(tmpDataGC, phenotype, by.x = 'Cell_Line', by.y = 'CellLine', all.x = TRUE)
  
  if(colorby == "None"){
    p <- ggplot(data = tmpDataGC, aes_string(x = 'mRNA', y = 'CNA', label = 'Cell_Line')) + 
      geom_point() + geom_smooth(method = lm) + customtheme + ggtitle(label = myText)
  }
  if(colorby != "None"){
    p <- ggplot(data = tmpDataGC, aes_string(x = 'mRNA', y = 'CNA', color = colorby, label = 'Cell_Line')) + 
      geom_point() + geom_smooth(method = lm) + customtheme + ggtitle(label = myText)
  }
  
  p <- plotly_build(p)
  p$x$layout$xaxis$title <- paste0("mRNA"," (",y.axis,")")
  p$x$layout$yaxis$title <- paste0("CNA"," (Copy Number)")
  
  return(p)
  
}