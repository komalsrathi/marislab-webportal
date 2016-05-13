#####################################
# mRNA/CNA correlation for a gene
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#####################################

plotGeneCNAvsRNA <- function(mrna, cna, gene1, customtheme)
{
  
  #Let's do other stuff
  intCL <- intersect(colnames(mrna), colnames(cna))
  tmpDataGC <- data.frame(t(rbind(mrna[gene1,intCL], cna[gene1,intCL])))
  colnames(tmpDataGC) <- c("mRNA", "CNA")
  
  #For title correlation and p-value
  tmpcor <- cor.test(tmpDataGC[,"mRNA"], tmpDataGC[,"CNA"])
  tmpcorp <- round(tmpcor$p.val,2)
  tmpcor <- round(tmpcor$estimate,2)
  myText <- paste("Cor=", tmpcor, " | P-Val=", tmpcorp, sep="")
  
  tmpDataGC[,"CL_NAME"] <- rownames(tmpDataGC)
  
  p <- ggplot(data = tmpDataGC, aes(x = mRNA, y = CNA)) + 
    geom_point(size = 2) + geom_smooth(method = lm) + customtheme +
    geom_text(vjust=-1.5, size = 3, aes(label=CL_NAME)) + ggtitle(label = myText)
  
  p <- ggplotly(p)
  
  return(p)
  
}