#####################################
# mRNA/CNA correlation for a gene
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#####################################

plotGeneCNAvsRNA <- function(mrna, cna, gene1, customtheme, correlation)
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
  
  tmpDataGC[,"CL_NAME"] <- rownames(tmpDataGC)
  
  # p <- ggplot(data = tmpDataGC, aes(x = mRNA, y = CNA)) + 
  #   geom_point(size = 2) + geom_smooth(method = lm) + customtheme +
  #   geom_text(vjust=-1.5, size = 3, aes(label=CL_NAME)) + ggtitle(label = myText)
  # 
  # p <- ggplotly(p + ylab(" ") + xlab(" "))
  # 
  # x <- list(
  #   title = "mRNA"
  # )
  # y <- list(
  #   title = "CNA"
  # )
  # p <- p %>% layout(xaxis = x, yaxis = y)
  
  p <- ggplot(data = tmpDataGC, aes(x = mRNA, y = CNA)) + 
    geom_point() + geom_smooth(method = lm) + customtheme + ggtitle(label = myText)
  
  p <- plotly_build(p)
  p$data[[1]]$text <- tmpDataGC$CL_NAME
  p$data[[1]]$mode <- "markers+text"
  p$data[[1]]$textposition <- "top center"
  p$data[[1]]$marker$size <- 4
  p$data[[1]]$marker$color <- "rgb(220,20,60)"
  p$data[[1]]$marker$line$color <- "rgb(220,20,60)"
  
  return(p)
  
}