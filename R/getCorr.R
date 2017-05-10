
getCorr <- function(dat, gene1, gene2, correlation){
  
  x <- dat[,gene1]
  y <- dat[,gene2]
  
  cor <- cor.test(x = x, y = y, method = correlation)
  
  if(cor$p.value==0){
    cor.pval <- '< 2.2e-16'
  }
  if(cor$p.value>0){
    cor.pval <- format(cor$p.value, scientific = T)
  }
  if(cor$estimate==1){
    cor.est <- 1
  }
  if(cor$estimate!=1){
    cor.est <- format(cor$estimate, scientific = T)
  }
  
  cor.title <- data.frame(Cor = cor.est, Pval = cor.pval)
  
  return(cor.title)
}
