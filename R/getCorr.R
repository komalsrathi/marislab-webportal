
getCorr <- function(dat, gene1, gene2, correlation){
  
  if(nrow(dat)<=2){
    cor.title <- data.frame(Cor = "NA", Pval = "NA")
    return(cor.title)
  }
  
  x <- dat[,gene1]
  y <- dat[,gene2]
  cor <- cor.test(x = x, y = y, method = correlation)

  if(is.na(cor$p.value) & is.na(cor$estimate)){
    cor.title <- data.frame(Cor = "NA", Pval = "NA")
    return(cor.title)
  }
  
  if(cor$p.value == 0){
    cor.pval <- '< 2.2e-16'
  } else if(cor$p.value > 0){
    cor.pval <- format(cor$p.value, scientific = T, digits = 3)
  }
  if(cor$estimate == 1){
    cor.est <- 1
  } else if(cor$estimate != 1){
    cor.est <- format(cor$estimate, scientific = T, digits = 3)
  }
  cor.title <- data.frame(Cor = cor.est, Pval = cor.pval)
  return(cor.title)
}
