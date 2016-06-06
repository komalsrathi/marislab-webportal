#####################################################
# plot kaplan meier plot for set of genes per dataset 
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#####################################################

# 1 = alive
# 2 = dead

kmScan <- function(x, tVar, eVar)
{
  
  # first sort data frame
  timeVar <- x[,as.character(tVar)]
  eventVar <- x[,as.character(eVar)]
  bestI <- 1
  pVal <- 1
  myDF <- data.frame()
  
  # now we must loop
  for(i in 8:(dim(x)[1]-8))
  {
    x[,"Gene"] <- 1
    x[1:i,"Gene"] <- 0
    x.Surv <- Surv(timeVar, eventVar)
    myP <- pchisq(survdiff(x.Surv~Gene, data=x, rho=0)$chisq, df=1, lower=F)
    
    if(myP<pVal)
    {
      pVal <- myP
      bestI <- i
    }
    
    # put all data in a frame to get adj p-value
    myDF <- rbind(myDF, c(i,myP))
  }
  
  # now p.adjust and return
  adjpval <- min(p.adjust(myDF[,2], method="bonferroni"))
  
  return(c(bestI,pVal, adjpval))
  
}