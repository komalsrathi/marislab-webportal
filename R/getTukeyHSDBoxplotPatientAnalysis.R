#############################################
# get Tukey HSD for a gene per dataset 
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#############################################

getTukeyHSDBoxplotPatientAnalysis <- function(datatype, gene1, colorby, myDataExp, myDataAnn, log)
{
  # get expression and annotation of the selected dataset
  # modify dataframe
  myDataExp$gene <- rownames(myDataExp)
  myDataExp.m <- melt(data = myDataExp, id.vars = 'gene')
  myDataExp.c <- dcast(data = myDataExp.m, formula = variable~gene, value.var = 'value')
  colnames(myDataExp.c)[1] = "Patient_Sample"
  
  # plot log values
  if(length(grep('FPKM',datatype))==0)
  {
    if(log==FALSE)
    {
      y.axis <- "RMA"
      myDataExp.tmp <- myDataExp.c[,-1]
      myDataExp.tmp <- as.data.frame(2^myDataExp.tmp)
      myDataExp.tmp <- cbind(Patient_Sample=myDataExp.c$Patient_Sample, myDataExp.tmp)
      myDataExp.c <- myDataExp.tmp
    }
    if(log==TRUE)
    {
      y.axis <- "log2(RMA)"
    }
  }
  if(length(grep('FPKM',datatype))==1)
  {
    if(log==FALSE)
    {
      y.axis <- "FPKM"
    }
    if(log==TRUE)
    {
      y.axis <- "log2(FPKM)"
      myDataExp.tmp <- myDataExp.c[,-1]
      myDataExp.tmp <- as.data.frame(log2(myDataExp.tmp+1))
      myDataExp.tmp <- cbind(Patient_Sample=myDataExp.c$Patient_Sample, myDataExp.tmp)
      myDataExp.c <- myDataExp.tmp
    }
  }
  
  
  # add annotation data to expression set
  myDataExp.c <- merge(myDataExp.c, myDataAnn, by.x="Patient_Sample",by.y='row.names')
  
  # eliminate confusion between MYCN gene and status
  if(length(grep('MYCN',colnames(myDataExp.c)))>1)
  {
    coln <- grep("MYCN.x", colnames(myDataExp.c))
    colnames(myDataExp.c)[coln] <- 'MYCN'
    coln <- grep("MYCN.y", colnames(myDataExp.c))
    colnames(myDataExp.c)[coln] <- 'MYCNS'
    if(colorby=="MYCN")
    {
      colorby = "MYCNS"
    }
  }
  
  # change colorby to factor
  myDataExp.c[,colorby] <- as.factor(myDataExp.c[,colorby])
  
  if(length(levels(myDataExp.c[,colorby]))>1)
  {
    anovaRes <- aov(lm(myDataExp.c[,gene1]~myDataExp.c[,colorby]))
    dat <- TukeyHSD(anovaRes)[[1]]
  }
  if(length(levels(myDataExp.c[,colorby]))==1)
  {
    dat <- data.frame()
  }
  return(dat)
}