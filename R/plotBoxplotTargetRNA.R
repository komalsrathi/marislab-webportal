#######################################################
# plot box plot for a gene per dataset in Target RNASeq
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
######################################################

plotBoxplotTargetRNA <- function(gene1, colorby, datatype, dat, log, customtheme, targetcode)
{
  dat <- dat[,grep(paste(colorby,collapse = "|"), colnames(dat))]
  dat$gene <- rownames(dat)
  dat.m <- melt(data = dat, id.vars = 'gene')
  dat.c <- dcast(data = dat.m, formula = variable~gene, value.var = 'value')
  colnames(dat.c)[1] = "Sample"
  
  # plot log values
  if(length(grep('FPKM',datatype))==1)
  {
    y.axis <- 'FPKM'
    if(log == FALSE)
    {
      y.axis <- y.axis
    }
    
    if(log == TRUE)
    {
      y.axis <- paste0('Log2','(',y.axis,')')
      dat.tmp <- dat.c[,-1]
      dat.tmp <- as.data.frame(log2(dat.tmp+1))
      dat.tmp <- cbind(Sample=dat.c$Sample, dat.tmp)
      dat.c <- dat.tmp
    }
  }
  if(length(grep('TPM',datatype))==1)
  {
    y.axis <- 'TPM'
    if(log == FALSE)
    {
      y.axis <- y.axis
    }
    
    if(log == TRUE)
    {
      y.axis <- paste0('Log2','(',y.axis,')')
      dat.tmp <- dat.c[,-1]
      dat.tmp <- as.data.frame(log2(dat.tmp+1))
      dat.tmp <- cbind(Sample=dat.c$Sample, dat.tmp)
      dat.c <- dat.tmp
    }
  }
  
  # modify gene name, dashes present
  gene1.mut <- paste('`',gene1,'`',sep='')
  
  # add codes to color by boxplot
  dat.c$Code <- gsub('(TARGET-[0-9]{2}-).*','\\1',dat.c$Sample)
  targetcode[] <- lapply(targetcode, as.character)
  dat.c <- merge(dat.c, targetcode, by = "Code")
  dat.c$Tumor <- as.factor(dat.c$Tumor)
  colorby = "Tumor"
  
  if(length(levels(dat.c[,colorby]))>1)
  {
    pval <- summary(aov(lm(dat.c[,gene1]~dat.c[,colorby])))[[1]][[5]][1]
    pval <- round(pval, 6)
    myText <- paste("Anova P-Val=", pval, sep="")
    p <- ggplot(dat.c, aes_string(x=colorby, y=gene1.mut, fill=colorby)) + geom_boxplot() + customtheme + ggtitle(paste0(gene1,'\n',myText)) + theme(legend.position = "none")
  }
  if(length(levels(dat.c[,colorby]))==1)
  {
    p <- ggplot(dat.c, aes(x=colorby, y=gene1.mut, fill=colorby)) + customtheme + geom_boxplot() + theme(legend.position = "none")
  }
  p <- plotly_build(p)
  p$layout$yaxis$title <- y.axis
  
  return(p)
}
