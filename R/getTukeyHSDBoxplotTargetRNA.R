#############################################
# get Tukey HSD for a gene per dataset 
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#############################################

getTukeyHSDBoxplotTargetRNA <- function(gene1, colorby, datatype, dat, log, targetcode)
{
  # get expression and annotation of the selected dataset
  # modify dataframe
  dat <- dat[,grep(paste(colorby,collapse = "|"), colnames(dat))]
  dat <- dat[rownames(dat) %in% gene1,]
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
      dat.c[,gene1] <- log2(dat.c[,gene1]+1)
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
      dat.c[,gene1] <- log2(dat.c[,gene1]+1)
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
    anovaRes <- aov(lm(dat.c[,gene1]~dat.c[,colorby]))
    dat <- TukeyHSD(anovaRes)[[1]]
  }
  if(length(levels(dat.c[,colorby]))==1)
  {
    dat <- data.frame()
  }
  
  return(dat)
}