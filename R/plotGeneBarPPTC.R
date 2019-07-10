####################################
# plot bar plot for a gene
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
####################################

# dat <- get(load('data/PPTC_FPKM_hg19_Wheeler_subtracted_data.RData'))
# gene1 <- "GPC2"
# log <- FALSE
# phenotype <- get(load('data/PPTC_FPKM_hg19_Wheeler_subtracted_mData.RData'))
# sortby <- 'Gene'
# tumor <- 'Neuroblastoma'

plotGeneBarPPTC <- function(dat, phenotype, gene1, log, customtheme, sortby, tumor, colorby)
{
  
  # load initial dataset and subset by gene
  dat <- dat[rownames(dat) %in% gene1,]
  dat$gene <- rownames(dat)
  dat.m <- melt(data = dat, id.vars = 'gene')
  dat.c <- dcast(data = dat.m, formula = variable~gene, value.var = 'value')
  colnames(dat.c)[1] <- "MODEL"
  
  # modify gene name, dashes present
  gene1.mut <- paste('`',gene1,'`',sep='')
  
  # datatype
  if(log == FALSE){
    y.axis <- 'FPKM'
  } else {
    dat.c[,gene1] <- log2(dat.c[,gene1]+1)
    y.axis <- 'log2 (FPKM)'
  }
  
  # add phenotype data - MYCN status
  dat.c <- merge(dat.c, phenotype, by = "MODEL", all.x = TRUE)
  
  # subset if tumor is defined
  dat.c <- dat.c[which(dat.c$CANCER_TYPE %in% tumor),]
  
  # sorting of bars
  if(sortby == "PDX"){
    dat.c$MODEL <- factor(x = dat.c$MODEL, levels = sort(as.character(dat.c$MODEL)))
  } else if(sortby == "Gene"){
    dat.c$MODEL <- reorder(dat.c$MODEL, dat.c[,gene1])
  }
  
  # ggplot 
  if(colorby == "None"){
    p <- ggplot(dat.c, aes_string(x = 'MODEL', y = gene1.mut)) + 
      geom_bar(stat = "identity", color = 'black', fill = 'gray', size = 0.2) + customtheme + xlab('') +
      ggtitle(gene1)
  } else {
    p <- ggplot(dat.c, aes_string(x = 'MODEL', y = gene1.mut, fill = colorby)) + 
      geom_bar(stat = "identity", color = 'black', size = 0.2) + customtheme + xlab('') +
      ggtitle(gene1)
  }
  p <- plotly_build(p)
  p$x$layout$yaxis$title <- y.axis
  
  return(p)
}
