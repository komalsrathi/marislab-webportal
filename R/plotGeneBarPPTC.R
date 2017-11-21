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

plotGeneBarPPTC <- function(dat, phenotype, gene1, log, customtheme, sortby, tumor)
{
  
  # load initial dataset and subset by gene
  dat <- dat[rownames(dat) %in% gene1,]
  dat$gene <- rownames(dat)
  dat.m <- melt(data = dat, id.vars = 'gene')
  dat.c <- dcast(data = dat.m, formula = variable~gene, value.var = 'value')
  colnames(dat.c)[1] = "sample"
  
  # modify gene name, dashes present
  gene1.mut <- paste('`',gene1,'`',sep='')
  
  # datatype
  if(log == FALSE){
    y.axis <- 'Log2 (FPKM)'
  } else {
    dat.c[,gene1] <- log2(dat.c[,gene1]+1)
    y.axis <- 'FPKM'
  }
  
  # add phenotype data - MYCN status
  dat.c <- merge(dat.c, phenotype, by = 'sample', all.x = TRUE)
  
  # subset if tumor is defined
  dat.c <- dat.c[which(dat.c$tumor_subtype == tumor),]
  
  # sorting of bars
  if(sortby == "PDX"){
    dat.c$sample <- factor(x = dat.c$sample, levels = sort(as.character(dat.c$sample)))
  } else if(sortby == "Gene"){
    dat.c$sample <- reorder(dat.c$sample, dat.c[,gene1])
  }
  
  # ggplot 
  p <- ggplot(dat.c, aes_string(x='sample', y=gene1.mut, fill = 'tumor_subtype')) + guides(fill=FALSE) + 
      geom_bar(stat="identity") + customtheme + theme(axis.text.x  = element_text(angle=45), 
                                                      plot.margin = unit(c(0.5, 0.5, 2, 0.5), "cm")) + 
      ggtitle(gene1)
  p <- plotly_build(p)
  p$x$layout$yaxis$title <- y.axis
  
  return(p)
}
