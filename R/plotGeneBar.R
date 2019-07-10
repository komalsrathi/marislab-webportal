####################################
# plot bar plot for a gene
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
####################################

plotGeneBar <- function(datatype, dat, phenotype, gene1, log, customtheme, sortby, colorby)
{
  
  # load initial dataset and subset by gene
  dat <- dat[rownames(dat) %in% gene1,]
  dat$gene <- rownames(dat)
  dat.m <- melt(data = dat, id.vars = 'gene')
  dat.c <- dcast(data = dat.m, formula = variable~gene, value.var = 'value')
  colnames(dat.c)[1] = "Cell_Line"
  
  # modify gene name, dashes present
  gene1.mut <- paste('`',gene1,'`',sep='')
  
  # datatype
  if(length(grep('RMA',datatype))==1) {
    y.axis <- 'RMA'
    if(log == FALSE) {
      y.axis <- y.axis
      dat.c[,gene1] <- 2^(dat.c[,gene1])
    } else {
      y.axis <- paste0('Log2','(',y.axis,')')
    }
  } else if(length(grep('FPKM',datatype)) == 1) {
    y.axis <- 'FPKM'
    if(log == FALSE) {
      y.axis <- y.axis
    } else {
      y.axis <- paste0('Log2','(',y.axis,')')
      dat.c[,gene1] <- log2(dat.c[,gene1]+1)
    }
  } else if(length(grep('TPM', datatype)) == 1) {
    y.axis <- 'TPM'
    if(log == FALSE) {
      y.axis <- y.axis
    } else {
      y.axis <- paste0('Log2','(',y.axis,')')
      dat.c[,gene1] <- log2(dat.c[,gene1]+1)
    }
  }
  
  # add phenotype data - MYCN status
  dat.c <- merge(dat.c, phenotype, by.x = 'Cell_Line', by.y = 'CellLine', all.x = TRUE)
  
  # sorting of bars
  if(sortby == "CellLine") {
    dat.c$Cell_Line <- factor(x = dat.c$Cell_Line, levels = sort(as.character(dat.c$Cell_Line)))
  } else if(sortby == "Gene") {
    dat.c$Cell_Line <- reorder(dat.c$Cell_Line, dat.c[,gene1])
  } else if(sortby == "MYCN_Status") {
    dat.c$Cell_Line <- reorder(dat.c$Cell_Line, as.numeric(as.factor(dat.c$MYCN_Status)))
  }
  
  # plot 
  if(colorby != "None"){
    p <- ggplot(dat.c, aes_string(x = 'Cell_Line', y = gene1.mut, fill = colorby)) + 
      geom_bar(stat = "identity", color = 'black', size = 0.2) + customtheme + xlab('') +
      ggtitle(gene1)
  } else if(colorby == "None"){
    p <- ggplot(dat.c, aes_string(x = 'Cell_Line', y = gene1.mut)) + 
      geom_bar(stat = "identity", color = 'black', fill = 'gray', size = 0.2) + customtheme + xlab('') +
      ggtitle(gene1)
  }
  
  p <- plotly_build(p)
  p$x$layout$yaxis$title <- y.axis
  
  return(p)
  
}
