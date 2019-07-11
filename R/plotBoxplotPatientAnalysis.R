#############################################
# plot box plot for a gene per dataset 
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#############################################

plotBoxplotPatientAnalysis <- function(datatype, gene1, colorby, myDataExp, myDataAnn, log, customtheme)
{
  # get expression and annotation of the selected dataset
  # modify dataframe
  myDataExp <- myDataExp[rownames(myDataExp) %in% gene1,]
  myDataExp$gene <- rownames(myDataExp)
  myDataExp.m <- melt(data = myDataExp, id.vars = 'gene')
  myDataExp.c <- dcast(data = myDataExp.m, formula = variable~gene, value.var = 'value')
  colnames(myDataExp.c)[1] = "Patient_Sample"
  
  # plot log values
  if(length(grep('FPKM',datatype))==0){
    if(log==FALSE){
      y.axis <- "RMA"
      myDataExp.c[,gene1] <- 2^(myDataExp.c[,gene1])
    }
    if(log==TRUE){
      y.axis <- "log2(RMA)"
    }
  }
  if(length(grep('FPKM',datatype))==1){
    if(log==FALSE){
      y.axis <- "FPKM"
    }
    if(log==TRUE){
      y.axis <- "log2(FPKM)"
      myDataExp.c[,gene1] <- log2(myDataExp.c[,gene1]+1)
    }
  }
  
  # add annotation data to expression set
  myDataExp.c <- merge(myDataExp.c, myDataAnn, by.x="Patient_Sample", by.y='row.names')
  
  # eliminate confusion between MYCN gene and status
  # if(length(grep('MYCN',colnames(myDataExp.c)))>1){
  #   coln <- grep("MYCN.x", colnames(myDataExp.c))
  #   colnames(myDataExp.c)[coln] <- 'MYCN'
  #   coln <- grep("MYCN.y", colnames(myDataExp.c))
  #   colnames(myDataExp.c)[coln] <- 'MYCNS'
  #   if(colorby == "MYCN"){
  #     colorby <- "MYCNS"
  #   }
  # }
  
  # modify gene name, dashes present
  gene1.mut <- paste('`',gene1,'`',sep='')
  
  # change colorby to factor
  if(colorby != 'None'){
    myDataExp.c[,colorby] <- as.factor(myDataExp.c[,colorby])
    if(length(levels(myDataExp.c[,colorby])) > 2){
      method = "anova"
    } else {
      method = "t.test"
    }
  } else {
    method <- "t.test"
  }
  
  p <- ggplot(myDataExp.c, aes_string(x=colorby, y=gene1.mut)) + 
    stat_boxplot(geom ='errorbar', width = 0.2) +
    geom_boxplot(lwd = 0.5, fatten = 0.7, outlier.shape = 1, width = 0.5, outlier.size = 1, aes_string(fill = colorby)) +
    geom_jitter(width = 0.1, pch = 21, stroke = 0.2, aes_string(fill = colorby)) +
    customtheme + 
    theme(axis.text.x = element_blank()) +
    ggtitle(gene1) +
    stat_compare_means(method = method, label.x.npc = "center", label.y.npc = "top", color = "red") + 
    stat_compare_means(label = "p.signif", ref.group = ".all.", color = "red", hide.ns = TRUE)
  
  p <- plotly_build(p)
  p$x$layout$yaxis$title <- y.axis
  p$x$layout$xaxis$title <- ""
  
  # remove outliers
  p$x$data[1:length(levels(myDataExp.c[,colorby])) + 1] <- lapply(p$x$data[1:length(levels(myDataExp.c[,colorby])) + 1], FUN = function(x){
    x$marker = list(opacity = 0)
    return(x)
  })
  
  # if(colorby != 'None'){
  #   if(length(levels(myDataExp.c[,colorby]))>1){
  #     anovaRes <- aov(lm(myDataExp.c[,gene1]~myDataExp.c[,colorby]))
  #     pval <- summary(anovaRes)[[1]][[5]][1]
  #     pval <- format(pval, scientific = T, digits = 3)
  #     myText <- paste("Anova P-Val=", pval, sep="")
  #     p <- ggplot(myDataExp.c, aes_string(x=colorby, y=gene1.mut, fill=colorby)) + 
  #       geom_boxplot() + customtheme + ggtitle(paste0(gene1,'\n',myText)) + 
  #       theme(legend.position = "none",
  #             axis.text.x = element_text(size = 12),
  #             axis.text.y = element_text(size = 12),
  #             axis.title.x = element_text(size = 12),
  #             axis.title.y = element_text(size = 12))
  #   }
  #   if(length(levels(myDataExp.c[,colorby]))==1){
  #     p <- ggplot(myDataExp.c, aes(x=colorby, y=gene1.mut, fill=colorby)) + 
  #       customtheme + geom_boxplot() + 
  #       theme(legend.position = "none",
  #             axis.text.x = element_text(size = 12),
  #             axis.text.y = element_text(size = 12),
  #             axis.title.x = element_text(size = 12),
  #             axis.title.y = element_text(size = 12))
  #   }
  # } else {
  #   tmp <- data.frame(gene = myDataExp.c[,gene1])
  #   p <- ggplot(tmp, aes(x='', y=gene, fill='')) + 
  #     customtheme + geom_boxplot() + ylab(gene1) + xlab('') +
  #     theme(legend.position = "none",
  #           axis.text.x = element_text(size = 12),
  #           axis.text.y = element_text(size = 12),
  #           axis.title.x = element_text(size = 12),
  #           axis.title.y = element_text(size = 12))
  # }
  
  # p <- plotly_build(p)
  # p$x$layout$yaxis$title <- y.axis
  
  return(p)
  
}