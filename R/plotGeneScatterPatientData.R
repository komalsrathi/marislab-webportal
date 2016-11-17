####################################
# plot scatter plot of 2 genes
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
####################################

# plot scatter plot of 2 genes##################
plotGeneScatterPatientData <- function(datatype, gene1, gene2, myDataExp, myDataAnn, log, colorby, correlation, customtheme)
{

  # get expression and annotation of the selected dataset

	myDataExp$gene <- rownames(myDataExp)
	myDataExp.m <- melt(data = myDataExp, id.vars = 'gene')
	myDataExp.c <- dcast(data = myDataExp.m, formula = variable~gene, value.var = 'value')
	colnames(myDataExp.c)[1] = "Sample"
	
	#For title correlation and p-value
	cor <- cor.test(myDataExp.c[,gene1], myDataExp.c[,gene2], method = correlation)
	if(cor$p.value==0){
	  cor.pval <- '< 2.2e-16'
	}
	if(cor$p.value>0){
	  cor.pval <- format(cor$p.value, scientific = T, digits=3)
	}
	if(cor$estimate==1){
	  cor.est <- 1
	}
	if(cor$estimate!=1){
	  cor.est <- format(cor$estimate, scientific = T, digits=3)
	}
	cor.title <- paste("Cor = ", cor.est, " | P-Val = ", cor.pval, sep="")

	# modify gene name, dashes present
	gene1.mut <- paste('`',gene1,'`',sep = '')
	gene2.mut <- paste('`',gene2,'`',sep = '')
	
	# plot log values? 
	if(length(grep('FPKM',datatype))==0)
	{
	  if(log==FALSE)
	  {
	    y.axis <- "RMA"
	    myDataExp.tmp <- myDataExp.c[,-1]
	    myDataExp.tmp <- as.data.frame(2^myDataExp.tmp)
	    myDataExp.tmp <- cbind(Sample=myDataExp.c$Sample, myDataExp.tmp)
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
	    myDataExp.tmp <- as.data.frame(apply(myDataExp.c[,-1], MARGIN = 2, function(x) log2(x+1)))
	    myDataExp.tmp$Sample <- myDataExp.c$Sample
	    myDataExp.c <- myDataExp.tmp
	  }
	}
	
	# add annotation data to expression set
	myDataExp.c <- merge(myDataExp.c, myDataAnn, by.x="Sample",by.y='row.names')
	
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
	
	# plot
	if(colorby == "None"){
	  p <- ggplot(data = myDataExp.c, aes_string(x = gene1.mut, y = gene2.mut, label = 'Sample')) + 
	    geom_point() + geom_smooth(method = lm) + customtheme + ggtitle(label = cor.title)
	}
	if(colorby != "None"){
	  p <- ggplot(data = myDataExp.c, aes_string(x = gene1.mut, y = gene2.mut, label = 'Sample', color = colorby)) + 
	    geom_point() + geom_smooth(method = lm) + customtheme + ggtitle(label = cor.title)
	}
	
	p <- plotly_build(p)
	p$x$layout$yaxis$title <- paste0(gene2,' (', y.axis,')')
	p$x$layout$xaxis$title <- paste0(gene1,' (', y.axis,')')
	
	return(p)
} 