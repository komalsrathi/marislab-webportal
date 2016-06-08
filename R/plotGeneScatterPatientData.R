####################################
# plot scatter plot of 2 genes
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
####################################

# load patient sample set
load('data/allDataPatient.RData')

# plot scatter plot of 2 genes##################
plotGeneScatterPatientData <- function(gene1, gene2, dataset, log, colorby, correlation, customtheme)
{

	# get selected dataset
	myData <- paste(dataset,'_All',sep='')
	myData <- get(myData)

	# get expression and annotation of the selected dataset
	myDataExp <- myData[[1]]
	myDataAnn <- myData[[2]]
	
	myDataExp$gene <- rownames(myDataExp)
	myDataExp.m <- melt(data = myDataExp, id.vars = 'gene')
	myDataExp.c <- dcast(data = myDataExp.m, formula = variable~gene, value.var = 'value')
	
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
	if(log==FALSE)
	{
	  myDataExp.tmp <- myDataExp.c[,-1]
	  myDataExp.tmp <- as.data.frame(2^myDataExp.tmp)
	  myDataExp.tmp <- cbind(variable=myDataExp.c$variable, myDataExp.tmp)
	  myDataExp.c <- myDataExp.tmp
	}
  
	# add annotation data to expression set
	myDataExp.c <- merge(myDataExp.c, myDataAnn, by.x="variable",by.y='row.names')
	
	# eliminate confusion between MYCN gene and status
	coln <- grep("MYCN.x", colnames(myDataExp.c))
	colnames(myDataExp.c)[coln] <- 'MYCN'
	coln <- grep("MYCN.y", colnames(myDataExp.c))
	colnames(myDataExp.c)[coln] <- 'MYCNS'
	if(colorby=="MYCN")
	{
	  colorby = "MYCNS"
	}
	
	# plot
	if(colorby == "None"){
	  p <- ggplot(data = myDataExp.c, aes_string(x = gene1.mut, y = gene2.mut)) + 
	    geom_point() + geom_smooth(method = lm) + customtheme + ggtitle(label = cor.title)
	}
	if(colorby != "None"){
	  p <- ggplot(data = myDataExp.c, aes_string(x = gene1.mut, y = gene2.mut)) + 
	    geom_point(aes_string(color = colorby)) + geom_smooth(method = lm) + customtheme + ggtitle(label = cor.title)
	}
	p <- plotly_build(p)
	return(p)
} 