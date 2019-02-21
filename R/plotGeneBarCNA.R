#####################################
# Cell line copy number code
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#####################################

plotGeneBarCNA <- function(gene1, dat, customtheme, sortby, phenotype, colorby, logby)
{
	
  dat <- dat[rownames(dat) %in% gene1,]
	dat$gene <- rownames(dat)
	dat.m <- melt(data = dat, id.vars = 'gene')
	dat.c <- dcast(data = dat.m, formula = variable~gene, value.var = 'value')
	colnames(dat.c)[1] <- "Cell_Line"

	gene1.mut <- paste('`', gene1, '`', sep = "")
  
	# add phenotype data - MYCN status
	dat.c <- merge(dat.c, phenotype, by.x = 'Cell_Line', by.y = 'CellLine', all.x = TRUE)
	
	# sorting of bars
	if(sortby == "CellLine"){
	  dat.c$Cell_Line <- factor(x = dat.c$Cell_Line, levels = sort(as.character(dat.c$Cell_Line)))
	}
	if(sortby == "Gene"){
	  dat.c$Cell_Line <- reorder(dat.c$Cell_Line,dat.c[,gene1])
	}
	if(sortby == "MYCN_Status"){
	  dat.c$Cell_Line <- reorder(dat.c$Cell_Line, as.numeric(dat.c$MYCN_Status))
	}
	
	# log data where possible
	if(logby == TRUE){
	  dat.c[,gene1] <- log2(dat.c[,gene1]-1)
	}
	
	# plot
	if(colorby != "None"){
	  p <- ggplot(dat.c, aes_string(x='Cell_Line', y=gene1.mut, fill = colorby)) + 
	    geom_bar(stat="identity") + customtheme + theme(axis.text.x  = element_text(angle=45), plot.margin = unit(c(0.5, 0.5, 2, 0.5), "cm")) + 
	    ggtitle(gene1)
	}
	if(colorby == "None"){
	  p <- ggplot(dat.c, aes_string(x='Cell_Line', y=gene1.mut)) + 
	    geom_bar(stat="identity") + customtheme + theme(axis.text.x  = element_text(angle=45), plot.margin = unit(c(0.5, 0.5, 2, 0.5), "cm")) + 
	    ggtitle(gene1)
	}

	# ggplotly
  p <- plotly_build(p)
  p$x$layout$yaxis$title <- "Copy Number"

	return(p)

}