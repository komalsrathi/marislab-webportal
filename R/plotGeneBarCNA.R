#####################################
# Cell line copy number code
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#####################################

plotGeneBarCNA <- function(gene1, dat, customtheme, sortby, phenotype, colorby)
{
	
	dat$gene <- rownames(dat)
	dat.m <- melt(data = dat, id.vars = 'gene')
	dat.c <- dcast(data = dat.m, formula = variable~gene, value.var = 'value')
	colnames(dat.c)[1] <- "Cell_Line"

	gene1.mut <- paste('`', gene1, '`', sep = "")
  
	# reorder labels
	idx <- sort(as.character(dat.c$Cell_Line), index=T)$ix
	dat.c$Cell_Line <- factor(dat.c$Cell_Line, levels = dat.c[idx,"Cell_Line"])
  
	# add phenotype data - MYCN status
	dat.c <- merge(dat.c, phenotype, by.x = 'Cell_Line', by.y = 'CellLine', all.x = TRUE)
	
	# sorting of bars
	if(sortby == "Gene"){
	  dat.c$Cell_Line <- reorder(dat.c$Cell_Line,dat.c[,gene1])
	}
	if(sortby == "MYCN_Status"){
	  dat.c$Cell_Line <- reorder(dat.c$Cell_Line, as.numeric(dat.c$MYCN_Status))
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
  p$layout$annotations[[1]]$text <- ""
  p$layout$yaxis$title <- "Copy Number"

	return(p)

}