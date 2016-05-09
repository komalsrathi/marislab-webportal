####################################
# plot scatter plot of 2 genes
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
####################################

# plotGeneScatter begins
  plotGeneScatter <- function(dat, gene1, gene2, log=T, customtheme){
    
    # load initial dataset
    dat$gene <- rownames(dat)
    dat.m <- melt(data = dat, id.vars = 'gene')
    dat.c <- dcast(data = dat.m, formula = variable~gene, value.var = 'value')
    
    # compute correlation
    cor <- cor.test(dat.c[,gene1], dat.c[,gene2])
    cor.pval <- round(cor$p.val,2)
    cor.est <- round(cor$estimate,2)
    cor.title <- paste("Cor = ", cor.est, " | P-Val = ", cor.pval, sep="")
    
    # modify gene name, dashes present
    gene1.mut <- paste('`',gene1,'`',sep = '')
    gene2.mut <- paste('`',gene2,'`',sep = '')
    
    # plot log values? 
    if(log==F)
    {
      dat.c <- 2^dat.c
    }
    
    # plot
    p <- ggplot(data = dat.c, aes_string(x = gene1.mut, y = gene2.mut, label = 'variable')) + geom_point(size = 3) + geom_smooth(method = lm) + customtheme +
      geom_text(vjust=-1.5, size = 4) + ggtitle(label = cor.title) + xlab(label = gene1) + ylab(label = gene2)
    
    return(p)
  } # plotGeneScatter ends