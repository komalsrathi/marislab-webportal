####################################
# plot scatter plot of 2 genes
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
####################################

# plotGeneScatter begins
  plotGeneScatter <- function(dat, gene1, gene2, log=T, customtheme, correlation){
    
    # load initial dataset
    dat$gene <- rownames(dat)
    dat.m <- melt(data = dat, id.vars = 'gene')
    dat.c <- dcast(data = dat.m, formula = variable~gene, value.var = 'value')
    
    # compute correlation
    cor <- cor.test(dat.c[,gene1], dat.c[,gene2], method = correlation)
    if(cor$p.value==0){
      cor.pval <- '< 2.2e-16'
    }
    if(cor$p.value>0){
      cor.pval <- format(cor$p.value, scientific = T)
    }
    if(cor$estimate==1){
      cor.est <- 1
    }
    if(cor$estimate!=1){
      cor.est <- format(cor$estimate, scientific = T)
    }
    cor.title <- paste("Cor = ", cor.est, " | P-Val = ", cor.pval, sep="")
    
    # modify gene name, dashes present
    gene1.mut <- paste('`',gene1,'`',sep = '')
    gene2.mut <- paste('`',gene2,'`',sep = '')
    
    # plot log values? 
    if(log==FALSE)
    {
      dat.tmp <- dat.c[,-1]
      dat.tmp <- as.data.frame(2^dat.tmp)
      dat.tmp <- cbind(variable=dat.c$variable, dat.tmp)
      dat.c <- dat.tmp
    }
    
    # ggplot
    # p <- ggplot(data = dat.c, aes_string(x = gene1.mut, y = gene2.mut)) + 
    #   geom_point(size = 2) + geom_smooth(method = lm) + customtheme +
    #   geom_text(aes(label = variable), vjust=-1.5, size = 3) + ggtitle(label = cor.title)
    
    # ggplotly
    # p <- ggplotly(p + ylab(" ") + xlab(" "))
    # 
    # x <- list(
    #   title = gene1
    # )
    # y <- list(
    #   title = gene2
    # )
    # p <- p %>% layout(xaxis = x, yaxis = y)
  
    p <- ggplot(data = dat.c, aes_string(x = gene1.mut, y = gene2.mut)) + 
      geom_point() + geom_smooth(method = lm) + customtheme + ggtitle(label = cor.title)
    
    p <- plotly_build(p)
    p$data[[1]]$text <- dat.c$variable
    p$data[[1]]$hoverinfo <- "x+y"
    p$data[[1]]$mode <- "markers+text"
    p$data[[1]]$textposition <- "top center"
    p$data[[1]]$marker$size <- 4
    p$data[[1]]$marker$color <- "rgb(220,20,60)"
    p$data[[1]]$marker$line$color <- "rgb(220,20,60)"
    p$layout$font$size <- 12
    
    return(p)
  } # plotGeneScatter ends