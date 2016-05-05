library(shiny)
library(reshape2)
library(ggplot2)
library(Rgraphviz)
library(DT)
library(shinydashboard)
library(shinyIncubator)

options(shiny.maxRequestSize = 30*1024^2)

shinyServer(function(input, output, session){
  
  datasetInput <-  reactive({
    file = paste('data/','2016-04-27-CellLineSTARcounts_2pass_matrix.txt',sep = '')
    dat = read.delim(file = file)
  })
  
  # output initial data in tab1
  output$table <- DT::renderDataTable({
    DT::datatable(datasetInput(),
                  extensions = c('ColVis','Scroller'), 
                  options = list(dom = 'RMDCT<"clear">lfrtip', 
                                 searchHighlight = TRUE,
                                 initComplete = JS("function(settings, json) {",
                                                   "$(this.api().table().header()).css({'background-color': '#005ab3', 'color': '#fff'});",
                                                   "}"),
                                 pageLength = 5,
                                 lengthMenu = list(c(5, 10, 15, 20, 25, -1), c('5', '10', '15', '20', '25', 'All')),
                                 scrollX = TRUE),
                  selection = 'single')
  })
  
  # corrplot begins
  corrplot <- function(){
    # load initial dataset
    dat <- datasetInput()
    dat$gene <- rownames(dat)
    dat.m <- melt(data = dat, id.vars = 'gene')
    dat.c <- dcast(data = dat.m, formula = variable~gene, value.var = 'value')
    
    # get gene names
    gene1 <- as.character(input$gene1)
    gene2 <- as.character(input$gene2)
    
    # compute correlation and plot
    cor.val <- round(cor(dat.c[,gene1], dat.c[,gene2]),3)
    p <- ggplot(data = dat.c, aes_string(x = gene1, y = gene2, label = 'variable')) + geom_point() + geom_smooth(method = lm) +
      geom_text(vjust=-1) + ggtitle(paste("Pearson:",cor.val)) + xlab(label = gene1) + ylab(label = gene2)
    p
  } # corrplot ends
  
  # output correlation plot
  output$plot1 <- renderPlot({
    if(input$submit1){
      isolate({
        corrplot()
      })
    }
  })

}) # shinyServer ends
  
 