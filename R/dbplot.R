dbplot <- function(){
  dat <- get(load("data/data_summary.RData"))
  p <- plotly_build(ggplot(data = dat, aes(x = Source, y = Genes)) + 
                      geom_bar(stat="identity",aes(color = Type)) + 
                      theme_bw() + geom_text(aes(label = Samples, color = Type), size=5) + 
                      theme_hc(bgcolor = "darkunica") +
                      scale_colour_hc("darkunica") +
                      theme(axis.title = element_blank(), 
                            axis.text.y = element_blank(), axis.ticks = element_blank(), 
                            panel.grid = element_blank()))
  p$layout$plot_bgcolor <- p$layout$paper_bgcolor
  p$layout$annotations[[1]]$text <- ""
  return(p)
}
