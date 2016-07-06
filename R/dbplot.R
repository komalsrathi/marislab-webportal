dbplot <- function(data_summary){
  data_summary <- data_summary[order(data_summary$Type, data_summary$Source, data_summary$Genes),]
  data_summary$Source <- factor(data_summary$Source, levels = as.character(data_summary$Source))
  p <- plotly_build(ggplot(data = data_summary, aes(x = Source, y = Genes)) +
                      geom_bar(stat="identity",aes(color = Type)) +
                      theme_bw() + geom_text(aes(label = Samples, color = Type), size=5) +
                      theme_hc(bgcolor = "darkunica") +
                      scale_colour_hc("darkunica") +
                      theme(axis.title = element_blank(),
                            axis.ticks = element_blank(),
                            axis.text.x = element_text(angle = -45, hjust = 0),
                            panel.grid = element_blank(),
                            plot.margin = unit(c(0.5, 0.5, 2, 0.5), "cm")))
  p$layout$plot_bgcolor <- p$layout$paper_bgcolor
  p$layout$xaxis$tickfont$size <- 16
  p$layout$yaxis$tickfont$size <- 16
  p$layout$annotations[[1]]$text <- ""
  return(p)
}
