dbplot <- function(data_summary){
  data_summary <- data_summary[order(data_summary$Type, data_summary$Samples),]
  data_summary$Dataset <- factor(data_summary$Dataset, levels = as.character(data_summary$Dataset))
  p <- plotly_build(ggplot(data = data_summary, aes(x = Dataset, y = Samples)) +
                      geom_bar(stat="identity",aes(colour = Type, line = Source)) +
                      theme_bw() +
                      theme_hc(bgcolor = "darkunica") +
                      scale_colour_hc("darkunica") +
                      theme(axis.title = element_blank(),
                            axis.ticks = element_blank(),
                            axis.text.x = element_text(angle = -45, hjust = 0),
                            panel.grid = element_blank(),
                            plot.margin = unit(c(0.5, 0.5, 2.5, 0.5), "cm")))
  p$layout$plot_bgcolor <- p$layout$paper_bgcolor
  p$layout$xaxis$tickfont$size <- 16
  p$layout$yaxis$tickfont$size <- 16
  p$layout$annotations[[1]]$text <- ""
  return(p)
}
