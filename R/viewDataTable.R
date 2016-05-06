viewDataTable <- function(dat){
  dt <- DT::datatable(dat,
                extensions = c('TableTools', 'ColVis','Scroller'), 
                options = list(dom = 'RMDCT<"clear">lfrtip', 
                               searchHighlight = TRUE,
                               initComplete = JS("function(settings, json) {",
                                                 "$(this.api().table().header()).css({'background-color': '#005ab3', 'color': '#fff'});",
                                                 "}"),
                               tableTools = list(sSwfPath = '//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls_pdf.swf'),
                               pageLength = 5,
                               lengthMenu = list(c(5, 10, 15, 20, 25, -1), c('5', '10', '15', '20', '25', 'All')),
                               scrollX = TRUE),
                selection = 'single')
  return(dt)
}