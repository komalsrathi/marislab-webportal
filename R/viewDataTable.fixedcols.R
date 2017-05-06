####################################
# add data table options
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
####################################

viewDataTable.fixedcols <- function(dat){
  DT::datatable(dat,
                rownames = FALSE,
                extensions = c('FixedColumns', 'Buttons'),
                selection = "single",
                filter = "bottom",
                options = list(
                  dom = 'Bfrtip',
                  buttons = list('colvis','pageLength', 'copy','print',
                                 list(extend = "collection",
                                      buttons = c('csv', 'excel', 'pdf'),
                                      text = 'Download'
                  )),
                  searchHighlight = TRUE,
                  lengthMenu = list(c(5, 10, 15, 20, 25, -1), c('5', '10', '15', '20', '25', 'All')),
                  initComplete = JS("function(settings, json) {",
                                    "$(this.api().table().header()).css({'background-color': '#005ab3', 'color': '#fff'});",
                                    "}"),
                  scrollX = TRUE,
                  fixedColumns = list(leftColumns = 3)
                ), class = 'nowrap display')
}