####################################
# add data table options
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
####################################

viewDataTable.fixedcols <- function(dat){
  DT::datatable(dat,
                extensions = c('FixedColumns','Scroller'),
                options = list(
                  dom = 'RMDCT<"clear">lfrtip',
                  searchHighlight = TRUE,
                  tableTools = list(sSwfPath = '//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf'),
                  pageLength = 5,
                  lengthMenu = list(c(5, 10, 15, 20, 25, -1), c('5', '10', '15', '20', '25', 'All')),
                  initComplete = JS("function(settings, json) {",
                                    "$(this.api().table().header()).css({'background-color': '#005ab3', 'color': '#fff'});",
                                    "}"),
                  scrollX = TRUE,
                  fixedColumns = list(leftColumns = 3)
                ), rownames = FALSE)
}