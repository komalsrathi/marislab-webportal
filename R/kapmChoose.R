#####################################################
# plot kaplan meier plot for set of genes per dataset 
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#####################################################

source('R/kapmPlot.R')

kapmChoose <- function(datatype, myDataExp, myDataAnn, risk, genes, endpoint)
{
  if(endpoint == "os") {
    tVar <- 'nti_surv_overall'
    eVar <- 'nti_event_overall_num'
  } else {
    tVar <- 'nti_surv_progrfree'
    eVar <- 'nti_event_progrfree_num'
  }
  out <- kapmPlot(genes, myDataExp, myDataAnn, risk, createPlot = T, tVar = tVar, eVar = eVar)
  return(out)
}
