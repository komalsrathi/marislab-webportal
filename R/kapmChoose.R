#####################################################
# plot kaplan meier plot for set of genes per dataset 
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#####################################################

source('R/kapmPlot.R')

kapmChoose <- function(datatype, dataset, genes, endpoint="os")
{
  if(datatype=="NB88" & endpoint=="os")
  {
    out <- kapmPlot(genes, dataset, createPlot=T, tVar="nti_surv_overall", eVar="nti_event_overall_num")
  }
  if(datatype=="IH250" & endpoint=="os")
  {
    out <- kapmPlot(genes, dataset, createPlot=T, tVar="stime", eVar="scens")
  }
  if(datatype=="NB88" & endpoint=="efs")
  {
    out <- kapmPlot(genes, dataset, createPlot=T, tVar="nti_surv_progrfree", eVar="nti_event_progrfree_num")
  }
  if(datatype=="IH250" & endpoint=="efs")
  {
    out <- kapmPlot(genes, dataset, createPlot=T, tVar="efstime", eVar="efscens")
  }
  
  out <- ggplotly(out)
  return(out)
}
