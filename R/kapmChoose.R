#####################################################
# plot kaplan meier plot for set of genes per dataset 
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#####################################################

source('R/kapmPlot.R')
load('data/allDataPatient3.RData')

kapmChoose <- function(dataset, genes, endpoint="os")
{
  if(dataset=="NB88" & endpoint=="os")
  {
    out <- kapmPlot(genes, NB88_All, createPlot=T, tVar="nti_surv_overall", eVar="nti_event_overall_num")
  }
  if(dataset=="IH250" & endpoint=="os")
  {
    out <- kapmPlot(genes, IH250_All, createPlot=T, tVar="stime", eVar="scens")
  }
  if(dataset=="NB88" & endpoint=="efs")
  {
    out <- kapmPlot(genes, NB88_All, createPlot=T, tVar="nti_surv_progrfree", eVar="nti_event_progrfree_num")
  }
  if(dataset=="IH250" & endpoint=="efs")
  {
    out <- kapmPlot(genes, IH250_All, createPlot=T, tVar="efstime", eVar="efscens")
  }
  
  out <- ggplotly(out)
  return(out)
}
