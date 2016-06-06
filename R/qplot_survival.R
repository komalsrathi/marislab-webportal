#####################################################
# plot kaplan meier plot for set of genes per dataset 
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#####################################################

qplot_survival <- function(f.frame, f.CI="default", f.shape=3, myTitle){
  
  # use different plotting commands dependig whether or not strata's are given
  if("strata" %in% names(f.frame) == FALSE){
    # confidence intervals are drawn if not specified otherwise
    if(f.CI=="default" | f.CI==TRUE ){
      # create plot with 4 layers (first 3 layers only events, last layer only censored)
      # hint: censoring data for multiple censoring events at timepoint are overplotted
      # (unlike in plot.survfit in survival package)
      ggplot(data=f.frame) + geom_step(aes(x=time, y=surv), direction="hv") + 
        geom_step(aes(x=time, y=upper), directions="hv", linetype=2) + geom_step(aes(x=time,y=lower), direction="hv", linetype=2) +
        geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape)+scale_y_continuous(limits = c(0, 1))
    }
    else {
      # create plot without confidence intervalls
      ggplot(data=f.frame) + geom_step(aes(x=time, y=surv), direction="hv") + 
        geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape) + scale_y_continuous(limits = c(0, 1))
      
    }
  }
  else {
    if(f.CI=="default" | f.CI==FALSE){
      # without CI
      ggplot(data=f.frame, aes(group=strata, colour=strata, shape=strata)) + geom_step(aes(x=time, y=surv), direction="hv") + 
        geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape) + ggtitle(myTitle) + scale_y_continuous(limits = c(0, 1));
    }
    else {
      # with CI (hint: use alpha for CI)
      ggplot(data=f.frame, aes(colour=strata, group=strata)) + geom_step(aes(x=time, y=surv), direction="hv") + 
        geom_step(aes(x=time, y=upper), directions="hv", linetype=2, alpha=0.5) +
        geom_step(aes(x=time,y=lower), direction="hv", linetype=2, alpha=0.5) +
        geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape)+scale_y_continuous(limits = c(0, 1))
    }
  }
}