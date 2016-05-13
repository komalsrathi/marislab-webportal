##############################
# Create Targets from 2 lists
# Author: Pichai Raman
# Organization: DBHi, CHOP
##############################

createTargets <- function(list1, list2)
{
  
  class <- c(rep("SET1", length(list1)), rep("SET2", length(list2)))
  mydf <- data.frame(c(list1, list2), class)
  rownames(mydf) <- mydf[,1]
  mydf <- mydf[-1]
  return(mydf)
}