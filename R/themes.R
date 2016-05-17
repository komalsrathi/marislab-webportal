####################################
# various ggplot2 themes
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
####################################

themebw <- function(){

	tt <- theme_bw() + theme(plot.margin = unit(c(2, 2, 2, 2), "cm"))
	return(tt)
}
