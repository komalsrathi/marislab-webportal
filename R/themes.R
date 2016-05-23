####################################
# various ggplot2 themes
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
####################################

themebw <- function(){

	tt <- theme_bw() + theme(plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm"))
	return(tt)
}
