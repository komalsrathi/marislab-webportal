####################################
# various ggplot2 themes
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
####################################

themebw <- function(){

	tt <- theme_bw() + theme(axis.title.x = element_text(face = "bold", size = 16),
                           axis.text.x  = element_text(size = 14),
                           axis.title.y = element_text(face = "bold", size = 16),
                           axis.text.y  = element_text(size = 14),
                           title = element_text(face = 'bold',size = 16))
	return(tt)
}
