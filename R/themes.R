####################################
# various ggplot2 themes
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
####################################

themebw <- function(){

	tt <- theme_bw() 
# 	+ theme(axis.title.x = element_text(face = "bold", size = 14),
#                            axis.text.x  = element_text(size = 12),
#                            axis.title.y = element_text(face = "bold", size = 14),
#                            axis.text.y  = element_text(size = 12),
#                            title = element_text(face = 'bold',size = 14))
	return(tt)
}
