#################################################################
# Function to parse out important meta-data and reform data frame
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#################################################################

parseGeneInfo <- function(x)
{
	m <- x[3]
	spl <- strsplit(m, "//")
	spl <- spl[[1]]
	output <- c(x[1], NA, NA, NA, NA, NA)
	if(length(spl)>4)
	{
		output <- c(x[1], gsub(" ", "",spl[1]), gsub(" ", "",spl[2]), gsub(" ", "",spl[3]), gsub(" ", "",spl[4]), gsub(" ", "",spl[5]))
	}
	return(output)
}