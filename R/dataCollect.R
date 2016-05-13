#################################################################
# Function to get data for selected project
# Authors: Pichai Raman, Komal Rathi
# Organization: DBHi, CHOP
#################################################################


dataCollect <- function(filename){
    project <- paste('data/', 'db.txt', sep = "")
    project <- read.csv(file = project, stringsAsFactors = F)
    project <- project[grep(filename, project$Project),]
    project <- as.character(project$Filename)
    project <- paste('data/', project, sep = "")
    return(project)
  }