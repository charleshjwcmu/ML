# 
###############################################################################

require(xts)

formatPercentages <- function(numbers) {
	#function: convert a number to percentage.
	#parameter: number should be numeric
	#return: a character like "XX%"
	
	paste0(numbers*100,"%")
}

cleanString <- function(string){
#	string <- model
	#function: clean a string to replace special characters with underscore(_).
	#parameter: a string
	#return: a cleaned string with underscores.
	
	
	string <- gsub("[.]","_",string)
	string <- gsub("&","_",string)
	string <- gsub(" ","_",string)
	string <- gsub(":","_",string)
#	string <- gsub("&","_",string)
	return(string)
}

