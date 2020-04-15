# 
# 
###############################################################################

characterizeTable <- function(table) {
	#function: convert all columns of a dataframe to character type.
	#parameter: table is a dataframe
	#return: a transformed data frame
	return(data.frame(lapply(table, as.character), stringsAsFactors=FALSE))
}

tableFilter <- function(table, columnname, value) {
#	table <- I_MOD_CSI_AC_prev
#	columnname <- "version"
#	value <- current_version
	tmp <- sum(grepl(columnname,colnames(table),ignore.case=TRUE))
	if (tmp==1) {
		table <- table[table[,grep(columnname,colnames(table),ignore.case=TRUE)]==value,]
	} else if (tmp == 0){
		print("warning: column name not found therefore no filter")
	} else if (tmp == 2){
		stop("warning: Multiple column names match. Please respecify")
	}
	
	return(table)
}

versionDataFrame <- function(table, version) {
	return(data.frame(VERSION=version,TimeStamp = gsub(":","-",Sys.time()),table))
}

mean_na_ignored <- function(data) {
	mean(data,na.rm=TRUE)
}