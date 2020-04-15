# Functions special designed to read and write.
# 
###############################################################################

writeVectorToTex <- function(filename,varname,vector,mode="w",command="newcommand") {
	#function: save a execution trail and assign a vector to a newcommand in latex
	#parameter: filename is the path to save file. 
	#	varname is the name of newcommand 
	#	vector is a vector that stores the values
	#return: NULL
	
	fileConn <- file(filename,mode)
	writeLines(paste0("\\",command,"\\",varname,"{",paste(vector,collapse=","),"}"), fileConn)
	close(fileConn)
}


writeMatrixToTex <- function(filename,cap,matrix) {
	require(xtable)
	#function: save a matrix/dataframe to a local file as a longtable in latex
	#parameter: filename is the path to save file
	#	caption is the caption of the long table
	#	matrix is a dataframe or matrix.
	#return: NULL
	print(xtable(matrix,caption=cap),file=filename,tabular.environment = "longtable",floating=FALSE, caption.placement="top", include.rownames=TRUE)
}

writeFitToTex <- function(filename,fit) {
	require(texreg)
	require(Hmisc)
	if (class(fit)[1]=="tslm") {
		print(xtable(fit,digits=2),file=filename,tabular.environment = "longtable",floating=FALSE, caption.placement="top", include.rownames=TRUE)
	} else {
		print(texreg(fit,float.pos="h!"),file=filename)
	}
}
