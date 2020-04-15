# Purpose of this document is to provide basic functions to read, write and manipulate
# Access database using R.
# 
# Author: e620927
###############################################################################

require(RODBC)
#require Library_DataFrame.R

openAccess <- function(file_path) {
	#function: open odbc connection to an existing MS Access database.
	#parameter: file_path is the path to database
	#return: an open database handler
	odbcDriverConnect(paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",file_path,sep=""))
}

dbHasTable <- function(dbc,tablename) {
	return(tablename%in%sqlTables(dbc)$TABLE_NAME)
}

fetchTable <- function(dbc,tablename,version) {
#dbc <- dbc_control
#tablename <- "List_Files"
#version <- current_version
	#function: fetch a table from database "dbc" with table name as "tablename"
	#	and lines with Version number "version"\\
	#parameter: dbc is the open database connection handler 
	#	tablename is the name of table in the database
	#	version is used to select the right rows of that table.
	#return: a dataframe
	
	if (!dbHasTable(dbc,tablename)){
		stop(paste0(tablename," is not in the database"))
	}
	table <- sqlFetch(dbc, tablename)
	table <- tableFilter(table,"version",version)
	return(table)
}

createVariables <- function(line) {
	#function: create variables from a single row of dataframe. variable name is column name
	#	variable value is the value in the dataframe.
	#parameter: line is a single row of a dataframe with appropriate column names
	#return: NULL. Variables are creted in global environment.
	
#	line <- current_version_info
	if (nrow(line)!=1) {
		stop("The nrow is not 1 when create variables based on dataframe")
	}
	for (i in 1:ncol(line)) {
		if (colnames(line)[i]%in%ls(envir = .GlobalEnv)) {
#			if (line[1,i] == get(colnames(line)[i])) {
#				print("Duplicate variable is created but with the same value")
#			} else {
				print(paste0("Existing variable ",colnames(line)[i]," is updated from ",get(colnames(line)[i]), " to ",line[1,i]))
#			}
		} else {
			print(paste0("Create a new variable ",colnames(line)[i]," with value ",line[1,i]))
		}
		assign(colnames(line)[i],line[1,i], envir = .GlobalEnv)
	}
}

concat_file_path <- function(dataframe) {
#dataframe <- current_version_info
	#function: get file path
	#parameter: a dataframe with the correct column names such as FileLocation, DBName, FileName or etc.
	#return: the path to file
	if (sum(grepl("DBName", colnames(dataframe),ignore.case=TRUE)==1)) {
		path <- paste(dataframe$FileLocation,dataframe[,grep("DBName", colnames(dataframe),ignore.case=TRUE)],sep="\\")
	} else if (sum("FileName"%in%colnames(dataframe))==1) {
		path <- paste(dataframe$FileLocation,dataframe[,grep("FileName", colnames(dataframe),ignore.case=TRUE)],sep="\\")
	} else {
		stop("Error: cannot find the path to file or database")
	}
	
	print(paste0("Find File Path: ",path))
	return(path)
}


#version = current_version
#file_name = "2017CCAR_I_MOD_CSI_AC.xlsx"
#variable_name = "data_mmd"
#getFilePath <- function(list_files,file_name=NA,variable_name=NA,version) {
#	#function: get file path by using an identifier such as file name or variable name
#	#parameter: a dataframe with the correct column names such as FileLocation, DBName, FileName or etc.
#	#return: the path to file
#	
#	if (sum(is.na(file_name),is.na(variable_name))!=1) {
#		stop("Should Specify Either file name or variable name")
#	}
#	
#	if (!is.na(variable_name)) {
#		tmp <- list_files$Version==version&list_files$VariableName==variable_name
#		tmp[is.na(tmp)] <- FALSE
#		if (sum(tmp)==1) {
#			return(concat_file_path(list_files[tmp,]))
#		} else if (sum(tmp)==0) {
#			warning("None variable name match.")
#			return(NA)
#		}
#	}
#	
#	if (!is.na(file_name)) {
#		tmp <- list_files$Version==version&list_files$FileName==file_name
#		tmp[is.na(tmp)] <- FALSE
#		if (sum(tmp)==1) {
#			return(concat_file_path(list_files[tmp,]))
#		} else if (sum(tmp)==0) {
#			warning("None variable name match.")
#			return(NA)
#		}
#	}
#}\

saveTable <- function(dbc,table_name,data,prohibitaddition=TRUE) {
#	dbc
#	table_name = "I_MOD_CSI_AC"
#	data = mapping
#	try(sqlDrop(dbc, table_name))
	#function: save a table to a database and/or merge with an existing table
	#parameter: dbc as database handler, table_name as table name, data as a dataframe to be saved.
	#return: NULL
	if (table_name%in%sqlTables(dbc)$TABLE_NAME==FALSE) {
		sqlSave(dbc, data, rownames  = FALSE, tablename = table_name, addPK=FALSE, safer = FALSE)
	} else {
		version = as.character(unique(data$VERSION))
		
		data_orig <- sqlFetch(dbc,table_name)
		data_orig <- data.frame(lapply(data_orig, as.character), stringsAsFactors=FALSE)
		
		flag1 <- all(colnames(data_orig)%in%colnames(data))
		if (!flag1){
			print(table_name)
			stop("Error: cannot save table because new table does not include all columns of old table")
		}
		
		flag2 <- all(colnames(data)%in%colnames(data_orig))
		if (flag2) {
			#identical column names
			if (version%in%unique(data_orig$VERSION)) {
				data_orig = data_orig[data_orig$VERSION!=version,]
				data = rbind(data_orig,data)
				sqlSave(dbc, data, rownames  = FALSE, append = FALSE, tablename = table_name, addPK=FALSE, safer = FALSE)
			} else {
				sqlSave(dbc, data, rownames  = FALSE, append = TRUE, tablename = table_name, addPK=FALSE, safer = FALSE)
			}
		} else if (!prohibitaddition) {
			print(table_name)
			print("Warning: new table has more columns, add to old one")
			
			if (version%in%unique(data_orig$VERSION)) {
				data_orig = data_orig[data_orig$VERSION!=version,]
			}
			data = merge(data_orig,data,all=TRUE,sort = FALSE)
			sqlDrop(dbc, table_name)
			sqlSave(dbc, data, rownames  = FALSE, append = FALSE, tablename = table_name, addPK=FALSE, safer = FALSE)
		} else {
			print(table_name)
			stop("Error: cannot save table because new table has more columns than old table")
		}
	}
}

deleteTable <- function(dbc,table_name) {
	if (dbHasTable(dbc,table_name)) {
		sqlDrop(dbc, table_name)
	} else {
		print(paste0(table_name,"table was not existed, therefore no deletion is required."))
	}
}
