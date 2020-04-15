# To take updated raw downlodas from market research providers (BoA, JPM, Citi
# and Barclays, and standardize observations and save to target Access database.
# It needs to be run once every time data is downloaded or updated. Take 5-10
# minutes to run.
# 
###############################################################################

Output_IV = file.path(OutputRoot,"MacroFactorVerification")
if(!file.exists(Output_IV)) {
	dir.create(Output_IV)
}

table_name <- "I_HIST_IV_MONEND";
file_name_iv = paste0(Output_IV,"/",table_name,".csv")

#Process CSI if table names cannot be found in DB or REPROCESS is TRUE
if (!file.exists(file_name_iv) | REPROCESS) {
	
	ac_ts_list <- list()
	##################################################################################
	###Process FRED Data
	data_fred <- read.csv(concat_file_path(tableFilter(list_files,"VariableName","FREDMacros")))
	data_fred <- characterizeTable(data_fred)
	data_fred$DATES <- as.Date(as.character(data_fred$DATES),format="%m/%d/%Y")
	
#j <-2
	for (j in 2:ncol(data_fred)) {
		ac_name <- colnames(data_fred)[j]
		ac_name <- gsub("[.]","-",ac_name)
		
		data_ac_original <- as.xts(as.numeric(as.character(data_fred[,j])),data_fred[,1])
		data_ac_original <- na.omit(data_ac_original)
		file_name = paste(Output_IV,"/",ac_name,"_monthly.png",sep="")
		png(file_name, width=600,height=400)
		print(plot.xts(data_ac_original,main=ac_name))
		dev.off()
		
		ac_ts_list[[ac_name]] <- data_ac_original
	}
	
	##################################################################################
	###Process BBG Data
	data_bbg <- read.csv(concat_file_path(tableFilter(list_files,"VariableName","BBGMacros")))
	data_bbg <- characterizeTable(data_bbg)
	data_bbg$DATES <- as.Date(as.character(data_bbg$DATES),format="%m/%d/%Y")
	
#j <-2
	for (j in 2:ncol(data_bbg)) {
		ac_name <- colnames(data_bbg)[j]
		ac_name <- gsub("[.]","-",ac_name)
		
		data_ac_original <- as.xts(as.numeric(as.character(data_bbg[,j])),data_bbg[,1])
		data_ac_original <- na.omit(data_ac_original)
		file_name = paste(Output_IV,"/",ac_name,"_daily.png",sep="")
		png(file_name, width=600,height=400)
		print(plot.xts(data_ac_original,main=ac_name))
		dev.off()
		
		ac_ts_list[[ac_name]] <- data_ac_original
	}
	
	##################################################################################
#calculation
	IV_MONTH_END <- list()
	
	for (i in 1:length(ac_ts_list)) {
#	i <- 75
		csi <- ac_ts_list[[i]]
		csi <- csi[as.numeric(as.yearperiod(index(csi)))<=History_END_DATE]
		
		#Monthly End
		ep1 <- endpoints(csi,on="months")
		csi_monthend <- csi[ep1]
		
		index(csi_monthend) <- as.Date(as.yearmon(index(csi_monthend))+1/12)-1
		
		IV_MONTH_END[[i]] <- csi_monthend
		
		file_name = paste(Output_IV,"/",gsub("[.]","-",names(ac_ts_list)[i]),"_MonthEnd.png",sep="")
		png(file_name, width=600,height=400)
		ts.plot(csi_monthend,col=1:3,type="b",main=names(ac_ts_list)[i])
		dev.off()
	}
	names(IV_MONTH_END) <- names(ac_ts_list)
	
	CSI_MONTH_END_db <- xtslist_to_db(IV_MONTH_END)
	colnames(CSI_MONTH_END_db) <- c("DATE","MONTH","QUARTER","IV_ID","IV_VALUE")
	write.csv(CSI_MONTH_END_db,file_name_iv)
	write.csv(CSI_MONTH_END_db,file.path(VersionRoot,paste0(table_name,".csv")))
}

# file_name_iv is the file path to read processed independent variables