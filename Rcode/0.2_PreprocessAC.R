# To take updated raw downlodas from market research providers (BoA, JPM, Citi
# and Barclays, and standardize observations and save to target Access database.
# It needs to be run once every time data is downloaded or updated. Take 5-10
# minutes to run.
# 
###############################################################################

Output_curve = file.path(OutputRoot,"CurveInfo")
if(!file.exists(Output_curve)) {
	dir.create(Output_curve)
}

table_name <- "I_HIST_AC_MONEND";
file_name_ac = paste0(Output_curve,"/",table_name,".csv")

if (!file.exists(file_name_ac) | REPROCESS) {
	
#read data inputs
	data_ac <- read.csv(concat_file_path(tableFilter(list_files,"VariableName","Assets")))
	##
	ac_ts_list <- list()
	
	##################################################################################
	###Process JPM
	data_ac <- characterizeTable(data_ac)
	data_ac$Date <- as.Date(as.character(data_ac$Date),format="%m/%d/%Y")
#	CSI_ALL_DailyClean <- list()
#j <-2
	for (j in 2:ncol(data_ac)) {
		ac_name <- colnames(data_ac)[j]
		ac_name <- gsub("[.]","-",ac_name)
		
		data_ac_original <- as.xts(as.numeric(as.character(data_ac[,j])),data_ac[,1])
		data_ac_original <- na.omit(data_ac_original)
		
		file_name = paste(Output_curve,"/",ac_name,"_daily.png",sep="")
		png(file_name, width=600,height=400)
		print(plot.xts(data_ac_original))
		dev.off()
		
		ac_ts_list[[ac_name]] <- data_ac_original
	}
		##################################################################################
#calculation
	AC_MONTH_END <- list()
	
	for (i in 1:length(ac_ts_list)) {
#	i <- 2
		csi <- ac_ts_list[[i]]
		csi <- csi[as.numeric(as.yearperiod(index(csi)))<=History_END_DATE]
		
		#Monthly End
		ep1 <- endpoints(csi,on="months")
		csi_monthend <- csi[ep1]
		
		index(csi_monthend) <- as.Date(as.yearmon(index(csi_monthend))+1/12)-1
		
		AC_MONTH_END[[i]] <- csi_monthend
				
		file_name = paste(Output_curve,"/",gsub("[.]","-",names(ac_ts_list)[i]),"_Monthly.png",sep="")
		png(file_name, width=600,height=400)
		ts.plot(csi_monthend,col=1:3,type="b",main=names(ac_ts_list)[i])
		dev.off()
	}
	names(AC_MONTH_END) <- names(ac_ts_list)
	
	AC_MONTH_END_db <- xtslist_to_db(AC_MONTH_END)
	
	write.csv(AC_MONTH_END_db,file_name_ac)
	write.csv(AC_MONTH_END_db,file.path(VersionRoot,paste0(table_name,".csv")))
}
