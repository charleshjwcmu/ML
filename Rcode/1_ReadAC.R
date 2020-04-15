# Read CSI data from database on monthly basis
#	compare CSI data with prior version to identify data errors
# 
###############################################################################
Output_curve = file.path(OutputRoot,"CurveInfo")

##Read and Update Mapping Table
I_MOD_CSI_AC <- mapping
file_name = paste(Output_curve,"/MAP_CURVE_MODEL.tex",sep="")
print(xtable(I_MOD_CSI_AC[,!grepl("MOD_ID",colnames(mapping))],caption="Asset Class and Indices"),file=file_name, tabular.environment = 'longtable', floating=FALSE, include.rownames=FALSE)

#process csi data to required format
CSI_CURVES <- extract_ts_from_df(read.csv(file_name_ac),date_column = "DATE",value_column = "CSI_VALUE",name_column = "CSI_ID")
CSI_NAMES_ALL <- names(CSI_CURVES)

#save records to tex file for presentation purpose
csi_names <- names(CSI_CURVES)
fileConn <- file(paste(Output_curve,"/Curves.tex",sep=""),"w")
writeLines(paste0("\\newcommand\\Curves{",paste(csi_names,collapse=","),"}"), fileConn)
writeLines(paste0("\\newcommand\\CurrentVersion{",current_version,"}"), fileConn)
close(fileConn)

#Plot Curves
if (OUTPUT_CSI_PLOT) {
	for(i in 1:length(csi_names)) {
		#	i = 16;
		file_name = paste(Output_curve,"/",csi_names[i],".png",sep="")
		png(file_name,width=600,height=400)
		ts.plot(na.omit(CSI_CURVES[[i]]),ylab=csi_names[i], main=paste0("CSI_ID: ",csi_names[i]),type="b")
		dev.off()
	}
}
