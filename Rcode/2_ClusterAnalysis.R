# Run hierarchical clustering analysis
# 
###############################################################################
###Cluster analysis of Monthly Version
Output_cluster = file.path(OutputRoot,"ClusterAnalysis")
if(!file.exists(Output_cluster)) {
	dir.create(Output_cluster)
}
AC_ID <- as.character(unique(I_MOD_CSI_AC$AC_ID))

CSI_Eigenvectors <- data.frame()
PC1TS <- data.frame()
for (i in 1:length(AC_ID)) {
#	i <- 1
	ac_id <- AC_ID[i]
	csi_tmp <- as.character(I_MOD_CSI_AC[I_MOD_CSI_AC$AC_ID==ac_id,]$CSI_ID)
	select_tmp <- csi_tmp[csi_tmp%in%names(CSI_CURVES)];
	if (length(select_tmp) > 1) {
		curves <- CSI_CURVES[select_tmp];
		curves_ac <- combineCurves(curves)
		curves_ac <- na.omit(curves_ac)
		result <- cluster_analysis(curves_ac, ac_id, Output_cluster)
		PC1TS <- rbind(PC1TS,result[[1]])
		CSI_Eigenvectors <- rbind(CSI_Eigenvectors,result[[2]])
	}
}

file_name <- paste(Output_cluster,"/AssetClassAll.tex",sep="")
writeVectorToTex(file_name,"ASSETCLASS",AC_ID)
