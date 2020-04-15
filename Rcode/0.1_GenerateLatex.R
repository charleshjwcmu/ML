# Generate Latex templates for presentation of model outputs
# 
###############################################################################

######################Output Latex Parameters for each Asset Class#####################
##Create Asset Class Tex Parameters
if (!dir.exists(PDFReportRoot)) {
	dir.create(PDFReportRoot)
}

if(grepl("Treasury Risk",PDFReportRoot)) {
	PDFReportDir = paste0("Z:",gsub(".*Treasury Risk","",PDFReportRoot))
} else {
	PDFReportDir = PDFReportRoot
}

# tex file: global parameters
fileConn <- file(paste(PDFReportDir,"/GlobalParameters.tex",sep=""),"w")
writeLines(paste0("\\newcommand\\Output{",gsub("[\\]","/",OutputRoot),"}"), fileConn)
writeLines("\\newcommand\\OutptCurveInfo{\\Output/CurveInfo}", fileConn)
writeLines("\\newcommand\\OutptMacroInfo{\\Output/MacroFactorVerification}", fileConn)
writeLines("\\newcommand\\OutptClusterAnalysis{\\Output/ClusterAnalysis}", fileConn)

writeLines("\\newcommand\\OutptPCA{\\Output/PCA_MONTHLY_MOD_ID}", fileConn)
writeLines("\\newcommand\\OutptPCAtwo{\\Output/PCA_MONTHLY_MOD_ID2}", fileConn)
writeLines("\\newcommand\\OutptPCAthree{\\Output/PCA_MONTHLY_MOD_ID3}", fileConn)
writeLines("\\newcommand\\OutptPCAfour{\\Output/PCA_MONTHLY_MOD_ID4}", fileConn)

writeLines("\\newcommand\\OutptAutoCorr{\\Output/AutoCorr}", fileConn)
writeLines("\\newcommand\\OutptFcst{\\Output/Forecast}", fileConn)
writeLines("\\newcommand\\OutptOutSample{\\Output/OutSample}", fileConn)
writeLines("\\newcommand\\OutptFcstCSI{\\Output/Forecast-CSI}", fileConn)
writeLines("\\newcommand\\OutptSFA{\\Output/SFA_MONTHLY}", fileConn)
writeLines("\\newcommand\\OutptMFA{\\Output/MFA_MONTHLY}", fileConn)
writeLines(paste0("\\newcommand\\MaxModel{1,...,",min(max_model_to_test,30),"}"), fileConn)
close(fileConn)

# tex file: parameters for each asset class
PDFReportDirAC <- paste0(PDFReportDir,"\\ACParameters")
if (!dir.exists(PDFReportDirAC)) {
	dir.create(PDFReportDirAC)
}

AC <- unique(mapping[,"AC_ID"])

for (i in 1:length(AC)) {
#	i <- 1
	fileConn <- file(paste(PDFReportDirAC,"/",AC[i],".tex",sep=""),"w")
	writeLines(paste0("\\newcommand\\CurrentVersion{",current_version,"}"), fileConn)
	writeLines(paste0("\\newcommand\\Curves{",unique(paste(mapping[mapping[,"AC_ID"]==AC[i],"CSI_ID"],collapse=",")),"}"), fileConn)
	writeLines(paste0("\\newcommand\\ASSETCLASS{",AC[i],"}"), fileConn)
	writeLines(paste0("\\newcommand\\CLUSTERS{",paste(na.omit(unique(as.vector(as.matrix(mapping[mapping[,"AC_ID"]==AC[i],modelid_columns])))),collapse=","),"}"), fileConn)
	close(fileConn)
	
	mapping_ac <- mapping[mapping[,"AC_ID"]==AC[i],]
	mapping_ac <- mapping_ac[,!apply(mapping_ac,2, function(data){all(is.na(data))})]
	mapping_ac <- mapping_ac[,!colnames(mapping_ac)%in%c("VERSION","TimeStamp","ID")]
	
	filename <- file(paste(PDFReportDirAC,"/",AC[i],"_mapping.tex",sep=""),"w")
	writeMatrixToTex(filename,AC[i],mapping_ac)
}

##### Generate BAT files
#curve info
fileConn <- file(paste(PDFReportDir,"/ACInfo_Production.bat",sep=""),"w")
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("pdflatex -interaction=nonstopmode ACInfo_document.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode ACInfo_document.tex", fileConn)
close(fileConn)

#independent variables
fileConn <- file(paste(PDFReportDir,"/IV_Production.bat",sep=""),"w")
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("pdflatex -interaction=nonstopmode IV_document.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode IV_document.tex", fileConn)
close(fileConn)

#PCA Analysis
fileConn <- file(paste(PDFReportDir,"/ClusterAnalysis_Production.bat",sep=""),"w")
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("pdflatex -interaction=nonstopmode ClusterAnalysis_document.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode ClusterAnalysis_document.tex", fileConn)
close(fileConn)

#cluster analysis
fileConn <- file(paste(PDFReportDir,"/PCA_Production.bat",sep=""),"w")
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("pdflatex -interaction=nonstopmode PCA_document.tex", fileConn)
writeLines("pdflatex -interaction=nonstopmode PCA_document.tex", fileConn)
close(fileConn)

# BAT for each asset class
fileConn <- file(paste(PDFReportDir,"/ProductPackage_all.bat",sep=""),"w")
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("echo \"Generate PDF report with LaTeX!\"", fileConn)
writeLines("for %%a in (", fileConn)
writeLines(paste(na.omit(unique(mapping$AC_ID)),collapse="\n\t"),fileConn)
writeLines(") do (\n	pdflatex -interaction=nonstopmode \"\\def\\ac{%%a} \\input{ProductPackage_%%a.tex}\"", fileConn)
writeLines("	pdflatex -interaction=nonstopmode \"\\def\\ac{%%a} \\input{ProductPackage_%%a.tex}\"\n)", fileConn)
close(fileConn)

ACs <- unique(mapping$AC_ID)
for (i in 1:length(ACs)) {
	fileConn <- file(paste(PDFReportDir,"/ProductPackage_",ACs[i],".bat",sep=""),"w")
	writeLines(paste0("set root=",PDFReportDir), fileConn)
	writeLines("cd /D %root%", fileConn)
	writeLines("echo \"Generate PDF report with LaTeX!\"", fileConn)
	writeLines("for %%a in (", fileConn)
	writeLines(paste0("\t",ACs[i]),fileConn)
	writeLines(") do (\n	pdflatex -interaction=nonstopmode -jobname=ProductPackage_%%a \"\\def\\ac{%%a} \\input{ProductPackage.tex}\"", fileConn)
	writeLines("	pdflatex -interaction=nonstopmode -jobname=ProductPackage_%%a \"\\def\\ac{%%a} \\input{ProductPackage.tex}\"\n)", fileConn)
	close(fileConn)
}

fileConn <- file(paste(PDFReportDir,"/ProductPackageShort_all.bat",sep=""),"w")
writeLines("echo \"Generate PDF report with LaTeX!\"", fileConn)
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("for %%a in (", fileConn)
writeLines(paste(na.omit(unique(mapping$AC_ID)),collapse="\n\t"),fileConn)
writeLines(") do (\n	pdflatex -interaction=nonstopmode \"\\def\\ac{%%a} \\input{ProductPackageShortVersion_%%a.tex}\"", fileConn)
writeLines("	pdflatex -interaction=nonstopmode \"\\def\\ac{%%a} \\input{ProductPackageShortVersion_%%a.tex}\"\n)", fileConn)
close(fileConn)

fileConn <- file(paste(PDFReportDir,"/Var_Selection_all.bat",sep=""),"w")
writeLines("echo \"Generate PDF report with LaTeX!\"", fileConn)
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("for %%x in (", fileConn)
writeLines(paste(na.omit(unique(as.vector(as.matrix(mapping[modelid_columns])))),collapse="\n\t"),fileConn)
writeLines(") do (\n	pdflatex -interaction=nonstopmode \"\\def\\CLUSTER{%%x} \\input{Var_Selection_%%x.tex}\"", fileConn)
writeLines("	pdflatex -interaction=nonstopmode \"\\def\\CLUSTER{%%x} \\input{Var_Selection_%%x.tex}\"\n)", fileConn)
close(fileConn)

fileConn <- file(paste(PDFReportDir,"/Var_Selection_Short_all.bat",sep=""),"w")
writeLines("echo \"Generate PDF report with LaTeX!\"", fileConn)
writeLines(paste0("set root=",PDFReportDir), fileConn)
writeLines("cd /D %root%", fileConn)
writeLines("for %%x in (", fileConn)
writeLines(paste(na.omit(unique(as.vector(as.matrix(mapping[modelid_columns])))),collapse="\n\t"),fileConn)
writeLines(") do (\n	pdflatex -interaction=nonstopmode \"\\def\\CLUSTER{%%x} \\input{Var_Selection_ShortVersion_%%x.tex}\"", fileConn)
writeLines("	pdflatex -interaction=nonstopmode \"\\def\\CLUSTER{%%x} \\input{Var_Selection_ShortVersion_%%x.tex}\"\n)", fileConn)
close(fileConn)

