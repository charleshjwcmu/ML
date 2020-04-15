# 
# 
# Author: e620927
###############################################################################

#### Run Latex PDF packages
system(paste(PDFReportDir,"/ACInfo_Production.bat",sep=""))

system(paste(PDFReportDir,"/IV_Production.bat",sep=""))

system(paste(PDFReportDir,"/PCA_Production.bat",sep=""))

system(paste(PDFReportDir,"/ClusterAnalysis_Production.bat",sep=""))

system(paste(PDFReportDir,"/ProductPackage_Equity.bat",sep=""))
system(paste(PDFReportDir,"/ProductPackage_FixedIncome.bat",sep=""))
system(paste(PDFReportDir,"/ProductPackage_Commodity.bat",sep=""))
