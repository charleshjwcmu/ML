# Assign global parameters and configure the run
# 
###############################################################################
require(xlsx)
#require LibraryAccessDatabase
options(digits=10)

History_END_DATE <- convertDateToNumeric(History_END_DATE)
Calibration_END_DATE <- convertDateToNumeric(Calibration_END_DATE)
Frequency = "MONTHLY"
MONTHLY_VERSION = TRUE

VersionRoot = file.path(OutputRoot,"Source")
dir.create(ORoot,showWarnings = FALSE)
dir.create(OutputRoot,showWarnings = FALSE)
dir.create(VersionRoot,showWarnings = FALSE)
dir.create(PRoot,showWarnings = FALSE)
dir.create(PDFReportRoot,showWarnings = FALSE)

####Read global parameters for variable selection
pthreshold <- 0.05
Max_var <- 2;
Num_lags <- 1;
lags <- paste("L",0:1,sep="")
stationarity_test_options <- 1 #1 is simple with no lag, 2 with lag
max_model_to_test <- 50
as.yearperiod <- as.yearmon
######	Files	######
list_files <- read.xlsx(file.path(Root_Development,"Inputs","File_List.xlsx"),sheetIndex=1)

###Process Excel and Predefined Mappings and Inputs. Save Excels to Destined Database
mapping <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","mapping")),sheetIndex=1)
mapping <- mapping[!is.na(mapping[,1]),]
mapping <- versionDataFrame(mapping,current_version)
mapping <- mapping[,!apply(mapping,2,function(data){all(is.na(data))})]
modelid_columns <- colnames(mapping)[grep("MOD_ID",colnames(mapping))]
for (i in 1:length(modelid_columns)) {
	mapping[,modelid_columns[i]] <- cleanString(mapping[,modelid_columns[i]])
}
write.csv(mapping,file.path(VersionRoot,"mapping.xlsx"))

D_IV <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","D_IV")),sheetIndex=1)
D_IV <- versionDataFrame(D_IV,current_version)
write.csv(mapping,file.path(VersionRoot,"D_IV.xlsx"))

IV_transform <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","IV_transform")),sheetIndex=1)
IV_transform <- versionDataFrame(IV_transform,current_version)
write.csv(IV_transform,file.path(VersionRoot,"IV_transform.xlsx"))

MOD_IV <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","MOD_IV")),sheetIndex=1)
write.csv(versionDataFrame(MOD_IV,current_version),file.path(VersionRoot,"MOD_IV.xlsx"))

D_CSI_SEC <- read.xlsx(concat_file_path(tableFilter(list_files,"VariableName","D_AC_SEC")),sheetIndex=1)
write.csv(D_CSI_SEC,file.path(VersionRoot,"D_AC_SEC.xlsx"))

