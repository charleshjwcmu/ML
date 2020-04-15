###############################################################################
# Updated on 1n/19/2018
# Author: Charles Huang
# Email: huangjiawei579@gmail.com
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; 
###############################################################################
####################Install and Load Packages, Parameters #####################
# install packages
# 
install.packages(c("RODBC","ggfortify","ggplot2", "gridExtra", "RODBC", "egcm", "ggdendro","zoo","xlsx","xtable","reshape","plm","car","tseries","urca","lmtest","leaps","sandwich","doParallel","forecast","RJDBC","plyr","dplyr"))
install.packages("xlsx")

######	Config	######
current_version = "FI_Development"
Root_Development = "C:/Users/huang/workspace1" # or "C:\\Users\\huang\\workspace1"

ORoot = file.path(Root_Development,"Output_FI")
PRoot = file.path(Root_Development,"PDFReport_FI")
Code_Development = file.path(Root_Development,"Rcode")
setwd(Code_Development)

source(file.path(getwd(),"RLibrary/Library_DataFrame.R"))
source(file.path(getwd(),"RLibrary/Library_DateAndString.R"))
source(file.path(getwd(),"RLibrary/Library_AccessDatabase.R"))
source(file.path(getwd(),"RLibrary/Library_IO.R"))
source(file.path(getwd(),"RLibrary/Library_TimeSeries.R"))
source(file.path(getwd(),"RLibrary/Library_Regression.R"))

ttime = cleanString(format(Sys.time(), "%Y %m %d %X"))
OutputRoot = file.path(Root_Development,"Output_FI",ttime)
PDFReportRoot = file.path(Root_Development,"PDFReport_FI",ttime)
History_END_DATE <- "2019 Q2"
Calibration_END_DATE <- "2019 Q2"

###############################################################################

################################Start Analysis####################################

source("./1_ReadData-Functions.R")
source("./2_ClusterAnalysis-Functions.R")
source("./4_ModelSelection-Functions.R")
source("./5_Forecast-Functions.R")

OUTPUT_VERBOSE = TRUE

source("./0.1_Config.R")

###########################################################################################
# Credit Spread Modeling and Forecast
###########################################################################################
### Step0: generate Latex Template for PDF reporting
source("./0.1_GenerateLatex.R")
### Step1: data preparation
REPROCESS = FALSE # if TRUE, the CSI will be reprocessed and the database will be refreshed.
source("./0.2_PreprocessAC.R")
REPROCESS = FALSE # if TRUE, the independent variables will be reprocessed and the database will be refreshed.
source("./0.3_PreprocessIV.R") # when new scenario data is available, sql needs to be updated (line 32-34).

### Step1: read Asset Class data from Excel sheets
OUTPUT_CSI_PLOT = TRUE;
source("./1_ReadAC.R")

### Step2: Analyze Asset Class data using hcluster algorithm
PARAM_CLUSTER = 0.85
source("./2_ClusterAnalysis.R")

### Step3: Review hcluster results and apply management overrides
#run PCA with final selection of clusters
#just need to run once each time original raw data is updated
source("./3_GeneratePC.R")

### Step4: Read CSI and IV and run verification if set TRUE
IV_ANALYSIS <- TRUE
DV_ANALYSIS <- TRUE
source("./4.1_Read_IV.R")

# read PC1 from all alternatives
source("./4.2_Read_DV.R")
source("./4.3_ModelSelection_Correlation.R")

# Model Speicification with single variable and two variables
OutSamplePeriods <- 9; #exclude the most recent or the most acient 1 to X periods
RERUN_SFA_FLAG <- FALSE
REPLOT_SFA_FLAG <- FALSE
source("./4.4_ModelSelection_SFA.R")

OutSamplePeriods <- 9; #exclude the most recent or the most acient 1 to X periods
RERUN_MFA_FLAG <- FALSE
REPLOT_MFA_FLAG <- TRUE
source("./4.5_ModelSelection_MFA.R")

### Step9: Run Latex PDF packages
source("./9.9_Generate_PDF.R")

