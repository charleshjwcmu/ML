#
# 
###############################################################################

Output_IV <- file.path(OutputRoot,"MacroFactorVerification")
if (!dir.exists(Output_IV)) {
	dir.create(Output_IV)
}

##Fetch Independent Variables --- History
IV <- read.csv(file_name_iv)
IV <- IV[,c("IV_ID","DATE","IV_VALUE")]
IV[,1] <- as.character(IV[,1])
IV[,2] <- as.Date(as.character(IV[,2]), "%m/%d/%Y")
IV[,3] <- as.numeric(as.character(IV[,3]))

IV_DATA <- retrieveIV(IV)
IV_Names <- names(IV_DATA)

file_name <- paste(Output_IV,"/IVALL.tex",sep="")
print(xtable(as.data.frame(IV_Names),caption=paste0("All Independent Variables")),file=file_name,tabular.environment = 'longtable', floating=FALSE, include.rownames=TRUE)

#Do IV transformation
IV_transform <- IV_transform[IV_transform$VERSION==current_version,]
IV_transform <- IV_transform[IV_transform$IV_ID%in%IV_Names,]

colnames = c()
#Initiate the matrix storing the variable transformations, lags, and group
IV_ID = matrix(ncol = 5, nrow = 0)
for (i in 1:nrow(IV_transform)) {
#	i <- 1
	#Identify the independent variable
	temp_ts = IV_DATA[[as.character(IV_transform$IV_ID[i])]]
	#Create transformations
	tranform_columns <- grep("Transform",colnames(IV_transform))
	for (j in tranform_columns) {
		# j <- 7
		if (IV_transform[i,j]==""|is.na(IV_transform[i,j])) {
			next
		}
		for (l in 0:Num_lags) {
#			l <- 0
			temp <- transformTS(temp_ts, IV_transform[i,j], l)
			# Create a matrix storing all the transformed variables
			if ((i == 1)&(j==tranform_columns[1])&(l==0)) {
				Ind_var = temp
			} else {
				Ind_var = ts.union(Ind_var,temp)
			}
			colname <- paste(IV_transform$IV_ID[i],IV_transform[i,j],lags[1+l],sep="")
			colnames <- c(colnames, colname)
			IV_ID <- rbind(IV_ID,c(as.character(IV_transform$IV_ID[i]),as.character(IV_transform[i,j]),lags[l+1],colname,as.character(IV_transform[i,]$IV_GROUP)))
		}
	}
}

IV_ID <- as.data.frame(IV_ID)
colnames(IV_ID) <- c("IV_ID", "TRANSFORM", "LAG", "IV_NAME","IV_GROUP")
colnames(Ind_var) = colnames
rownames(Ind_var) <- as.yearmon(index(Ind_var))

write.csv(Ind_var,file.path(VersionRoot,"Ind_var.csv"))
write.csv(IV_ID,file.path(VersionRoot,"I_MOD_IV.csv"))


fileConn<-file(paste(Output_IV,"/IVFactors.tex",sep=""),"w")
writeLines(paste0("\\newcommand\\IVFactors{",paste(IV_Names,collapse=","),"}"), fileConn)
close(fileConn)

iv_names <- IV_Names
for (i in 1:length(iv_names)) {
	#		i <- 1
	#graph output
	file_name <- paste(Output_IV,"/",iv_names[i],".png",sep="")
	png(file_name,width=800,height=600)
	tmp_data <- IV_DATA[[iv_names[i]]]
	ts.plot(tmp_data,main=iv_names[i])
	dev.off()
	
	#tex output
	file_name <- paste(Output_IV,"/",iv_names[i],".tex",sep="")
	print(xtable(tmp_data),file=file_name,tabular.environment = 'longtable', floating=FALSE, include.rownames=TRUE)
}

if (IV_ANALYSIS) {
	
	#Stationarity Test
	Num_variables = length(Ind_var[1,])
	test_results = matrix(NA,Num_variables,10)
	
	for (i in 1:Num_variables) {
		#	i <- 1
		op <- options(warn = (-1)) 
		
		temp_var = try(na.omit(Ind_var[,i],silent = TRUE))
		
		if (inherits(temp_var,"try-error")) {
			temp_var = Ind_var[,i]
			temp_var = temp_var[!is.na(temp_var)]
			test_results[i,1] = "Internal missing values omitted"
		}
		
		options(op) 
		
		#If there are no infinite values in data
		if(sum(temp_var) < Inf&sum(temp_var) > -Inf) {
			
			op <- options(warn = (-1)) 
			stats_adf0 = AdfTest(temp_var,simpleoutput=FALSE)$p.value
			stats_adf1 = AdfTest(temp_var,simpleoutput=FALSE)$p.value
			stats_pp1 = PPTest_tseries(temp_var,simpleoutput=FALSE)$p.value
			stats_kpss1 = KPSSTest_tseries(temp_var,simpleoutput=FALSE)$p.value
			
			# Verdict for level stationarity tests
			if ((min(stats_adf1,stats_pp1)<0.05)&(stats_kpss1>0.05)) {
				test_results[i,6] = "Level Stationary"
			} else if ((min(stats_adf1,stats_pp1)>0.05)&(stats_kpss1<0.05)) {
				test_results[i,6] = "Not Level Stationary"
			} else if ((min(stats_adf1,stats_pp1)>0.05)&(stats_kpss1>0.05)) {
				test_results[i,6] = "Inconclusive Level Stationarity Tests"
			} else if ((min(stats_adf1,stats_pp1)<0.05)&(stats_kpss1<0.05)) {
				test_results[i,6] = "Conflicting Level Stationarity Tests"
			}
			
			adf2_test = ur.df(temp_var, type = "trend", lags = trunc((length(temp_var)-1)^(1/3)))
			
			# Classify the test results for adf-test:
			adf2_class = sum(1*(adf2_test@teststat[1,1] < adf2_test@cval[1,]))
			if (adf2_class == 3) {
				stats_adf2 = "Less than 1%"
				stats_adf2_ex = 0.005
			} else if (adf2_class == 2) {
				stats_adf2 = "Between 1% and 5%"
				stats_adf2_ex = 0.025
			} else if (adf2_class == 1) {
				stats_adf2 = "Between 5% and 10%"
				stats_adf2_ex = 0.075
			} else if (adf2_class == 0) {
				stats_adf2 = "Over 10%"
				stats_adf2_ex = 0.45
			}
			
			pp2_test = ur.pp(temp_var, model = "trend", lags = "short", type = "Z-tau")
			# Classify the test results for pp-test:
			pp2_class = sum(1*(pp2_test@teststat < pp2_test@cval))
			if (pp2_class == 3) {
				stats_pp2 = "Less than 1%"
				stats_pp2_ex = 0.005
			} else if (pp2_class == 2) {
				stats_pp2 = "Between 1% and 5%"
				stats_pp2_ex = 0.025
			} else if (pp2_class == 1) {
				stats_pp2 = "Between 5% and 10%"
				stats_pp2_ex = 0.075
			} else if (pp2_class == 0) {
				stats_pp2 = "Over 10%"
				stats_pp2_ex = 0.45
			}
			
			kpss2_test = ur.kpss(temp_var, type = "tau", lags = "short")
			kpss2_class = sum(1*(kpss2_test@teststat < kpss2_test@cval[-3]))
			if (kpss2_class == 3) {
				stats_kpss2 = "Less than 1%"
				stats_kpss2_ex = 0.005
			} else if (kpss2_class == 2) {
				stats_kpss2 = "Between 1% and 5%"
				stats_kpss2_ex = 0.025
			} else if (kpss2_class == 1) {
				stats_kpss2 = "Between 5% and 10%"
				stats_kpss2_ex = 0.075
			} else if (kpss2_class == 0) {
				stats_kpss2 = "Over 10%"
				stats_kpss2_ex = 0.45
			}
			
			options(op) 
			
			# Verdict for trend stationarity tests
			if ((min(stats_adf2_ex,stats_pp2_ex)<0.05)&(stats_kpss2_ex>0.05)) {
				test_results[i,10] = "Trend Stationary"
			} else if ((min(stats_adf2_ex,stats_pp2_ex)>0.05)&(stats_kpss2_ex<0.05)) {
				test_results[i,10] = "Not Trend Stationary"
			} else if ((min(stats_adf2_ex,stats_pp2_ex)>0.05)&(stats_kpss2_ex>0.05)) {
				test_results[i,10] = "Inconclusive Trend Stationarity Tests"
			} else if ((min(stats_adf2_ex,stats_pp2_ex)<0.05)&(stats_kpss2_ex<0.05)) {
				test_results[i,10] = "Conflicting Trend Stationarity Tests"
			}
			
			test_results[i,2:5] = round(c(stats_adf0,stats_adf1,stats_pp1,stats_kpss1),digits=3)
			test_results[i,7:9] = c(stats_adf2,stats_pp2,stats_kpss2)
			
		} else {
			test_results[i,6] = "Undesirable (Infinite value in time series)"
			test_results[i,10] = "Undesirable (Infinite value in time series)"
		}
	}
	
	colnames(test_results) = c("Internal NAs?","ADF p-val","ADF p-val","PP p-val", "KPSS p-val","Level Stationarity Test Verdict","ADF p-val (trend)","PP p-val (trend)", "KPSS p-val (trend)","Trend Stationarity Test Verdict")
	rownames(test_results) = colnames(Ind_var)
	test_results = cbind(IV_NAME=as.character(colnames(Ind_var)),test_results)
	file_name <- paste0(Output_IV,"/IV_Stationarity_nolag_MONTHLY.xlsx")
	data = data.frame(VERSION=current_version,TimeStamp = gsub(":","-",Sys.time()),as.data.frame(test_results))
	write.xlsx(data,file_name)
	
	##With lags
	Num_variables = length(Ind_var[1,])
	test_results = matrix(NA,Num_variables,18)
	
	for (i in 1:Num_variables) {
		op <- options(warn = (-1)) 
		
		temp_var = try(na.omit(Ind_var[,i],silent = TRUE))
		
		if (inherits(temp_var,"try-error")) {
			temp_var = Ind_var[,i]
			temp_var = temp_var[!is.na(temp_var)]
			test_results[i,2] = "Internal missing values omitted"
		}
		
		options(op) 
		
		#If there are no infinite values in data
		if(sum(temp_var) < Inf&sum(temp_var) > -Inf) {
			
			op <- options(warn = (-1)) 
			
			#Length of dependent variable
			test_results[i,1] = length(temp_var)
			#Length of dependent variable
			IV_len = length(temp_var)
			test_results[i,1] = IV_len
			
			########## ADF test with drift ##############
			if (IV_len < 50) {
				max_lag = floor(4*(IV_len/100)^(1/4)) #Use Schwart's l4
			} else {
				max_lag = floor(12*(IV_len/100)^(1/4)) #Use Schwart's l12
			}
			stationarity = ur.df(temp_var, type = "drift", lags = max_lag, selectlags = "BIC")
			test_stat = stationarity@teststat[1]
			cval = stationarity@cval[1,]
			stats_adf1 = approx(x = cval, y =c(0.01,0.05,0.10), xout = test_stat, method = "linear", rule = 2)$y
			stats_adf1_lag = length(rownames(stationarity@testreg$coef)) - 2
			
			########## PP test with drift using Schwert's lag##############
			if (IV_len < 50) {
				max_lag = floor(4*(IV_len/100)^(1/4)) #Use Schwart's l4
				stationarity = ur.pp(temp_var, type = "Z-tau", model = "constant", lags = "short") #Use Schwart's l4
			} else {
				max_lag = floor(12*(IV_len/100)^(1/4)) #Use Schwart's l12
				stationarity = ur.pp(temp_var, type = "Z-tau", model = "constant", lags = "long")
			}
			test_stat = stationarity@teststat[1]
			cval = stationarity@cval[1,]
			stats_pp1 = approx(x = cval, y =c(0.01,0.05,0.10), xout = test_stat, method = "linear", rule = 2)$y
			stats_pp1_lag = max_lag
			
			########## KPSS test with drift using Schwert's lag##############
			
			if (IV_len < 50) {
				max_lag = floor(4*(IV_len/100)^(1/4)) #Use Schwart's l4
				stationarity = ur.kpss(temp_var, type = "mu", lags = "short") #Use Schwart's l4
			} else {
				max_lag = floor(12*(IV_len/100)^(1/4)) #Use Schwart's l12
				stationarity = ur.kpss(temp_var, type = "mu", lags = "long")
			}
			test_stat = stationarity@teststat[1]
			cval = stationarity@cval[1,]
			stats_kpss1 = approx(x = cval, y =c(0.1,0.05,0.025, 0.01), xout = test_stat, method = "linear", rule = 2)$y
			stats_kpss1_lag = max_lag
			
			########## DF-GLS test with drift ##############
			#http://www.stata.com/manuals13/tsdfgls.pdf
			if (IV_len < 50) {
				max_lag = floor(4*(IV_len/100)^(1/4)) #Use Schwart's l4
			} else {
				max_lag = floor(12*(IV_len/100)^(1/4)) #Use Schwart's l12
			}
			select_lag = -1
			select_BIC = Inf
			for (k in 1:max_lag) {
				
				stationarity = ur.ers(temp_var, type = "DF-GLS", model = "constant", lag.max = k)
				residuals = stationarity@testreg$residuals
				if (max_lag > k) {
					residuals = residuals[-1*seq(1,max_lag - k)]
				}
				BIC = log(1/((IV_len-1) - max_lag)*sum(residuals^2)) + (k+1)*log((IV_len-1)-max_lag)/((IV_len-1)-max_lag)
				if (BIC < select_BIC) {
					select_BIC = BIC
					select_lag = k
				}
			}
			stationarity = ur.ers(temp_var, type = "DF-GLS", model = "constant", lag.max = select_lag)
			test_stat = stationarity@teststat[1]
			cval = stationarity@cval[1,]
			stats_dfgls1 = approx(x = cval, y =c(0.01,0.05,0.10), xout = test_stat, method = "linear", rule = 2)$y
			stats_dfgls1_lag = select_lag
			
			########## ADF test with TREND ##############
			if (IV_len < 50) {
				max_lag = floor(4*(IV_len/100)^(1/4)) #Use Schwart's l4
			} else {
				max_lag = floor(12*(IV_len/100)^(1/4)) #Use Schwart's l12
			}
			stationarity = ur.df(temp_var, type = "trend", lags = max_lag, selectlags = "BIC")
			test_stat = stationarity@teststat[1]
			cval = stationarity@cval[1,]
			stats_adf2 = approx(x = cval, y =c(0.01,0.05,0.10), xout = test_stat, method = "linear", rule = 2)$y
			stats_adf2_lag = length(rownames(stationarity@testreg$coef)) - 3
			
			########## PP test with TREND using Schwert's lag##############
			if (IV_len < 50) {
				max_lag = floor(4*(IV_len/100)^(1/4)) #Use Schwart's l4
				stationarity = ur.pp(temp_var, type = "Z-tau", model = "trend", lags = "short") #Use Schwart's l4
			} else {
				max_lag = floor(12*(IV_len/100)^(1/4)) #Use Schwart's l12
				stationarity = ur.pp(temp_var, type = "Z-tau", model = "trend", lags = "long")
			}
			test_stat = stationarity@teststat[1]
			cval = stationarity@cval[1,]
			stats_pp2 = approx(x = cval, y =c(0.01,0.05,0.10), xout = test_stat, method = "linear", rule = 2)$y
			stats_pp2_lag = max_lag
			
			########## KPSS test with TREND using Schwert's lag##############
			
			if (IV_len < 50) {
				max_lag = floor(4*(IV_len/100)^(1/4)) #Use Schwart's l4
				stationarity = ur.kpss(temp_var, type = "tau", lags = "short") #Use Schwart's l4
			} else {
				max_lag = floor(12*(IV_len/100)^(1/4)) #Use Schwart's l12
				stationarity = ur.kpss(temp_var, type = "tau", lags = "long")
			}
			test_stat = stationarity@teststat[1]
			cval = stationarity@cval[1,]
			stats_kpss2 = approx(x = cval, y =c(0.1,0.05,0.025, 0.01), xout = test_stat, method = "linear", rule = 2)$y
			stats_kpss2_lag = max_lag
			
			########## DF-GLS test with TREND ##############
			#http://www.stata.com/manuals13/tsdfgls.pdf
			if (IV_len < 50) {
				max_lag = floor(4*(IV_len/100)^(1/4)) #Use Schwart's l4
			} else {
				max_lag = floor(12*(IV_len/100)^(1/4)) #Use Schwart's l12
			}
			
			select_lag = -1
			select_BIC = Inf
			for (k in 1:max_lag) {
				
				stationarity = ur.ers(temp_var, type = "DF-GLS", model = "trend", lag.max = k)
				residuals = stationarity@testreg$residuals
				if (max_lag > k) {
					residuals = residuals[-1*seq(1,max_lag - k)]
				}
				BIC = log(1/((IV_len-1) - max_lag)*sum(residuals^2)) + (k+1)*log((IV_len-1)-max_lag)/((IV_len-1)-max_lag)
				if (BIC < select_BIC) {
					select_BIC = BIC
					select_lag = k
				}
			}
			stationarity = ur.ers(temp_var, type = "DF-GLS", model = "trend", lag.max = select_lag)
			test_stat = stationarity@teststat[1]
			cval = stationarity@cval[1,]
			stats_dfgls2 = approx(x = cval, y =c(0.01,0.05,0.10), xout = test_stat, method = "linear", rule = 2)$y
			stats_dfgls2_lag = length(rownames(stationarity@testreg$coef)) - 1
			
			
			test_results[i,3:18] = round(c(stats_adf1,stats_adf1_lag,
							stats_pp1,stats_pp1_lag,
							stats_kpss1,stats_kpss1_lag,
							stats_dfgls1,stats_dfgls1_lag,
							stats_adf2,stats_adf2_lag,
							stats_pp2,stats_pp2_lag,
							stats_kpss2,stats_kpss2_lag,
							stats_dfgls2,stats_dfgls2_lag),digits=3)
			
		} else {
			test_results[i,7] = "Undesirable (Infinite value in time series)"
			test_results[i,11] = "Undesirable (Infinite value in time series)"
		}
	}
	
	colnames(test_results) = c("Obs","Internal NAs?","ADF p-val (drift)","ADF lag (drift)",
			"PP p-val (drift)","PP lag (drift)",
			"KPSS p-val (drift)","KPSS lag (drift)",
			"DF-GLS p-val (drift)","DF_GLS lag (drift)",
			"ADF p-val (trend)","ADF lag (trend)",
			"PP p-val (trend)","PP lag (trend)",
			"KPSS p-val (trend)","KPSS lag (trend)",
			"DF-GLS p-val (trend)","DF_GLS lag (trend)")
	
	test_results_clean <- cbind(IV_NAME=colnames(Ind_var),test_results)
	
	file_name <- paste0(Output_IV,"/IV_Stationarity_wlags_MONTHLY.xlsx")
	data = data.frame(VERSION=current_version,TimeStamp = gsub(":","-",Sys.time()),as.data.frame(test_results_clean))
	write.xlsx(data,file_name)
}