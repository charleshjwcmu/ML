# 
# 
###############################################################################

library(plm)
library(tseries)
library(urca)
library(lmtest)
library(leaps)
library(sandwich)
library(doParallel)

getScenId <- function(scenario_name) {
	return(scenario_name)
}

retrievePC <- function(PC1) {
	
	if (any(colnames(PC1)=="QUARTER")) {
		colnames(PC1)[colnames(PC1)=="QUARTER"] = "PERIOD"
	} else if (any(colnames(PC1)=="MONTH")) {
		colnames(PC1)[colnames(PC1)=="MONTH"] = "PERIOD"
	}
	
	PC_DATA <- list()
	Models <- unique(PC1$MOD_ID)
	for (i in 1:length(Models)) {
		#	i <- 1
		model <- Models[i]
		
		pc <- PC1[PC1$MOD_ID==model,]
		model_name <- as.character(pc$MOD_ID[1])
		
		if (MONTHLY_VERSION) {
			data <- pc[,c("PERIOD","PC1")]
			data <- data[order(as.yearperiod(data$PERIOD)),]
			dd1 <- as.yearperiod(data$PERIOD[1])
			mon <- as.numeric(format(dd1, "%m"))
			yr <- as.numeric(format(dd1, "%Y"))
			
			PC_DATA[[i]] <- ts(data$PC1,start=c(yr,mon),frequency=12)
			names(PC_DATA)[i] <- model_name
		} else {
			data <- pc[,c("PERIOD","PC1")]
			data <- data[order(as.yearperiod(data$PERIOD)),]
			dd1 <- data$PERIOD[1]
			yr <- as.numeric(substring(dd1,1,4))
			qtr <- as.numeric(substring(dd1,7,7))
			
			PC_DATA[[i]] <- ts(data$PC1,start=c(yr,qtr),frequency=4)
			names(PC_DATA)[i] <- model_name
		}
	}
	return(PC_DATA)
}

retrieveIV <- function(IV_all) {
#	IV_all <- IV
	IV_DATA <- list();
	IV_all$DATE <- as.Date(IV_all$DATE)
	IV_IDs <- unique(IV_all$IV_ID)
	for (i in 1:length(IV_IDs)) {
		id <- as.character(IV_IDs[i])
		
		iv <- IV_all[IV_all$IV_ID==id,c("DATE","IV_VALUE")]
		iv <- iv[order(iv$DATE),]
		data <- as.numeric(as.character(iv$IV_VALUE))
		
		IV_DATA[[i]] <- ts(data,start=determineTsStartDate(iv$DATE),frequency=determineTsFrequency(iv$DATE))
		names(IV_DATA)[i] <- as.character(id)
	}
	return(IV_DATA)	
}

transformTS <- function(temp_ts, transform, lag) {
	temp = NULL
	
	if (Frequency != "MONTHLY") {
		if (as.character(transform) == "No transform") {
			temp = temp_ts
		} else if (as.character(transform) == "QoQPercentChange") {
			temp = diff(temp_ts,1)/lag(temp_ts,-1)
		} else if (as.character(transform) == "YoYPercentChange") {
			temp = diff(temp_ts,4)/lag(temp_ts,-4)		
		} else if (as.character(transform) == "QoQDifference") {
			temp = diff(temp_ts,1)
		} else if (as.character(transform) == "YoYDifference") {
			temp = diff(temp_ts,4)
		} else if (as.character(transform) == "1YAverage") {
			temp = (temp_ts+lag(temp_ts,-1)+lag(temp_ts,-2)+lag(temp_ts,-3))/4
		} else if (as.character(transform) == "Log") {
			temp = log(temp_ts)
		} else if (as.character(transform) == "LogDifference") {
			temp = log(temp_ts) - log(lag(temp_ts,-1))
		} else if (as.character(transform) == "QoQoQDifference") {
			temp = diff(diff(temp_ts,1),1)
		}
	} else {
		if (as.character(transform) == "No transform") {
			temp = temp_ts
		} else if (as.character(transform) == "MoMPercentChange") {
			temp = diff(temp_ts,1)/lag(temp_ts,-1)
		} else if (as.character(transform) == "QoQPercentChange") {
			temp = diff(temp_ts,4)/lag(temp_ts,-4)
		} else if (as.character(transform) == "YoYPercentChange") {
			temp = diff(temp_ts,13)/lag(temp_ts,-13)		
		} else if (as.character(transform) == "MoMDifference") {
			temp = diff(temp_ts,1)
		} else if (as.character(transform) == "QoQDifference") {
			temp = diff(temp_ts,4)
		} else if (as.character(transform) == "YoYDifference") {
			temp = diff(temp_ts,14)
		} else if (as.character(transform) == "1YAverage") {
			temp = (temp_ts+lag(temp_ts,-1)+lag(temp_ts,-2)+lag(temp_ts,-3))/4
		} else if (as.character(transform) == "Log") {
			temp = log(temp_ts)
		} else if (as.character(transform) == "LogDifference") {
			temp = log(temp_ts) - log(lag(temp_ts,-1))
		} else if (as.character(transform) == "QoQoQDifference") {
			temp = diff(diff(temp_ts,1),1)
		}
	}
	return(lag(temp,-lag))
}

stationaryTest <- function(data) {
	stats_adf1 = AdfTest(data)
	stats_pp1 = PPTest_tseries(data)
	stats_kpss1 = KPSSTest_tseries(data)
	if ((stats_adf1|stats_pp1)&stats_kpss1) {
		return("")
	}
	else {
		return("Fail")
	}
}

getNonStationaryTSNames <- function(output=TRUE,QUARTERLY=NA) {
	file_name <- paste0(Output_IV,"/IV_Stationarity_nolag_MONTHLY.xlsx")
	Stationary_result = read.xlsx(file_name,sheetIndex=1)
	
	IV_names = Stationary_result$IV_NAME
	
	Stationary_result <- data.frame(lapply(Stationary_result, as.character), stringsAsFactors=FALSE)
	colnames(Stationary_result)=gsub("[.]","",colnames(Stationary_result))
# list of independent ivs names to be deleted 
	remove_IV_list = character()
	Num_inv = dim(Stationary_result)[1]                 # number of total independent variables 
	
# independent variables names 
	
	###################################################################################################################
# Unit root tests: ADF,PP,DF-GLS
# Stationary test: KPSS 
# ADF: null: unit root
# PP: null: unit root
# DF-GLS: null: unit root 
# KPSS: null:stationary 
#
	####################################################################
# Test p-values 
# drift
	ADF_drift_pvals = Stationary_result$ADFpval
	PP_drift_pavals = Stationary_result$PPpval
	
#	GLS_drift_pvals = Stationary_result$DFGLSpvaldrift
	Kpss_drift_pvals = Stationary_result$KPSSpval
	
## trend 
#	ADF_trend_pvals = Stationary_result$ADFpvaltrend
#	PP_trend_pvals = Stationary_result$PPpvaltrend
#	
#	Gls_trend_pvals = Stationary_result$DFGLSpvaltrend
#	kpss_trend_pvals = Stationary_result$KPSSpvaltrend
#	
#  Consider both trend and drift cases (if any of them indicate non-stationary,we will consider the variable as nonstaitonary) 
#  For ADF, PP, DF-GLS, remove independet varialbes not reject the null hypothesis (p-value > 0.05)
#  For KPSS test, remove indepent variables reject the null hypothesis (p-value < 0.05)
#  If any of the unit root tests and stationary test not passed (P-value > 0.05 for unit root and p-value < 0.05 for stationary)
#  we consider the variable to be nonstatioanry 
	for(i in 1:Num_inv){
#	i <- 1
		if(((!is.na(ADF_drift_pvals[i]) & ADF_drift_pvals[i] > 0.05) & (!is.na(PP_drift_pavals[i]) & PP_drift_pavals[i] > 0.05 )) | (!is.na(Kpss_drift_pvals[i]) & Kpss_drift_pvals[i] < 0.05)) {
			#print(IV_names[i])
			if(!IV_names[i] %in% remove_IV_list)
			{
				remove_IV_list = c(remove_IV_list,as.character(IV_names[i]))
			}
		} 
	}
		
	##### Rules for lags 
	## If a variable with lag 0 was considered as nonstatioanry above the this variable with lag 1 and lag 2 will be considered nonstationary even they might pass 
	## all the tests (same time series with less data)
	## If a variable with lag 1 or lag 2 was considered as nonstatioanry and this varialbe with lag 0 passed all the tests, we will keep the varialbe with lag 0 as 
	## statioanry and discard lag 1 or lag 2
	## variables with lag 0 non-stationary 
	#non_stationary_list = character()
	#for(i in 1:Num_inv)
	#{
	#	name = IV_names[i]
	#	name_vec = strsplit(name,"|",fixed = T)   # split the name by "|"
	#	lag = name_vec[[1]][3]       # "L0","L1","L2"
	#	
	#	name_lag_1 = paste(name_vec[[1]][1],"|",name_vec[[1]][2],"|","L1",sep = "")
	#	name_lag_2 = paste(name_vec[[1]][1],"|",name_vec[[1]][2],"|","L2",sep = "")
	#	
	#	if(lag == "L0" & name %in% remove_IV_list)
	#	{
	#		# save the 0 lag variabels that nonstationary 
	#		var_name = paste(name_vec[[1]][1],"|",name_vec[[1]][2],sep = "")
	#		non_stationary_list = c(non_stationary_list,var_name)
	#		
	#		# related 1 lag and 2 lag
	#		if(!name_lag_1 %in% remove_IV_list)
	#		{
	#			remove_IV_list = c(remove_IV_list,name_lag_1)
	#		}
	#		
	#		if(!name_lag_2 %in% remove_IV_list)
	#		{
	#			remove_IV_list = c(remove_IV_list,name_lag_2)
	#		}
	#		
	#	}
	#}
	#
	#
	#if(length(remove_IV_list) == 0)
	#{
	#	saved_IV_list = IV_names
	#}else{
	#	saved_IV_list = IV_names[!(IV_names %in% remove_IV_list)]
	#}
	## 
	#
	#write.csv(remove_IV_list,file = "RData\\Data Generated\\removed IVs(stationary).csv")
	#write.csv(saved_IV_list,file = "RData\\Data Generated\\saved IVs(stationary).csv")
	#
	#
	if (output) {
		file_name <- paste(Output_root,"/NonStationaryIVList.xlsx",sep="")
		write.xlsx(remove_IV_list,file_name)
	}
	return(remove_IV_list)	
}

white_test <- function (lmobj) 
{
	stopifnot(class(lmobj) == "lm")
	mydata <- lmobj$model
	mydata[, 1] <- lmobj$residual^2
	
	fml <- lmobj$call$formula
	
	pvs <- attr(lmobj$terms, "term.labels")
	k <- length(pvs)
	n <- length(lmobj$fit)
	for (i in 1:k) {
		tmp <- NULL
		if (substr(pvs[i], 1, 2) == "I(") {
			tmp2 <- substr(pvs[i], 3, nchar(pvs[i]) - 1)
		}
		else {
			tmp2 <- pvs[i]
		}
		for (j in 1:nchar(tmp2)) {
			tmp1 <- substr(tmp2, j, j)
			if (tmp1 == ":") 
				tmp <- paste(tmp, "*", sep = "")
			else tmp <- paste(tmp, tmp1, sep = "")
		}
		pvs[i] <- tmp
	}
	formula2 <- paste(fml[2], fml[1])
	for (i in 1:k) {
		if (i > 1) 
			formula2 <- paste(formula2, "+", sep = "")
		formula2 <- paste(formula2, "I(", pvs[i], ")", sep = "")
		for (j in i:k) formula2 <- paste(formula2, "+I(", pvs[i], 
					"*", pvs[j], ")", sep = "")
	}
	out <- lm(as.formula(formula2), data = mydata)
	if (summary(out)$r.squared == 1) {
		RVAL <- NULL
		warning("Test failed.  Possible reasons:\n\t (1) collinearity, or (2) sample size is not big enough for the White's test.")
	}
	else {
		LM = summary(out)$r.squared * n
		names(LM) <- "White"
		df <- out$rank - 1
		names(df) <- "df"
		RVAL <- list(statistic = LM, parameter = df, method = "White test for constant variance", 
				p.value = pchisq(LM, df, lower.tail = FALSE), data.name = NULL)
		class(RVAL) <- "htest"
	}
	return(RVAL)
}


diffToLevel <- function(level,EOP=0,Floor=NULL) {
#	level <- na.omit(ts_to_plot[,2]*multiplier)
#	EOP <- pc_level_SOP
#	Floor <- min_history
	#function: convert differenced vectors to original level value with floored values
	#parameter: 
	#return: 
	
	if (is.null(Floor)) {
		for (j in 2:length(level)) {
			level[j] <- level[j-1]+level[j]
		}
		return(level+as.numeric(EOP))
	} else {
		level_result <- c();
		last_period <- as.numeric(EOP);
		for (j in 1:length(level)) {
#			j<-2
			this_period <- last_period + level[j]
			if (this_period < Floor) {
				level_result <- c(level_result,Floor)
			} else {
				level_result <- c(level_result,this_period)
			}
			last_period <- this_period
		}
		return(level_result)		
	}
}

generateVarList <- function(Ind_var_clean) {
	#Number of variables from which to choose
	Num_IV <- ncol(Ind_var_clean)
	Name_IV <- colnames(Ind_var_clean)
	
	if (!all(c("IV_ID","IV_NAME","IV_GROUP")%in%colnames(IV_ID))){
		print(columns(IV_ID))
		print(c("IV_ID","IV_NAME","IV_GROUP"))
		stop("error: table IV_ID does not have correct columns")
	}
	
	IV_ID_Dict <- IV_ID[IV_ID$IV_NAME%in%colnames(Ind_var_clean),]
	for (j in 1:ncol(IV_ID_Dict)) {
		IV_ID_Dict[,j] <- as.character(IV_ID_Dict[,j])
	}
	
	Combinations2 <- t(combn(Name_IV, 2))
	Combinations2 <- cbind(Combinations2,rep(NA,nrow(Combinations2)))
	Combinations <- Combinations2
	
	cor_vec <- numeric(nrow(Combinations))
	group_vec <- logical(nrow(Combinations))
	csi_vec <- logical(nrow(Combinations))
	
	IV_Group <- list()
	IV_CSI <- list()
	for (i in 1:nrow(IV_ID_Dict)) {
		IV_Group[[i]] <- IV_ID_Dict$IV_GROUP[i]
		IV_CSI[[i]] <- IV_ID_Dict$IV_ID[i]
	}
	names(IV_Group) <- IV_ID_Dict$IV_NAME
	names(IV_CSI) <- IV_ID_Dict$IV_NAME
	
	for (i in 1:nrow(Combinations)) {
		tmp_data <- Ind_var_clean[,na.omit(Combinations[i,])]
		cor_vec[i] = max(abs(cor(tmp_data,use="pairwise.complete.obs")- diag(ncol(tmp_data))))
		group_vec[i] <- any(duplicated(IV_Group[Combinations[i,]]))
		csi_vec[i] <- any(duplicated(IV_CSI[Combinations[i,]]))
	}

	# two variables should have correlation less than 0.7, and not belonging to the same group
	return(t(Combinations[cor_vec<=0.7&!group_vec&!csi_vec,]))
}
