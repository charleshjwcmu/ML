# 
# 
###############################################################################
#	function tsPlot require Library_TimeSeries
library(egcm)

AdfTest <- function(data,pthreshold = 0.05,simpleoutput=TRUE) {
	require(tseries)
	data <- na.omit(data)
	test <- adf.test(data, alternative = "stationary")
	if (simpleoutput) {
		return(test$p.value < pthreshold)
	} else{
		return(test)
	}
}

AdfTest_urdf <- function(data,simpleoutput=TRUE) {
	require(urca)
	data <- na.omit(data)
	test <- ur.df(data, type = "none");
	print(summary(test))
	if (simpleoutput) {
		return(test@teststat<test@cval[2])
	} else {
		return(test)
	}
}

PPTest_tseries <- function(data, pthreshold = 0.05,simpleoutput=TRUE) {
	test <- pp.test(data, alternative = "stationary")
	if (simpleoutput) {
		return(test$p.value<pthreshold)
	} else {
		return(test)
	}
}

KPSSTest_tseries <- function(data, pthreshold = 0.05,simpleoutput=TRUE) {
	KPSSstationarity = kpss.test(data, null = "Level")
	if (simpleoutput) {
		return(KPSSstationarity$p.value>pthreshold)
	} else {
		return(KPSSstationarity)
	}
}


KPSSTest <- function(data, pthreshold = 0.05,simpleoutput=TRUE) {
	require(urca)
	res_len = length(data)
	
	if (res_len < 50) {
		KPSSstationarity = ur.kpss(data, type = "mu", lags = "short") #Use Schwart's l4
	} else {
		KPSSstationarity = ur.kpss(data, type = "mu", lags = "long")
	}
	KPSStest_stat = KPSSstationarity@teststat[1]
	KPSScval = KPSSstationarity@cval[1,]
	stats_kpss = approx(x = KPSScval, y =c(0.1,0.05,0.025, 0.01), xout = KPSStest_stat, method = "linear", rule = 2)$y
	
	if (simpleoutput) {
		return(stats_kpss>pthreshold)
	} else {
		return(KPSSstationarity)
	}
}

POTest <- function(data, pthreshold = 0.05) {
	require(tseries)
	data <- na.omit(data)
	return(po.test(data)$p.value < pthreshold)
}

JHTest <- function(data) {
	require(urca)
	tryCatch(
			{
				joh <- ca.jo(data,type="trace",K=2,ecdet="none",spec="longrun");
				return(any(joh@teststat[2] > joh@cval[2,]))
			}, error = function(e) {
				print(e);
				return(-1)
			}, finally = {}
	)
}

DWTest <- function(fit, pthreshold = 0.05,simpleoutput=TRUE) {
	require(lmtest)
	test = dwtest(fit,alternative= "two.sided")
	print(paste0("DW test's statistic of is ",test$statistic))
	print(paste0("DW test's p value of is ",test$p.value))
	if (simpleoutput) {
		return(test$p.value>pthreshold)
	} else {
		return(test)
	}
}

BPTest <- function(fit, pthreshold = 0.05,simpleoutput=TRUE) {
	require(lmtest)
	tryCatch({
		test = bptest(fit)
		if (simpleoutput) {
			return(test$p.value>pthreshold)
		} else {
			return(test)
		}
	},error=function(cond){
		return(NA)
	})
}

ShapiroTest <- function(fit, pthreshold = 0.05,simpleoutput=TRUE) {
	test = shapiro.test(fit$residuals)
	print(paste0("Shapiro test's statistic of is ",test$statistic))
	print(paste0("Shapiro test's p value of is ",test$p.value))
	if (simpleoutput) {
		return(test$p.value>pthreshold)
	} else {
		return(test)
	}
}

JarqueBeraTest <- function(fit, pthreshold = 0.05,simpleoutput=TRUE) {
	test = jarque.bera.test(fit$residuals)
	print(paste0("JarqueBera test's statistic of is ",test$statistic))
	print(paste0("JarqueBera test's p value of is ",test$p.value))
	if (simpleoutput) {
		return(test$p.value>pthreshold)
	} else {
		return(test)
	}
}

BGTest <- function(fit, pthreshold = 0.05,simpleoutput=TRUE) {
	test = bgtest(fit$residuals)
	print(paste0("BG test's statistic of is ",test$statistic))
	print(paste0("BG test's p value of is ",test$p.value))
	if (simpleoutput) {
		return(test$p.value>pthreshold)
	} else {
		return(test)
	}
}

diagnostic_test_suit <- function(fit) {
	Tests <- as.data.frame(t(as.matrix(rep(FALSE,2))))
	colnames(Tests) <- c("DW","BP")
	Tests["DW"] <- DWTest(fits[[i]])
	tryCatch({
		Tests["BP"] <- BPTest(fits[[i]])
	},error = function(cond){
	})
	return(Tests)
}

diagnostic_test_for_fits <- function(fits) {
	stationarity_indep_adf <- matrix(rep("",3*length(fits)),ncol=3)
	colnames(stationarity_indep_adf) <- c("ADF_Indep_statistic","ADF_Indep_criticalvalue","ADF_Indep_Flag")
	stationarity_indep_kpss <- matrix(rep("",3*length(fits)),ncol=3)
	colnames(stationarity_indep_kpss) <- c("KPSS_Indep_statistic","KPSS_Indep_criticalvalue","KPSS_Indep_Flag")
	
	stationarity_dep_adf <- matrix(rep("",3*length(fits)),ncol=3)
	colnames(stationarity_dep_adf) <- c("ADF_Dep_statistic","ADF_Dep_criticalvalue","ADF_Dep_Flag")
	stationarity_dep_kpss <- matrix(rep("",3*length(fits)),ncol=3)
	colnames(stationarity_dep_kpss) <- c("ADF_Dep_statistic","ADF_Dep_criticalvalue","ADF_Dep_Flag")
	
	coefficients <- matrix(rep("",5*length(fits)),ncol=5)
	colnames(coefficients) <- rep("Coefficients",5)
	pvalues <- matrix(rep("",5*length(fits)),ncol=5)
	colnames(pvalues) <- rep("Pvalues",5)
	pvaluesflags <- matrix(rep("",5*length(fits)),ncol=5)
	colnames(pvaluesflags) <- rep("SignificanceFlag",5)
	rsquare <- matrix(rep("",1*length(fits)),ncol=1)
	colnames(rsquare) <- c("adj_R_square")
	
	dwtest <- matrix(rep("",3*length(fits)),ncol=3)
	colnames(dwtest) <- c("DWTest_statistic","DWTest_criticalvalue","DWTest_Flag")
	
	bptest <- matrix(rep("",3*length(fits)),ncol=3)
	colnames(bptest) <- c("DWTest_statistic","DWTest_criticalvalue","BPTest_Flag")
	
	stationarity_residual_adf <- matrix(rep("",3*length(fits)),ncol=3)
	colnames(stationarity_residual_adf) <- c("ADF_Residual_statistic","ADF_Residual_criticalvalue","ADF_Residual_Flag")
	stationarity_Residual_kpss <- matrix(rep("",3*length(fits)),ncol=3)
	colnames(stationarity_Residual_kpss) <- c("KPSS_Residual_statistic","KPSS_Residual_criticalvalue","KPSS_Residual_Flag")
	
	aic <- matrix(rep("",1*length(fits)),ncol=1)
	colnames(aic) <- "AIC"
	
	for (i in 1:length(fits)) {
		#	i <- 10	
		test <- AdfTest_urdf(fits[[i]]$model[,1],simpleoutput=FALSE)
		if(test@teststat<test@cval[2]) {flag = "Pass"} else {flag = "Fail"}
		stationarity_indep_adf[i,] <- c(as.numeric(test@teststat),test@cval[2],flag)
		
		test <- KPSSTest(fits[[i]]$model[,1],simpleoutput=FALSE)
		if(test@teststat<test@cval[2]) {flag = "Pass"} else {flag = "Fail"}
		stationarity_indep_kpss[i,] <- c(as.numeric(test@teststat),test@cval[2],flag)
		
		test <- AdfTest_urdf(fits[[i]]$model[,2],simpleoutput=FALSE)
		if(test@teststat<test@cval[2]) {flag = "Pass"} else {flag = "Fail"}
		stationarity_dep_adf[i,] <- c(as.numeric(test@teststat),test@cval[2],flag)
		
		test <- KPSSTest(fits[[i]]$model[,2],simpleoutput=FALSE)
		if(test@teststat<test@cval[2]) {flag = "Pass"} else {flag = "Fail"}
		stationarity_dep_kpss[i,] <- c(as.numeric(test@teststat),test@cval[2],flag)
		
		coefficients[i,1:nrow(summary(fits[[i]])$coefficients)] <- paste(rownames(summary(fits[[i]])$coefficients),round(summary(fits[[i]])$coefficients[,1],2))
		pvalues[i,1:nrow(summary(fits[[i]])$coefficients)] <- paste(rownames(summary(fits[[i]])$coefficients),round(summary(fits[[i]])$coefficients[,4],2))
		
		flags <- rep("",nrow(summary(fits[[i]])$coefficients))
		flags[summary(fits[[i]])$coefficients[,4]<=pthreshold]<-"Signi."
		flags[summary(fits[[i]])$coefficients[,4]>pthreshold]<-"Not Sig."
		pvaluesflags[i,1:nrow(summary(fits[[i]])$coefficients)] <- paste(rownames(summary(fits[[i]])$coefficients),flags)
		
		rsquare[i,1] <- round(summary(fits[[i]])$adj.r.squared,3)
		
		test <- DWTest(fits[[i]],simpleoutput=FALSE)
		if(test$p.value>0.05) {flag = "Pass"} else {flag = "Fail"}
		dwtest[i,] <- c(as.numeric(test$statistic),test$p.value,flag)
		
		tryCatch({
					test <- BPTest(fits[[i]],simpleoutput=FALSE)
					if(test$p.value>0.05) {flag = "Pass"} else {flag = "Fail"}
					bptest[i,] <- c(as.numeric(test$statistic),test$p.value,flag)
				},error = function(cond){})
		
		test <- AdfTest_urdf(resid(fits[[i]]),simpleoutput=FALSE)
		if(test@teststat<test@cval[2]) {flag = "Pass"} else {flag = "Fail"}
		stationarity_residual_adf[i,] <- c(as.numeric(test@teststat),test@cval[2],flag)
		
		test <- KPSSTest(resid(fits[[i]]),simpleoutput=FALSE)
		if(test@teststat<test@cval[2]) {flag = "Pass"} else {flag = "Fail"}
		stationarity_Residual_kpss[i,] <- c(as.numeric(test@teststat),test@cval[2],flag)
		
		aic[i] <- round(AIC(fits[[i]]),2)
	}
	
	results_all <- cbind(stationarity_dep_adf,stationarity_dep_kpss,stationarity_indep_adf,stationarity_indep_kpss,coefficients,pvalues,rsquare,pvaluesflags,
			dwtest,bptest,stationarity_residual_adf,stationarity_Residual_kpss)
	rownames(results_all) <- names(fits)
	
	###Summary Table For Presentation###
	stationarity_dep <- matrix(rep("",1*length(fits)),ncol=1)
	colnames(stationarity_dep) <- c("Stationarity_DepVar")
	stationarity_dep[stationarity_dep_adf[,3]=="Pass"&stationarity_dep_kpss[,3]=="Pass"] = "Pass"
	stationarity_dep[!(stationarity_dep_adf[,3]=="Pass"&stationarity_dep_kpss[,3]=="Pass")] = "Fail"	
	
	stationarity_indep <- matrix(rep("",1*length(fits)),ncol=1)
	colnames(stationarity_indep) <- c("Stationarity_IndepVar")
	stationarity_indep[stationarity_indep_adf[,3]=="Pass"&stationarity_indep_kpss[,3]=="Pass"] = "Pass"
	stationarity_indep[!(stationarity_indep_adf[,3]=="Pass"&stationarity_indep_kpss[,3]=="Pass")] = "Fail"	
	
	stationarity_Residual <- matrix(rep("",1*length(fits)),ncol=1)
	colnames(stationarity_Residual) <- c("Stationarity_Residual")
	stationarity_Residual[stationarity_residual_adf[,3]=="Pass"&stationarity_Residual_kpss[,3]=="Pass"] = "Pass"
	stationarity_Residual[!(stationarity_residual_adf[,3]=="Pass"&stationarity_Residual_kpss[,3]=="Pass")] = "Fail"	
	
	results_summary <- cbind(stationarity_dep,stationarity_indep,stationarity_Residual,rsquare,pvaluesflags,
			dwtest[,3,drop=FALSE],bptest[,3,drop=FALSE],aic)
	rownames(results_summary) <- names(fits)
	
	#separate tables
	results_summary_table1 <- cbind(stationarity_dep,stationarity_indep,stationarity_Residual,rsquare,
			dwtest[,3,drop=FALSE],bptest[,3,drop=FALSE],aic)
	rownames(results_summary_table1) <- names(fits)
	results_summary_table2 <- pvaluesflags
	rownames(results_summary_table2) <- names(fits)
	
	return(list(results_all,results_summary,results_summary_table1,results_summary_table2))
}

retrieveFormula <- function(fit) {
	tmp <- coef(fit)
	return(paste("R^2: ",round(summary(fit)$adj.r.squared,digits=2),"/ ",paste(paste(round(tmp,4),names(tmp),sep=" * "),collapse=" + "),sep=""))
}

ECMmodel_onebeta_level_with_intercept_fit <- function(dependentV,independentV) {
	require(dynlm)
	#	"d(R_t) = beta1*d(r_s,t-) + beta2*d(r_s,t+) + epsilon"
	data <- ts.union(diff(dependentV),dependentV,independentV,diff(independentV))
	fit <- dynlm(diff(dependentV)~diff(independentV)+L(dependentV)+L(independentV),data = data)
	beta1 = summary(fit)$coefficients[2,1]
	beta2 = summary(fit)$coefficients[1,1]
	print("==========")
	subtitle = "d(R_t) = beta1*d(r_s,t-) + beta2*d(r_s,t+) + epsilon"
	print(paste0(subtitle))
	print(paste0("Beta1: ",round(beta1,4)))
	print(paste0("Beta2: ",round(beta2,4)))
	print(summary(fit))
	return(fit)
}

ECMmodel_onebeta_level_without_intercept_fit <- function(dependentV,independentV) {
	require(dynlm)
	#	"d(R_t) = beta1*d(r_s,t-) + beta2*d(r_s,t+) + epsilon"
	data <- ts.union(diff(dependentV),dependentV,independentV,diff(independentV))
	fit <- dynlm(diff(dependentV)~-1+diff(independentV)+L(dependentV)+L(independentV),data = data)
	beta1 = summary(fit)$coefficients[2,1]
	beta2 = summary(fit)$coefficients[1,1]
	print("==========")
	subtitle = "d(R_t) = beta1*d(r_s,t-) + beta2*d(r_s,t+) + epsilon"
	print(paste0(subtitle))
	print(paste0("Beta1: ",round(beta1,4)))
	print(paste0("Beta2: ",round(beta2,4)))
	print(summary(fit))
	return(fit)
}

ECMmodel_twobeta_level_with_intercept_fit <- function(dependentV,independentV) {
#	"d(R_t) = beta1*d(r_s,t-) + beta2*d(r_s,t+) + epsilon"
	require(dynlm)
	varnegative <- varpositive <- diff(independentV)
	varnegative[varnegative>0] <- 0
	varpositive[varpositive<0] <- 0
	data <- na.omit(ts.union(diff(dependentV),varnegative,varpositive,independentV))
	
	fit <- dynlm(diff(dependentV)~varnegative+varpositive+L(dependentV)+L(independentV),data = data)
	beta1 = summary(fit)$coefficients[2,1]
	beta2 = summary(fit)$coefficients[1,1]
	print("==========")
	subtitle = "d(R_t) = beta1*d(r_s,t-) + beta2*d(r_s,t+) + epsilon"
	print(paste0(subtitle))
	print(paste0("Beta1: ",round(beta1,4)))
	print(paste0("Beta2: ",round(beta2,4)))
	print(summary(fit))
	return(fit)
}

ECMmodel_twobeta_level_without_intercept_fit <- function(dependentV,independentV) {
	require(dynlm)
	#	"d(R_t) = beta1*d(r_s,t-) + beta2*d(r_s,t+) + epsilon"
	varnegative <- varpositive <- diff(independentV)
	varnegative[varnegative>0] <- 0
	varpositive[varpositive<0] <- 0
	data <- na.omit(ts.union(diff(dependentV),varnegative,varpositive,independentV))
	
	fit <- dynlm(diff(dependentV)~-1+varnegative+varpositive+L(dependentV)+L(independentV),data = data)
	beta1 = summary(fit)$coefficients[2,1]
	beta2 = summary(fit)$coefficients[1,1]
	print("==========")
	subtitle = "d(R_t) = beta1*d(r_s,t-) + beta2*d(r_s,t+) + epsilon"
	print(paste0(subtitle))
	print(paste0("Beta1: ",round(beta1,4)))
	print(paste0("Beta2: ",round(beta2,4)))
	print(summary(fit))
	return(fit)
}

ARXmodel <- function(dependentV,independentV) {
#  & ARX Model & $R_t = \alpha + \gamma R_{t-1} + \beta_1 r_{s,t} + \epsilon_t$\\
	require(dynlm)
	data <- ts.union(diff(dependentV),dependentV,independentV,diff(independentV))
	fit <- dynlm(dependentV~L(dependentV)+independentV,data = data)
	print(summary(fit))
	return(fit)
}

ARDLmodel <- function(dependentV,independentV) {
#	& ARDL Model & $R_t = \alpha + \gamma R_{t-1} + \beta_1 r_{s,t} + \beta_2 r_{s,t-1}+ \epsilon_t$\\
	require(dynlm)
	data <- ts.union(diff(dependentV),dependentV,independentV,diff(independentV))
	fit <- dynlm(dependentV~L(dependentV)+L(independentV,0:1),data = data)
	print(summary(fit))
	return(fit)
}

Model_DiffDep_DiffIndep_NoIntercept <- function(dependentV,independentV) {
#	& DiffNoIntercept Model& $\Delta R_t = \beta_1\Delta r_{s,t}$
	require(dynlm)
	data <- ts.union(diff(dependentV),dependentV,independentV,diff(independentV))
	fit <- dynlm(d(dependentV)~-1+d(independentV),data = data)
	print(summary(fit))
	return(fit)
}

Model_DiffDep_DiffIndep_WithIntercept <- function(dependentV,independentV) {
#	& Diff Model& $\Delta R_t = \alpha + \beta_1\Delta r_{s,t}$
	require(dynlm)
	data <- ts.union(diff(dependentV),dependentV,independentV,diff(independentV))
	fit <- dynlm(d(dependentV)~d(independentV),data = data)
	print(summary(fit))
	return(fit)
}

Model_DiffDep_DiffPlusIndep_DiffNegIndep_NoIntercept <- function(dependentV,independentV) {
	require(dynlm)
#	"d(R_t) = beta1*d(r_s,t-) + beta2*d(r_s,t+) + epsilon"
	varnegative <- varpositive <- diff(independentV)
	varnegative[varnegative>0] <- 0
	varpositive[varpositive<0] <- 0
	data <- na.omit(ts.union(diff(dependentV),varnegative,varpositive))
	fit <- dynlm(diff(dependentV)~-1+varnegative+varpositive,data = data)
	beta1 = summary(fit)$coefficients[2,1]
	beta2 = summary(fit)$coefficients[1,1]
	print("==========")
	subtitle = "d(R_t) = beta1*d(r_s,t-) + beta2*d(r_s,t+) + epsilon"
	print(paste0(subtitle))
	print(paste0("Beta1: ",round(beta1,4)))
	print(paste0("Beta2: ",round(beta2,4)))
	print(summary(fit))
	return(fit)
}

Model_DiffDep_DiffPlusIndep_DiffNegIndep_WithIntercept <- function(dependentV,independentV) {
	require(dynlm)
#	"d(R_t) = beta1*d(r_s,t-) + beta2*d(r_s,t+) + epsilon"
	varnegative <- varpositive <- diff(independentV)
	varnegative[varnegative>0] <- 0
	varpositive[varpositive<0] <- 0
	data <- na.omit(ts.union(diff(dependentV),varnegative,varpositive))
	fit <- dynlm(diff(dependentV)~varnegative+varpositive,data = data)
	beta1 = summary(fit)$coefficients[2,1]
	beta2 = summary(fit)$coefficients[1,1]
	print("==========")
	subtitle = "d(R_t) = beta1*d(r_s,t-) + beta2*d(r_s,t+) + epsilon"
	print(paste0(subtitle))
	print(paste0("Beta1: ",round(beta1,4)))
	print(paste0("Beta2: ",round(beta2,4)))
	print(summary(fit))
	return(fit)
}


Model_DiffDep_DiffLagIndep_Nontercept <- function(dependentV,independentV,lag=1) {
#	& DiffNoIntercept Model& $\Delta R_t = \beta_1\Delta r_{s,t-1}$
	require(dynlm)
	data <- ts.union(diff(dependentV),dependentV,independentV,diff(independentV))
	fit <- dynlm(d(dependentV)~-1+L(diff(independentV),lag),data = data)
	print(summary(fit))
	return(fit)
}

Model_DiffDep_DiffLagIndep_WithIntercept<- function(dependentV,independentV,lag=1) {
#	& Diff Model& $\Delta R_t = \alpha + \beta_1\Delta r_{s,t-1}$
	require(dynlm)
	data <- ts.union(diff(dependentV),dependentV,independentV,diff(independentV))
	fit <- dynlm(d(dependentV)~L(diff(independentV),lag),data = data)
	print(summary(fit))
	return(fit)
}

Model_DiffDep_DiffLagDep_DiffIndep_NoIntercept <- function(dependentV,independentV,lag=1) {
#	& Diff Model& $\Delta R_t = \beta_1\Delta r_{s,t} + \beta_2\Delta R_{t-1}$
	require(dynlm)
	data <- ts.union(diff(dependentV),dependentV,independentV,diff(independentV))
	fit <- dynlm(d(dependentV)~-1+diff(independentV)+L(diff(dependentV),lag),data = data)
	print(summary(fit))
	return(fit)
}

Model_DiffDep_DiffLagDep_DiffIndep_WithIntercept <- function(dependentV,independentV,lag=1) {
#	& Diff Model& $\Delta R_t = \alpha + \beta_1\Delta r_{s,t} + \beta_2\Delta R_{t-1}$
	require(dynlm)
	data <- ts.union(diff(dependentV),dependentV,independentV,diff(independentV))
	fit <- dynlm(d(dependentV)~diff(independentV)+L(diff(dependentV),lag),data = data)
	print(summary(fit))
	return(fit)
}

Model_DiffDep_DiffLagDep_DiffLagIndep_NoIntercept <- function(dependentV,independentV,lag=1) {
#	& Diff Model& $\Delta R_t = \beta_1\Delta r_{s,t-1} + \beta_2\Delta R_{t-1}$
	require(dynlm)
	data <- ts.union(diff(dependentV),dependentV,independentV,diff(independentV))
	fit <- dynlm(d(dependentV)~-1+L(diff(independentV),lag)+L(diff(dependentV),lag),data = data)
	print(summary(fit))
	return(fit)
}

Model_DiffDep_DiffLagDep_DiffLagIndep_WithIntercept <- function(dependentV,independentV,lag=1) {
#	& Diff Model& $\Delta R_t = \beta_1\Delta r_{s,t-1} + \beta_2\Delta R_{t-1}$
	require(dynlm)
	data <- ts.union(diff(dependentV),dependentV,independentV,diff(independentV))
	fit <- dynlm(d(dependentV)~L(diff(independentV),lag)+L(diff(dependentV),lag),data = data)
	print(summary(fit))
	return(fit)
}

Polyfitmodel <- function(dependentV, DegreeOfFreedom=3) {
	require(forecast)
	independentV <- ts(1:length(dependentV),start=start(dependentV),frequency=frequency(dependentV))
	independentV <- poly(as.vector(independentV),degree=DegreeOfFreedom)
	colnames(independentV) <- paste0("x^",colnames(independentV))
	fit = tslm(dependentV ~ independentV)
	print(summary(fit))
	return(fit)
}

Loessfitmodel <- function(dependentV) {
	independentV <- ts(1:length(dependentV),start=start(dependentV),frequency=frequency(dependentV))
	fit <- loess(dependentV ~ independentV)
	print(summary(fit))
	return(fit)
}

Bsplinefitmodel <- function(dependentV) {
	return(0)
}

plot_fit <- function(fit, fitname=fit$call, dep_name="") {
#	function tsPlot require Library_TimeSeries
#	fit <- fits[[i]]
	dat <- ts.union(fitted(fit),fit$model[1])
	colnames(dat) <- c(fitname,colnames(fit$model)[1])
	dat <- na.omit(dat)
#	tsPlot(dat,leg_names=colnames(dat),dep=dep_name,subtitle=retrieveFormula(fit))
	tsPlotGGplot(dat,dep_name,combined=TRUE,subtitle=retrieveFormula(fit))
}

FitComaprisonAIC <- function(fits) {
	require(AICcmodavg)
	require(plyr)
	require(stringr)
	tmp_aic <- function(mod){ 
#		data.frame(AICc = AICc(mod), AIC = AIC(mod), model = deparse(formula(mod))) 
		data.frame(AICc = AICc(mod), AIC = AIC(mod)) 
	}
	ldply(fits, tmp_aic)
}
