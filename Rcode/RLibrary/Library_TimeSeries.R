# 
# 
###############################################################################

require(xts)
library(ggfortify)

determineTsFrequency <- function(dates) {
#	dates <- as.Date(as.yearqtr(index(data_out)))
	#function: determine the frequency of dates.
	#parameter: dates is a vector of Date objects with sorted values.
	#return: 1 if date is annually, 5 quarterly, 12 monthly.
	
	if (length(dates)==1) {
		return(4)
	}
	dates <- as.Date(dates)
	len <- (tail(dates,1) - dates[1])/(length(dates)-1)
	
	if(len < 100 & len>80) {
		return(4)
	}else if(len <35&len>25){
		return(12)
	}else if(len < 2){
		return(1)
	}else{
		stop("ERROR: cannot determine the frequency of time series")
	}
}

determineTsStartDate <- function(dates,freqy=NULL) {
	#function: determine the start date.
	#parameter: dates is a vector of Date objects
	#return: c(year, month)
	
	dates <- as.Date(dates)
	start_date = dates[1];
	if (is.null(freqy)){
		freq = determineTsFrequency(dates)
	} else {
		freq = freqy
	}
	year = as.numeric(substring(as.Date(start_date),1,4))
	month = as.numeric(substring(as.Date(start_date),6,7))
	if (freq == 4) {
		if(month <4)
			return(c(year,1))
		if(month <7)
			return(c(year,2))
		if(month <10)
			return(c(year,3))
		if(month <13)
			return(c(year,4))
	}else if(freq == 12) {
		return(c(year,month))
	}
}

ts_time_to_date <- function(ts) {
	if (class(ts) != "ts") {
		stop("**ts_time_to_date need ts as inputs.")
	}
	
	if (MONTHLY_VERSION) {
		return(as.Date(as.yearmon(time(ts))+1/12)-1)
	} else {
		return(as.Date(as.yearqtr(time(ts))+0.25)-1)
	}
}


tsPlot <- function(ts_to_plot,dep=NULL,leg_names=NULL,subtitle=NULL) {
	if (class(ts_to_plot)[1] == "ts") {
		ts.plot(ts_to_plot,type="b",main=paste(dep,paste(round(range(index(na.omit(ts_to_plot))),2),collapse="-")))
		if (!is.null(leg_names))
			legend("topleft",leg_names,col=1,pch=1)
	}else {
		ts.plot(ts_to_plot ,col=1:ncol(ts_to_plot),type="b",main=paste(dep,paste(round(range(index(na.omit(ts_to_plot[,1]))),2),collapse="-")))
		if (!is.null(leg_names))
			legend("topleft",leg_names,col=1:ncol(ts_to_plot),pch=1)
		if (!is.null(subtitle))
			title(sub=subtitle)
	}
		
}

tsPlotGGplot <- function(data_tmp,dep_name,combined = FALSE,subtitle="") {
	require(ggplot2)
	require(reshape)
	dat <- melt(data.frame(time=as.numeric(time(data_tmp)), data_tmp), id.vars="time")
	if (combined) {
		ggplot(dat, aes(time, value)) + geom_line(aes(colour=variable)) + labs(title=dep_name,subtitle=subtitle) + theme(legend.position = "bottom")
	} else {
		ggplot(dat, aes(time, value)) + geom_line(aes(colour=variable)) + facet_grid(variable ~ .,scales="free_y")+ labs(title=dep_name,subtitle=subtitle) + theme(legend.position = "bottom")
	}
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
	library(grid)
	
	# Make a list from the ... arguments and plotlist
	plots <- c(list(...), plotlist)
	
	numPlots = length(plots)
	
	# If layout is NULL, then use 'cols' to determine layout
	if (is.null(layout)) {
		# Make the panel
		# ncol: Number of columns of plots
		# nrow: Number of rows needed, calculated from # of cols
		layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
				ncol = cols, nrow = ceiling(numPlots/cols))
	}
	
	if (numPlots==1) {
		print(plots[[1]])
	} else {
		# Set up the page
		grid.newpage()
		pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
		
		# Make each plot, in the correct location
		for (i in 1:numPlots) {
			# Get the i,j matrix positions of the regions that contain this subplot
			matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
			
			print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
							layout.pos.col = matchidx$col))
		}
	}
}