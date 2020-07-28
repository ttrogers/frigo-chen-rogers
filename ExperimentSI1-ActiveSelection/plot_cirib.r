function(d, x=NA, genplot=F, color = 2, y.lim = c(100,200), zthresh = 2.5){
#This function plots the column means and confidence intervals of a data matrix as a ribbon plot
#===========


	npts <- dim(d)[2]                #number of data points
	if(is.na(x)) x <- c(1:npts)      #set x values if not specified as argument

	nsjs <- dim(d)[1]                #number of subjects
	mn <- colMeans(d, na.rm=T)       #compute column means across rows
	cis<-rep(0, times = npts)        #initialize vector to hold cis

	for(i1 in c(1:npts)){
		currpt <- d[,i1]
		if(!is.na(zthresh)) currpt[abs(zscore(currpt)) > zthresh] <- NA
		nvalid <- nsjs - sum(is.na(d[,i1]))   #How many valid entries?
		tval <- qt(0.975,nvalid)	      #t value for df = nvalid
		cis[i1] <- tval * (sqrt(var(currpt, na.rm=T))/sqrt(nvalid)) #Store CI as tval * standard error
		}
		
	#If the plot is to be generated:
	if(genplot) plot(0,0,xlim = range(x), ylim = y.lim, type="n", ylab="Boundary estimate", xlab="Window")

	points(x, mn, col = color)        #add points
	polygon(c(x,x[npts:1]), c(mn-cis, c(mn+cis)[npts:1]), col=hsv((color-2)/3,1,1,.5), border=NA)
}