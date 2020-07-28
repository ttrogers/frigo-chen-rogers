plot.cis<-function(d, pcol=1, x=NA, y.lab="", y.lim = NA){
    #This function computes mean and 95% confidence intervals for each column of a matrix
    #then plots these as filled circles and error bars
    #d: matrix of data
    #pcol: plotting color vector
    #x: which rows of data should be used, use all as default
    #y.lab: Label for y axis
    
    nitems<-dim(d)[1] #number of items
    nvars <- dim(d)[2] #number of variables
    if(is.na(x[1])) x <- c(1:nvars)  #If x values are not specified, just use 1:nvars
    
    mns <- colMeans(d, na.rm=T)
    serr <- rep(0, times=nvars)  #holder for standard error
    for(i in c(1:nvars)){
        serr[i] <-sqrt(var(d[,i], na.rm=T))/sqrt(nitems-sum(is.na(d[,i]))) #serr
        }
    ci <- serr * 1.96
    if(is.na(y.lim[1])) y.lim <- c(min(mns-ci), max(mns+ci)) * 1.1  #y limit for plot
    x.lim <- c(min(x) - 0.1*(max(x)-min(x)), max(x) + 0.1*(max(x)-min(x)))
    
    plot(x, mns, pch=16, cex = 2, col=pcol, ylim = y.lim, xlim = x.lim, xaxt="n", xlab="", ylab=y.lab)
    arrows(x,mns - ci,x, mns + ci, angle=90, length=.1, code=3, col = pcol)
    box()
}
