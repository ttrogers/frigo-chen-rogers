function (d, sj=1, wsz=38) 
{
	#Given crazy-beliefs data, a subject number, and a window size, this function estimates
	#the location of the category boundary from the subject's responses over the window
	#beginning with the initial grid test and running through the multi-source learning trials
	#d = data list of the form generated by get.data
	#sj = subject index number (ie, the element of d containing the subject's data)
	#wsz = window size, by default set to the number of trials in the initial grid estimate
	g1 <- d[[sj]][[2]][, c(10, 11, 13)]  #Get data for first test grid
	g1 <- g1[order(g1[,1]),2:3]          #order by trial number
    tmp <- d[[sj]][[3]][, c(10, 11, 13)] #get data from multi-source trials
    tmp <- tmp[order(tmp[, 1]), 2:3]     #order by trial number
    tmp <- rbind(g1,tmp)                 #ajoin grid 1 and multi-source trials
    npts <- dim(tmp)[1] - wsz            #Number of data point taking window size into account
    out <- rep(0, times = npts)	         #initialize output vector
    for (i1 in c(1:npts)) {              #for each windows
        tmprange <- c(i1:(i1 + wsz))     #pull out data
        is <- glm(acc ~ Pic, family = "binomial", data = tmp[tmprange, 
            ])$coef                      #fit logistic model
        out[i1] <- -(is[1]/is[2])        #compute boundary and store in output vector
    }
    out
}
