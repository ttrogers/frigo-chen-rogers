function(dlist, sjs=NA){
#This function reads a list object containing data from a 
#Crazy Beliefs experiment, in the format read in by
#get.data. Each element of the list corresponds to a different
#subject, and contains a sub-list with that subject's data.
#
#This function fits, for each subject in dlist, a logistic curve to
#their test 1 and test 2 performance and stores the parameters, including
#slope, intercept, and boundary, in an output matrix.
#
#dlist is the data list containing subject data
#sjs us a list of subjects to include in the analysis.


if(is.na(sjs)){                #if sjs is not specified, include all sjs
	nitems<-length(dlist)  # and set nitems to the number of subjects
	sjs <- 1:nitems}
else nitems<-length(sjs)

t1 <- matrix(0, nitems,2)	#create two temporary matrices to hold
t2<-t1				#test 1 and test 2 paramter estimates

#create temporary matrix to hold other data
cond<-matrix(c("E","E","E","0","E"), nitems, 5, byrow=T) 

#Loop over subjects to get logistic parameters estimates for 
#test 1 and test 2
for(i1 in c(1:nitems)){
	sdat <- dlist[[sjs[i1]]] #pull out data for current subject
	tmod1<- glm(acc~Pic, data = sdat[[2]], family="binomial") 	#fit test 1 model
	tmod2<- glm(acc~Pic, data = sdat[[4]], family="binomial") 	#fit test 2 model
	t1[i1,]<-tmod1$coefficients 					#store test 1 coefficients
	t2[i1,]<-tmod2$coefficients					#store test 2 coefficients
	cond[i1,]<-as.matrix((sdat[[1]])[1,1:5])			#pull out other info about this sj and condition
    print(paste("Processing sj:", i1))
	}								#and store in cond matrix

b1<- -1 * t1[,1]/t1[,2]		#estimate boundary for test 1
b2<- -1 * t2[,1]/t2[,2]		#estimate boundary for test 2


cond<-as.data.frame(cond)	#convert cond matrix to data-frame object

out<-as.data.frame(cbind(cond,t1,b1,t2,b2)) #glue condition, t1, and t2 matrices together in data frame
names(out)<-c("axlab","noingrp","closebound","sno","FaceLeft","t1int","t1slope","t1bound","t2int","t2slope","t2bound") #name columns

out$closebound <- as.numeric(as.character(out$closebound))  #Change field type to numeric instead of factor
cbside <- rep("left", times = nitems) #initialize close-bound-size vector
cbside[out$closebound > 150] <- "right"  #change side label to right if close bound was above 150
out <- cbind(out[,1:3], cbside, out[4:11])

bshift <- b2 - b1		#compute shift in boundary
alshift <- bshift		#copy bshift to alshift

#Flip direction for subjects with close source at 125
#so positive numbers mean toward pole where source resides (aligned shift)
alshift[out$closebound==125] <- bshift[out$closebound==125] * -1

out<-cbind(out, bshift, alshift) #glue boundary shift data to output dataframe

Zalshift <- (out$alshift - mean(out$alshift))/sqrt(var(out$alshift))  #compute Zscore for alshift across all conditions

#Compute Zscore for alshift separately for each boundary condition
#Zmsbycond <- rep(0, times = nitems) #make zero vector to hold data
#for(i1 in levels(out$bounds)){
#        mscond <- out$alshift[out$bounds==i1] #pull out data for current condition#
#	Zmsbycond[out$bounds == i1] <- (mscond - mean(mscond))/sqrt(var(mscond))  #compute zscores for current condition
#       }
out<-cbind(out, Zalshift) #glue Zscores onto ouput data frame
out <- cbind("one25", out) #Add column to indicate the experiment condition (one close)
names(out)[1]<-"cond"  #name that column
PID <- paste(out$cond, substring(out$axlab,4,4), out$closebound, out$sno, sep="_")  #Make unique participant ID vector
t1dist <- as.numeric(as.character(out$closebound)) - out$t1bound  #Compute distance of t1bound to close source
out <- cbind(PID, out)   #Add PID vector to output matrix
out <- cbind(out, t1dist)   #Add PID vector to output matrix
out <- out[,c(1:3, 5:18)] #Remove the "number in group" field which is always the same
out #output result
}