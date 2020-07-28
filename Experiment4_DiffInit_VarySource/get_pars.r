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
    print(i1)
	}								#and store in cond matrix

b1<- -1 * t1[,1]/t1[,2]		#estimate boundary for test 1
b2<- -1 * t2[,1]/t2[,2]		#estimate boundary for test 2


cond<-as.data.frame(cond)	#convert cond matrix to data-frame object

out<-as.data.frame(cbind(cond,t1,b1,t2,b2)) #glue condition, t1, and t2 matrices together in data frame
names(out)<-c("axlab","group","bounds","sno","FaceLeft","t1int","t1slope","t1bound","t2int","t2slope","t2bound") #name columns

bshift <- b2 - b1		#compute shift in boundary
midshift <- bshift		#copy bshift to midshift

#Flip direction for people in right-biased group
#so positive numbers mean toward midline and negative numbers mean
#away from midline:
midshift[out$group=="RB"] <- midshift[out$group=="RB"] * -1

out<-cbind(out, bshift, midshift) #glue boundary shift data to output dataframe

Zmidshift <- (out$midshift - mean(out$midshift))/sqrt(var(out$midshift))  #compute Zscore for midshift across all conditions

#Compute Zscore for midshift separately for each boundary condition
Zmsbycond <- rep(0, times = nitems) #make zero vector to hold data
for(i1 in levels(out$bounds)){
        mscond <- out$midshift[out$bounds==i1] #pull out data for current condition
	Zmsbycond[out$bounds == i1] <- (mscond - mean(mscond))/sqrt(var(mscond))  #compute zscores for current condition
       }
out<-cbind(out, Zmidshift, Zmsbycond) #glue Zscores onto ouput data frame
out #output result
}