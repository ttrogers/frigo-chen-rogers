function(data){
#This data computes slopes, intercepts, boundaries, and shifts for
#two grid tests in the crazy beliefs study.
#
# data should be a matrix containing data from two grid tests as follows:
#Subject	TrialNo	Ttype	CookRaw	FarSrc	Pic	Response	RT
#1	1	Grid1	C	L	100	1	4009
#1	2	Grid1	C	L	80	1	1503
#
#Function returns a matrix where each row is a subject and fields note
#experiment conditions and the decision curves for each subject in each
#grid test.

sjs<-unique(data$Subject)  #make a vector contaning all the unique subject numbers
nsjs<-length(sjs)

t1 <- matrix(0, nsjs, 2)	#create two temporary matrices to hold
t2<-t1				#test 1 and test 2 paramter estimates

#create temporary matrix to hold other data

cond<-cbind(sjs, rep("E", times = nsjs), rep("E", times = nsjs))


#Loop over subjects to get logistic parameters estimates for 
#test 1 and test 2
for(i1 in c(1:nsjs)){
	sdat <- subset(data, data$Subject==sjs[i1] & data$Ttype=="Grid1") 	#pull out grid 1 data for current subject
	tmod1<- glm(Response~Pic, data = sdat, family="binomial") 		#fit test 1 model
	sdat <- subset(data, data$Subject==sjs[i1] & data$Ttype=="Grid2") 	#pull out grid 2 data for current subject
	tmod2<- glm(Response~Pic, data = sdat, family="binomial") 		#fit test 2 model
	t1[i1,]<-tmod1$coefficients 					#store test 1 coefficients
	t2[i1,]<-tmod2$coefficients					#store test 2 coefficients
	sdat[,4]<-as.character(sdat[,4])					#change conditions from factor to string object
	sdat[,5]<-as.character(sdat[,5])
	cond[i1,2:3]<-as.character(sdat[1, 4:5])			#pull out other info about this sj and condition
	}								#and store in cond matrix

b1<- -1 * t1[,1]/t1[,2]		#estimate boundary for test 1
b2<- -1 * t2[,1]/t2[,2]		#estimate boundary for test 2

cond<-as.data.frame(cond)	#convert cond matrix to data-frame object

#out <- cbind(t1,b1,t2,b2)

out<-cbind(cond,t1,b1,t2,b2) 	#glue condition, t1, and t2 matrices together in data frame
names(out)<-c("sno", "axlab","group","t1int","t1slope","t1bound","t2int","t2slope","t2bound") #name columns
bshift <- b2 - b1		#compute shift in boundary
midshift <- bshift		#copy bshift to midshift

#Flip direction for people in right-biased group
#so positive numbers mean toward midline and negative numbers mean
#away from midline:
midshift[out$group=="R"] <- midshift[out$group=="R"] * -1

out<-cbind(out, bshift, midshift) #glue boundary shift data to output dataframe

Zmidshift <- (out$midshift - mean(out$midshift))/sqrt(var(out$midshift))  #compute Zscore for midshift across all conditions
out<-cbind(out, Zmidshift) #glue Zscores onto ouput data frame
out #output result
}
