function(dvec, zthresh = 2.0){
#This function takes a data vector and returns a logical
#vector indicating which elements have a z-score larger than
#zthresh in either direction.

zvec<-(dvec - mean(dvec))/sqrt(var(dvec)) #compute zscores for dvec
abs(zvec) > zthresh #return logical vectors with a z score larger than zthresh in either direction
}