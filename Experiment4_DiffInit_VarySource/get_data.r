function (fname) 
{
	###This code reads in one SJ's data from files formatted as in the CleanRaw directory
	###and puts different phases of the experiment into a list object

	###Pull out subject-specific info from file name
	sinfo<-strsplit(fname, "[_-]")[[1]]  						#Splits file name into chunks by the _ and - characters
	sinfo<-c(paste(sinfo[1], sinfo[2], sep="-"), sinfo[3:5]) 		#Take the parts we want

	##Read data and add to subject info
	tmp<-read.csv(fname, skip = 1, header = T)
    names(tmp)[8:9] <- c("acc", "RT")							#Rename columns 8 and 9 to make shorter
	nitems<-dim(tmp)[1]											#nitems = number of rows in data
	sdat<-matrix(sinfo, nitems, 4, byrow=T)  					#Put sinfo into a matrix with nitems rows
	sdattmp<-as.data.frame(sdat)								#convert to data frame
    for(i1 in c(1:4)) sdattmp[,i1]<-as.character(sdat[,i1])  #convert factors to characters
	sdattmp[,4]<-as.numeric(sdattmp[,4])						#convert sno to numeric
	sdat<-sdattmp
	
	names(sdat)<-c("axlab", "group","sources","sno")			#name columns
	faceassign <- 2 - sdat$sno %% 2								#Note whether subject number is even/odd for face assignment
	sdat<-cbind(sdat, faceassign)								#Add this column to subject data matrix
	names(sdat)[5]<-"FaceLeft"									#Name colun appropriately
	tmp<-cbind(sdat, tmp)							#join subject info to main data

	##Sort out the face data
    faces <- subset(tmp, tmp$Block > 6)				#Pull out face rating trials, store in faces

    tmp <- subset(tmp, tmp$Block < 7)				#Remove those trials from main data frame
    tmp$Pic <- as.numeric(as.character(tmp$Pic))	#Change Pic data to numeric format

	#Splits the data frame into a list with each element containing
	#data from a different phase of the experiment
    out <- list(subset(tmp, tmp$Block == 1), subset(tmp, tmp$Block == 
        2), subset(tmp, tmp$Block == 4), subset(tmp, tmp$Block == 
        5), faces, subset(tmp, tmp$Block == 3 | tmp$Block == 
        6))
	#Order of list elements is:
	#1: initial learning with Captain's feedback
	#2: first grid test
	#3: learning with companion feedback
	#4: second grid test
	#5: face rating data
	#6: explicit boundary data

	#This loop sorts each block of data so items are in increasing order
	#by stimulus number
		for (i1 in c(1:4)) out[[i1]] <- out[[i1]][order(out[[i1]]$Pic), 
        ]
    out	
}
