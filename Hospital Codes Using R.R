# Introduction
The data for this exercise came from the Hospital Compare web site (http://hospitalcompare.hhs.gov) run by the U.S. Department of Health and Human Services. The purpose of the web site is to provide data and information about the quality of care at over 4,000 Medicare-certiﬁed hospitals in the U.S. This dataset essentially covers all major U.S. hospitals. This dataset is used for a variety of purposes, including determining whether hospitals should be ﬁned for not providing high quality care to patients (see http://goo.gl/jAXFX for some background on this particular topic).
A csv file named "outcome-of-care-measures.csv" contains information about 30-day mortality and readmission rates for heart attacks, heart failure, and pneumonia for over 4,000 hospitals.

##1. Finding the best hospital in a state
First objective is to Know which Hospital in a perticular state has minimum mortality rate in case of a perticular disease(Heart attack, Heart failure, Pneumonia. So the function called best that take two arguments: the 2-character abbreviated name of a state and an outcome name. The function reads the outcome-of-care-measures.csv ﬁle and returns a character vector with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome in that state. Write a function called best that take two arguments: the 2-character abbreviated name of a state and an outcome name. The function reads the outcome-of-care-measures.csv ﬁle and returns a character vector with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome in that state.  The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.
. The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.

following is the code:

best<-function(state,outcome){
	data<-read.csv("outcome-of-care-measures.csv",colClasses="character") #colClasses, specify the classes of all columns as character
	temp<-as.data.frame(cbind(data[,7],
							data[,2],
							data[,11],
							data[,17],
							data[,23]))		
	colnames(temp)<-c("state","hospital","heart attack","heart failure","pneumonia")
	if(!state %in% temp[,"state"]){
		stop("Invalid State")
	} else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){	
		stop("Invalid Outcome")
	} else{
		list<-which(temp[,"state"]==state) #returns list of indexes of the rows in which specific state(function augument) lies
		temp2<-temp[list,] #New dataframe that contains the all the row values of specific state
		list2<-as.numeric(temp2[,eval(outcome)])
		minimum<-min(list2,na.rm=TRUE)
		temp3<-temp2[,"hospital"][which(list2==minimum)]   #creating dataframe temp2 which contain names of hopitals which have minimum mortality
		result<-temp3[order(temp3)]
	}
return(result)
}	
		

Following are the example outputs :
> best("TX", "heart failure")
[1] "FORT DUNCAN MEDICAL CENTER"
> best("MD", "heart attack")
[1] "JOHNS HOPKINS HOSPITAL, THE"
> best("MD", "pneumonia")
[1] "GREATER BALTIMORE MEDICAL CENTER"


##2.  Ranking hospitals by outcome in a state
 A function called rankhospital that takes three arguments: the 2-character abbreviated name of a state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (rank). The function reads the outcome-of-care-measures.csv ﬁle and returns a character vector with the name of the hospital that has the ranking speciﬁed by the num argument.

rankhospital<-function(state,outcome,rank="best"){
  path<-"c:/Coursera/R studio/Hospital"
  setwd(path)
  data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  temp<-as.data.frame(cbind(data[,2],data[,7],data[,11],data[,17],data[,23]))
  colnames(temp)<-c("hospital","state","heart attack","heart failure","pneumonia")
  if(!state %in% temp[,"state"]){
    stop("Invalid State Name")
  }else if(!outcome %in% c("heart attack","heart failure","pneumonia")){
    stop("Invalid Outcome")
  }else if(is.numeric(rank)){
    list<-which(temp[,"state"]==state)
    temp2<-temp[list,]
    temp2[,eval(outcome)]<-as.numeric(temp2[,eval(outcome)])
    temp2<-temp2[order(temp2[,eval(outcome)],temp2[,"hospital"]),]
    result<-temp2[,"hospital"][rank]
    
  }else if(!is.numeric(rank)){
    if(rank=="best"){
      result<-best(state,outcome)
    }else if(rank=="worst"){
      list<-which(temp[,"state"]==state)
      temp2<-temp[list,]
      temp2[,eval(outcome)]<-as.numeric(temp2[,eval(outcome)])
      temp2<-temp2[order(ts[,eval(outcome)],temp2[,"hospital"],decreasing=TRUE),]
      result<-temp2[1,"hospital"]
    }else{
      stop("Invalid Rank")
    
    }
  }
return(result)
  
}

Followings are output:
 rankhospital("TX", "heart failure", 4)
[1] "DETAR HOSPITAL NAVARRO"
> rankhospital("MD", "heart attack", "worst")
[1] "HARFORD MEMORIAL HOSPITAL"



##3. Ranking Hospitals in all States
 A function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking (rank). The function reads the outcome-of-care-measures.csv ﬁle and returns a 2-column data frame containing the hospital in each state that has the ranking specified in rank.
Code: 

rankall<-function(outcome,rank="best"){
  path<-"c:/Coursera/R studio/Hospital"
  setwd(path)
  resultframe<-data.frame(State=character(),Hospital=character())
  data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  temp<-as.data.frame(cbind(data[,2],data[,7],data[,11],data[,17],data[,23]))
  unique_state<-unique(data$State)
  colnames(temp)<-c("hospital","state","heart attack","heart failure","pneumonia")
  if(!outcome %in% c("heart attack","heart failure","pneumonia")){
    stop("Invalid outcome")
  }else{
    for(i in unique_state){
      ranker<-rankhospital(i,outcome,rank)
      resultframe<-rbind(resultframe,c(i,ranker))
  }
    
  }
  return(resultframe)
}




