rankall<-function(outcome, num = "best") {
        data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data<-data[,c("Hospital.Name","State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
        OK_outcome<-c("heart attack","heart failure", "pneumonia")
        if (outcome %ni% OK_outcome) stop("invalid outcome") 
        if (outcome=="heart attack") index<-3
        if (outcome=="heart failure") index<-4
        if (outcome=="pneumonia") index<-5
        data[,index]<-as.numeric(data[,index])
        OK_num<-c(seq(1:length(data[,index])),"best","worst")
        split_data<-split(data,data$State)       
        sorted_data<-lapply(names(split_data),function (x) split_data[[x]] [order(split_data[[x]] [,index], split_data[[x]] [,1] ), ])
        if (num == "best") position<-as.numeric(1)
        else position<-num
        list<-lapply(1:length(sorted_data), function(x) sorted_data[[x]] [position,1])
        output<-as.data.frame(cbind(list,names(split_data)))
        colnames(output)<-c("hospital","state")
        output
}

