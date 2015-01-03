rankhospital<-function(state, outcome, num = "best") {
        data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data<-data[,c("Hospital.Name","State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
        OK_outcome<-c("heart attack","heart failure", "pneumonia")
        OK_state<-unique(data[,"State"])
        OK_num<-c(seq(1:length(OK_state)),"best","worst")
        if (state %ni% OK_state) stop("invalid state")
        if (outcome %ni% OK_outcome) stop("invalid outcome") 
        if (outcome=="heart attack") index<-3
        if (outcome=="heart failure") index<-4
        if (outcome=="pneumonia") index<-5
        state_data<-subset(data,data$State==state)
        state_data[,index]<-as.numeric(state_data[,index])   
        rank_order<-state_data[order(state_data[,index],state_data$Hospital.Name),]
        rank_order<-rank_order[which(rank_order[,index] != "NA"),]
        if (num == "best") num<-as.numeric(1)
        if (num == "worst") num<-as.numeric(length(rank_order[,index]))
        rank_column<-seq_along(rank_order[,index])
        rank_order<-cbind(rank_order,rank_column)
        output<-rank_order[num,1]
        if (num %ni% OK_num) output<-NA
        output      
}

