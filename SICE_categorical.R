# R Code for SICE-Categorical Algorithm
 
setwd("D:/Categorical/25-03-2020")
library(mice)
library(modeest)
library(dplyr)
library(caret)

#taking input
data <- read.csv("HMX.csv")



#creating a copy of the original data
original <- data



#inserting NA
data[sample(1:nrow(data), 14322), "Gender"] <- NA
sapply(data, function(x) sum(is.na(x)))


#calculating index of the missing data
na_index=which(is.na(data$Gender),arr.ind = TRUE)


#original values of the missing data
na_value<-(original$Gender[na_index])
na_value <- as.numeric(na_value)


#mice algorithm

#Declaring a empty vector to collect each row
singleRowSample <- vector()

#convert all columns of data to numeric value for cart, pmm method

start <- Sys.time()
miceResult <- mice(data,m=7,maxit = 5,method = "lda")
end <- Sys.time()

#execution time for mice
mice_ex_time <- end - start

#mice Result
miceResult<- miceResult$imp$Gender
mice_iter<<-miceResult
miceResult <- as.numeric(miceResult)


#mice result accuracy

#random values generated for miceResult
imp_range<- c(seq(1:7))

#empty vector to store the mice result
mice_res<-vector()
for(i in 1:nrow(miceResult))
{
  j = sample(imp_range,1)
  mice_res<-c(mice_res, miceResult[i,j])
}


#calculating accuracy of mice
matched=which(na_value == mice_res)
mice_accuracy<- (length(matched)/length(na_value))*100




#SICE Algorithm

#empty vector to save row-wise most frequent value
SICEResult <- c()

#empty vector to store SICE Result
SICE_res <- vector()

start <- Sys.time()
SICENom <- function(miceResult){
  startTime <- Sys.time()
  numericMiceResult <- miceResult
  
  for(i in 1:nrow(numericMiceResult)){
    singleRowSample <- as.vector(numericMiceResult[i,])
    singleRowSample <- unlist(singleRowSample)
    #modedSampleMOdel<- mlv.factor(singleRowSample)
    #SICEResult[[i]] <- modedSampleMOdel$M[[1]]
    ux<-  unique(singleRowSample)
    SICEResult[i] <- ux[which.max(tabulate(match(singleRowSample,ux)))]
  }
  temp <- as.numeric(SICEResult)
  SICE_res <<- c(SICE_res, temp)
  
}
SICENom(miceResult)

end <- Sys.time()

#calculating SICE execution time
SICE_ex_time <- (end - start) + mice_ex_time


#Accuracy of SICE
matched2 <- which(na_value == SICE_res)
SICE_accuracy = (length(matched2) / length(na_value) ) * 100


print("Accuracy calculated by mice: ")
print(mice_accuracy)
print(mice_ex_time)

print("Accuracy calculated by SICE: ")
print(SICE_accuracy)
print(SICE_ex_time)