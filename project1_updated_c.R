#------------------------------------------------------------------------------------#
#                                  Life Inurance Project                             #
#                               Central Washington University                        #
#                            Graduate School of Computer Science                     #
#                          Chao Huang, Hermann Yepdjio, Lubna Alzamil                #      
#                                      Supervised By                                 #
#                               Professor Donald Davendra                            #
#                                            &                                       #
#                               Professor Chin-Mei Chuen                             #
#------------------------------------------------------------------------------------#


#setwd("/media/hermann/Tonpi/tonpi/Collegecourses/CWU/Graduate School/Winter 2019/CS 567/Projects/Project1/3R")
setwd("C:\\Users\\Lubna\\Desktop\\CWU\\Winter2019\\Stat\\ProjectOneTwo\\CS567_Project1")
#setwd("C:/Users/chao_/Desktop/CWU/Courses/Q1 Winter 2019/CS567 Computational Statistics R/Project2/CS567_Project1")
inputsProject1 <- read.delim("project1_inputs.txt", header = TRUE, sep = "\t", dec = ".", stringsAsFactors=FALSE) #read the inputs values from the project1_inputs.txt file
print (inputsProject1)
#this file is to run 

#making life table based on csv files
source("life_table.R")


#install library if not exist
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(reshape)) install.packages('reshape')
library(ggplot2)
library(reshape)
x11()

myGraph <- ggplot(lifeTable, aes(ages, A_x))
myGraph <- myGraph + geom_point() + labs(title = "Ages Vs Ax") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Age") + ylab("Ax")
print(myGraph)
ggsave(filename = "images/A_x Vs Ages.png", plot = myGraph)



inputAges = as.numeric(inputsProject1[inputsProject1$label == "inputAges", c("value")])
inputBenefit = as.numeric(inputsProject1[inputsProject1$label == "inputBenefit", c("value")])
nsp <- lifeTable[lifeTable$ages == inputAges, c("A_x")]*inputBenefit
print(paste("input Ages: ", inputAges))
print(paste("Whole Life Net Single Premium: ", nsp))




#-------------------Calculating Probability of Dead Curve--------------
CalculateFinalAge <- function(inputAge) { 
  
  
  #-------------------bug fixed---------------
  
  #calculate probability of (x) lives t years  t_p_x where x = inputAges
  previousAgeP <- lifeTable[ages == (inputAge - 1), c("t_p_x0")]
  if (inputAge > 0) {
    pLives <- lifeTable[ages >= inputAge, c("t_p_x0")]/previousAgeP
  } else {
    pLives <- lifeTable[ages >= inputAge, c("t_p_x0")]
  }
  
  pLivesAges <- lifeTable[ages >= inputAge, c("ages")]
  pCurveX <- data.frame(pLivesAges, pLives)
  
  #calculate probability tl1_q_x that x survives t years and dies within 1 year
  pCurveXRow <- nrow(pCurveX)
  
  #probability of dead based on input age
  pCurveX$tl1_q_x[1] <- 1 - pCurveX$pLives[1] # firt data important,,, to avoid bug
  if(pCurveXRow > 1) { #bug -fixed,  avoid enter for when nrow(pCurveX) == 0, this occur when inputAge is the last value of the tablelife 
    for (j in 2:(pCurveXRow)) {
      
      #small bug which does not consider the 0|1_q_x data..... also add q_x of 1 in the last year automatically? 
      pCurveX$tl1_q_x[j] <- pCurveX$pLives[j-1] - pCurveX$pLives[j]
      #pCurveX$tl1_q_x[j] <- pCurveX$pLives[j] - pCurveX$pLives[j+1] #i = u in the pdf   1 = t in pdf
    }
  }

  
  #generate random dies based on input age
  finalAge <- 0
  if(pCurveXRow > 1) { #bug -fixed,  avoid enter when nrow(pCurveX) == 0, this occur when inputAge is the last value of the tablelife
    finalAge <- sample(pCurveX$pLivesAges, 1, replace = T, pCurveX$tl1_q_x)
  }else
  {
    finalAge <- pCurveX$pLivesAges[1] # only has 1 probability
  }

  return (finalAge)
}


CalculateFinalAgePerfectData <- function(inputAge) { #---only for testing
  
  #calculate probability of (x) lives t years  t_p_x where x = inputAges
  previousAgeP <- lifeTable[ages == (inputAge - 1), c("t_p_x0")]
  if (inputAge > 0) {
    pLives <- lifeTable[ages >= inputAge, c("t_p_x0")]/previousAgeP
  } 
  else {
    pLives <- lifeTable[ages >= inputAge, c("t_p_x0")]
  }
  
  pLivesAges <- lifeTable[ages >= inputAge, c("ages")]
  pCurveX <- data.frame(pLivesAges, pLives)
  
  #calculate probability tl1_q_x that x survives t years and dies within 1 year
  pCurveXRow <- nrow(pCurveX)
  
  #probability of dead based on input age
  pCurveX$tl1_q_x[1] <- 1 - pCurveX$pLives[1] # firt data important,,, to avoid bug
  for (j in 2:(pCurveXRow)) {
    
    #small bug which does not consider the 0|1_q_x data..... also add q_x of 1 in the last year automatically? 
    pCurveX$tl1_q_x[j] <- pCurveX$pLives[j-1] - pCurveX$pLives[j]
    #pCurveX$tl1_q_x[j] <- pCurveX$pLives[j] - pCurveX$pLives[j+1] #i = u in the pdf   1 = t in pdf
  }
  
  #------generate perfect pCurve data
  perfectData <- vector(mode="numeric", length=inputNumberClients) # it is better to initialize variable to make it faster
  
  bIndex <- 1 #begin index
  fIndex <- 0 #final index
  counter <- 0
  for(k in 1:pCurveXRow){
    
    nRepeat <- pCurveX$tl1_q_x[k]*inputNumberClients
    if(nRepeat%%1 >= 0.5){ # use this to avoid number of data > pCurveXRow
      if(counter%%2 == 0){
        nRepeat <- round(nRepeat)
      }
      else{
        nRepeat <- trunc(nRepeat) # use trunc to avoid number of data > pCurveXRow
      }
      counter <- counter +1
    }
    else{
      nRepeat <- trunc(nRepeat)
    }
    
    fIndex <- bIndex + nRepeat - 1
    perfectData[bIndex:fIndex] <- pCurveX$pLivesAges[k]
    bIndex <- bIndex + nRepeat
  }  

  nFill <- inputNumberClients - fIndex
  #fill residual round values from nData to numberclient 
  perfectData[(fIndex+1):inputNumberClients] <- sample(pCurveX$pLivesAges, nFill, replace = T, pCurveX$tl1_q_x)
  #return a vector
  return(perfectData)
}


CalculateFinalAgeSemiPerfectData <- function(inputAge) { #---only for testing
#calculate probability of (x) lives t years  t_p_x where x = inputAges
previousAgeP <- lifeTable[ages == (inputAge - 1), c("t_p_x0")]
if (inputAge > 0) {
    pLives <- lifeTable[ages >= inputAge, c("t_p_x0")]/previousAgeP
  } 
else {
    pLives <- lifeTable[ages >= inputAge, c("t_p_x0")]
  }
  
  pLivesAges <- lifeTable[ages >= inputAge, c("ages")]
  pCurveX <- data.frame(pLivesAges, pLives)
  
  #calculate probability tl1_q_x that x survives t years and dies within 1 year
  pCurveXRow <- nrow(pCurveX)
  
  #probability of dead based on input age
  pCurveX$tl1_q_x[1] <- 1 - pCurveX$pLives[1] # firt data important,,, to avoid bug
  for (j in 2:(pCurveXRow)) {
    
    #small bug which does not consider the 0|1_q_x data..... also add q_x of 1 in the last year automatically? 
    pCurveX$tl1_q_x[j] <- pCurveX$pLives[j-1] - pCurveX$pLives[j]
    #pCurveX$tl1_q_x[j] <- pCurveX$pLives[j] - pCurveX$pLives[j+1] #i = u in the pdf   1 = t in pdf
  }
  
  #note: returns a vector
  bFAge <- sample(pCurveX$pLivesAges, inputNumberClients, replace = T, pCurveX$tl1_q_x)
  return(bFAge)
}






#----------now creating a block of 10000 people who are in diffrent ages and want diffrent benefits


BussinessBlock <- function(rAge,rBenefit) { 
  netSinglePremium <- lifeTable[lifeTable$ages == rAge, c("A_x")]*rBenefit
  return(netSinglePremium)
}

inputNumberClients <- as.numeric(inputsProject1[inputsProject1$label == "inputNumberClients", c("value")])
bAge <- vector(mode="integer", length=inputNumberClients) #initialize variables
bBen <- vector(mode="integer", length=inputNumberClients)
bNps <- vector(mode="numeric", length=inputNumberClients)
bFAge <- vector(mode="integer", length=inputNumberClients)
maxAges <- max(lifeTable$ages)
lifeTableAges <- lifeTable$ages[ages < maxAges]# avoid picking max ages



#---------------------- looping 10000 -------------------------------------
lifeTableAges <- data.frame(Age=ages) # creating a data frame that contains the ages

print("---------Calculating Lifetimes-----------")
print(paste("Number of clients: ", inputNumberClients))
lifeTableAsVector = as.vector(lifeTable[1,], mode = 'numeric') #create a vector representation of lifeTable so I can pass it to the c function
for (i in 2:lifeTableTotalRow)
{
  lifeTableAsVector = c(lifeTableAsVector, as.vector(lifeTable[i,], mode = 'numeric'))
}
dim = c(length(lifeTableAges$Age), 10, length(lifeTableAges$Age)) 
dyn.load("c_code.dll")
res = .C("for_loop", lifeTable=as.numeric(lifeTableAsVector), dim = as.integer(dim), bAge = as.integer(bAge), bBen=as.integer(bBen), bNps = as.numeric(bNps), bFAge = as.integer(bFAge), lifeTableAges = as.integer(lifeTableAges$Age), inputNumberClients =as.integer(inputNumberClients))
bFAge = res[6]$bFAge
bAge = res[3]$bAge
bBen= res[4]$bBen
bNps= res[5]$bNps


bDataFrame <- data.frame(Age = bAge, Benefit = bBen, NetSinglePremium = bNps, Die = bFAge) #Creating the final dataframe
bDataFrame$SurviveYears <- bDataFrame$Die - bDataFrame$Age
write.table(bDataFrame, file="BusinessData.csv", row.names=F, col.names=T, append=F, sep=",")


paymentTable <- bDataFrame[c("SurviveYears", "Benefit")]
paymentTable <- aggregate(. ~SurviveYears, data=paymentTable, sum, na.rm = TRUE) #group sum, this part can be improve creating own code for grouping and put 0 in the year without payment
investmentInterest <- as.numeric(inputsProject1[inputsProject1$label == "investmentInterest", c("value")])
nYears <- max(paymentTable$SurviveYears)
money <- sum(bDataFrame$NetSinglePremium)
earnInterest <- vector(mode="numeric", length=(nYears+1))
earnInterest[1] <- money*investmentInterest
benefitPayment <- vector(mode="integer", length=(nYears+1))



#find payment of the year 0
payment <- paymentTable[paymentTable$SurviveYears == 0, c("Benefit")] 
if(length(payment) == 0){ #sometime there is no payment for specific year, so convert numeric(0) to 0
  benefitPayment[1] <- 0
}else{
  benefitPayment[1] <- payment
}

surviveYears <- vector(mode="integer", length=(nYears+1))
surviveYears[1] <- 0

for (i in 1:nYears) {
  surviveYears[i+1] <- i
  money[i+1] <- money[i] + earnInterest[i] - benefitPayment[i]
  if(money[i+1] > 0){ #only calculate earnInterest when money > 0
    earnInterest[i+1] <- money[i+1]*investmentInterest
  }
  else {
    earnInterest[i+1] <- 0
  }
  
  #find payment of next year
  payment <- paymentTable[paymentTable$SurviveYears == i, c("Benefit")] 
  if(length(payment) == 0){ #sometime there is no payment for specific year, so convert numeric(0) to 0
    benefitPayment[i+1] <- 0
  }
  else{
    benefitPayment[i+1] <- payment
  }
  
} 
#benefitPayment[i+1] <- 0 #last year payment = 0
profitTable <- data.frame(surviveYears, money, earnInterest, benefitPayment)



# -------------- Plot business data frame -------------------------------
x11()
myGraph <- ggplot(bDataFrame, aes(Age))
myGraph <- myGraph + 
           geom_histogram(binwidth=1, colour="black", fill="white") + 
           labs(title = "Random Ages Histogram") +
           theme(plot.title = element_text(hjust = 0.5))
print(myGraph)
ggsave(filename = "images/Random Ages Histogram.png", plot = myGraph)

x11()
myGraph <- ggplot(bDataFrame, aes(Benefit))
myGraph <- myGraph + 
           geom_histogram(colour="black", fill="white") + labs(title = "Random Benefit Histogram") +
           theme(plot.title = element_text(hjust = 0.5))
print(myGraph)
ggsave(filename = "images/Random Benefit Histogram.png", plot = myGraph)

x11()
myGraph <- ggplot(bDataFrame, aes(NetSinglePremium))
myGraph <- myGraph +
           geom_histogram(colour="black", fill="white") +
           labs(title = "Random Net Single Premium Histogram") +
           theme(plot.title = element_text(hjust = 0.5))
print(myGraph)
ggsave(filename = "images/Random Net Single Premium Histogram.png", plot = myGraph)

x11()
myGraph <- ggplot(bDataFrame, aes(Die))
myGraph <- myGraph + 
           geom_histogram(binwidth=1, colour="black", fill="white") + 
           labs(title = "Random Death Ages Histogram") + 
           xlab("Death Age")+
           theme(plot.title = element_text(hjust = 0.5))

print(myGraph)
ggsave(filename = "images/Random Dead Ages Histogram.png", plot = myGraph)

x11()
myGraph <- ggplot(bDataFrame, aes(SurviveYears))
myGraph <- myGraph +
           geom_histogram(binwidth=1, colour="black", fill="white") + 
           labs(title = "Random Survive Years Histogram") +
           theme(plot.title = element_text(hjust = 0.5))
print(myGraph)
ggsave(filename = "images/Random Survive Years Histogram.png", plot = myGraph)


#------------------- Plot profit graph -------------------------------------
meltProfitTable <- melt(profitTable, id="surviveYears")
x11()
myGraph <- ggplot(meltProfitTable, aes(x = surviveYears, y = value, colour = variable))
myGraph <- myGraph + 
           geom_point() + 
           labs(title="Company Profit Graph", y = "Money [$US]") + 
           geom_line(linetype = "dashed") +
           scale_color_manual(labels = c("Profit", "Interest", "Payment"), 
           values = c("Green","blue", "red")) 
myGraph <- myGraph +theme(plot.title = element_text(hjust = 0.5))
print(myGraph)
ggsave(filename = "images/Company Profit Graph.png", plot = myGraph)

#-------------------------------------------------------------------------------------------------

########## Project 2 ################
# F(t)= Premium (t)+ accumlatedMonthlyinterest(t)- benefit(t)
tTimes <- length(profitTable$surviveYears)
# calucalting the monthy interest by A=p*((1+(annual interest/12))^(12*i)) - from the web
# or by calucalting the monthy interest by 1+((i)^12)/12  -given in the class 

#initialized array
fundValues <- vector(mode="numeric", length=tTimes)
interestMonthBased <- vector(mode="numeric", length=tTimes)
accInterMonthBased <- vector(mode="numeric", length=tTimes)
accBenPay <- vector(mode="numeric", length=tTimes)
#data for year 0
year <- profitTable$surviveYears
fundValues[1] <- profitTable$money[1]
interestMonthBased[1] <- fundValues[1] * (1+investmentInterest/12)^12 - fundValues[1] # only calculate the interest
benefitPay <- profitTable$benefitPayment
accInterMonthBased[1] <- interestMonthBased[1] 
accBenPay[1]<- benefitPay[1]

for (i in 1:(tTimes-1)) {
  
  fundValues[i+1] <- fundValues[i] + interestMonthBased[i] - benefitPay[i]
  
  if(fundValues[i+1] > 0){ #only calculate Interest when fund > 0
    interestMonthBased[i+1] <- fundValues[i+1] * (1+investmentInterest/12)^12 - fundValues[i+1] 
  }
  else {
    interestMonthBased[i+1] <- 0
  }
  accInterMonthBased[i+1] <- accInterMonthBased[i] +  interestMonthBased[i+1]
  accBenPay[i+1] <- accBenPay[i] +  benefitPay[i+1]
  
}
# Now, claulating the Fund value, F(t)= Premium (t)+ accumlatedMonthlyinterest(t)- benefit(t)

fundValueTable<-data.frame(year,fundValues, interestMonthBased, benefitPay, accInterMonthBased, accBenPay) # getting negative values of fund !!!!



#------------------- print Fund Value profit graph -------------------------------------
meltFundValueTable <- melt(fundValueTable, id="year")
x11()
myGraph <- ggplot(meltFundValueTable, aes(x = year, y = value, colour = variable))
myGraph <- myGraph + 
           geom_point() + 
           labs(title="Company Fund Graph", y = "Money [$US]") +
           geom_line(linetype = "dashed") +
           scale_color_manual(labels = c("Fund", "Interest Month based", "Benefit Payment", "Accumulated Interest", "Accumulated Benefit Payment"),
           values = c("Green","blue", "red", "orange", "chocolate")) +
           theme(plot.title = element_text(hjust = 0.5))
print(myGraph)
ggsave(filename = "images/Company Fund Graph month based interest.png", plot = myGraph)

########## end of Project 2 ##################

#------------------------------------------------------------------------------------------------------


endTime <- Sys.time() # calulating the ending time
totalTime <- endTime - startTime #calulating the total time
print(totalTime) # printing the total time
