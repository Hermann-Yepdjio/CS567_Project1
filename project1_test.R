#setwd("C:/Users/chao_/Desktop/CWU/Courses/Q1 Winter 2019/CS567 Computational Statistics R/Project1/Project1 Github/CS567_Project1")
setwd("C:/Users/huanglinc/Desktop/Project1 Statistics/github/CS567_Project1")
inputsProject1 <- read.delim("project1_inputs_test.txt", header = TRUE, sep = "\t", dec = ".", stringsAsFactors=FALSE) #read the inputs values from the project1_inputs.txt file
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
myGraph <- myGraph + geom_point()
print(myGraph)
ggsave(filename = "images_test/A_x Vs Ages.png", plot = myGraph)



inputAges = as.numeric(inputsProject1[inputsProject1$label == "inputAges", c("value")])
inputBenefit = as.numeric(inputsProject1[inputsProject1$label == "inputBenefit", c("value")])
nsp <- lifeTable[lifeTable$ages == inputAges, c("A_x")]*inputBenefit
print(paste("input Ages: ", inputAges))
print(paste("Whole Life Net Single Premium: ", nsp))









#----------now creating a block of 10000 people who are in diffrent ages and want diffrent benefits


BussinessBlock <- function(rAge,rBenefit) { 
  netSinglePremium <- lifeTable[lifeTable$ages == rAge, c("A_x")]*rBenefit
  return(netSinglePremium)
}

bAge <- 0
bBen <- 0
bNps <- 0
bFAge <- 0
maxAges <- max(lifeTable$ages)
lifeTableAges <- lifeTable$ages[ages < maxAges]# avoid picking max ages

inputNumberClients <- as.numeric(inputsProject1[inputsProject1$label == "inputNumberClients", c("value")])

# ---------------------- looping 10000 -------------------------------------
#lifeTableAges <- data.frame(Age=ages) # creating a data frame that contains the ages 

print("---------Calculating Lifetimes-----------")
print(paste("Number of clients: ", inputNumberClients))
# for (i in 1:inputNumberClients){ # 10,000 (whole life) incurances with Net Single Premium
#   
#   randomAge <- 2
#   #randomAge <-  sample(lifeTableAges, 1) 
#   randomBenefit <- 1000 #testing with only 1000 us for all the clients
#   #randomBenefit <- sample.int(9000, 1, replace=TRUE) + 1000 # picking one randonm integer from range $1000-$1000000 benefit
#   bAge[i] <- randomAge # concatenate
#   bBen[i] <- randomBenefit # concatenate
#   bNps[i] <- BussinessBlock(randomAge,randomBenefit) # calling the function to calculate the net single premium then concatenate
# 
#   #--------calculate random dies based on mortality table ----------   this part can be improved in efficiency
#   #calculate probability of (x) lives t years  t_p_x where x = inputAges
#   previousAgeP <- lifeTable[ages == (randomAge - 1), c("t_p_x0")]
#   if (randomAge > 0) {
#     pLives <- lifeTable[ages >= randomAge, c("t_p_x0")]/previousAgeP
#   } else {
#     pLives <- lifeTable[ages >= randomAge, c("t_p_x0")]
#   }
#   
#   pLivesAges <- lifeTable[ages >= randomAge, c("ages")]
#   pCurveX <- data.frame(pLivesAges, pLives)
#   
#   #calculate probability tl1_q_x that x survives t years and dies within 1 year
#   pCurveXRow <- nrow(pCurveX)
#   
#   #probability of dead based on input age
#   for (j in 1:(pCurveXRow-1)) {
#     pCurveX$tl1_q_x[j] <- pCurveX$pLives[j] - pCurveX$pLives[j+1] #i = u in the pdf   1 = t in pdf
#   }
#   pCurveX$tl1_q_x[pCurveXRow] <- pCurveX$pLives[pCurveXRow] # last line
#   
#   
#   #generate random dies based on input age
#   randomFinalAge <- sample(pCurveX$pLivesAges, 1, replace = T, pCurveX$tl1_q_x)
#   bFAge[i] <- randomFinalAge
#   
#   #-----this simulation part is for testing, do not delete-----------
#   #myGraph <- ggplot(pCurveX, aes(pLivesAges, tl1_q_x))
#   #print(myGraph + geom_point() + geom_point(aes(x=randomFinalAge, y=0.02), color="red"))
#   #Sys.sleep(2)
# }
randomAge <- 1
bAge <- rep(randomAge, inputNumberClients) # concatenate
bBen <- rep(100, inputNumberClients) # concatenate
n <- length(bAge)
for (k in 1:n){
  bNps[k] <- BussinessBlock(bAge[k], bBen[k])
}


CalculateFinalAgePerfectData <- function(inputAge) { 
  
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
  for (j in 2:(pCurveXRow)) {
    
    #small bug which does not consider the 0|1_q_x data..... also add q_x of 1 in the last year automatically? 
    pCurveX$tl1_q_x[j] <- pCurveX$pLives[j-1] - pCurveX$pLives[j]
    #pCurveX$tl1_q_x[j] <- pCurveX$pLives[j] - pCurveX$pLives[j+1] #i = u in the pdf   1 = t in pdf
  }
  #pCurveX$tl1_q_x[pCurveXRow] <- pCurveX$pLives[pCurveXRow] # last line
  
  #bFAge <- sample(pCurveX$pLivesAges, inputNumberClients, replace = T, pCurveX$tl1_q_x)
  
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
      }else{
        nRepeat <- trunc(nRepeat) # use trunc to avoid number of data > pCurveXRow
      }
      counter <- counter +1
    }else{
      nRepeat <- trunc(nRepeat)
    }
    
    fIndex <- bIndex + nRepeat - 1
    perfectData[bIndex:fIndex] <- pCurveX$pLivesAges[k]
    bIndex <- bIndex + nRepeat
  }  
  #nData <- length(perfectData)
  nFill <- inputNumberClients - fIndex
  
  #fill residual round values from nData to numberclient 
  perfectData[(fIndex+1):inputNumberClients] <- sample(pCurveX$pLivesAges, nFill, replace = T, pCurveX$tl1_q_x)
  #perfectData[(nData+1):inputNumberClients] <- perfectData[nData]
  
  return(perfectData)
}



CalculateFinalAgeSemiPerfectData <- function(inputAge) { 
  
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
  for (j in 2:(pCurveXRow)) {
    
    #small bug which does not consider the 0|1_q_x data..... also add q_x of 1 in the last year automatically? 
    pCurveX$tl1_q_x[j] <- pCurveX$pLives[j-1] - pCurveX$pLives[j]
    #pCurveX$tl1_q_x[j] <- pCurveX$pLives[j] - pCurveX$pLives[j+1] #i = u in the pdf   1 = t in pdf
  }
  #pCurveX$tl1_q_x[pCurveXRow] <- pCurveX$pLives[pCurveXRow] # last line
  
  bFAge <- sample(pCurveX$pLivesAges, inputNumberClients, replace = T, pCurveX$tl1_q_x)
  
  
  return(bFAge)
}




bFAge <- CalculateFinalAgePerfectData(randomAge)

bDataFrame <- data.frame(Age = bAge, Benefit = bBen, NetSinglePremium = bNps, Die = bFAge) #Creating the final dataframe
bDataFrame$SurviveYears <- bDataFrame$Die - bDataFrame$Age

#write.csv(bDataFrame, file="BusinessData.csv",  append = FALSE) # creating the CSV file, each time we run the code new file will be created and the older file will be replaced 
write.table(bDataFrame, file="BusinessData.csv", row.names=F, col.names=T, append=F, sep=",")

#-------------------------------------------------------------------------

paymentTable <- bDataFrame[c("SurviveYears", "Benefit")]
paymentTable <- aggregate(. ~SurviveYears, data=paymentTable, sum, na.rm = TRUE) #group sum, this part can be improve creating own code for grouping and put 0 in the year without payment
investmentInterest <- as.numeric(inputsProject1[inputsProject1$label == "investmentInterest", c("value")])
nYears <- max(paymentTable$SurviveYears)
money <- sum(bDataFrame$NetSinglePremium)
earnInterest <- vector(mode="numeric", length=(nYears+1))
earnInterest[1] <- money*investmentInterest
benefitPayment <- vector(mode="integer", length=(nYears+1))
reserveHold <- vector(mode="numeric", length=(nYears+1))


#----part of project 2, calculating reserve------
nrowbDataFrame <- nrow(bDataFrame)
reserveTable <- data.frame(matrix(ncol = 3, nrow = nYears))
colnames(reserveTable) <- c("SurviveYears", "Reserve", "NumberPolicies")

for (reserveYear in 1:nYears){
  counter<-0
  reserveAmount <- 0
  for (i in 1:nrowbDataFrame){
    if (bDataFrame$SurviveYears[i] >= reserveYear)
    {
      counter <- counter + 1 
      #t_V = 1 - a_(x+t)/a_x
      x <- bDataFrame$Age[i]
      #a_x <- lifeTable[lifeTable$ages == x, c("a_x")]
      #a_x <- lifeTable$a_x[(x+1)] # faster
      #a_x_t <- lifeTable[lifeTable$ages == (x+reserveYear), c("a_x")]
      #a_x_t <- lifeTable$a_x[(x+1+reserveYear)] # faster
      #t_V <- 1 - (a_x_t/a_x)
      #A_x_t <- lifeTable[lifeTable$ages == (x+reserveYear), c("A_x")]
      A_x_t <- lifeTable$A_x[(x+1+reserveYear)] # faster
      t_V <- A_x_t
      #A_x_t <- lifeTable$A_x[(x+1+reserveYear)]
      reserveAmount <- reserveAmount + bDataFrame$Benefit[i]*t_V
    }
  }
  reserveTable$SurviveYears[reserveYear] <- reserveYear-1
  reserveTable$NumberPolicies[reserveYear] <- counter
  reserveTable$Reserve[reserveYear] <- reserveAmount
  
}


#-----end calculating reserve--------------------



#---------calculating profit table---------------project2: reserve added----
#find payment of the year 0
payment <- paymentTable[paymentTable$SurviveYears == 0, c("Benefit")] 
if(length(payment) == 0){ #sometime there is no payment for specific year, so convert numeric(0) to 0
  benefitPayment[1] <- 0
}else{
  benefitPayment[1] <- payment
}
#find reserve of the year 0
reserve <- reserveTable[reserveTable$SurviveYears ==0, c("Reserve")]
if(length(reserve) == 0){ #sometime there is no reserve for specific year, so convert numeric(0) to 0
  reserveHold[1] <- 0
}else{
  reserveHold[1] <- reserve
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
  
  #find reserve of next year
  reserve <- reserveTable[reserveTable$SurviveYears == i, c("Reserve")]
  if(length(reserve) == 0){ #sometime there is no reserve for specific year, so convert numeric(0) to 0
    reserveHold[i+1] <- 0
  }else{
    reserveHold[i+1] <- reserve
  }
  
} 

#benefitPayment[i+1] <- 0 #last year payment = 0
profitTable <- data.frame(surviveYears, money, earnInterest, benefitPayment, reserveHold)
#calculate  profit
profitTable$profit <- profitTable$money + profitTable$earnInterest - profitTable$benefitPayment - profitTable$reserveHold

#----------end calculating profit table--------

# -------------- print business data frame -------------------------------
x11()
myGraph <- ggplot(bDataFrame, aes(Age))
myGraph <- myGraph + geom_histogram() + labs(title = "Random Ages Histogram")
print(myGraph)
ggsave(filename = "images_test/Random Ages Histogram.png", plot = myGraph)

x11()
myGraph <- ggplot(bDataFrame, aes(Benefit))
myGraph <- myGraph + geom_histogram() + labs(title = "Random Benefit Histogram")
print(myGraph)
ggsave(filename = "images_test/Random Benefit Histogram.png", plot = myGraph)

x11()
myGraph <- ggplot(bDataFrame, aes(NetSinglePremium))
myGraph <- myGraph + geom_histogram() + labs(title = "Random Net Single Premium Histogram")
print(myGraph)
ggsave(filename = "images_test/Random Net Single Premium Histogram.png", plot = myGraph)

x11()
myGraph <- ggplot(bDataFrame, aes(Die))
myGraph <- myGraph + geom_histogram(binwidth=1, colour="black", fill="white") + labs(title = "Random Dead Ages Histogram")
print(myGraph)
ggsave(filename = "images_test/Random Dead Ages Histogram.png", plot = myGraph)

x11()
myGraph <- ggplot(bDataFrame, aes(SurviveYears))
myGraph <- myGraph + geom_histogram(binwidth=1, colour="black", fill="white") + labs(title = "Random Survive Years Histogram")
print(myGraph)
ggsave(filename = "images_test/Random Survive Years Histogram.png", plot = myGraph)


#------------------- Plot profit graph -------------------------------------
meltProfitTable <- melt(profitTable, id="surviveYears")
x11()
myGraph <- ggplot(meltProfitTable, aes(x = surviveYears, y = value, colour = variable))
myGraph <- myGraph + 
  geom_point() + 
  labs(title="Company Profit Graph", y = "Money [$US]") + 
  geom_line(linetype = "dashed") +
  scale_color_manual(labels = c("Fund", "Interest", "Payment", "Reserve", "Profit"), 
                     values = c("yellow","blue", "red", "purple", "green")) 
myGraph <- myGraph +theme(plot.title = element_text(hjust = 0.5))
print(myGraph)

ggsave(filename = "images_test/Company Profit Graph.png", plot = myGraph)
#myGraph  <- ggplot() + geom_point(data=profitTable, aes(x=year, y=money), color = "black") +
#  geom_point(data=profitTable, aes(x=year, y=benefitPayment), color = "red") +
#  geom_point(data=profitTable, aes(x=year, y=earnInterest), color = "blue") +
#  labs(title="Profit Graph")
#print(myGraph)


#print(sum(profitTable$benefitPayment))
#print(sum(bDataFrame$Benefit))
#print(sum(paymentTable$Benefit))
#---------------------------------


#----------------------------------


endTime <- Sys.time() # calulating the ending time
totalTime <- endTime - startTime #calulating the total time
print(totalTime) # printing the total time