setwd("C:/Users/huanglinc/Desktop/Project1 Statistics/github/CS567_Project1")
#setwd("C:/Users/chao_/Desktop/CWU/Courses/Q1 Winter 2019/CS567 Computational Statistics R/Project1/Project1 Github/CS567_Project1")
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
myGraph <- myGraph + geom_point()
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
  #----------------------------------
  
  
  #print(sum(pCurveX$tl1_q_x))
  
  #plot lives probability based on input age
  #myGraph <- ggplot(pCurveX, aes(pLivesAges, pLives))
  #myGraph + geom_point()
  #ggsave(filename = "images/lives probability based on input age.png", plot = myGraph)
  
  #x11()
  #plot tl1_q_x based on input age
  #myGraph <- ggplot(pCurveX, aes(pLivesAges, tl1_q_x))
  #myGraph <- myGraph + geom_point() + labs(title = "probability of (x) survive t years and die next year")
  #print(myGraph)
  #ggsave(filename = "images/probability of (X) survive t years and die next year.png", plot = myGraph)
  #----------------------------------------------
  
  #generate random dies based on input age
  finalAge <- 0
  if(pCurveXRow > 1) { #bug -fixed,  avoid enter when nrow(pCurveX) == 0, this occur when inputAge is the last value of the tablelife
    finalAge <- sample(pCurveX$pLivesAges, 1, replace = T, pCurveX$tl1_q_x)
  }else
  {
    finalAge <- pCurveX$pLivesAges[1] # only has 1 probability
  }
  
  #ttt <- data.frame(s)
  #myGraph <- ggplot(ttt, aes(s))
  #myGraph + geom_histogram()
  return (finalAge)
}


CalculateFinalAgePerfectData <- function(inputAge) { #---only for testing
  
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
  
  #return a vector
  return(perfectData)
}


CalculateFinalAgeSemiPerfectData <- function(inputAge) { #---only for testing
  
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
  
  
  #note: return a vector
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

#pNormal <- dnorm(lifeTableAges$Age,35,10) # create a normal distribution mean = 35 years standard deviation = 10




for (i in 1:inputNumberClients){ # 10,000 (whole life) incurances with Net Single Premium
  
  #randomAge <-  sample(lifeTableAges$Age, 1, replace = T, prob = pNormal) # there was a bug before lifeTableAges instead of lifeTableAges$Age
  randomAge <-  sample(lifeTableAges$Age, 1, replace = T) # there was a bug before lifeTableAges instead of lifeTableAges$Age
  randomBenefit <- 1000 #sample.int(9000, 1, replace=TRUE) + 1000 # picking one randonm integer from range $1000-$1000000 benefit
  bAge[i] <- randomAge # concatenate
  bBen[i] <- randomBenefit # concatenate
  bNps[i] <- BussinessBlock(randomAge,randomBenefit) # calling the function to calculate the net single premium then concatenate
  
  #--------calculate random dies based on mortality table ----------   this part can be improved in efficiency
  
  
  
  #-------------------bug fixed---------------
  
  #calculate probability of (x) lives t years  t_p_x where x = randomAges
  previousAgeP <- lifeTable[ages == (randomAge - 1), c("t_p_x0")]
  if (randomAge > 0) {
    pLives <- lifeTable[ages >= randomAge, c("t_p_x0")]/previousAgeP
  } else {
    pLives <- lifeTable[ages >= randomAge, c("t_p_x0")]
  }
  
  pLivesAges <- lifeTable[ages >= randomAge, c("ages")]
  pCurveX <- data.frame(pLivesAges, pLives)
  
  #calculate probability tl1_q_x that x survives t years and dies within 1 year
  pCurveXRow <- nrow(pCurveX)
  
  #probability of dead based on input age
  pCurveX$tl1_q_x[1] <- 1 - pCurveX$pLives[1] # firt data important,,, to avoid bug
  if(pCurveXRow > 1) { #bug -fixed,  avoid enter for when nrow(pCurveX) == 0, this occur when randomAge is the last value of the tablelife 
    for (j in 2:(pCurveXRow)) {
      
      #small bug which does not consider the 0|1_q_x data..... also add q_x of 1 in the last year automatically? 
      pCurveX$tl1_q_x[j] <- pCurveX$pLives[j-1] - pCurveX$pLives[j]
      #pCurveX$tl1_q_x[j] <- pCurveX$pLives[j] - pCurveX$pLives[j+1] #i = u in the pdf   1 = t in pdf
    }
  }
  #----------------------------------
  
  #generate random dies based on input age
  finalAge <- 0
  if(pCurveXRow > 1) { #bug -fixed,  avoid enter when nrow(pCurveX) == 0, this occur when randomAge is the last value of the tablelife
    finalAge <- sample(pCurveX$pLivesAges, 1, replace = T, pCurveX$tl1_q_x)
  }else
  {
    finalAge <- pCurveX$pLivesAges[1] # only has 1 probability
  }
  
  bFAge[i] <- finalAge
  
  
  
  
  #-----this simulation part is for testing, do not delete-----------
  #myGraph <- ggplot(pCurveX, aes(pLivesAges, tl1_q_x))
  #print(myGraph + geom_point() + geom_point(aes(x=randomFinalAge, y=0.02), color="red"))
  #Sys.sleep(2)
}



# #---only for testing
# randomAge <- 0
# bAge <- rep(randomAge, inputNumberClients) # concatenate
# bBen <- rep(100, inputNumberClients) # concatenate
# for (k in 1:inputNumberClients){
#   bNps[k] <- BussinessBlock(bAge[k], bBen[k])
# }
# bFAge <- CalculateFinalAgePerfectData(randomAge)


bDataFrame <- data.frame(Age = bAge, Benefit = bBen, NetSinglePremium = bNps, Die = bFAge) #Creating the final dataframe
bDataFrame$SurviveYears <- bDataFrame$Die - bDataFrame$Age

#write.csv(bDataFrame, file="BusinessData.csv",  append = FALSE) # creating the CSV file, each time we run the code new file will be created and the older file will be replaced 
write.table(bDataFrame, file="BusinessData.csv", row.names=F, col.names=T, append=F, sep=",")


paymentTable <- bDataFrame[c("SurviveYears", "Benefit")]
paymentTable <- aggregate(. ~SurviveYears, data=paymentTable, sum, na.rm = TRUE)

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
  }else {
    earnInterest[i+1] <- 0
  }
  
  #find payment of next year
  payment <- paymentTable[paymentTable$SurviveYears == i, c("Benefit")] 
  if(length(payment) == 0){ #sometime there is no payment for specific year, so convert numeric(0) to 0
    benefitPayment[i+1] <- 0
  }else{
    benefitPayment[i+1] <- payment
  }
  
}
#benefitPayment[i+1] <- 0 #last year payment = 0
profitTable <- data.frame(surviveYears, money, earnInterest, benefitPayment)

# -------------- print business data frame -------------------------------
x11()
myGraph <- ggplot(bDataFrame, aes(Age))
myGraph <- myGraph + geom_histogram(binwidth=1, colour="black", fill="white") + labs(title = "Random Ages Histogram")
print(myGraph)
ggsave(filename = "images/Random Ages Histogram.png", plot = myGraph)

x11()
myGraph <- ggplot(bDataFrame, aes(Benefit))
myGraph <- myGraph + geom_histogram(colour="black", fill="white") + labs(title = "Random Benefit Histogram")
print(myGraph)
ggsave(filename = "images/Random Benefit Histogram.png", plot = myGraph)

x11()
myGraph <- ggplot(bDataFrame, aes(NetSinglePremium))
myGraph <- myGraph + geom_histogram(colour="black", fill="white") + labs(title = "Random Net Single Premium Histogram")
print(myGraph)
ggsave(filename = "images/Random Net Single Premium Histogram.png", plot = myGraph)

x11()
myGraph <- ggplot(bDataFrame, aes(Die))
myGraph <- myGraph + geom_histogram(binwidth=1, colour="black", fill="white") + labs(title = "Random Dead Ages Histogram")
print(myGraph)
ggsave(filename = "images/Random Dead Ages Histogram.png", plot = myGraph)

x11()
myGraph <- ggplot(bDataFrame, aes(SurviveYears))
myGraph <- myGraph + geom_histogram(binwidth=1, colour="black", fill="white") + labs(title = "Random Survive Years Histogram")
print(myGraph)
ggsave(filename = "images/Random Survive Years Histogram.png", plot = myGraph)


#------------------- print profit graph -------------------------------------
meltProfitTable <- melt(profitTable, id="surviveYears")
x11()
myGraph <- ggplot(meltProfitTable, aes(x = surviveYears, y = value, colour = variable))
myGraph <- myGraph + geom_point() + labs(title="Company Profit Graph", y = "Money [$US]") + geom_line(linetype = "dashed") +
  scale_color_manual(labels = c("Profit", "Interest", "Payment"), values = c("Green","blue", "red")) 
print(myGraph)

ggsave(filename = "images/Company Profit Graph.png", plot = myGraph)
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