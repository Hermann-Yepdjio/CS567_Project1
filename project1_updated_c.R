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
#setwd("C:\\Users\\Lubna\\Desktop\\CWU\\Winter2019\\Stat\\ProjectOneTwo\\CS567_Project1")
#setwd("C:/Users/chao_/Desktop/CWU/Courses/Q1 Winter 2019/CS567 Computational Statistics R/Project2/CS567_Project1")
#setwd("C:/Users/huanglinc/Desktop/P2 Sta/CS567_Project1")
#setwd("D:/tonpi/Collegecourses/CWU/Graduate-School/Winter 2019/CS 567/Projects/final/CS567_Project1")
#setwd("C:/Users/huanglinc/Desktop/P2 Sta 1/CS567_Project1")
setwd("C:/Users/AlzamilL/Desktop/CS567_Project1-master/CS567_Project1-master")
inputsProject1 <- read.delim("project1_inputs.txt", header = TRUE, sep = "\t", dec = ".", stringsAsFactors=FALSE) #read the inputs values from the project1_inputs.txt file
print (inputsProject1)
#this file is to run 

#making life table based on csv files
source("life_table.R")


#install library if not exist
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(reshape)) install.packages('reshape')
if (!require(plotly)) install.packages('plotly')
library(ggplot2)
library(reshape)
library(plotly)

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
lifeTableAsVector = as.vector(lifeTable[1,], mode = 'numeric') #create a vector representation of lifeTable that can be passed to the c function
for (i in 2:lifeTableTotalRow)
{
  lifeTableAsVector = c(lifeTableAsVector, as.vector(lifeTable[i,], mode = 'numeric'))
}

#command to compile in R:  R CMD SHLIB c_code.c 
dim = c(length(lifeTableAges$Age),length(lifeTable), length(lifeTableAges$Age)) 
print(dim)
dyn.load("c_code.dll") #load the .dll file
res = .C("for_loop", lifeTable=as.numeric(lifeTableAsVector), dim = as.integer(dim), bAge = as.integer(bAge), bBen=as.integer(bBen), bNps = as.numeric(bNps), bFAge = as.integer(bFAge), lifeTableAges = as.integer(lifeTableAges$Age), inputNumberClients =as.integer(inputNumberClients))
bFAge = res$bFAge
bAge = res$bAge
bBen= res$bBen
bNps= res$bNps
dyn.unload("c_code.dll") #unload the .dll file


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
reserveHold <- vector(mode="numeric", length=(nYears+1))

# reserveTableAsVector = as.vector(reserveTable[1,], mode = 'numeric') #create a vector representation of reserveTable so I can pass it to the c function
# for (i in 2:nrow(reserveTable))
# {
#   reserveTableAsVector = c(reserveTableAsVector, as.vector(reserveTable[i,], mode = 'numeric'))
# }

bdfAge = bDataFrame$Age
bdfSurviveYears = bDataFrame$SurviveYears
bdfBenefit = bDataFrame$Benefit
lifeTable_A_x = lifeTable$A_x
SurviveYears<- vector(mode="integer", length=nYears) #initialize variables
Reserve <- vector(mode="numeric", length=nYears)
NumberPolicies<- vector(mode="integer", length=nYears)
dim2 = c(nrow(bDataFrame), nYears)
dyn.load("c_code_2.dll") # load the .dll file
res2 = .C("for_loop_2", lifeTable_A_x=as.numeric(lifeTable_A_x), dim2 = as.integer(dim2), bdfAge = as.integer(bdfAge), bdfSurviveYears = as.integer(bdfSurviveYears), bdfBenefit = as.numeric(bdfBenefit), SurviveYears = as.integer(SurviveYears), Reserve = as.numeric(Reserve), NumberPolicies = as.integer(NumberPolicies))

#----part of project 2, calculating reserve------ convert this part  to c
nrowbDataFrame <- nrow(bDataFrame)
reserveTable <- data.frame(matrix(ncol = 3, nrow = nYears))
colnames(reserveTable) <- c("SurviveYears", "Reserve", "NumberPolicies")
dyn.unload("c_code_2.dll") # unload the .dll file
reserveTable$SurviveYears <- res2$SurviveYears
reserveTable$Reserve <- res2$Reserve
reserveTable$NumberPolicies <- res2$NumberPolicies
print (reserveTable)



# for (reserveYear in 1:nYears){
#   counter<-0
#   reserveAmount <- 0
#   for (i in 1:nrowbDataFrame){
#     if (bDataFrame$SurviveYears[i] >= reserveYear)
#     {
#       counter <- counter + 1
#       #t_V = 1 - a_(x+t)/a_x
#       x <- bDataFrame$Age[i]
#       A_x_t <- lifeTable$A_x[(x+1+reserveYear)] # faster
#       t_V <- A_x_t
#       reserveAmount <- reserveAmount + bDataFrame$Benefit[i]*t_V
#     }
#   }
#   reserveTable$SurviveYears[reserveYear] <- reserveYear-1
#   reserveTable$NumberPolicies[reserveYear] <- counter
#   reserveTable$Reserve[reserveYear] <- reserveAmount
# 
# }
# print (reserveTable)

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
           scale_color_manual(labels = c("Fund", "Interest", "Payment", "Reserve", "Profit"),
           values = c("yellow","blue", "red", "purple", "green"))
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


fund3D<-data.frame(year=as.factor(year),fundValues=as.factor(fundValues), interestMonthBased=as.factor(interestMonthBased))
# 3D Scatter
p2 <- plot_ly(fund3D, x = interestMonthBased, y = year, z = fundValues,  colors = c('red')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Monthly Interests'),
                      yaxis = list(title = 'Year'),
                      zaxis = list(title = 'Fund [$US]')))
#print(p2)

htmlwidgets::saveWidget(as_widget(p2), "Scattered3DFundValues.html")
########## Function Project 2 ################

# F(t)= Premium (t)+ accumlatedMonthlyinterest(t)- benefit(t)

CalculateFundValue <- function(investmentInterest){
  tTimes <- length(profitTable$surviveYears)
  #initialized array
  fundValues <- vector(mode="numeric", length=tTimes)
  interestMonthBased <- vector(mode="numeric", length=tTimes)

  #data for year 0
  year <- profitTable$surviveYears
  fundValues[1] <- profitTable$money[1]
  interestMonthBased[1] <- fundValues[1] * (1+investmentInterest/12)^12 - fundValues[1] # only calculate the interest
  benefitPay <- profitTable$benefitPayment


  for (i in 1:(tTimes-1)) {
    fundValues[i+1] <- fundValues[i] + interestMonthBased[i] - benefitPay[i]
    if(fundValues[i+1] > 0){ #only calculate Interest when fund > 0
      interestMonthBased[i+1] <- fundValues[i+1] * (1+investmentInterest/12)^12 - fundValues[i+1]
    }
    else {
      interestMonthBased[i+1] <- 0
    }
  }

  fT <- data.frame(year,fundValues, interestMonthBased, benefitPay)
  return(fT)

}

###############Function calculate profit project2###################

CalculateProfit <- function(investmentInterest){
  #---------calculating profit table---------------project2: reserve added----
  tTimes <- length(profitTable$surviveYears)
  #initialized array
  fundValues <- vector(mode="numeric", length=tTimes)
  interest <- vector(mode="numeric", length=tTimes)

  #data for year 0
  year <- profitTable$surviveYears
  fundValues[1] <- profitTable$money[1]
  interest[1] <- fundValues[1] * investmentInterest # only calculate the interest
  benefitPay <- profitTable$benefitPayment
  reserveHold <- profitTable$reserveHold

  for (i in 1:(tTimes-1)) {
    fundValues[i+1] <- fundValues[i] + interest[i] - benefitPay[i]
    if(fundValues[i+1] > 0){ #only calculate Interest when fund > 0
      interest[i+1] <- fundValues[i+1] * investmentInterest
    }
    else {
      interest[i+1] <- 0
    }
  }

  pT <- data.frame(year,fundValues, interest, benefitPay, reserveHold)
  pT$profit <- pT$fundValues + pT$interest - pT$benefitPay - pT$reserveHold

  return(pT)
}

##############end calculate profit project2#################


#-------surface plot of fund, monthly based interest----------------
interestSeq <- seq(0.05, 0.055, 0.0005)
yearSeq <- fundValueTable$year
matrixFund <- matrix(, nrow = length(yearSeq), ncol = length(interestSeq)) # empty matrix
column <- 1

for (i in interestSeq){
  matrixFund[,column] <- CalculateFundValue(i)$fundValues
  column <- column + 1
}


p <- plot_ly(x = interestSeq  , y = yearSeq, z = matrixFund)  %>%
  layout(
    title = "Fund Values Evolution based on different Interest",
    scene = list(
      xaxis = list(title = "Monthly Interests"),
      yaxis = list(title = "Years"),
      zaxis = list(title = "Fund [$US]")
    )) %>%
  add_surface(
    contours = list(
      z = list(
        show=TRUE,
        usecolormap=TRUE,
        highlightcolor="#ff0000",
        project=list(z=TRUE)
      )
    )
  ) %>%
  layout(
    scene = list(
      camera=list(
        eye = list(x=1.87, y=0.88, z=-0.64)
      )
    )
  )

print(p)
htmlwidgets::saveWidget(as_widget(p), "Surface3DFundValues.html")
#-------End:  surface plot of fund, monthly based interest----------------

#https://plot.ly/r/3d-line-plots/


#-----------line plot of fund, monthly based interest--------------------

df <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("year", "fundValues", "interest")
colnames(df) <- x

for (i in interestSeq){
  fv <- CalculateFundValue(i)
  tempdf <- fv[,c("year", "fundValues")]
  tempdf$interest <- rep(i, length(tempdf$year))
  df <- rbind(df, tempdf)
}

df$interest <- as.factor(df$interest)

p <- plot_ly(df,
             x = ~year,
             y = ~fundValues,
             z = ~interest,
             type = 'scatter3d',
             mode = 'lines',
             color = ~interest,
             line = list(width = 4)
             )
print(p)
htmlwidgets::saveWidget(as_widget(p), "Line3DFundValuesDiff.html")


#-------surface plot of profit----------------
interestSeq <- seq(0.05, 0.053, 0.0005)
yearSeq <- profitTable$surviveYears
matrixProfit <- matrix(, nrow = length(yearSeq), ncol = length(interestSeq)) # empty matrix
column <- 1

for (i in interestSeq){
  matrixProfit[,column] <- CalculateProfit(i)$profit
  column <- column + 1
}


p <- plot_ly(x = interestSeq  , y = yearSeq, z = matrixProfit)  %>%
  layout(
    title = "Profit Evolution based on different Interest",
    scene = list(
      xaxis = list(title = "Interests"),
      yaxis = list(title = "Years"),
      zaxis = list(title = "Profit [$US]")
    )) %>%
  add_surface(
    contours = list(
      z = list(
        show=TRUE,
        usecolormap=TRUE,
        highlightcolor="#ff0000",
        project=list(z=TRUE)
      )
    )
  ) %>%
  layout(
    scene = list(
      camera=list(
        eye = list(x=1.87, y=0.88, z=-0.64)
      )
    )
  )

print(p)
htmlwidgets::saveWidget(as_widget(p), "Surface3DProfitValues.html")
#-------End:  surface plot of profit---------------

#-------end line plot----------------------

# p5 <- plot_ly(df,
#               x = ~year,
#               y = ~fundValues,
#               z = ~interest,
#               type = 'scatter3d',
#               mode = 'markers',
#               color = ~interest,
#               marker = list(size = 3, symbol = 104)
#               )
#
# print(p5)
# htmlwidgets::saveWidget(as_widget(p5), "marker3DFundValuesDiff.html")


########## end of Project 2 ##################

#------------------------------------------------------------------------------------------------------


endTime <- Sys.time() # calulating the ending time
totalTime <- endTime - startTime #calulating the total time
print(totalTime) # printing the total time
