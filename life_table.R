
startTime <- Sys.time()
#setwd("/media/hermann/Tonpi/tonpi/Collegecourses/CWU/Graduate School/Winter 2019/CS 567/Projects/Project1/3R")
inputs <- read.table("life_table_inputs.txt", header = TRUE, sep = "\t", dec = ".", stringsAsFactors=FALSE) #read the inputs values from the life_table_inputs.txt file
#inputMortalityFile <- inputs$value[1]
inputMortalityFile <- (inputs[inputs$label == "inputMortalityFile", c("value")])
print(inputMortalityFile)
data <- read.csv(inputMortalityFile, header = TRUE, stringsAsFactors=FALSE) #avoid convert string to factor
dataTotalRow <- nrow(data)
beginRowData = 1

ages = as.numeric(data[beginRowData:dataTotalRow, 1])
mortality = as.numeric(data[beginRowData:dataTotalRow, 2])

lifeTable <- data.frame(ages,mortality)

lifeTableTotalRow = nrow(lifeTable)
lifeTable$t <- lifeTable$ages + 1
lifeTable$q <- lifeTable$mortality
lifeTable$p <- 1 - lifeTable$q

#calculate v^t
interest <- as.numeric(inputs[inputs$label == "interest", c("value")])
print(interest)
lifeTable$vt <- 1/(1+interest)^lifeTable$t

#calculate t_p_x | x=0
lifeTable$t_p_x0[1] <-  lifeTable$p[1]
for (i in 2:lifeTableTotalRow) {
  lifeTable$t_p_x0[i] <- lifeTable$t_p_x0[i-1] *  lifeTable$p[i]
}

#calculate t_E_x | x=0
lifeTable$t_E_x0 <- lifeTable$vt * lifeTable$t_p_x0


#calculate a_x
lifeTable$a_x[1] <- 1 + sum(lifeTable$t_E_x0)
for (i in 2:lifeTableTotalRow) {
  lifeTable$a_x[i] <- 1 + sum(lifeTable$t_E_x0[i:lifeTableTotalRow])/lifeTable$t_E_x0[i-1]
}


#calculate A_x
d = interest/(1 + interest)
lifeTable$A_x <- 1 - d * lifeTable$a_x
