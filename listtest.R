setwd("C:\\Users\\Lubna\\Desktop\\CWU\\Winter2019\\Stat\\ProjectOneTwo\\CS567_Project1")

mylist<-list(7,20,30,70)
L=length(mylist)
for(i in 1:L)
print(sum(as.numeric(mylist[c(1:i)])))