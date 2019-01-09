InnovationIndex<-read.csv("InnovationIndex.csv", header=TRUE)
head(InnovationIndex)
barplot(InnovationIndex$Carmaker,InnovationIndex$Innovation.index.for.communications.system,InnovationIndex$Innovation.index.for.driver.assistance, col="red",
        barplot(InnovationIndex$Innovation.index.for.communications.system,InnovationIndex$Innovation.index.for.driver.assistance, InnovationIndex$Carmaker,space=0.3, ylim=c(0,40), border=NA,col="red", xlab="Carmaker", ylab="Innovation",main="Most innovative carmakers")
        