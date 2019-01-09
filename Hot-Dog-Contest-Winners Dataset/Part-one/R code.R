hot_dog_places<-read.csv('http://datasets.flowingdata.com/hot-dog-places.csv', sep = ",", header = TRUE)
hot_dog_places
names(hot_dog_places)<-c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010")
hot_dog_places
hot_dog_matrix<-as.matrix(hot_dog_places)
barplot(hot_dog_matrix,border = NA, space = 0.25, ylim = c(0,200), xlab = "Year", ylab = "Hot dogs and buns (HDBs) eaten", main = "Hot Dog Eating Contest Result, 1980-2010")