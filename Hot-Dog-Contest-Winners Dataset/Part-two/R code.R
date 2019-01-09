hotdogs<-read.csv("http://datasets.flowingdata.com/hot-dog-contest-winners.csv",sep = ",",header = TRUE)
barplot(hotdogs$Dogs.eaten)
barplot(hotdogs$Dogs.eaten,names.arg = hotdogs$Year)
barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year, col = "red", border = NA, xlab = "Year", ylab = "Hot dogs and buns(HDB)eaten")
fill_colors<-c()
for (i in 1:length(hotdogs$Country)){
  if (hotdogs$Country[i]== "United States") {
    fill_colors<-c(fill_colors,"#821122")
  } else
  { fill_colors<-c(fill_colors,"#cccccc")}
}
barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year, col = fill_colors, border = NA, xlab = "Year", ylab = "Hot dogs and buns(HDB) eaten")
fill_colors<-c()
for (i in 1:length(hotdogs$New.record)){
  if (hotdogs$New.record[i]==1){
    fill_colors<-c(fill_colors,"#821122")
  } else
  { fill_colors<-c(fill_colors,"#cccccc")
    }
}
barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year, col=fill_colors, border = NA, xlab = "Year", ylab = "Hot Dogs and buns(HDB)eaten")
barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year, col = fill_colors, border = NA, space = 0.3, xlab = "Year", ylab = "Hot dogs and buns (HDB) eaten")
