test<-read.csv("test.csv",header=TRUE)
train<-read.csv("train.csv",header=TRUE)
test.survived<-data.frame(survived=rep("None",nrow(test)),test[,])
data.combined<-rbind(train,test.survived)
str(data.combined)
data.combined$pclass<-as.factor(data.combined$pclass)
data.combined$survived<-as.factor(data.combined$survived)
table(data.combined$survived)
table(data.combined$pclass)
library(ggplot2)
train$pclass <- as.factor(train$pclass)
train$survived<-as.factor(train$survived)
ggplot(train, aes(x = pclass, fill = factor(survived))) +  geom_bar() + xlab("Pclass") +  ylab("Total Count") +  labs(fill = "Survived") 
head(as.character(train$name))
length(unique(as.character(data.combined$name)))
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$name))), "name"])
data.combined[which(data.combined$name %in% dup.names),]
library(stringr)
misses <- data.combined[which(str_detect(data.combined$name, "Miss.")),]
misses[1:5,]
mrses <- data.combined[which(str_detect(data.combined$name, "Mrs.")), ]
mrses[1:5,]
males <- data.combined[which(train$sex == "male"), ]
males[1:5,]
extractTitle <- function(name) {
  name <- as.character(name)
  if (length(grep("Miss.", name)) > 0) {return ("Miss.")}
  else if (length(grep("Master.", name)) > 0) {return ("Master.")}
  else if (length(grep("Mrs.", name)) > 0) {return ("Mrs.")}
  else if (length(grep("Mr.", name)) > 0) {return ("Mr.")}
  else {return ("Other") }}
titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"name"]))}
data.combined$title <- as.factor(titles)
ggplot(data.combined[1:891,],aes(x=title, fill= survived))+
  stat_count(width = 0.5) +
  facet_wrap(~pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")
table(data.combined$sex)
ggplot(data.combined[1:891,], aes(x=sex, fill=survived))+
  stat_count(width = 0.5) + facet_wrap(~pclass) + ggtitle("pclass") + xlab("Sex") + ylab("Total Count") + labs(fill = "Survived")
summary(data.combined$age)
summary(data.combined[1:891,"age"])
ggplot(data.combined[1:891,], aes(x=age, fill= survived)) + facet_wrap(~sex+pclass)+ geom_histogram(binwidth = 10) + xlab("Age")+ ylab("Total Count")
boys<- data.combined[which(data.combined$title=="Master."),]
summary(boys$age)
misses<- data.combined[which(data.combined$title=="Miss."),]
summary(misses$age)
ggplot(misses[misses$survived != "None",], aes(x = age, fill = survived)) +
  facet_wrap(~pclass) + geom_histogram(binwidth = 5) + ggtitle("Age for 'Miss.' by Pclass") +
  xlab("Age") +
  ylab("Total Count")
misses.alone <- misses[which(misses$sibsp == 0 & misses$parch == 0),]
summary(misses.alone$age)
length(which(misses.alone$age <= 14.5))
summary(data.combined$sibsp)
length(unique(data.combined$sibsp))
data.combined$sibsp<- as.factor(data.combined$sibsp)
ggplot(data.combined[1:891,], aes(x = sibsp, fill = survived)) + stat_count(width = 1) +
  facet_wrap(~pclass + title) + ggtitle("Pclass, Title") +
  xlab("SibSp") + ylab("Total Count") +
  ylim(0,300) + labs(fill = "Survived")
data.combined$parch<- as.factor(data.combined$parch)
ggplot(data.combined[1:891,], aes(x = parch, fill = survived)) +
  stat_count(width = 1) +
  facet_wrap(~pclass + title) + ggtitle("Pclass, Title") +
  xlab("ParCh") + ylab("Total Count") +
  ylim(0,300) +  labs(fill = "Survived")
temp.sibsp <- c(train$sibsp, test$sibsp)
temp.parch<- c(train$sibsp, test$sibsp)
data.combined$family.size<- as.factor(temp.sibsp + temp.parch + 1)
ggplot(data.combined[1:891,], aes(x = family.size, fill = survived)) +
  stat_count(width = 1) +
  facet_wrap(~pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

