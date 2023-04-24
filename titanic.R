install.packages("ggplot2")
install.packages("stringr")
install.packages("dbplyr")
install.packages("randomForest")
install.packages("caret")
install.packages("doSNOW")
install.packages("ipred")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("infotheo")
install.packages("Rtsne")
library(ggplot2)
library(stringr)
library(dbplyr)
library(randomForest)
library(caret)
library(doSNOW)
library(ipred)
library(rpart)
library(rpart.plot)
library(infotheo)
library(Rtsne)

setwd("~/R PROJ/DAVE ON DATA")

messy_train <- read.csv("train.csv", header = TRUE)
messy_test <- read.csv("test.csv", header =TRUE)

train <- messy_train [ , ! names(messy_train) %in% ("PassengerId")]
test <- messy_test [ , ! names(messy_test) %in% ("PassengerId")]

test.survied <- data.frame(Survived = rep("None", nrow(test)), test[,])

# COMBINE THE TRAIN & TEST SETS
data.combined <- rbind(train, test.survied)

str(data.combined)

data.combined$Survived <-as.factor(data.combined$Survived)
data.combined$Pclass <-as.factor(data.combined$Pclass)
data.combined$Embarked <-as.factor(data.combined$Embarked)
data.combined$Sex<-as.factor(data.combined$Sex)

table(data.combined$Survived)
table(data.combined$Pclass)

ggplot(train, aes(x = Pclass, fill = factor(Survived)))+
  geom_histogram(width = 0.5)+ 
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

head(as.character(train$Name))

#how many unique names are there across both train & test?
length(unique(as.character(data.combined$Name)))

# 2 duplicate names, take a closer look
# get the duplicate names & store as vector
dup.names <- as.character( data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])

# look @ records in combined data set
data.combined[which(data.combined$Name %in% dup.names),]

# correlation w. title & other vars
misses <- data.combined[which(str_detect(data.combined$Name, "Miss")),]
misses[1:5,]

mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs")),]
mrses[1:5,]

males <- data.combined[which(train$Sex == "male"),]
males[1:5,]

# create a new title col
extractTitle <-function(Name) {
  Name <- as.character(Name)
  
  if (length(grep("Miss." , Name)) >0) {
return("Miss.")
} else if (length(grep("Master.", Name)) >0) {
return("Master.")
} else if (length(grep("Mrs", Name)) >0) {
return("Mrs.")
} else if (length(grep("Mr", Name)) >0) {
return("Mr.")
} else {
return("Other")
}
}

titles <- NULL
for (l in 1:nrow(data.combined)) {
  titles <-c(titles, extractTitle(data.combined[l,"Name"]))
}

data.combined$Title <- as.factor(titles)

#only use 1st 891 rows as this is the train set data w. Survived
ggplot(data.combined[1:891,], aes(x = Title, fill = Survived))+
  geom_bar(width = 0.5)+ 
  facet_wrap(~Pclass)+
  ggtitle(~Pclass)+
  xlab("Title") +
  ylab("Total Count") +
    labs(fill = "Survived")
  
  #DISTRIBUTION OF FEMALES TO MALES ACCROSS TRAIN & TEST
  table(data.combined$Sex)
  
  # 3-way relationship sex pclass & survival
ggplot(data.combined[1:891,], aes(x =Sex, fill = Survived)) +
  geom_bar(binwidth = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab ("Total Count") +
  labs (fill = "Survived")
  
summary(data.combined$Age)
summary(data.combined[1:891,"Age"])

#survival by sex pclass & age
ggplot(data.combined[1:891,], aes(x =Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab ("Total Count")

#validate master is good proxy for male child
lads <-data.combined[which(data.combined$Title == "Master."),]
summary(lads$Age)

# & for Miss
misses <-data.combined[which(data.combined$Title == "Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None",], aes(x =Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_histogram(bidwidth = 5)+
  ggtitle("Age for 'Miss' by Pclass") +
  xlab("Age") +
  ylab ("Total Count") 


misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))

summary(data.combined$SibSp)

length(unique(data.combined$SibSp))


#survival by sibsp pclass & title
ggplot(data.combined[1:891,], aes(x =SibSp, fill = Survived)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title")+
  xlab("Sibsp") +
  ylab ("Total Count")+
  ylim(0,300)+
  labs(fill = "survived")
  
# & by Parch
  temp.sibsp <- c(train$SibSp, test$SibSp)
  
#feature engineer a family size
  temp.sibsp <- c(train$SibSp, test$SibSp)
  temp.parch <- c(train$Parch, test$Parch)
  data.combined$family.size <- as.integer(temp.sibsp + temp.parch +1)

  ggplot(data.combined[1:891,], aes(x =family.size, fill = Survived)) +
    geom_histogram(binwidth = 1) +
    facet_wrap(~Pclass + Title) +
    ggtitle("Pclass, Title")+
  xlab("family.size") +
    ylab ("Total Count")+
    ylim(0,300)+
    labs(fill = "survived")

#take look @ ticket variable
str(data.combined$Ticket)

data.combined$Ticket <-as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

Ticket.1st.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1,1))
unique(Ticket.1st.char)


data.combined$Ticket.1st.char <-as.character(Ticket.1st.char)

ggplot(data.combined[1:891,], aes(x =Ticket.1st.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survived by Ticket.1st.char")+
xlab("Ticket.1st.char") +
  ylab ("Total Count")+
  ylim(0,350)+
  labs(fill = "survived")
 
ggplot(data.combined[1:891,], aes(x =Ticket.1st.char, fill = Survived)) +
  geom_bar()+ 
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Ticket.1st.char") +
  ylab ("Total Count")+
  ylim(0,150)+
  labs(fill = "survived")

ggplot(data.combined[1:891,], aes(x =Ticket.1st.char, fill = Survived)) +
  geom_bar()+ 
  facet_wrap(~Pclass + Title)+
  ggtitle("Pclass + Title")+
  xlab("Ticket.1st.char") +
  ylab ("Total Count")+
  ylim(0,200)+
  labs(fill = "survived")

#look @ fraes
summary(data.combined$Fare)
length(unique(data.combined$Fare))

ggplot(data.combined, aes(x =Fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Combined Faer Distribution")+
xlab("fare") +
  ylab ("Total Count")+
  ylim(0,200)

ggplot(data.combined[1:891,], aes(x =Fare, fill = Survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass + Title)+
  ggtitle("Pclass, Title")+
xlab("fare") +
  ylab ("Total Count")+
  ylim(0,50)+
  labs(fill = "Survived")

#Analysis of cabin var
str(data.combined$Cabin)
data.combined$Cabin[1:100]

# replace empty cabins with "U"
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]

Cabin.1st.char <- as.character (substr(data.combined$Cabin, 1,1))
str(Cabin.1st.char)

#add cabin 1st char to data set
data.combined$Cabin.1st.char <-as.character(Cabin.1st.char)

ggplot(data.combined[1:891,], aes(x =Cabin.1st.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survived by Cabin.1st.char")+
  xlab("Cabint.1st.char") +
  ylab ("Total Count")+
  ylim(0,750)+
  labs(fill = "survived")

ggplot(data.combined[1:891,], aes(x =Cabin.1st.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass)+
  ggtitle("Survived by Cabin.1st.char")+
  xlab("Pclass") +
  ylab ("Total Count")+
  ylim(0,500)+
  labs(fill = "survived")

ggplot(data.combined[1:891,], aes(x =Cabin.1st.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title)+
  ggtitle("Pclass + Title")+
  xlab("cabin.1st.char") +
  ylab ("Total Count")+
  ylim(0,500)+
  labs(fill = "survived")

#what about people with multiple cabins?
data.combined$Cabin.multi <- as.character(ifelse(str_detect(data.combined$Cabin ," "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x =Cabin.multi, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title)+
  ggtitle("Pclass + Title")+
  xlab("cabin.multi") +
  ylab ("Total Count")+
  ylim(0,350)+
  labs(fill = "survived")

#does survival depend on where boarded
str(data.combined$Embarked)
levels((data.combined$Embarked))

ggplot(data.combined[1:891,], aes(x =Embarked, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title)+
  ggtitle("Pclass + Title")+
  xlab("emabrked") + 
  ylab ("Total Count")+
  ylim(0,300)+
  labs(fill = "survived")

 ###################EXPLORATORY MODELING PART 1#####################

#train a random forest witht he default parameters using pclass & title
rf.train.1 <- data.combined[1:891, c("Pclass", "Title")]
rf.label <-as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y =rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

#train a random forest using pclass title & sibsp
rf.train.2 <- data.combined[1:891, c("Pclass", "Title", "SibSp")]
set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y =rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)

#train a random forest using pclass title & parch
rf.train.3 <- data.combined[1:891, c("Pclass", "Title", "Parch")]
set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y =rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)


#train a random forest using pclass title sibsp & parch
rf.train.4 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "Parch")]
set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y =rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)

#train a random forest using pclass title &family size
rf.train.5 <- data.combined[1:891, c("Pclass", "Title", "family.size")]
set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y =rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)

#train a random forest using pclass title sibsp &family size
rf.train.6 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "family.size")]
set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y =rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)

#train a random forest using pclass title parch &family size
rf.train.7 <- data.combined[1:891, c("Pclass", "Title", "Parch", "family.size")]
set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y =rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)

###################################CROSS VALIDATION##########################

#SUBMIT RF.5 TO KAGGLE TO SEE IF 00B ERROR RATE IS ACCURATE
#SUBSET TEST RECORDS & FEATURES
test.submit.df <- data.combined[892:1309, c ("Pclass","Title", "family.size" )]

#make a prediction
rf.5.predict <-predict(rf.5, test.submit.df)
table(rf.5.predict)

#transfrorm predictions into a csv file for kaggle
submit.df <- data.frame(PassengerId = rep (892:1309), Survived = rf.5.predict)
write.csv (submit.df, file =  "titanic_1.csv", row.names = FALSE)

#scored 0.779 but oob predicted 0817
#try caret package to get more accurate
set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)

#check stratification
table(rf.label)
342/549

table(rf.label[cv.10.folds[[33]]])
308/494

#setup carets train control object
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,index = cv.10.folds)

#setup doSNOW package for multicore training
cl <-makeCluster(6,type = "SOCK")
registerDoSNOW(cl)

#set seed for reproducibility & train
set.seed(34324)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, ntree = 1000, trControl = ctrl.1)

#shutdown cluster
stopCluster(cl)

#check out results
rf.5.cv.1

#5 fold repeated 10 times
set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)
ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10,index = cv.5.folds)
cl <-makeCluster(6,type = "SOCK")
registerDoSNOW(cl)
set.seed(89472)
rf.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, ntree = 1000, trControl = ctrl.2)
stopCluster(cl)
rf.5.cv.2

#3 fold repeated 10 times
set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)
ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10,index = cv.3.folds)
cl <-makeCluster(6,type = "SOCK")
registerDoSNOW(cl)
set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3, ntree = 1000, trControl = ctrl.3)
stopCluster(cl)
rf.5.cv.3

###################EXPLORITORY MODELIING PT2######################

#create utillity function
rpart.cv <- function(seed, training, labels, ctrl) {
cl <-makeCluster(6,type = "SOCK")
registerDoSNOW(cl)

set.seed(seed)
rpart.cv <- train(x =training, y = labels, method = "rpart", tuneLength = 30, trControl =ctrl)
stopCluster(cl)
return (rpart.cv)
}

#grab features
features <-c("Pclass", "Title", "family.size")
rpart.train.1 <-data.combined[1:891, features]

#run cross validation & see results
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1

#plot
prp(rpart.1.cv.1$finalModel, type=0, extra = 1, under =TRUE)

#MR & OTHER HAVE PERISH PREDICTION ACCURACY OF 83.2%
#MASTER MR & MRS ARE IN 1ST & 2ND CLASS HAVE SURVIVAL ACCURACY OF 94.9%

table(data.combined$Title)
#Parse out lastname & title
data.combined[1:25, "Name"]
name.splits <-str_split(data.combined$Name, ",")
name.splits[1]
last.names <- sapply(name.splits, "[", 1)
last.names[1:10]

#add last names to dataframe incase its usfull later
data.combined$last.name <- last.names

#now do it for titles
name.splits <- str_split(sapply(name.splits, "[",2), " ")
titles <- sapply(name.splits, "[", 2)
unique(titles)

#explore the title of "the"
data.combined[which(titles == "the"),]

#re-map titles to be more exact
titles[titles %in% c("Dona.", "the")] <- "Lady."
titles[titles %in% c("Ms.", "Mlle.")] <- "Miss."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Capt.", "Major.")] <- "Officer"
table(titles)

data.combined$new.title<- as.factor(titles)

ggplot(data.combined[1:891,],aes (x = new.title, fill = Survived))+
geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Survival rates for new title by pclass")

#collapse titles based on the graph
indexes <- which(data.combined$new.title == "Lady.")
data.combined$new.title[indexes] <- "Mrs."

indexes <- which(data.combined$new.title == "Dr." |
                   data.combined$new.title == "Rev." |
                   data.combined$new.title == "Sir." |
                   data.combined$new.title == "Officer")
data.combined$new.title[indexes] <- "Mr."

ggplot(data.combined[1:891,],aes (x = new.title, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Survival rates for new title by pclass")

#grab features
features <- c("Pclass", "new.title", "family.size")
rpart.train.2 <- data.combined[1:891, features]
#run cv
rpart.2.cv.1 <- rpart.cv(96422, rpart.train.2, rf.label, ctrl.3)
rpart.2.cv.1

#plot
prp(rpart.2.cv.1$finalModel, type=0, extra = 1, under =TRUE)

#look @ 1st MR
indexs.1st.mr <- which(data.combined$new.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexs.1st.mr, ]
summary(first.mr.df)

#look @ female
first.mr.df[first.mr.df$Sex == "female",]

#update her new title
indexes <- which(data.combined$new.title == "Mr." &
                   data.combined$Sex == "female")
data.combined$new.title[indexes] <- "Mrs."

#any other other titles mis classified
length(which(data.combined$Sex == "female" &
               (data.combined$new.title == "Master." |
               data.combined$new.title == "Mr.")))

#refresh the data frame
indexs.1st.mr <- which(data.combined$new.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexs.1st.mr, ]

#look @ 1st class mr survival rate
summary(first.mr.df[first.mr.df$Survived == "1",])
View(first.mr.df[first.mr.df$Survived == "1",])

#look @ high fares
indexes <-which (data.combined$Ticket == "PC 17755" |
                   data.combined$Ticket == "PC 17611" |
                   data.combined$Ticket == "113760")
View(data.combined[indexes,])

ggplot(first.mr.df, aes(x =Fare, fill = Survived )) +
  geom_density(alpha = 0.5) +
  ggtitle(" 1st calss mr survival rates by fare")
                   
#feature engineer based on all passengers with same ticket
ticket.party.size <- rep(0, nrow(data.combined))
ave.fare <- rep(0.0, nrow(data.combined))
tickets <-unique(data.combined$Ticket)

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$Ticket == current.ticket)
  current.ave.fare <- data.combined[party.indexes[1], "Fare"] / length(party.indexes)

for (k in 1:length(party.indexes)) {
  ticket.party.size[party.indexes[k]] <-length(party.indexes)
  ave.fare[party.indexes[k]] <- current.ave.fare
  }
}

data.combined$ticket.party.size <- ticket.party.size
data.combined$ave.fare <- ave.fare

#REFRESH 1ST CLASS MR DATAFRAME
first.mr.df <- data.combined[indexs.1st.mr, ]
summary(first.mr.df)

ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x= ticket.party.size, fill = Survived))+
  geom_density(alpha = 0.5) +
  ggtitle("1st calss mr survival rates by ticket party size")

ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x= ave.fare, fill = Survived))+
  geom_density(alpha = 0.5) +
  ggtitle("1st calss mr survival rates by ave fare")
                   
#is ticket.party.size correlated with ave.fare?
summary(data.combined$ave.fare)

#look @ that missing value
data.combined[is.na(data.combined$ave.fare), ]

#GET RECORDS FOR SIMILAR PASSENGERS &  SUMMARISE AVE FARES
indexes <- with(data.combined, which(Pclass == "3" & Title == "Mr." & family.size == 1 & Ticket != "3701"))
similar.na.passengers <- data.combined[indexes,]
summary(similar.na.passengers$ave.fare)

#use median
data.combined[is.na(ave.fare), "ave.fare"] <-7.840

#LEVERAGE CARETS PREPROCESS FUNCTION TO NORMALIZE DATA
preproc.data.combined <- data.combined[, c("ticket.party.size", "ave.fare")]
preProc <- preProcess(preproc.data.combined, method = c ("center", "scale"))
postproc.data.combined <- predict(preProc, preproc.data.combined)

#calc the corelation
cor(postproc.data.combined$ticket.party.size, postproc.data.combined$ave.fare)

#try cor for all of 1st class
indexes <- which(data.combined$Pclass == "1")
cor(postproc.data.combined$ticket.party.size[indexes],
    postproc.data.combined$ave.fare[indexes])

#see if the feature engineering has made any difference
features <- c("Pclass", "new.title", "family.size", "ticket.party.size", "ave.fare")
rpart.train.3 <-data.combined[1:891, features]

#run cv
rpart.3.cv1 <- rpart.cv(94622, rpart.train.3, rf.label, ctrl.3)
rpart.3.cv1

#plot
prp(rpart.3.cv1$finalModel, type= 0, extra = 1, under = TRUE)

##########################SUBMISSION & FINAL THOUGHTS'#######################

#SUB TEST RECORDS & FEATURES
test.submit.df <- data.combined[892:1309, features]

#make predictions
rpart.3.predict <-predict(rpart.3.cv1$finalModel, test.submit.df, type = "class")
rpart.3.predict
table(rpart.3.predict)

#write a csv file for submission to kaggle
submit2.df <-data.frame(PassengerID = rep(892:1309),Survived = rpart.3.predict)
write.csv(submit2.df, file = "titanic_2.csv", row.names = FALSE)
#score 0.77

features <-c("Pclass", "new.title", "ticket.party.size", "ave.fare")
rf.train.temp <-data.combined[1:891, features]

set.seed(1234)
rf.temp <-randomForest(x = rf.train.temp, y = rf.label, ntree = 1000)
rf.temp

test.submit.df <- data.combined[892:1309, features]
rand_for.predict <-predict(rf.temp, test.submit.df)
table(rand_for.predict)

#write a csv file for submission to kaggle
submit3.df <-data.frame(PassengerID = rep(892:1309),Survived = rpart.3.predict)
write.csv(submit2.df, file = "titanic_3.csv", row.names = FALSE)
#score 0.77

#EXPLORE MUTUAL INFORMATION

mutinformation(rf.label, data.combined$Pclass[1:891])
mutinformation(rf.label, data.combined$Sex[1:891])
mutinformation(rf.label, data.combined$SibSp[1:891])
mutinformation(rf.label, data.combined$Parch[1:891])
mutinformation(rf.label, discretize(data.combined$Fare[1:891]))
mutinformation(rf.label, data.combined$Embarked[1:891])
mutinformation(rf.label, data.combined$Title[1:891])
mutinformation(rf.label, data.combined$family.size[1:891])
mutinformation(rf.label, data.combined$Ticket.1st.char[1:891])
mutinformation(rf.label, data.combined$Cabin.multi[1:891])
mutinformation(rf.label, data.combined$new.title[1:891])
mutinformation(rf.label, data.combined$ticket.party.size[1:891])
mutinformation(rf.label, discretize(data.combined$ave.fare[1:891]))

#leverage tsne algortithum to create a 2D representation of the data
#start w, women & children
most.correct <-data.combined[data.combined$new.title != "Mr.",]
indexes <-which(most.correct$Survived != "None")

tsne.1 <-Rtsne(most.correct[,features], check_duplicates = FALSE)
dim(tsne.1$Y)

ggplot(NULL, aes(x = tsne.1$Y[indexes, 1], y = tsne.1$Y[indexes, 2],
                 color = most.correct$Survived[indexes]))+
  geom_point()+
  labs(color = "Survived")+
  ggtitle("TSNE 2D VISUAL OF FEATURES FOR WOMEN & CHILDREN")

#get a baseline by using conditional mutual information on tsne x&y features
#for women & children IN 1ST&2ND CLASS this combination maybe higher
#than any individual feature
condinformation(most.correct$Survived[indexes], discretize(tsne.1$Y[indexes,]))

#leverage CMI using top 2 features in tree plot - new.title & pclass
condinformation(rf.label, data.combined[1:891, c ("new.title", "Pclass")])

#visualize adult males with tsnse (biggest upside for improvement here as 86 predictions are inaccurate)
misters <-data.combined[data.combined$new.title == "Mr.",]
indexes <-which(misters$Survived != "None")

tsne.2 <-Rtsne(misters[,features], check_duplicates = FALSE)

ggplot(NULL, aes(x = tsne.2$Y[indexes, 1], y = tsne.2$Y[indexes, 2],
                 color = misters$Survived[indexes]))+
  geom_point()+
  labs(color = "Survived")+
  ggtitle("TSNE 2D VISUAL OF FEATURES FOR NEW TITLE OF MR")

#conditional mutal information score for tsne male features
condinformation(misters$Survived[indexes], discretize(tsne.2$Y[indexes,]))

#create TSNE features for all the training data & use it in the model
tsne.3<- Rtsne(data.combined[, features], check_duplicates = FALSE)

ggplot(NULL, aes(x = tsne.3$Y[indexes, 1], y = tsne.3$Y[indexes, 2],
                 color = misters$Survived[indexes]))+
  geom_point()+
  labs(color = "Survived")+
  ggtitle("TSNE 2D VISUAL OF FEATURES ALL TRAINING DATA")

condinformation(data.combined$Survived[1:891], discretize(tsne.3$Y[1:891]))

#add the tsne features to our data frame for use in model building
data.combined$tnse.x <- tsne.3$Y[,1]
data.combined$tnse.y <- tsne.3$Y[,2]


