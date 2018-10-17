##### EDA for Track_id data set #####
library("tidyverse", lib.loc="~/R/win-library/3.4")
library("ggplot2", lib.loc="~/R/win-library/3.4")
library("ggcorrplot", lib.loc="~/R/win-library/3.4")

###1. Load Data Set
setwd("C:\\Users\\sorel\\Desktop\\MSc Data Science UQ\\Introduction to Data Science\\Semester Project\\Group Project")
TDS<-read.csv("TrackDataSet.csv", header = TRUE, sep=",")
TDS<-TDS[,2:length(TDS)]

###2. Summary Statistics and Data Types
str(TDS)
summary(TDS)

#Compute number of times song appears in playlist
TDS_freqz<-TDS %>% group_by(id) %>% filter(n()>0) %>% summarize(n=n())

#Unique Rows from TDS
TDS_raw<-unique(TDS)

#Merge both data frames
TDS_merge<-merge(TDS_raw,TDS_freqz,by="id")

#Separate Data Set in purely numeric and factor candidates features
TDS_num<-subset(as.data.frame(TDS_merge),select=c('id', 'acousticness','danceability', 'duration_ms', 
                            'energy', 'instrumentalness', 'liveness', 'loudness', 'speechiness', 'n'))
TDS_fac<-subset(as.data.frame(TDS_merge),select=c('id', 'key', 'mode', 'time_signature'))

###Sampling
sTDS_num<-TDS_num[sample(1:nrow(TDS_num),10000,replace=TRUE),]

#Scatterplot Matrix and Histograms
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use="complete.obs")
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor*0.5, col=c("gray60", "black")[(abs(r)>0.65)+1])
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2],0,1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

pairs(sTDS_num[,c('acousticness','danceability', 'duration_ms', 'energy', 'instrumentalness', 
                  'liveness', 'loudness', 'speechiness', 'n')],
      upper.panel = panel.cor, diag.panel = panel.hist)

#####Explore Concentrated Pairs (Duration, Instrumentalness, n)
###Instrumentalness
ggplot(TDS_num, aes(x=TDS_num$instrumentalness))+
  geom_histogram(color="darkblue", fill="lightblue", binwidth=0.1)+
  geom_vline(aes(xintercept=mean(TDS_num$instrumentalness)),color="blue", linetype="dashed", size=1)+
  labs(title="Key Histogram Plot",x="Instrumentalness", y = "Count")

#Interesting pairs
ggplot(TDS_num, aes(x=log(TDS_num$instrumentalness+1)))+
  geom_histogram(color="darkblue", fill="lightblue", binwidth=0.1)+
  geom_vline(aes(xintercept=mean(log(TDS_num$instrumentalness+1))),color="blue", linetype="dashed", size=1)+
  labs(title="Key Histogram Plot",x="Instrumentalness", y = "Count")

###Duration_ms
ggplot(TDS_num, aes(x=TDS_num$duration_ms/1000))+
  geom_histogram(color="darkblue", fill="lightblue", binwidth=10)+
  geom_vline(aes(xintercept=mean(TDS_num$duration_ms/1000)),color="blue", linetype="dashed", size=1)+
  labs(title="Key Histogram Plot",x="Duration MS", y = "Count")
#Interesting pairs
ggplot(TDS_num, aes(x=log(TDS_num$duration_ms/1000)))+
  geom_histogram(color="darkblue", fill="lightblue", binwidth=0.1)+
  geom_vline(aes(xintercept=mean(log(TDS_num$duration_ms/1000))),color="blue", linetype="dashed", size=1)+
  labs(title="Key Histogram Plot",x="Duration MS", y = "Count")

###n
ggplot(TDS_num, aes(x=TDS_num$n))+
  geom_histogram(color="darkblue", fill="lightblue", binwidth=10)+
  geom_vline(aes(xintercept=mean(TDS_num$n)),color="blue", linetype="dashed", size=1)+
  labs(title="Key Histogram Plot",x="n", y = "Count")
#Interesting pairs
ggplot(TDS_num, aes(x=log(TDS_num$n)))+
  geom_histogram(color="darkblue", fill="lightblue", binwidth=1)+
  geom_vline(aes(xintercept=mean(log(TDS_num$n))),color="blue", linetype="dashed", size=1)+
  labs(title="Song Appearance Freq. Histogram Plot",x="n", y = "Count")

#Correlation Matrix
corrM<-cor(TDS_num[,-1])
ggcorrplot(corrM)

###Boxplots for categorical features
par(mfrow=c(3,1))
key_box<-boxplot(TDS_fac$key,main="Boxplot Key Feature", ylab="Values")
mode_box<-boxplot(TDS_fac$mode,main="Boxplot Mode Feature", ylab="Values")
time_signature_box<-boxplot(TDS_fac$time_signature,main="Boxplot Time Signature Feature", ylab="Values")


theme_set(theme_bw())
#Histograms for categorical features
g <- ggplot(TDS_fac, aes(key))
g + geom_bar(aes(fill=mode), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + scale_fill_brewer(palette = "Set3") +
  labs(title="Key Distribution ", x="Keys", y = "Count",
       subtitle="Mode (minor or major) per keys")

g <- ggplot(TDS_fac, aes(time_signature))
g + geom_bar(aes(fill=mode), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Time Signature Distribution", x="Time Signature", y = "Count",
       subtitle="Mode (minor or major) per Time Signature")



###Histograms for categorical features
par(mfrow=c(1,1))
ggplot(TDS_fac, aes(x=TDS_fac$key))+
  geom_histogram(color="darkblue", fill="lightblue", binwidth=2)+
  geom_vline(aes(xintercept=mean(TDS_fac$key)),color="blue", linetype="dashed", size=1)+
  labs(title="Key Histogram Plot",x="Keys", y = "Count")

ggplot(TDS_fac, aes(x=TDS_fac$mode))+
  geom_histogram(color="darkblue", fill="lightblue", binwidth=1)+
  geom_vline(aes(xintercept=mean(TDS_fac$mode)),color="blue", linetype="dashed", size=1)+
  labs(title="Mode Histogram Plot",x="Mode", y = "Count")

ggplot(TDS_fac, aes(x=TDS_fac$time_signature))+
  geom_histogram(color="darkblue", fill="lightblue", binwidth=1)+
  geom_vline(aes(xintercept=mean(TDS_fac$time_signature)),color="blue", linetype="dashed", size=1)+
  labs(title="Time Signature Histogram Plot",x="Time Signature", y = "Count")

#Normality Testing
shapiro.test(sTDS_num$acousticness)
shapiro.test(sTDS_num$duration_ms)
shapiro.test(sTDS_num$danceability)
shapiro.test(sTDS_num$energy)
shapiro.test(sTDS_num$instrumentalness)
shapiro.test(sTDS_num$liveness)
shapiro.test(sTDS_num$loudness)
shapiro.test(sTDS_num$speechiness)
shapiro.test(sTDS_num$n)


###Transformed Features
TDS_num$duration_ms<-log(TDS_num$duration_ms/1000)
TDS_num$instrumentalness<-(TDS_num$instrumentalness-min(TDS_num$instrumentalness))/(max(TDS_num$instrumentalness)-min(TDS_num$instrumentalness))
TDS_num$n<-log(TDS_num$n)
TDS_fac$key<-as.factor(TDS_fac$key)
TDS_fac$mode<-as.factor(TDS_fac$mode)
TDS_fac$time_signature<-as.factor(TDS_fac$time_signature)

TDS_Merge<-merge(TDS_num,TDS_fac,by="id")

###OUTLIER DETECTION
#Using LOF
library("DMwR", lib.loc="~/R/win-library/3.4")
outlier.scores<-lofactor(TDS_Merge[,2:10], k=11)
outliers<-order(outlier.scores, decreasing=T)[1:2000]

#Using PCA to reduce number of features and perform outlier detection over those
library("factoextra", lib.loc="~/R/win-library/3.4")

set.seed(100)
pc_TDS<-prcomp(TDS_Merge[,2:10], scale=TRUE)
fviz_eig(pc_TDS)
#Getting original coordinates in the new feature space
scores<-as.data.frame(pc_TDS$x) 

#Flagging Outliers using Boxplots
box1<-boxplot(scores$PC1,main ="Boxplot PC1", ylab="PC1")
out_index1=which(scores$PC1 %in% box1$out)

box2<-boxplot(scores$PC2, main = "Boxplot PC2", ylab="PC2")
out_index2=which(scores$PC2 %in% box2$out)

box3<-boxplot(scores$PC3, main = "Boxplot PC3", ylab="PC3")
out_index3<-which(scores$PC3 %in% box3$out)

out_index<-unique(c(out_index1))

#Data Set Without Outliers
TDS_Merge[as.numeric(rownames(TDS_Merge)) %in% out_index,2:length(TDS_Merge)]<-NA
TDS_na<-na.omit(TDS_Merge)

#Check if the data structure has changed
#Scatterplot Matrix and Histograms
sTDS_na<-TDS_na[sample(1:nrow(TDS_na),10000,replace=TRUE),]
pairs(sTDS_na[,c('acousticness','danceability', 'duration_ms', 'energy', 'instrumentalness', 'liveness', 'loudness', 'speechiness', 'n')],
      upper.panel = panel.cor, diag.panel = panel.hist)

#Correlation Matrix
corrM2<-cor(TDS_na[,2:10])
ggcorrplot(corrM2)

#Export Cleansed Data Set
colnames(TDS_na)<-c('id', 'acousticness', 'danceability', 'log_duration_s', 'energy', 'norm_instrumentalness', 'liveness',
                    'loudness', 'speechiness', 'log_n', 'key', 'mode', 'time_signature')
write.csv(TDS_na, file="CleanedTrackDataSet.csv", sep=";")

####Clear Enviroment####
remove(box1)
remove(box2)
remove(box3)
remove(out_index)
remove(out_index1)
remove(out_index2)
remove(out_index3)
remove(panel.cor)
remove(panel.hist)
remove(pc_TDS)
remove(scores)
remove(sTDS_num)
remove(TDS)
remove(TDS_fac)
remove(TDS_freqz)
remove(TDS_merge)
remove(TDS_Merge)
remove(TDS_num)
remove(TDS_raw)


###GET THE DATA SETS
library("caret", lib.loc="~/R/win-library/3.4")
library("rpart.plot", lib.loc="~/R/win-library/3.4")
library(party)
pids<-read.csv("PlaylistDataSet.csv", header = TRUE, sep=",")[1,]
pids<-colnames(pids[,2:length(pids)])
trackids<-read.csv("PlaylistDataSet.csv", header = TRUE, sep=",")[,1]

#Data Objects to store Estimates and Accuracy for Logfit and Variable Importance and Accuracy for Tree
LogisticRegression<-data.frame()
DecisionTree<-data.frame()

init<-2
final<-2002
for (i in init:final){
pid<-pids[79]#
#Get label
label<-read.csv("PlaylistDataSet.csv", header = TRUE, sep=",")[,79]

#Create Playlist Data Set
PDS<-data.frame(trackids, label)

#Transform label to factors
colnames(PDS)<-c('id', 'label')
PDS$label<-as.factor(PDS$label)
#levels(PDS$label)<-c("No", "Yes")
levels(PDS$label)<-make.names(levels(factor(PDS$label)))

#Create Labeled Data Set
LDS<-merge(TDS_na,PDS,by="id")

#Create Training Data Set
labeled<-subset(as.data.frame(LDS), LDS$label=='X1')
unlabeled<-subset(as.data.frame(LDS), LDS$label=='X0')
unlabeled_sample<-unlabeled[sample(1:nrow(unlabeled), nrow(labeled),replace=TRUE),]
bDS<-rbind(labeled, unlabeled_sample)
#Train Model
set.seed(1234)
#Data Split
trainIndex<-createDataPartition(bDS$label, p = 0.7, list = FALSE, times = 1)
train<-bDS[trainIndex,]
test<-bDS[-trainIndex,]
#Train Control Function
gc()
fitControl<-trainControl(method = "repeatedcv", number = 10, repeats = 10, classProbs = TRUE, 
                        summaryFunction = twoClassSummary)


#Logistic Regression
set.seed(1234)
logfit<-train(label~acousticness+danceability+log_duration_s+
                     energy+norm_instrumentalness+liveness+speechiness+log_n+
                     key+mode+time_signature,
                     data=train, method = "glm", family="binomial", trControl = fitControl)
fit1<-predict(logfit, newdata=test)
CM1<-confusionMatrix(data = fit1, test$label)
logAcc<-CM1$overall['Accuracy']
logFeatures<-as.vector(rownames(summary(logfit)$coefficients))
Estimates<-summary(logfit)$coefficients[,1]
df_log<-data.frame(pid, logFeatures, Estimates, logAcc)
colnames(df_log)<-c('pid','LogFeatures','Estimates', 'Accuracy')
rownames(df_log)<-c()
LogisticRegression<-rbind(LogisticRegression, df_log)


#Decision Tree
set.seed(1234)
Grid<-expand.grid(.cp=seq(0.01, 1, 0.01))
dtreefit<-train(label~acousticness+danceability+log_duration_s+
                  energy+norm_instrumentalness+liveness+speechiness+log_n+loudness+
                  key+mode+time_signature, data = train, method = "rpart",
                tuneGrid=Grid, metric="Accuracy",
                trControl=fitControl)
fit2<-predict(dtreefit, newdata=test)
CM2<-confusionMatrix(data = fit2, test$label)
ctreeVarImp=varImp(dtreefit)
rpart.plot(dtreefit$finalModel)
plot(dtreefit)

treeAcc<-CM2$overall['Accuracy']
treeFeatures<-as.vector(rownames(ctreeVarImp$importance))
treeImportance<-ctreeVarImp$importance
df_tree<-data.frame(pid, treeFeatures, treeImportance, treeAcc)
colnames(df_tree)<-c('pid','TreeFeatures','Importance 0', 'Importance 1', 'Accuracy')
rownames(df_tree)<-c()
DecisionTree<-rbind(DecisionTree, df_tree)
}

write.csv(DecisionTree, file="DecisionTree.csv")
write.csv(LogisticRegression, file="LogisticRegression.csv")





