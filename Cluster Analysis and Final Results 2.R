library("tidyverse", lib.loc="~/R/win-library/3.4")
library("factoextra", lib.loc="~/R/win-library/3.4")
library("cluster", lib.loc="C:/Program Files/R/R-3.4.3/library")
setwd("C:\\Users\\sorel\\Desktop\\MSc Data Science UQ\\Introduction to Data Science\\Semester Project\\Group Project")

###Logistic Regression Estimators
LogReg<-data.frame(read.csv("LogisticRegression_tranpose.csv", header = TRUE, sep=","))
colnames(LogReg)<-c('X', 'pid', 'Accuracy','X.Intercept.', 'acousticness', 'danceability', 'energy', 
                    'key1', 'key10', 'key11', 'key2',
                    'key3', 'key4', 'key5', 'key6', 'key7', 'key8', 'key9', 'liveness', 'log_duration_s',
                    'log_n', 'mode1', 'norm_instrumentalness', 'speechiness', 'time_signature1', 'time_signature3',
                    'time_signature4', 'time_signature5')
LogReg1<-data.frame(read.csv("LR2_trasnposed.csv", header = TRUE, sep=","))
LogReg2<-data.frame(read.csv("LR3_transposed.csv", header = TRUE, sep=","))

LogitReg<-na.omit(rbind(LogReg, LogReg1, LogReg2))
#Summary Statistics
summary(na.omit(LogitReg$Accuracy))


###Clustering For Decision Tree
#Getting Data Set
DTree<-data.frame(read.csv("DecisionTree_transposed.csv", header = TRUE, sep=","))[,2:14]
DTree1<-data.frame(read.csv("DT2_transposed.csv", header = TRUE, sep=","))[,2:14]
DTree2<-data.frame(read.csv("DT3_transposed.csv", header = TRUE, sep=","))[,2:14]

#Rbind both data sets
DecisionTree_raw<-rbind(DTree, DTree1, DTree2)
DecisionTree<-subset(DecisionTree_raw, DecisionTree_raw$Accuracy>0.6)

#Transform variables into same scale as accuracy
DecisionTree[,3:length(DecisionTree)]<-DecisionTree[,3:length(DecisionTree)]/100

###Run KMeans Iteratively for n_clust number of clusters and select number of clusters
set.seed(1234)
n_clust<-20
Models<-data.frame()
WSS<-list()

for (i in 2:n_clust){
  WSS[i]<-kmeans(DecisionTree[,3:length(DecisionTree)], centers = i, nstart=10, 
                 iter.max=100, algorithm=c("MacQueen"))$tot.withinss
}

##Selecting Number of Clusters
#Scree Plot
WSS<-WSS[2:length(WSS)]
elbow<-data.frame(2:n_clust, as.numeric(WSS))
colnames(elbow)<-c('ClusterNumber', 'WSS')

ggplot(elbow, aes(x = ClusterNumber, y = WSS)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1))

#Gap Statistic
set.seed(123)
gap_stat <- clusGap(DecisionTree[,3:length(DecisionTree)], FUN = kmeans, nstart = 10, K.max = 20, B = 100)
fviz_gap_stat(gap_stat)

#Final Cluster Number and visualization
Kmodel<-kmeans(DecisionTree[,3:length(DecisionTree)], centers = 10, nstart=10, iter.max=100, algorithm=c("MacQueen"))


#Mutate the data set to create HeatMap
cluster<-c(1:10)
center_df<-data.frame(cluster, Kmodel$centers)
colnames(center_df)<-c('Cluster', 'acoustic', 'dance', 'energy', 'key', 'live', 'logdurs', 'logn', 'mode', 'loudness', 'speech', 'timesign')
center_reshape<-gather(center_df, features, values, acoustic: timesign)
head(center_reshape)

#Create color pallete
library(RColorBrewer)
hm.palette <-colorRampPalette(rev(brewer.pal(10, 'RdYlGn')),space='Lab')

#Generate HeatMap
# Plot the heat map
p1<-ggplot(data = center_reshape, aes(x = features, y = Cluster, fill = values)) + 
  guides(fill = guide_legend(title = "Values", title.position = "left")) +
  scale_y_continuous(breaks = seq(1, 11, by = 1)) + 
  geom_tile() + theme(plot.title = element_text(hjust = 0.5)) +
  coord_equal() +
  scale_fill_gradientn(colours = hm.palette(90)) +
  theme_classic()+
  labs(title="Feature Importance Analysis",
       x ="Features", y = "Cluster")
p1


p2<-ggplot(data =  center_reshape, aes(x = features, y = Cluster)) + 
  geom_tile(aes(fill = values), colour = "white") +
  scale_y_continuous(breaks = seq(1, 10, by = 1))+
  scale_fill_gradientn(colours = terrain.colors(10), guide="colorbar")+
  guides(fill = guide_legend(title = "Values", title.position = "left"))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Feature Importance Analysis",
       x ="Features", y = "Cluster")
p2



