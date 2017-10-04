#Plot the inputs (CRITICALITY	CONNECTIVITY	RELIABILITY, UTILITY.DROP)
#How are the features distributed?
#How are the features correlated?

library(ggplot2)

plotInputs<- function(fileName){
  
  fileName<-"failureRules_data.csv";
  setwd("C://Users//chris//OneDrive//Documentos//GitHub//ML_SelfHealingUtility//");
  
  data_all <- read.csv(fileName,header = TRUE,sep=",");
  
  dataf <- data.frame(data_all);
  summary(dataf)
  
  ggplot(data=dataf, aes(x=dataf$UTILITY.DROP)) +
    geom_histogram(binwidth = 0.5,alpha=.2, position="identity")+
    geom_vline(aes(xintercept=mean(dataf$UTILITY.DROP, na.rm=T)),   # Ignore NA values for mean
               color="red", linetype="dashed", size=1) +
    ggtitle("Utility drop distribution")+
    labs(x="Utility drop from disconnecting a component", 
         y="Frequency");
  
  ggplot(data=dataf, aes(x=dataf$CRITICALITY)) +
    geom_histogram(binwidth = 0.2,alpha=.2, position="identity")+
    geom_vline(aes(xintercept=mean(dataf$CRITICALITY, na.rm=T)),   # Ignore NA values for mean
               color="red", linetype="dashed", size=1) +
    ggtitle("Component criticality distribution")+
    labs(x="Criticality of the component", 
         y="Frequency");
 
  ggplot(data=dataf, aes(x=dataf$RELIABILITY)) +
    geom_histogram(binwidth = 0.1,alpha=.2, position="identity")+
    geom_vline(aes(xintercept=mean(dataf$RELIABILITY, na.rm=T)),   # Ignore NA values for mean
               color="red", linetype="dashed", size=1) +
    ggtitle("Component reliability distribution")+
    labs(x="Criticality of the component", 
         y="Frequency");
  
  #Correlations
  # Data is not normal
  shapiro.test(dataf$UTILITY.DROP);
  shapiro.test(dataf$CRITICALITY);
  shapiro.test(dataf$CONNECTIVITY);
  shapiro.test(dataf$RELIABILITY);
    
  matrixInput<-data.frame(dataf$UTILITY.DROP,dataf$CRITICALITY,dataf$CONNECTIVITY,dataf$RELIABILITY);
  colnames(matrixInput)<-c("Utility_Drop","Criticality","Connectivity","Reliability");
  
  cor(matrixInput, method="kendall",use="pairwise");
  #               Utility_Drop Criticality Connectivity Reliability
  # Utility_Drop    1.0000000   0.8496351    0.9023943   0.3956244
  # Criticality     0.8496351   1.0000000    0.6542992   0.4622369
  # Connectivity    0.9023943   0.6542992    1.0000000   0.3286646
  # Reliability     0.3956244   0.4622369    0.3286646   1.0000000
  
  
  #As we can see that the data is medium to strongly correlated
  
}

