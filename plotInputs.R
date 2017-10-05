#Plot the inputs (CRITICALITY,	CONNECTIVITY	RELIABILITY, UTILITY.DROP)
#How are the features distributed? Right skewed.
#How are the features correlated? Yes, medium to highly correlated.

library(ggplot2)

plotInputs<- function(fileName){
  
  fileName<-"MLDATA2_data.csv";
  fileName<-"MLDATA2_STATIC.csv";
  setwd("C://Users//chris//OneDrive//Documentos//GitHub//ML_SelfHealingUtility//");
  
  data_all <- read.csv(fileName,header = TRUE,sep=",");
  
  dataf <- data.frame(data_all);
  summary(dataf);
  
  ggplot(data=dataf, aes(x=dataf$UTILITY.DROP)) +
    geom_bar(alpha=.2, position="identity")+
    geom_vline(aes(xintercept=mean(dataf$UTILITY.DROP, na.rm=T)),   # Ignore NA values for mean
               color="red", linetype="dashed", size=1) +
    ggtitle("Utility drop distribution")+
    labs(x="Utility drop from disconnecting a component", 
         y="Frequency");
  
  ggplot(data=dataf, aes(x=dataf$CRITICALITY)) +
    geom_bar(alpha=.2, position="identity")+
    geom_vline(aes(xintercept=mean(dataf$CRITICALITY, na.rm=T)),   # Ignore NA values for mean
               color="red", linetype="dashed", size=1) +
    ggtitle("Component criticality distribution")+
    labs(x="Criticality of the component", 
         y="Frequency")+
    scale_x_discrete(limits=c(min(dataf$CRITICALITY):max(dataf$CRITICALITY)));
 
  ggplot(data=dataf, aes(x=dataf$RELIABILITY)) +
    geom_histogram(binwidth = 0.1,alpha=.2, position="identity")+
    geom_vline(aes(xintercept=mean(dataf$RELIABILITY, na.rm=T)),   # Ignore NA values for mean
               color="red", linetype="dashed", size=1) +
    ggtitle("Component reliability distribution")+
    labs(x="Reliability of the component", 
         y="Frequency")+
    scale_x_discrete(limits=c(min(dataf$RELIABILITY):max(dataf$RELIABILITY)));
  
  ggplot(data=dataf, aes(x=dataf$CONNECTIVITY)) +
    geom_bar(alpha=.2, position="identity")+
    geom_vline(aes(xintercept=mean(dataf$CONNECTIVITY, na.rm=T)),   # Ignore NA values for mean
               color="red", linetype="dashed", size=1) +
    ggtitle("Component Connectivity distribution")+
   labs(x="Connectivity of the component", 
         y="Frequency")+
    scale_x_discrete(limits=c(min(dataf$CONNECTIVITY):max(dataf$CONNECTIVITY)));

#    
  
  #Correlations
  # Data is not normal
  shapiro.test(dataf$UTILITY.DROP);
  shapiro.test(dataf$CRITICALITY);
  shapiro.test(dataf$CONNECTIVITY);
  shapiro.test(dataf$RELIABILITY);
    
  matrixInput<-data.frame(dataf$UTILITY.DROP,dataf$CRITICALITY,dataf$CONNECTIVITY,dataf$RELIABILITY);
  colnames(matrixInput)<-c("Utility_Drop","Criticality","Connectivity","Reliability");
  
  cor(matrixInput, method="kendall",use="pairwise");
#                 Utility_Drop Criticality Connectivity Reliability
#  Utility_Drop    1.0000000   0.6989180    0.4979811   0.3579032
#  Criticality     0.6989180   1.0000000    0.1198513   0.3172049
#  Connectivity    0.4979811   0.1198513    1.0000000   0.3265254
#  Reliability     0.3579032   0.3172049    0.3265254   1.0000000
  
  
  #As we can see that the data is medium to strongly correlated
  
}

