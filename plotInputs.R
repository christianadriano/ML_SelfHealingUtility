#Plot the inputs (CRITICALITY,	CONNECTIVITY	RELIABILITY, UTILITY.DROP)
#How are the features distributed? Right skewed.
#How are the features correlated? Yes, medium to highly correlated.

library(ggplot2)
install.packages("Hmisc")
library("Hmisc")

  # load data
  source("C://Users//chris//OneDrive//Documentos//GitHub//ML_SelfHealingUtility//loadData.R");
  dataf<-loadData(fileName="Random_proper_comp_names.csv");
  
  #Remove all reliability and utility values equal to zero
  dataf<- dataf[dataf$RELIABILITY!=0,];
  dataf <- dataf[dataf$UTILITY.INCREASE!=0,] 

  #validationf<- loadData(fileName = "MLDATA2_STATIC.csv")

  summary(dataf);
  
  ggplot(data=dataf, aes(x=dataf$UTILITY.INCREASE)) +
    geom_bar(alpha=1, position="identity")+
    geom_vline(aes(xintercept=mean(dataf$UTILITY.INCREASE, na.rm=T)),   # Ignore NA values for mean
               color="red", linetype="dashed", size=1) +
    ggtitle("Utility increase distribution")+
    labs(x="Utility increase from disconnecting a component", 
         y="Frequency");
#    scale_x_discrete(limits=c(min(dataf$UTILITY.INCREASE):max(dataf$UTILITY.INCREASE)));
  
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
  #Data is not normal (did not reject the null hypothesis)
  shapiro.test(dataf$UTILITY.DROP);
  shapiro.test(dataf$CRITICALITY);
  shapiro.test(dataf$CONNECTIVITY);
  shapiro.test(dataf$RELIABILITY);
    
  matrixInput<-data.frame(dataf$UTILITY.DROP,dataf$CRITICALITY,dataf$CONNECTIVITY,dataf$RELIABILITY);
  colnames(matrixInput)<-c("Utility_Drop","Criticality","Connectivity","Reliability");
  
  res <- rcorr(data.matrix(matrixInput),type=c("spearman"));
  res$r
  
  ##CORRELATIONS
  #                Utility_Drop Criticality Connectivity Reliability
  # Utility_Drop   1.00000000  0.87580705   0.41702881 -0.09647729
  # Criticality    0.87580705  1.00000000  -0.05173893 -0.17340651
  # Connectivity   0.41702881 -0.05173893   1.00000000  0.11248654
  # Reliability   -0.09647729 -0.17340651   0.11248654  1.00000000
  
  ##p-values
  res$P
  #             Utility_Drop  Criticality Connectivity Reliability
  # Utility_Drop           NA 2.508149e-11   0.01575973   0.5932733
  # Criticality  2.508149e-11           NA   0.77491735   0.3345085
  # Connectivity 1.575973e-02 7.749174e-01           NA   0.5331181
  # Reliability  5.932733e-01 3.345085e-01   0.53311814          NA
  
  
qqnorm(dataf$CRITICALITY,main="Normal Q-Q Plot - Criticality")+ qqline();
qqline(dataf$CRITICALITY)
  
qqnorm(dataf$CONNECTIVITY,main="Normal Q-Q Plot - Connectivity") + qqline();
qqline(dataf$CONNECTIVITY)
  
qqnorm(dataf$RELIABILITY,main="Normal Q-Q Plot - Reliability")
qqline(dataf$RELIABILITY)
  

