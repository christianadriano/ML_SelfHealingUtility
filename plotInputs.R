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

  
###########################################################################
# 
  
  ggplot(data=dataf, aes(x=dataf$CONNECTIVITY, y=dataf$UTILITY.INCREASE)) +
    geom_point() + geom_smooth(method="lm")+
    ggtitle("Utility by CONNECTIVITY ")+
    labs(x="Connectivity of the component", 
         y="Utility Increase")+
    scale_x_discrete(limits=c(min(dataf$CONNECTIVITY):max(dataf$CONNECTIVITY)));
  
  ggplot(data=dataf, aes(x=dataf$CRITICALITY, y=dataf$UTILITY.INCREASE)) +
    geom_point() + geom_smooth(method="lm")+
    ggtitle("Utility by CRITICALITY ")+
    labs(x="Criticality of the component", 
         y="Utility Increase")+
    scale_x_discrete(limits=c(min(dataf$CRITICALITY):max(dataf$CRITICALITY)));
  
  ggplot(data=dataf, aes(x=dataf$RELIABILITY, y=dataf$UTILITY.INCREASE)) +
    geom_point() + geom_smooth(method="lm")+
    ggtitle("Utility by RELIABILITY ")+
    labs(x="Reliability of the component", 
         y="Utility Increase");
    scale_x_discrete(limits=c(min(dataf$RELIABILITY):max(dataf$RELIABILITY)));
  
  
###########################################################################  
# Normality test

qqnorm(dataf$CRITICALITY,main="Normal Q-Q Plot - Criticality")+ qqline();
qqline(dataf$CRITICALITY)
  
qqnorm(dataf$CONNECTIVITY,main="Normal Q-Q Plot - Connectivity") + qqline();
qqline(dataf$CONNECTIVITY)
  
qqnorm(dataf$RELIABILITY,main="Normal Q-Q Plot - Reliability")
qqline(dataf$RELIABILITY)
  

