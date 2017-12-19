# Investigate correlations among explanatory variables

install.packages("Hmisc")
library("Hmisc")

# load data
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_SelfHealingUtility//loadData.R");
dataf<-loadData(fileName="data//Probabilistic.csv");

#Remove all reliability and utility values equal to zero
dataf<- dataf[dataf$RELIABILITY!=0,];
dataf <- dataf[dataf$UTILITY.INCREASE!=0,] 

#    
#Correlations
#Data is not normal (did not reject the null hypothesis)
shapiro.test(dataf$UTILITY.DROP);
shapiro.test(dataf$CRITICALITY);
shapiro.test(dataf$CONNECTIVITY);
shapiro.test(dataf$RELIABILITY);

###########################################################################  
# Normality test

qqnorm(dataf$CRITICALITY,main="Normal Q-Q Plot - Criticality")+ qqline();
qqline(dataf$CRITICALITY)

qqnorm(dataf$CONNECTIVITY,main="Normal Q-Q Plot - Connectivity") + qqline();
qqline(dataf$CONNECTIVITY)

qqnorm(log(dataf$PROVIDED_INTERFACE+1),main="Normal Q-Q Plot - Provided.Interface")
qqline(log(dataf$PROVIDED_INTERFACE+1))

qqnorm(dataf$RELIABILITY,main="Normal Q-Q Plot - Reliability")
qqline(dataf$RELIABILITY)

matrixInput<-data.frame(dataf$UTILITY.INCREASE,dataf$CRITICALITY,dataf$CONNECTIVITY,dataf$RELIABILITY,
                        dataf$IMPORTANCE, dataf$PROVIDED_INTERFACE, dataf$REQUIRED_INTERFACE,dataf$ADT);
colnames(matrixInput)<-c("Utility.Increase","Criticality","Connectivity","Reliability","Importance",
                         "Provided.Inteface","Required.Interface","ADT");

res <- rcorr(data.matrix(matrixInput),type=c("pearson"));
res$r
res$P

cor(dataf$PROVIDED_INTERFACE,dataf$CONNECTIVITY,method=c("pearson"))
cor(dataf$PROVIDED_INTERFACE,dataf$CONNECTIVITY,method=c("kendall"))
cor(dataf$PROVIDED_INTERFACE,dataf$CONNECTIVITY,method=c("spearman"))

plot(dataf$CONNECTIVITY,dataf$PROVIDED_INTERFACE)

hist(dataf$PROVIDED_INTERFACE)
dataf <- dataf[dataf$PROVIDED_INTERFACE!=0,]
dataf$PROVIDED_INTERFACE!=0
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
