#Regression Trees to predict the utility drop based on the
#following features: criticality, connectivity, reliability, 
#fan-in, fan-out, average time to deploy (ATD)

#Imports

# load data
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_SelfHealingUtility//loadData.R");
dataf<-loadData(fileName="data//Random_with_different_Increase_for_each_rule.csv");

summary(dataf);

#Centralize features (divide them by their mean)
centralize<- function(featureData){
  featureData <- featureData/mean(featureData);
  return(featureData);
}


dataf$UTILITY.DROP <- centralize(dataf$UTILITY.DROP);
dataf$CRITICALITY <- centralize(dataf$CRITICALITY);
dataf$CONNECTIVITY <- centralize(dataf$CONNECTIVITY);
dataf$RELIABILITY <- centralize(dataf$RELIABILITY);
dataf$IMPORTANCE <- centralize(dataf$IMPORTANCE);
dataf$PROVIDED_INTERFACE <- centralize(dataf$PROVIDED_INTERFACE);
dataf$REQUIRED_INTERFACE <- centralize(dataf$REQUIRED_INTERFACE);
dataf$ADT <- centralize(dataf$ADT);