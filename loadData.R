

#---------------------------------------------------------------
#Load all data into a dataframe
loadData<- function(fileName){
  
  setwd("C://Users//chris//OneDrive//Documentos//GitHub//ML_SelfHealingUtility//");
  
  data_all <- read.csv(fileName,header = TRUE,sep=",");
  
  dataf <- data.frame(data_all);
  #Remove NA's
  dataf <- dataf[complete.cases(dataf),] 
  #summary(dataf)
  
  return(dataf);
}


#-------------------------------------------------------------
#Scramble the dataset before extracting the training set.
scrambleData<-function(dataf){
  set.seed(8850);
  g<- runif((nrow(dataf))); #generates a random distribution
  dataf <- dataf[order(g),];
  return (dataf);
}

#--------------------------------------------------------------
#Extract the unique items from a column and return them sorted
listUniqueItems<- function(column,columnName){
  
  #obtain a list of unique items
  uniqueItems <- data.frame(unique(column));
  colnames(uniqueItems) <- c(columnName);
  
  #Sort items in ascending order
  uniqueItems <- uniqueItems[with(uniqueItems,order(columnName)),];
  return(uniqueItems);
}


# Centralize data ---------------------------------------------------------

#Centralize features (divide them by their mean)
centralize<- function(featureColumn){
  featureColumn <- featureColumn/mean(featureData);
  return(featureColumn);
}


# RMSE --------------------------------------------------------------------

# Root mean square error
# https://en.wikipedia.org/wiki/Root-mean-square_deviation
rmse <- function(error){
  sqrt(mean(error^2))
}


# R_squared ---------------------------------------------------------------

# Coefficient of determination
# https://en.wikipedia.org/wiki/Coefficient_of_determination
r_squared <- function(prediction, actual){
  
  SS_ExplainedVariance <- sum((prediction - actual)^2);
  SS_TotalVariance <- sum((actual-mean(actual))^2);
  R2<- 1- SS_ExplainedVariance / SS_TotalVariance;
  return (R2);  
}


