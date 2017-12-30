

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


# Replace component names -------------------------------------------------
#Authentication components have different names, but are still of the same type
#Twitter Authentication Service
#Facebook Authentication Service
#Google Authentication Service

renameAuthenticationServices <- function (df){
  
  flag<- df$AFFECTED_COMPONENT=="Twitter Authentication Service"
  df$AFFECTED_COMPONENT <- replace(df$AFFECTED_COMPONENT,flag,"Authentication Service")
  
  flag<- df$AFFECTED_COMPONENT=="Facebook Authentication Service"
  df$AFFECTED_COMPONENT <- replace(df$AFFECTED_COMPONENT,flag,"Authentication Service")
  
  flag<- df$AFFECTED_COMPONENT=="Google Authentication Service"
  df$AFFECTED_COMPONENT <- replace(df$AFFECTED_COMPONENT,flag,"Authentication Service")
  
  return(df);
}


select_Linear <- function(dataf){
  # Select feature columns --------------------------------------------------
  featuresdf<- data.frame(dataf$CRITICALITY,dataf$CONNECTIVITY,dataf$RELIABILITY,                        
                          dataf$UTILITY_INCREASE); 
  
  
  colnames(featuresdf) <- c("CRITICALITY","CONNECTIVITY","RELIABILITY",
                            "UTILITY_INCREASE");
  
  return(featuresdf);
}

select_Probabilistic <- function(dataf){
  # Select feature columns --------------------------------------------------
  featuresdf<- data.frame(dataf$CRITICALITY,dataf$CONNECTIVITY,dataf$RELIABILITY,
                          dataf$ADT,
                          dataf$UTILITY_INCREASE); 
  
  
  colnames(featuresdf) <- c("CRITICALITY","CONNECTIVITY","RELIABILITY",
                            "ADT",
                            "UTILITY_INCREASE");
  
  return(featuresdf);
}


select_Saturation <- function(dataf){
  
  # Select feature columns --------------------------------------------------
  featuresdf<- data.frame(dataf$CRITICALITY,dataf$CONNECTIVITY,dataf$RELIABILITY,
                          dataf$PMax,dataf$alpha,dataf$REPLICA_Original,dataf$REQUEST,                         
                          dataf$UTILITY_INCREASE); 
  
  
  colnames(featuresdf) <- c("CRITICALITY","Connectivity","RELIABILITY",
                            "PMax","alpha","REPLICA_Original","REQUEST",
                            "UTILITY_INCREASE");
  
  return(featuresdf);
}


select_Discontinous <- function(dataf){
  
  # Select feature columns --------------------------------------------------
  featuresdf<- data.frame(dataf$CRITICALITY,dataf$RELIABILITY,dataf$IMPORTANCE, 
                          dataf$PROVIDED_INTERFACE, dataf$REQUIRED_INTERFACE,
                          dataf$UTILITY_INCREASE); 
  
  
  colnames(featuresdf) <- c("CRITICALITY","RELIABILITY","IMPORTANCE",
                            "PROVIDED_INTERFACE","REQUIRED_INTERFACE",
                            "UTILITY_INCREASE");
  
  return(featuresdf);
}

select_ALL <- function(dataf){
  
  # Select feature columns --------------------------------------------------
  featuresdf<- data.frame(dataf$CRITICALITY,dataf$RELIABILITY,dataf$IMPORTANCE, 
                          dataf$PROVIDED_INTERFACE, dataf$REQUIRED_INTERFACE,
                          dataf$REPLICA,dataf$REQUEST,dataf$ADT,                         
                          dataf$PMax,dataf$alpha,
                           dataf$UTILITY_INCREASE); 
  
  
  colnames(featuresdf) <- c("CRITICALITY","RELIABILITY","IMPORTANCE",
                            "PROVIDED_INTERFACE", "REQUIRED_INTERFACE",
                            "REPLICA" ,"REQUEST","ADT",
                            "PMax","alpha",
                           "UTILITY_INCREASE");

  
    
  
  return(featuresdf);
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


# MAPD --------------------------------------------------------------------

# Mean Absolute Percent Deviation MAPD
# https://en.wikipedia.org/wiki/Mean_absolute_percentage_error
mapd <- function(prediction, actual){
  return(100* (sum(abs(actual - prediction)/actual) )/ length(actual));
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

# Average RMSE ------------------------------------------------------------

#sampleSize that was use to compute RMSE datapoint (assuming we used the same sampleSize for all RMSE datapoints)
#https://stats.stackexchange.com/questions/99263/average-of-root-mean-square-error
averageRMSE <- function(RMSEVector, sampleSize){
  RMSE_sqr <- sqrt((RMSEVector^2) * sampleSize);
  RMSE_points <- length(RMSEVector);
  return (sum(RMSE_sqr /(RMSE_points * sampleSize)))
}

