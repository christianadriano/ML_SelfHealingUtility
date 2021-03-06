

#---------------------------------------------------------------
#Load all data into a dataframe
loadData<- function(fileName){
  
  setwd("C://Users//Chris//Documents//GitHub//ML_SelfHealingUtility//");
  
  data_all <- read.csv(fileName,header = TRUE,sep=",");
  
  dataf <- data.frame(data_all);
  #Remove NA's
  dataf <- dataf[complete.cases(dataf),] 
  #summary(dataf)
  
  dataf <- renameAuthenticationServices(dataf)
  #dataf <- dataf[dataf$AFFECTED_COMPONENT=="Authentication Service",];
  
  #Remove negative values
  dataf <- dataf[dataf$UTILITY_INCREASE>0,]
  
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


# select_Linear <- function(dataf){
#   # Select feature columns --------------------------------------------------
#   features.df<- data.frame(dataf$CRITICALITY,dataf$CONNECTIVITY, 
#                           dataf$RELIABILITY,                        
#                           dataf$UTILITY_INCREASE); 
#   
#   
#   colnames(features.df) <- c("CRITICALITY","CONNECTIVITY", 
#                             "RELIABILITY",
#                             "UTILITY_INCREASE");
#   
#   return(features.df);
# }

select_Linear <- function(dataf){
  # Select feature columns --------------------------------------------------
  features.df<- data.frame(dataf$CRITICALITY,dataf$PROVIDED_INTERFACE, dataf$REQUIRED_INTERFACE,
                          dataf$RELIABILITY,                        
                          dataf$UTILITY_INCREASE); 
  
  
  colnames(features.df) <- c("CRITICALITY","PROVIDED_INTERFACE","REQUIRED_INTERFACE",
                            "RELIABILITY",
                            "UTILITY_INCREASE");
  
  return(features.df);
}



select_Saturation <- function(dataf){
  
  # Select feature columns --------------------------------------------------
  features.df<- data.frame(dataf$CRITICALITY,dataf$PROVIDED_INTERFACE, dataf$REQUIRED_INTERFACE,
                          dataf$RELIABILITY,
                          dataf$PMax,dataf$alpha,dataf$REPLICA,dataf$REQUEST, 
                          dataf$UTILITY_INCREASE); 
  
  colnames(features.df) <- c("CRITICALITY","PROVIDED_INTERFACE","REQUIRED_INTERFACE",
                            "RELIABILITY",
                            "PMax","alpha","REPLICA","REQUEST",
                            "UTILITY_INCREASE");
  
  return(features.df);
}


select_Discontinuous <- function(dataf){
  
  # Select feature columns --------------------------------------------------
  features.df<- data.frame(dataf$CRITICALITY,dataf$RELIABILITY,dataf$IMPORTANCE, 
                          dataf$PROVIDED_INTERFACE, dataf$REQUIRED_INTERFACE,
                          dataf$ADT,dataf$UTILITY_INCREASE); 
  
  
  colnames(features.df) <- c("CRITICALITY","RELIABILITY","IMPORTANCE",
                            "PROVIDED_INTERFACE","REQUIRED_INTERFACE",
                            "ADT","UTILITY_INCREASE");
  
  return(features.df);
}

select_Combined <- function(dataf){
  
  # Select feature columns --------------------------------------------------
  features.df<- data.frame(dataf$CRITICALITY,dataf$RELIABILITY, dataf$IMPORTANCE, 
                          dataf$PROVIDED_INTERFACE, dataf$REQUIRED_INTERFACE,
                          dataf$REPLICA,dataf$REQUEST,dataf$ADT,                         
                          dataf$PMax,
                           dataf$UTILITY_INCREASE); 
  # dataf$alpha
  
  colnames(features.df) <- c("CRITICALITY","RELIABILITY", "IMPORTANCE",
                            "PROVIDED_INTERFACE", "REQUIRED_INTERFACE",
                            "REPLICA" ,"REQUEST","ADT",
                            "PMax",
                           "UTILITY_INCREASE");

  # "alpha",
    
  
  return(features.df);
}

#-------------------------------------------------------------
#Scramble the dataset before extracting the training set.
scrambleData<-function(datadf){
  set.seed(8850);
  g<- runif((nrow(datadf))); #generates a random distribution
  return(datadf[order(g),]);
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

# Mean Absolute Percent Deviation MADP
# https://en.wikipedia.org/wiki/Mean_absolute_percentage_error
madp <- function(prediction, actual){
  error <- abs(actual-prediction);
  return(100* (sum(error/abs(actual) ))/ length(actual));
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

# Save results to file ----------------------------------------------------
resultsToFile <- function(results, modelName, methodName,extension){
  fileName <- paste0("results_",methodName,"_",modelName,"_",extension);
  write.table(results,fileName,sep=",",col.names = TRUE, row.names=FALSE);
  return (paste0("file written:",fileName));
}

# Generate the dataset names that will be trained -------------------------
generateDataSetNames <- function(modelName,datasetSizeList,s_idx){
  ###s_idx=0 generates for all sizes in the dataset.
  ###s_idx=1 generates only for the first element of datasetSizeList
  
  if(s_idx==0 & length(datasetSizeList)>0){#Generate for all sizes
    datasetName <- paste0(modelName,datasetSizeList[1]);
    for(i in c(2:length(datasetSizeList))){
      datasetName <- cbind(datasetName,paste0(modelName,datasetSizeList[i]));
    }
  }
  else{
    datasetName <- paste0(modelName,datasetSizeList[s_idx]);
  }
  return(datasetName);
}

# Prepare features --------------------------------------------------------
prepareFeatures <- function(dataf,selectionType){
  
  #Do feature selection (or not)
  if(selectionType=="Combined")
    features.df<- select_Combined(dataf) 
  else
    if(selectionType=="Linear")
      features.df<- select_Linear(dataf) 
    else
      if(selectionType=="Discontinuous")
        features.df<- select_Discontinuous(dataf) 
      else
        if(selectionType=="Saturating")
          features.df<- select_Saturation(dataf) 
        
        #Remove zero utilities
        features.df <- features.df[features.df$UTILITY_INCREASE!=0,];
        
        # Scramble data 
        features.df <- scrambleData(datadf=features.df);
        
        return (features.df);
}

# Generate PMML file ------------------------------------------------------

generatePMML <- function(trained.model, training.df, pmmlFileName, numberOfTrees){  
  #browser();
  last.column.explanatory <- dim(training.df)[2] - 1; #last column is the target variable
  
  # Generate feature map
  feature.map = r2pmml::genFMap(training.df[1:last.column.explanatory])
  r2pmml::writeFMap(feature.map, "feature.map")
  
  # Save the model in XGBoost proprietary binary format
  #xgb.save(model, "xgboost.model")
  
  # Dump the model in text format
  #  xgb.dump(model, "xgboost.model.txt", fmap = "feature.map");
  
 #for gbm
  r2pmml(trained.model, pmmlFileName);#, fmap = feature.map, response_name = "UTILITY_INCREASE", 
         #missing = NULL, compact = TRUE)
  
  #for xgboost
  #r2pmml(trained.model, pmmlFileName, fmap = feature.map, response_name = "UTILITY_INCREASE", 
  #       missing = NULL, ntreelimit = numberOfTrees, compact = TRUE)
}


# Convert time to Data Frame ----------------------------------------------
convertTimeToDataFrame <- function(time){
  
  time.df <- data.frame(matrix(data=NA,nrow=1,ncol=3));
  colnames(time.df) <- c("user.time","sys.time","elapsed.time");
  
  df <- data.frame(unlist(lapply(time, '[[', 1)));
  
  time.df$user.time <- df[1,1];
  time.df$sys.time <- df[2,1];
  time.df$elapsed.time <-df[3,1];
  
  return (time.df);
}



