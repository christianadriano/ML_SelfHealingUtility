
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
