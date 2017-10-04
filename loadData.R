#Load all data into a dataframe

loadData<- function(fileName){
  
  setwd("C://Users//chris//OneDrive//Documentos//GitHub//ML_SelfHealingUtility//");
  
  data_all <- read.csv(fileName,header = TRUE,sep=",");
  
  dataf <- data.frame(data_all);
  #summary(dataf)
  
  return(dataf);
}

