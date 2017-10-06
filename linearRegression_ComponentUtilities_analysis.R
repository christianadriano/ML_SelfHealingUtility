#Predict Utility of Components

#Filter data by component


#Extract the unique items from a column and return them sorted
listUniqueItems<- function(column,columnName){
  uniqueItems <- data.frame(unique(column));
  colnames(uniqueItems) <- c(columnName);
  uniqueItems <- uniqueItems[with(uniqueItems,order(columnName)),];
  return(uniqueItems);
}

listUniqueItems(dataf$FAILURE.NAME,"FAILURE.NAME")
