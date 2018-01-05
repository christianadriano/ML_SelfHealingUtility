
#Investigating why Saturating got such a low error
#Reason is that data is not very well distributed across the linear and non-linear areas.
#Solution is to inject more CF5 failures which are the one that affect the number of requests


name="saturating1000";
folder="data//New4Cases//";
fileName = paste0(folder,name,".csv");
dataf <- loadData(fileName = fileName);

#Plot all
performance <- dataf$PMax * tanh(dataf$alpha * dataf$REPLICA/dataf$REQUEST);
replica_request_RATIO <- dataf$alpha* dataf$REPLICA / dataf$REQUEST;
plot(x=replica_request_RATIO,performance) + title ("All10K performance")

#Plot by component
affectedComponent = c("Authentication Service","Item Management Service","Bid and Buy Service",
                      "User Management Service","Reputation Service","Persistence Service","Inventory Service",
                      "Comment Item Filter","Category Item Filter","Recommendation Item Filter","Future Sales Item Filter",
                      "Buy Now Item Filter", "Availability Item Filter","Region Item Filter","Seller Reputation Item Filter",
                      "Query Service","Last Second Sales Item Filter","Past Sales Item Filter");

for(i in c(1:length(affectedComponent))){ 
  i <- 2;
  dtf <- dataf[dataf$AFFECTED_COMPONENT==affectedComponent[i],]
  performance <- dtf$PMax * tanh(dtf$alpha * dtf$REPLICA/dtf$REQUEST);
  replica_request_RATIO <- dtf$alpha* dtf$REPLICA / dtf$REQUEST;
  events = paste("events =",dim(dtf)[1]);
  replica_request_RATIO_validation <- validationData$alpha* validationData$REPLICA / validationData$REQUEST;
  plot(replica_request_RATIO,validationData$UTILITY_INCREASE)
  plot(replica_request_RATIO_validation,y_pred,type="p",col="red", pch=4, xlab=events, ylab = "Utility Increase")
  points(validationData$UTILITY_INCREASE) 
  
  points()
  title (paste(name,affectedComponent[i],sep=","));
  #plot(dtf$UTILITY_INCREASE, pch=4, col="red", xlab=events, ylab = "Utility Increase") 
}

tableView <- data.frame(replica_request_RATIO,performance)

view <- tableView[tableView$replica_request_RATIO<50,]
view <- view[view$performance>7,]

dim(view)
dim(tableView)
dataf <- dtf


plot(dataf$UTILITY_INCREASE)+ title("ALL10K")
plot(dataf$UTILITY_INCREASE) + title("Saturating10K")
