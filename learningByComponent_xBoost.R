
#Learning by component

affectedComponent = c("Authentication Service","Item Management Service","Bid and Buy Service",
                      "User Management Service","Reputation Service","Persistence Service","Inventory Service",
                      "Comment Item Filter","Category Item Filter","Recommendation Item Filter","Future Sales Item Filter",
                      "Buy Now Item Filter", "Availability Item Filter","Region Item Filter","Seller Reputation Item Filter",
                      "Query Service","Last Second Sales Item Filter","Past Sales Item Filter");

name = "Saturating10K";
fileName = paste0(folder,name,".csv");

for(i in c(1:length(affectedComponent))){
  dataf <- loadData(fileName);
  dataf <- dataf[dataf$AFFECTED_COMPONENT==affectedComponent[i],]
  dim(dataf)
  
  #Train model
  mcResultsf <- trainModel(i,dataf,mcResultsf);
  mcResultsf
}