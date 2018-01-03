
#Investigating why Saturating got such a low error
#Reason is that data is not very well distributed across the linear and non-linear areas.
#Solution is to inject more CF5 failures which are the one that affect the number of requests

dataf <- loadData(fileName = "data//New4Cases//Saturating10K.csv");
dataf2 <- loadData(fileName = "data//New4Cases//ALL10K.csv");


performance <- dataf$PMax * tanh(dataf$alpha * dataf$REPLICA/dataf$REQUEST);
replica_request_RATIO <- dataf$alpha* dataf$REPLICA / dataf$REQUEST;
plot(x=replica_request_RATIO,performance) + title ("Saturating performance")

dtf <- dataf[dataf$AFFECTED_COMPONENT=="Item Management Service",]
performance <- dtf$PMax * tanh(dtf$alpha * dtf$REPLICA/dtf$REQUEST);
replica_request_RATIO <- dtf$alpha* dtf$REPLICA / dtf$REQUEST;
plot(replica_request_RATIO,performance) + title ("Saturating performance - Item Management Service Component")

tableView <- data.frame(replica_request_RATIO,performance)

view <- tableView[tableView$replica_request_RATIO<50,]
view <- view[view$performance>7,]

dim(view)
dim(tableView)
dataf <- dtf


plot(dataf2$UTILITY_INCREASE)+ title("ALL10K")
plot(dataf$UTILITY_INCREASE) + title("Saturating10K")
