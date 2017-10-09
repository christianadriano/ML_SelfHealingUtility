#PCA Principal Component Analysis
#Which software components are similar with regards to the principal components?
#How the possible inputs to the utility function are related?

install.packages("ir.pca");
library (ir.pca);

# load data
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_SelfHealingUtility//loadData.R");
dataf<-loadData(fileName="MLDATA2_data.csv");
summary(dataf);

#Scramble the dataset before extracting the training set.
dataf <- scrambleData(dataf);

#Remove all Failures that do not cause utility increase
#dataf<- dataf[dataf$FAILURE.NAME=="CF2",];

#Select only the rows that have the Authentication component
#dataf<-dataf[grep("Auth", dataf$AFFECTED.COMPONENT), ]

#merge columns Failure.Name, Affected.Component, Rule
library(tidyr);
#Create columns combinging two other columns
dataf<-unite(dataf, FAILURE_RULE, c("FAILURE.NAME","RULE"), remove=FALSE);
dataf<-unite(dataf, FAILURE_COMPONENT, c("FAILURE.NAME","AFFECTED.COMPONENT"), remove=FALSE);
dataf<-unite(dataf, RULE_COMPONENT, c("RULE","AFFECTED.COMPONENT") , remove=FALSE);
               
# consider only the feature columns
features_df<-data.frame(dataf$CRITICALITY,
                        dataf$RELIABILITY,
                       dataf$CONNECTIVITY);# dataf$UTILITY.DROP);

names<-c("Criticality","Reliability","Connectity"); # "Utility_Drop",
colnames(features_df) <- names;

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
features_pca <- prcomp(features_df,center=TRUE,scale=TRUE);
print(features_pca);

# Standard deviations:
#   [1] 1.2182116 0.9712837 0.7566826
# 
# Rotation:
#                 PC1        PC2        PC3
# Criticality 0.6386511 -0.3874427  0.6648405
# Reliability 0.6805970 -0.1187240 -0.7229746
# Connectity  0.3590438  0.9142170  0.1878691

plot(features_pca, type="l");
#from the plot we can see that the first two PC's explain most of the variability in the data

summary(features_pca);
# Importance of components:
#                          PC1    PC2    PC3     PC4
# Standard deviation     1.4479 0.9745 0.7568 0.61751
# Proportion of Variance 0.5241 0.2374 0.1432 0.09533
# Cumulative Proportion  0.5241 0.7615 0.9047 1.00000

install.packages("devtools")
library(devtools)
install.packages("scales")
library(scales)
install_github("ggbiplot", "vqv")
library(ggbiplot)

#Plotting Principal Components for certains columns

plot_pca<-function(group_classes,pca_model){
  g <- ggbiplot(pca_model, obs.scale = 1, var.scale = 1, 
                groups = group_classes, ellipse = FALSE, 
                circle = TRUE);
  g <- g + scale_color_discrete(name = '')
  g <- g + theme(legend.direction = 'horizontal', 
                 legend.position = 'top');
  print(g);
}

plot_pca(group_classes=dataf$FAILURE.NAME,pca_model=features_pca);
plot_pca(group_classes=dataf$RULE,pca_model=features_pca);

plot_pca(group_classes=dataf$FAILURE_RULE,pca_model=features_pca);

#Cannot analyze by component because they are all unique
#plot_pca(group_classes=dataf$RULE_COMPONENT,pca_model=features_pca);
#plot_pca(group_classes=dataf$FAILURE_COMPONENT,pca_model=features_pca);
#plot_pca(group_classes=dataf$AFFECTED.COMPONENT,pca_model=features_pca);




