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
dataf<- dataf[dataf$FAILURE.NAME=="CF3",];

#Select only the rows that have the Authentication component
dataf<-dataf[grep("Auth", dataf$AFFECTED.COMPONENT), ]

#merge columns Failure.Name, Affected.Component, Rule
library(tidyr)
#dataf<-unite(dataf, FAILURE_RULE, c("FAILURE.NAME","RULE"), remove=FALSE);
#dataf<-unite(dataf, FAILURE_COMPONENT, c("FAILURE.NAME","AFFECTED.COMPONENT"), remove=FALSE);

#Create columns combinging two other columns
dataf<-unite(dataf, RULE_COMPONENT, c("RULE","AFFECTED.COMPONENT") , remove=FALSE);
               
# consider only the feature columns
features_df<-data.frame(dataf$CRITICALITY,
                        dataf$RELIABILITY);
#                       dataf$CONNECTIVITY, dataf$UTILITY.DROP, dataf$DECISION);

names<-c("Criticality","Reliability"); #"Utility_Drop","Connectity"
colnames(features_df) <- names;

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
features_pca <- prcomp(features_df,center=TRUE,scale=TRUE);
print(features_pca);

# Standard deviations:
#   [1] 1.4864492 0.9750478 0.7559267 0.5180014
# 
# Rotation:
#                PC1         PC2         PC3        PC4
# Utility_Drop 0.6128382 -0.05281935  0.04842819  0.7869525
# Criticality  0.4815655  0.44886497 -0.68929320 -0.3024728
# Connectity   0.3160844 -0.87535050 -0.22137859 -0.2912794
# Reliability  0.5409387  0.17173191  0.68812870 -0.4520756

plot(features_pca, type="l");
#from the plot we can see that the first two PC's explain most of the variability in the data

summary(features_pca);
# Importance of components:
#                           PC1    PC2    PC3     PC4
# Standard deviation     1.4864 0.9750 0.7559 0.51800
# Proportion of Variance 0.5524 0.2377 0.1429 0.06708
# Cumulative Proportion  0.5524 0.7901 0.9329 1.00000

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
plot_pca(group_classes=dataf$AFFECTED.COMPONENT,pca_model=features_pca);
plot_pca(group_classes=dataf$RULE,pca_model=features_pca);

# plot_pca(group_classes=dataf$FAILURE_RULE,pca_model=features_pca);
# plot_pca(group_classes=dataf$RULE_COMPONENT,pca_model=features_pca);
# plot_pca(group_classes=dataf$FAILURE_COMPONENT,pca_model=features_pca);




