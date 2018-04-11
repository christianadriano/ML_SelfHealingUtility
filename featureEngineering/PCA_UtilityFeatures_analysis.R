#PCA Principal Component Analysis
#Which software components are similar with regards to the principal components?
#How the possible inputs to the utility function are related?

install.packages("ir.pca");
library (ir.pca);

# load data
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_SelfHealingUtility//loadData.R");
dataf<-loadData(fileName="Random_proper_comp_names.csv");
summary(dataf);

#Remove all reliability and utility values equal to zero
dataf<- dataf[dataf$RELIABILITY!=0,];
dataf <- dataf[dataf$UTILITY.INCREASE!=0,] 

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
                       dataf$CONNECTIVITY);

names<-c("Criticality","Reliability","Connectity"); # "Utility_Drop",
colnames(features_df) <- names;

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
features_pca <- prcomp(features_df,center=TRUE,scale=TRUE);
print(features_pca,type="l");

# Standard deviations:
#   [1] 1.0767434 0.9781258 0.9401562
# 
# Rotation:
#                PC1        PC2       PC3
# Criticality -0.6396407  0.2159066 0.7377290
# Reliability  0.4712771  0.8683510 0.1544812
# Connectity   0.6072542 -0.4464872 0.6571845

plot(features_pca, type="l");
#from the plot we can see that the first two PC's explain most of the variability in the data

summary(features_pca);
# Importance of components:
#                          PC1    PC2    PC3
# Standard deviation     1.0767 0.9781 0.9402
# Proportion of Variance 0.3865 0.3189 0.2946
# Cumulative Proportion  0.3865 0.7054 1.0000

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

plot_pca(group_classes=dataf$FAILURE_COMPONENT,pca_model=features_pca);

plot_pca(group_classes=dataf$COMPONENT,pca_model=features_pca);

#Cannot analyze by component because they are all unique
#plot_pca(group_classes=dataf$RULE_COMPONENT,pca_model=features_pca);
#plot_pca(group_classes=dataf$FAILURE_COMPONENT,pca_model=features_pca);
#plot_pca(group_classes=dataf$AFFECTED.COMPONENT,pca_model=features_pca);




