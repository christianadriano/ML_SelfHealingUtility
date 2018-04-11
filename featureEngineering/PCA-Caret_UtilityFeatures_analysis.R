#PCA Principal Component Analysis - USING CARET

#NOT WORKING YET

install.packages("ir.pca");
library (ir.pca);

# load data
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_SelfHealingUtility//loadData.R");
data_df<-loadData(fileName="MLDATA2_STATIC.csv");
summary(data_df);

#-----------------------------------------------------------
#Scramble the dataset before extracting the training set.
set.seed(8850);
g<- runif((nrow(data_df))); #generates a random distribution
data_df <- data_df[order(g),];

#merge columns Failure.Name, Affected.Component, Rule
library(tidyr)
data_df<-unite(data_df, DECISION, c("FAILURE.NAME","AFFECTED.COMPONENT","RULE"), remove=FALSE);

# consider only the feature columns
features_df<-data.frame(data_df$UTILITY.DROP,data_df$CRITICALITY,
                        data_df$CONNECTIVITY,data_df$RELIABILITY);

names<-c("Utility_Drop","Criticality","Connectity","Reliability");
colnames(features_df) <- names;

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 

require(caret);
trans = preProcess(features_df, 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
features_pca= predict(trans, features_df)

features_pca <- prcomp(features_df,center=TRUE,scale=TRUE);
print(features_pca);

trans$rotation
#                 PC1         PC2        PC3
# Utility_Drop 0.6203476 -0.06597448  0.2444351
# Criticality  0.5143144 -0.57907044  0.2723650
# Connectity   0.3884862  0.81086540  0.2726878
# Reliability  0.4469094  0.05312336 -0.8897809

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

plot_pca<-function(group){
  g <- ggbiplot(features_pca, obs.scale = 1, var.scale = 1, 
                groups = data_df$FAILURE.NAME, ellipse = FALSE, 
                circle = TRUE);
  g <- g + scale_color_discrete(name = '')
  g <- g + theme(legend.direction = 'horizontal', 
                 legend.position = 'top');
  print(g);
}

plot_pca(data_df$FAILURE.NAME);


