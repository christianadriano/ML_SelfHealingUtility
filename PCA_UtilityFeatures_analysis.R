#PCA Principal Component Analysis
#How the events are distributed?
#Which features are more important?

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
#                        data_df$DECISION);

names<-c("Utility_Drop","Criticality","Connectity","Reliability");
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

g <- ggbiplot(features_pca, obs.scale = 1, var.scale = 1, 
              groups = data_df$AFFECTED.COMPONENT, ellipse = FALSE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

