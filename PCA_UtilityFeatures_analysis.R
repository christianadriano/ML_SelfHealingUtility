#PCA Principal Component Analysis
#How the events are distributed?
#Which features are more important?

install.packages("ir.pca");
library (ir.pca);

# load data
source("C://Users//chris//OneDrive//Documentos//GitHub//ML_SelfHealingUtility//loadData.R");
data_df<-loadData(fileName="failureRules_data.csv");
summary(data_df);

# consider only the feature columns
features_df<-data.frame(data_df$UTILITY.DROP,data_df$CRITICALITY,
                        data_df$CONNECTIVITY,data_df$RELIABILITY);
names<-c("Utility_Drop","Criticality","Connectity","Reliability");
colnames(features_df) <- names;

#Scramble the dataset before extracting the training set.
set.seed(8850);
g<- runif((nrow(data_df))); #generates a random distribution
features_df <- features_df[order(g),];

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
features_pca <- prcomp(features_df,center=TRUE,scale=TRUE);
print(features_pca);

# Standard deviations:
#   [1] 1.70137043 1.00540703 0.30002057 0.06695541
# 
# Rotation:
#                   PC1        PC2        PC3         PC4
# Utility_Drop -0.5613952 -0.2844102  0.1943732  0.75243954
# Criticality  -0.5404412  0.3411924  0.6324458 -0.43763382
# Connectity   -0.4155399 -0.6961814 -0.3190971 -0.49074949
# Reliability  -0.4691325  0.5639415 -0.6785340  0.03842274

plot(features_pca, type="l");
#from the plot we can see that the first two PC's explain most of the variability in the data

summary(features_pca);
# Importance of components:
#                         PC1    PC2    PC3     PC4
# Standard deviation     1.7014 1.0054 0.3000 0.06696
# Proportion of Variance 0.7237 0.2527 0.0225 0.00112
# Cumulative Proportion  0.7237 0.9764 0.9989 1.00000

log_features <- features_df;
predict(features_pca, newdata = tail(log_features,2));
#        PC1       PC2         PC3         PC4
# 474 1.0516996 0.3411401 -0.03128631  0.03971460
# 430 0.1631741 0.1247542  0.39535469 -0.09866065

install.packages("devtools")
library(devtools)
install.packages("scales")
library(scales)
install_github("ggbiplot", "vqv")
library(ggbiplot)
g <- ggbiplot(features_pca, obs.scale = 1, var.scale = 1, 
              groups = features_df[,4], ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

