library(tidyverse)
library(datasets)
library(gridExtra)
library(cluster)
library("readxl")
library(ggpubr)
library(plotly)

setwd("~/R/CFTR2")

std <- function(x) sd(x)/sqrt(length(x))

mean_2hours <- read_excel("~/R/CFTR2/Mean2_vangoor.xlsx", sheet = "LUMIER", trim_ws = TRUE)

#PCA and summmary
set.seed(20)
CFTR2.pca <- prcomp(mean_2hours[,5:16], center = TRUE, scale. = TRUE)
CFTR2.pca.df <- as.data.frame(CFTR2.pca$x)
summary(CFTR2.pca)

#Plot cumalative variation explained
plot(cumsum(CFTR2.pca$sdev^2 / sum(CFTR2.pca$sdev^2)), type="b")

#Determine amount of K using SSE
gaps <- clusGap(CFTR2.pca.df, kmeans, K.max=20)
best.k <- maxSE(gaps$Tab[,"gap"], gaps$Tab[,"SE.sim"])
best.k

#Plot K-means groups with red line for maxSE (best.k)
plot(gaps$Tab[,"gap"], xlab="Number of clusters", ylab="Gap statistic")
abline(v=best.k, col="red")

#Perform K-means clustering with optimal number of K (best.K)
CFTR2_clusters <-kmeans(mean_2hours[,5:16], best.k, nstart = 20)
CFTR2_clusters 

#Get PC1 scores
PC1_scores <- CFTR2.pca$rotation[,1]
PC1_scores <- abs(PC1_scores) ## get the magnitudes
PC1_scores <- sort(PC1_scores, decreasing=TRUE)
PC1_scores <- names(PC1_scores[])

CFTR2.pca$rotation[PC1_scores,1] ## show the scores (and +/- sign)

#Get PC2 scores
PC2_scores <- CFTR2.pca$rotation[,2]
PC2_scores <- abs(PC2_scores) ## get the magnitudes
PC2_scores <- sort(PC2_scores, decreasing=TRUE)
PC2_scores <- names(PC2_scores[])

CFTR2.pca$rotation[PC2_scores,1] ## show the scores (and +/- sign)

#Get PC3 scores
PC3_scores <- CFTR2.pca$rotation[,3]
PC3_scores <- abs(PC3_scores) ## get the magnitudes
PC3_scores <- sort(PC3_scores, decreasing=TRUE)
PC3_scores <- names(PC3_scores[])

CFTR2.pca$rotation[PC3_scores,1] ## show the scores (and +/- sign)

#Get PC4 scores
PC4_scores <- CFTR2.pca$rotation[,4]
PC4_scores <- abs(PC4_scores) ## get the magnitudes
PC4_scores <- sort(PC4_scores, decreasing=TRUE)
PC4_scores <- names(PC4_scores[])

CFTR2.pca$rotation[PC4_scores,1] ## show the scores (and +/- sign)

#Make K-means clusters as factor, to be used as colors instead of values
CFTR2_clusters$cluster <- as.factor(CFTR2_clusters$cluster)

#PC1 vs PC2
plot1 <- ggplot(CFTR2.pca.df, aes(PC1, PC2, color = CFTR2_clusters$cluster, labels=mean_2hours$Mutant_name)) + geom_point(size = 3) +
  theme_light()

#PC1 vs PC3
plot2 <- ggplot(CFTR2.pca.df, aes(PC1, PC3, color = CFTR2_clusters$cluster, labels=mean_2hours$Mutant_name)) + geom_point(size = 3) +
  theme_light()

#PC2 vs PC3
plot3 <- ggplot(CFTR2.pca.df, aes(PC2, PC3, color = CFTR2_clusters$cluster, labels=mean_2hours$Mutant_name)) + geom_point(size = 3) +
  theme_light()

#PC1 vs PC4
plot4 <- ggplot(CFTR2.pca.df, aes(PC1, PC4, color = CFTR2_clusters$cluster, labels=mean_2hours$Mutant_name)) + geom_point(size = 3) +
  theme_light()

# function to create a circle
circle <- function(center = c(0, 0), npoints = 100) {
  r = 1
  tt = seq(0, 2 * pi, length = npoints)
  xx = center[1] + r * cos(tt)
  yy = center[1] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
corcir = circle(c(0, 0), npoints = 100)

# create data frame with correlations between variables and PCs
correlations = as.data.frame(cor(mean_2hours[,5:16], CFTR2.pca$x))

# data frame with arrows coordinates PC1 and PC2
arrows = data.frame(x1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), y1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), x2 = correlations$PC1, 
                    y2 = correlations$PC2)

# Make cirlce graph with factors influencing PC1 and PC2
Cirlce_plot <- ggplot() + geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") + 
  geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "gray65") + 
  geom_text(data = correlations, aes(x = PC1, y = PC2, label = rownames(correlations))) + 
  geom_hline(yintercept = 0, colour = "gray65") + geom_vline(xintercept = 0, 
                                                             colour = "gray65") + xlim(-1.1, 1.1) + ylim(-1.1, 1.1) + labs(x = "pc1 axis", 
                                                                                                                           y = "pc2 axis") + ggtitle("Circle of correlations") +
  theme_bw()

arrows2 = data.frame(x1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), y1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), x2 = correlations$PC1, 
                     y2 = correlations$PC3)

#  Make cirlce graph with factors influencing PC1 and PC3
Cirlce_plot2 <- ggplot() + geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") + 
  geom_segment(data = arrows2, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "gray65") + 
  geom_text(data = correlations, aes(x = PC1, y = PC3, label = rownames(correlations))) + 
  geom_hline(yintercept = 0, colour = "gray65") + geom_vline(xintercept = 0, 
                                                             colour = "gray65") + xlim(-1.1, 1.1) + ylim(-1.1, 1.1) + labs(x = "pc1 axis", 
                                                                                                                           y = "pc3 axis") + ggtitle("Circle of correlations") +
  theme_bw()

arrows3 = data.frame(x1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), y1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), x2 = correlations$PC2, 
                     y2 = correlations$PC3)

#  Make cirlce graph with factors influencing PC2 and PC3
Cirlce_plot3 <- ggplot() + geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") + 
  geom_segment(data = arrows3, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "gray65") + 
  geom_text(data = correlations, aes(x = PC2, y = PC3, label = rownames(correlations))) + 
  geom_hline(yintercept = 0, colour = "gray65") + geom_vline(xintercept = 0, 
                                                             colour = "gray65") + xlim(-1.1, 1.1) + ylim(-1.1, 1.1) + labs(x = "pc2 axis", 
                                                                                                                           y = "pc3 axis") + ggtitle("Circle of correlations") +
  theme_bw()

arrows4 = data.frame(x1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), y1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), x2 = correlations$PC1, 
                     y2 = correlations$PC4)

#  Make cirlce graph with factors influencing PC1 and PC4
Cirlce_plot4 <- ggplot() + geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") + 
  geom_segment(data = arrows4, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "gray65") + 
  geom_text(data = correlations, aes(x = PC1, y = PC4, label = rownames(correlations))) + 
  geom_hline(yintercept = 0, colour = "gray65") + geom_vline(xintercept = 0, 
                                                             colour = "gray65") + xlim(-1.1, 1.1) + ylim(-1.1, 1.1) + labs(x = "pc1 axis", 
                                                                                                                           y = "pc4 axis") + ggtitle("Circle of correlations") +
  theme_bw()


Cirlce_plot
Cirlce_plot2
Cirlce_plot3
Cirlce_plot4
plot1 <- ggplotly(plot1)
plot2 <- ggplotly(plot2)
plot3 <- ggplotly(plot3)
plot4 <- ggplotly(plot4)

plot1
plot2
plot3
plot4