library(tidyverse)   
library(factoextra) 
library(cluster)    
library(psych)      
library(corrplot)    
library(stats)      
library(NbClust)
library(readxl)
file_path <- file.choose()  
smartwatch_data <- read_excel(file_path)
# Viewing the structure of the data
str(smartwatch_data)
# Summary statistics
summary(smartwatch_data)
# Converting categorical variables to factors
smartwatch_data$AmznP <- as.factor(smartwatch_data$AmznP)
smartwatch_data$Female <- as.factor(smartwatch_data$Female)
smartwatch_data$Degree <- as.factor(smartwatch_data$Degree)
smartwatch_data$Income <- as.factor(smartwatch_data$Income)
# Descriptive statistics for preference variables
describe(smartwatch_data[, c("ConstCom", "TimelyInf", "TaskMgm", "DeviceSt", "Wellness", "Athlete", "Style")])
# Correlation Analysis
# Creating a correlation matrix for numeric variables
cor_matrix <- cor(smartwatch_data[, c("ConstCom", "TimelyInf", "TaskMgm", "DeviceSt", "Wellness", "Athlete", "Style", "Age")], use = "complete.obs")
# Visualizing the correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)

smartwatch_data$Income_numeric <- as.numeric(as.character(smartwatch_data$Income))

# Model for Income
income_model <- lm(Income_numeric ~ AmznP + Female + Degree + Age, data = smartwatch_data)
summary(income_model)
# Relationship between Age and smartwatch feature preferences
age_model <- lm(Age ~ Athlete + Style + Wellness, data = smartwatch_data)
summary(age_model)
# Clustering Analysis for Market Segmentation
# Selecting variables for clustering
cluster_vars <- smartwatch_data[, c("ConstCom", "TimelyInf", "TaskMgm", "DeviceSt", "Wellness", "Athlete", "Style")]
# Standardizing the data for clustering
cluster_data <- scale(cluster_vars)
# Determining optimal number of clusters
# Elbow method
fviz_nbclust(cluster_data, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  labs(subtitle = "Elbow Method")
# Silhouette method
fviz_nbclust(cluster_data, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette Method")
# Gap statistic method
set.seed(123)
gap_stat <- clusGap(cluster_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat) +
  labs(subtitle = "Gap Statistic Method")
# Based on the methods above, 3 clusters appear to be optimal
# Performing K-means clustering with 3 clusters
set.seed(123)
k_means <- kmeans(cluster_data, centers = 3, nstart = 25)
# Adding cluster assignment to original data
smartwatch_data$cluster <- k_means$cluster
# Visualizing the clusters
fviz_cluster(list(data = cluster_data, cluster = k_means$cluster),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"),
             ellipse.type = "convex",
             repel = TRUE,
             ggtheme = theme_minimal())
# Mean values of preference variables by cluster
cluster_means <- aggregate(cluster_vars, by = list(Cluster = smartwatch_data$cluster), mean)
print(cluster_means)
# Descriptive analysis of clusters by demographic variables
demographic_clusters <- aggregate(cbind(Age, Income_numeric) ~ cluster, data = smartwatch_data, mean)
print(demographic_clusters)
# Percentage of Amazon Prime members by cluster
amazon_clusters <- smartwatch_data %>%
  group_by(cluster) %>%
  summarise(
    Amazon_Prime_Percentage = sum(AmznP == 1) / n() * 100
  )
print(amazon_clusters)
# Percentage of females by cluster
gender_clusters <- smartwatch_data %>%
  group_by(cluster) %>%
  summarise(
    Female_Percentage = sum(Female == 1) / n() * 100
  )
print(gender_clusters)
# Education level by cluster
education_clusters <- smartwatch_data %>%
  group_by(cluster) %>%
  summarise(
    Undergrad_Percentage = sum(Degree == 1) / n() * 100,
    Graduate_Percentage = sum(Degree == 2) / n() * 100
  )
print(education_clusters)
# Income distribution by cluster
income_clusters <- smartwatch_data %>%
  group_by(cluster) %>%
  summarise(
    Income_Below_40K = sum(Income == 1) / n() * 100,
    Income_40K_70K = sum(Income == 2) / n() * 100,
    Income_71K_100K = sum(Income == 3) / n() * 100,
    Income_101K_175K = sum(Income == 4) / n() * 100,
    Income_Above_175K = sum(Income == 5) / n() * 100
  )
print(income_clusters)
library(fmsb)
# Preparing data for radar chart
radar_data <- data.frame(rbind(rep(7,7), rep(1,7), as.matrix(cluster_means[,-1])))
rownames(radar_data) <- c("Max", "Min", paste("Cluster", 1:3))
# Creating radar chart
radarchart(radar_data, 
           pcol = c("#2E9FDF", "#00AFBB", "#E7B800"), 
           pfcol = scales::alpha(c("#2E9FDF", "#00AFBB", "#E7B800"), 0.3),
           plwd = 2,
           cglcol = "grey", 
           cglty = 1, 
           axislabcol = "grey", 
           caxislabels = seq(1, 7, 1), 
           cglwd = 0.8,
           vlcex = 0.8)

legend("topright", 
       legend = paste("Cluster", 1:3),
       col = c("#2E9FDF", "#00AFBB", "#E7B800"), 
       lty = 1, 
       lwd = 2,
       bty = "n")
# Bar plot of feature importance by cluster
feature_means <- cluster_means %>%
  pivot_longer(cols = -Cluster, names_to = "Feature", values_to = "Mean")

ggplot(feature_means, aes(x = Feature, y = Mean, fill = factor(Cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Feature Importance by Cluster",
       x = "Feature",
       y = "Mean Score",
       fill = "Cluster") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
