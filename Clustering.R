# OBJECTIVE 2: Clustering of patients using baseline measurements

# Load libraries

library(factoextra)
library(cluster)
library(clusternor)

# Load data : Read csv files

df <- read.table(
  "/Users/nayana/Desktop/Stanford/Data Mining/Final Project/Dataset/StudyABCD.csv", 
  header = TRUE,
  sep = ",")

# Set seed
set.seed(1)

# Filter out only day 0 visits
df.day.zero <- df[df$VisitDay == 0, ]

# Choose columns: P1 - P7, N1 - N7, G1 - G16
df.clust <- df.day.zero[,9:38] 

# Scale data to have mean 0 and stddev 1
df.clust.scaled <- scale(df.clust)

# K-Means clustering
num.centers <- 2
km <- kmeans(df.clust.scaled, centers = num.centers, nstart = 25)
print(km)

# Plot results
fviz_cluster(km, data = df.clust.scaled)

# Create dataframe with original data and cluster assignments
cluster.out <- cbind(df.clust, clusterNum = km$cluster)

# Calculate proportions of each score (1-7) for each feature in each cluster
# Create and save barplots of the same

# Loop over features
for (i in 1:30){
  # Get ith column i.e. ith feature
  cluster.i <- cluster.out[,i]
  
  # Display feature name
  cat(colnames(cluster.out)[i],"\n")
  
  # Initialize matrix to store proportions
  proportions = matrix(0, num.centers, 7)
  
  # Loop over clusters
  for (j in 1:num.centers){
    
    # Display cluster number
    cat("Cluster ", j, "\n")
    
    # Get all values of the ith feature in the jth cluster
    cluster.ij <- cluster.i[cluster.out[,31]==j]
    
    len = length(cluster.ij)
    
    # Calculate proportions
    for (k in 1:7) {
      proportions[j,k] <- signif(length(cluster.ij[cluster.ij==k])/len,6)
      cat(paste(proportions[j,k],"   "))
    }
    print("")
  }
  
  # Save plots
  png(paste(colnames(cluster.out)[i], ".png", sep = ""), width = 1200, height = 750)
  barplot(proportions, width = 1,beside = TRUE, names.arg = c(1,2,3,4,5,6,7), 
          main = colnames(cluster.out)[i])
  dev.off()
}
