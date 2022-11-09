# Load the dplyr library for subsetting data and dummies for creating dummy variables.
library(dplyr)
library(dummies)

# Set the random number generator seed so that we get the same results each time.
set.seed(12345)

# Load the data frame from Module 1 (not the homework data frame).
load("lending_data_2017_Q1.rda")

# Select the variables of interest.
my_df <- lending_data %>%
  dplyr::select(annual_inc, loan_amnt, emp_length, home_ownership, dti)  

# Inspect the data.  Note:
# There are outliers for annual_inc
# emp_length is encoded as a factor
# Only 1 has a home ownership of NONE
# There are outliers and missing values for dti.  
summary(my_df)

# Keep only those with annual income below 200000 and remove the 
# loan with home ownership of NONE and keep only those with dti less than 25.
lending_data_new <- lending_data %>%
  dplyr::filter(annual_inc < 200000 & 
                  home_ownership != "NONE" &
                  (is.na(dti == TRUE) | dti < 25)) %>%
  droplevels()

# Select the variables of interest
my_df <- lending_data_new %>%
  dplyr::select(annual_inc, loan_amnt, emp_length, home_ownership, dti)  

summary(my_df)

# Impute missing values for dti with the median.
my_df$dti[is.na(my_df$dti)] <- median(my_df$dti, na.rm=TRUE)
summary(my_df)

# Convert employment length to numeric
my_df$emp_length <- as.character(my_df$emp_length)
my_df$emp_length <- sub("years", "", my_df$emp_length)
my_df$emp_length <- sub("< 1 year", "1", my_df$emp_length)
my_df$emp_length <- sub("1 year", "1", my_df$emp_length)
my_df$emp_length <- sub("10\\+ ", "10", my_df$emp_length)
my_df$emp_length[my_df$emp_length == "n/a"] <- NA
my_df$emp_length <- as.numeric(my_df$emp_length)
my_df$emp_length[is.na(my_df$emp_length)] <- median(my_df$emp_length, na.rm=TRUE)
summary(my_df)

# Create dummy variables for home_ownership.
my_df <- dummy.data.frame(my_df, names=c("home_ownership"))

# Scale data.
my_df_scale <- scale(my_df, center=TRUE, scale=TRUE)

#  Apply kmeans clustering.
my_kmeans <- kmeans(my_df_scale, centers=7)

# Conduct PCA.
my_pca <- prcomp(my_df_scale, retx=TRUE)

# Plot
plot(my_pca$x[,1:2], col=my_kmeans$cluster, pch=my_kmeans$cluster)
legend("topright", legend=1:7, col=1:7, pch=1:7)

# Plot only cluster 1
plot(my_pca$x[,1:2], type="n")
points(my_pca$x[my_kmeans$cluster == 1,1:2], 
     col=my_kmeans$cluster[my_kmeans$cluster== 1], 
     pch=my_kmeans$cluster[my_kmeans$cluster == 1])

# Plot only cluster 2
plot(my_pca$x[,1:2], type="n")
points(my_pca$x[my_kmeans$cluster == 2,1:2], 
     col=my_kmeans$cluster[my_kmeans$cluster== 2], 
     pch=my_kmeans$cluster[my_kmeans$cluster == 2])

# Plot only cluster 3
# Plot only cluster 4
# Plot only cluster 5
# Plot only cluster 6
# Plot only cluster 7

# Inspect the PC loadings
my_pca$rotation[,1:2]

# Plot by loan grade
plot(my_pca$x[,1:2], col=lending_data_new$grade, pch=as.numeric(lending_data_new$grade))


