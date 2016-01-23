
library(dplyr)
library(tree)

# Import and prepare the data
hitters <- read.table("./hitters.csv", sep = ",", header = TRUE)

str(hitters)
# Remove NAs
hitters<- na.omit(hitters)
# Take a look
hist(hitters$Salary)
# log transform Salary to make it more normal
hitters$Salary <- log(hitters$Salary)
# Take a look
hist(hitters$Salary)

# Find the mean and range of Salary
x <- mean(hitters$Salary)
x
range((hitters$Salary))

# Compute the residual sum of squares (RSS)
RSS <- function(x){
  rss <- sum((x - mean(x))**2)
  rss
}

# Show the mean and RSS for Salary
R0_mean <- mean(hitters$Salary)
R0_mean
R0_rss <- RSS(hitters$Salary)
R0_rss

# Create and begin a Deviations vector
deviations <- c("0 partitions" = R0_rss)
deviations

# =====================================================================================================
# Take a look at the color-coded data, and plot two dotted lines where there seem to be natural breaks

# Functions taken from Stack Overflow for a rainbow plot
# https://stackoverflow.com/questions/7420281/create-a-rainbow-color-scale-based-on-a-vector-in-the-order-of-that-vector
range01 <- function(x)(x-min(x))/diff(range(x))
rainbow(7)
cRamp <- function(x){
  cols <- colorRamp(rainbow(7))(range01(x))
  apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
}  

#Plot Years vs Hits, colored by Salary
plot(hitters$Years, hitters$Hits, col=cRamp(hitters$Salary), 
     main="Salary as a Function of Years and Hits", xlab = "Years", 
     ylab = "Hits")
# Vertical partition at 4.5
segments(x0 = 4.5, y0 = -10, x1 = 4.5, y1 = 250, lty = "dotted", col = "black" )
# Horizontal partition at 117.5
segments(x0 = 4.5, y0 = 117.5, x1 = 30, y1 = 117.5, lty = "dotted", col = "black")

# =====================================================================================================
# Now start partitioning analytically

# Function to make vertical partition on Years at lowest RSS
make_vertical_partition <- function(df) {
  
  # Remove any existing list return objects
  if(exists("vertical_partition")) {remove("vertical_partition")}
    
  # Iterate through the sorted, unique Years
  for(i in unique(sort(df$Years))){
    
    # Create a subset lower than the current Years == i
    lower_years <- filter(df, df$Years < i)
    # Compute the RSS of that lower subset
    lower_rss <- sum((lower_years$Salary - mean(lower_years$Salary))^2)
    
    # Create a subset higher than or equal to the current Years == i
    higher_years <- filter(df, df$Years >= i)
    # Compute the RSS of that higher subset
    higher_rss <- sum((higher_years$Salary - mean(higher_years$Salary))^2)

    # If a list return object does not exist, create one and populate it
    if(!exists("vertical_partition")){
      # Populate the list return object
      vertical_partition <- list(i, lower_years, lower_rss, higher_years, 
                               higher_rss)
      # But if a list return object does exist...
    } else {
      # ...compare the sum of the current RSS values to sum of existing
      # RSS values...
      if (lower_rss + higher_rss < vertical_partition[[3]] + 
          vertical_partition[[5]]){
        # ...and repopulate the list with new values if appropriate
        vertical_partition <- list(i, lower_years, lower_rss, higher_years, 
                                higher_rss)
      }
    }
  }
  vertical_partition
}
  
# Function to make horizontal partition on Hits at lowest RSS
make_horizontal_partition <- function(df) {
  
  # Remove any existing list return objects
  if(exists("horizontal_partition")) {remove("horizontal_partition")}
  
  # Iterate through the sorted, unique Hits
  for(i in unique(sort(df$Hits))){
    
    # Create a subset lower than the current Hits == i
    lower_hits <- filter(df, df$Hits < i)
    # Compute the RSS of that lower subset
    lower_rss <- sum((lower_hits$Salary - mean(lower_hits$Salary))^2)
    
    # Create a subset higher than or equal to the current Hits == i
    higher_hits <- filter(df, df$Hits >= i)
    # Compute the RSS of that higher subset
    higher_rss <- sum((higher_hits$Salary - mean(higher_hits$Salary))^2)
    
    # If a list return object does not exist, create one and populate it
    if(!exists("horizontal_partition")){
      horizontal_partition <- list(i, lower_hits, lower_rss, higher_hits, 
                                 higher_rss)
    # But if a list return object does exist...
    } else {
      # ...compare the sum of the current RSS values to sum of existing
      # RSS values...
      if (lower_rss + higher_rss < horizontal_partition[[3]] + 
          horizontal_partition[[5]]){
        # ...and repopulate the list with new values if appropriate
        horizontal_partition <- list(i, lower_hits, lower_rss, higher_hits, 
                                   higher_rss)
      }
    }
  }
  horizontal_partition
}

# Take a look at the list objects we have created with the two
# partition functions:
# [[1]] - The value that partitioned the data frame
# [[2]] - The data frame containing values less than i
# [[3]] - The RSS for the lower-valued data frame
# [[4]] - The data frame containing values greater than or equal to i
# [[5]] - The RSS for the higher-valued data frame

# Partition the data frame vertically (along Years axis)
vpart <- make_vertical_partition(hitters)
# Index associated with the lowest combined RSS
vpart[[1]]
# Assign lower data frame to V1
V1 <- vpart[[2]]
# RSS of V1
vpart[[3]]
# Assign upper data frame to V2
V2 <- vpart[[4]]
# RSS of V2
vpart[[5]]
# Sum of lower (V1) RSS and higher (V2) RSS
vpart[[3]] + vpart[[5]]

# Show rainbow plot with vertical partition
plot(hitters$Years, hitters$Hits, col=cRamp(hitters$Salary), 
     main="Salary as a Function of Years and Hits", xlab = "Years", 
     ylab = "Hits")
segments(x0 = 4.5, y0 = -10, x1 = 4.5, y1 = 250, col = "black" )

# Partition the data frame horizontally (along Hits axis)
hpart <- make_horizontal_partition(hitters)
# Index associated with the lowest combined RSS
hpart[[1]]
# Assign lower data frame to H1
H1 <- hpart[[2]]
# RSS of H1
hpart[[3]]
# Assign higher data frame to H2
H2 <- hpart[[4]]       
# RSS of H2
hpart[[5]]
# Sum of lower (H1) RSS and higher (H2) RSS
hpart[[3]] + hpart[[5]]

# Show rainbow plot with horizontal partition
plot(hitters$Years, hitters$Hits, col=cRamp(hitters$Salary), 
     main="Salary as a Function of Years and Hits", xlab = "Years", 
     ylab = "Hits")
segments(x0 = 0, y0 = 117.5, x1 = 30, y1 = 117.5, col = "black")

# Show and compare RSS results from vertical and horizontal partitions
# Compare to total RSS of unpartitioned data
vpart[[3]] + vpart[[5]] 
hpart[[3]] + hpart[[5]]
R0_rss
vpart[[3]] + vpart[[5]] < R0_rss

# Determine means for each partition
mean(V1$Salary)
mean(V2$Salary)

# Append the 1-partition lowest RSS result to the Deviations vector
deviations <- append(deviations, c("1 partition" = vpart[[3]] + vpart[[5]]))
deviations

# Assign temporary vertical partitions to regions
R1 <- V1
R1_rss <- vpart[[3]]
R1_mean <- mean(R1$Salary)
R2 <- V2
R2_rss <- vpart[[5]]
R2_mean <- mean(R2$Salary)

# Remove all temporary partition
remove(V1)
remove(V2)
remove(H1)
remove(H2)

# Find the region with the highest RSS
max(R1_rss, R2_rss)

# Repeat the partitioning recursively on the region with the highest RSS
