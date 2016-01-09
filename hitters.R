
#install.packages("./ISLR_1.0.tgz", repos = NULL, type = "source")
#library(ISLR)
library(dplyr)
library(tree)

#write.table(Hitters, "./hitters.csv", sep = ",")

hitters <- read.table("./hitters.csv", sep = ",", header = TRUE)

str(hitters)
hitters<- na.omit(hitters)
str(hitters)

hist(hitters$Salary)


# Create log2Salary

hitters$Salary <- log(hitters$Salary)
str(hitters)

hist(hitters$Salary)


# Find the mean
range((hitters$Salary))
x <- mean(hitters$Salary)
x

# Compute the sum of squares

RSS <- function(x){
  rss <- sum((x - mean(x))**2)
  rss
}

mean(hitters$Salary)
R0_rss <- RSS(hitters$Salary)
R0_rss

deviations <- c("0 partitions" = R0_rss)
deviations

# https://stackoverflow.com/questions/7420281/
# create-a-rainbow-color-scale-based-on-a-vector-in-the-order-of-that-vector
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

segments(x0 = 4.5, y0 = -10, x1 = 4.5, y1 = 250, lty = "dotted", col = "black" )

segments(x0 = 4.5, y0 = 117.5, x1 = 30, y1 = 117.5, lty = "dotted", col = "black")


make_vertical_partition <- function(df) {

  if(exists("vertical_partition")) {remove("vertical_partition")}
    
  for(i in unique(sort(df$Years))){
    
    lower_years <- filter(df, df$Years < i)
    lower_rss <- sum((lower_years$Salary - mean(lower_years$Salary))^2)
    
    higher_years <- filter(df, df$Years >= i)
    higher_rss <- sum((higher_years$Salary - mean(higher_years$Salary))^2)

    if(!exists("vertical_partition")){
      vertical_partition <- list(i, lower_years, lower_rss, higher_years, 
                               higher_rss)
    } else {
      if (lower_rss + higher_rss < vertical_partition[[3]] + 
          vertical_partition[[5]]){
        vertical_partition <- list(i, lower_years, lower_rss, higher_years, 
                                higher_rss)
      }
    }
  }
  vertical_partition
}
  
make_horizontal_partition <- function(df) {
  
  if(exists("horizontal_partition")) {remove("horizontal_partition")}
  
  for(i in unique(sort(df$Hits))){
    
    lower_hits <- filter(df, df$Hits < i)
    lower_rss <- sum((lower_hits$Salary - mean(lower_hits$Salary))^2)
    
    higher_hits <- filter(df, df$Hits >= i)
    higher_rss <- sum((higher_hits$Salary - mean(higher_hits$Salary))^2)
    
    if(!exists("horizontal_partition")){
      horizontal_partition <- list(i, lower_hits, lower_rss, higher_hits, 
                                 higher_rss)
    } else {
      if (lower_rss + higher_rss < horizontal_partition[[3]] + 
          horizontal_partition[[5]]){
        horizontal_partition <- list(i, lower_hits, lower_rss, higher_hits, 
                                   higher_rss)
      }
    }
  }
  horizontal_partition
}

# Partitions (lists):
# [[1]] - The value that partitioned the data frame
# [[2]] - The data frame containing values less than i
# [[3]] - The RSS for the lower-valued data frame
# [[4]] - The data frame containing values greater than or equal to i
# [[5]] - The RSS for the higher-valued data frame

vpart1 <- make_vertical_partition(hitters)
vpart1[[1]]            
V1 <- vpart1[[2]]
vpart1[[3]]
V2 <- vpart1[[4]]
vpart1[[5]]
vpart1[[3]] + vpart1[[5]]

plot(hitters$Years, hitters$Hits, col=cRamp(hitters$Salary), 
     main="Salary as a Function of Years and Hits", xlab = "Years", 
     ylab = "Hits")

segments(x0 = 4.5, y0 = -10, x1 = 4.5, y1 = 250, col = "black" )

hpart1 <- make_horizontal_partition(hitters)
hpart1[[1]]            
H1 <- hpart1[[2]]
hpart1[[3]]
H2 <- hpart1[[4]]       
hpart1[[5]]
hpart1[[3]] + hpart1[[5]]

plot(hitters$Years, hitters$Hits, col=cRamp(hitters$Salary), 
     main="Salary as a Function of Years and Hits", xlab = "Years", 
     ylab = "Hits")

segments(x0 = 0, y0 = 117.5, x1 = 30, y1 = 117.5, col = "black")

mean(H1$Salary)
mean(H2$Salary)

vpart1[[3]] + vpart1[[5]] < hpart1[[3]] + hpart1[[5]]
vpart1[[3]] + vpart1[[5]] < total_rss

deviations <- append(deviations, c("1 partition" = vpart1[[3]] + vpart1[[5]]))
deviations


# Partition R2
str(V2)
vpart3 <- make_vertical_partition(V2)
vpart3[[1]]
V5 <- vpart3[[2]]
V6 <- vpart3[[4]]
vpart3[[3]] + vpart3[[5]]

hpart2 <- make_horizontal_partition(V2)
hpart2[[1]]            
H3 <- hpart2[[2]]      
mean(H3$Salary)
hpart2[[3]]
H4 <- hpart2[[4]]      
mean(H4$Salary)
hpart2[[5]]
hpart2[[3]] + hpart2[[5]]

plot(hitters$Years, hitters$Hits, col=cRamp(hitters$Salary), 
     main="Salary as a Function of Years and Hits", xlab = "Years", 
     ylab = "Hits")

segments(x0 = 4.5, y0 = -10, x1 = 4.5, y1 = 250, col = "black" )
segments(x0 = 4.5, y0 = 117.5, x1 = 30, y1 = 117.5, col = "black")


vpart1[[3]] + hpart2[[3]] + hpart2[[5]]

deviations <- append(deviations, c("2 partitions" = vpart1[[3]] + 
                                     hpart2[[3]] + hpart2[[5]]))
deviations




hpart3 <- make_horizontal_partition(V2)
hpart3[[1]]            # i, the value that partitioned the data frame
H5 <- hpart3[[2]]      # data frame of values less than Years = i
H6 <- hpart3[[4]]      # data frame of values greater than or equal to Years = i 
vpart1[[3]]
vpart1[[5]]
hpart3[[3]] + hpart3[[5]]



vpart[[3]] + hpart3[[3]] + hpart3[[5]]

# Create a tree
hitters.tree <- tree(Salary ~ Hits + Years, data = hitters)

plot(hitters.tree)
text(hitters.tree)

cv.hitters <- cv.tree(hitters.tree)
plot(cv.hitters$size, cv.hitters$dev, type = "b")

cv.hitters$dev

prune.hitters <- prune.tree(hitters.tree, best = 4)
plot(prune.hitters)
text(prune.hitters, pretty = 0)

# Create training and test subsets

set.seed(123)

train <- sample_n(hitters, 132)
test <- setdiff(hitters, train) 
