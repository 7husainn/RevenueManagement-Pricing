#Read the table:----
product_2 <- read.csv("WTP_500ML_DATA.csv", header = T)
View(product_2)

# Variables initialization
# Number of Records/Rows:
N <- nrow(product_2)

# We will get the maximum willingness to pay across all prices.
maxPrice <- max(product_2[, 2:3])
#Price of standard product obtained from online demand learning.
baseprice <- 5       

# Step 1: Calculating Surplus for Standard product----
#Surplus for Standard Product
product_2$Surplus_Standard <- product_2$Standard_WTP - baseprice
View(product_2)

# Step 2: We will Calculate Surplus for Premium Product for each consumer across all price levels ----
# First, we will create a matrix to store surplus values:

# Then, We will get number of columns for each price level
column_values <- seq(1,maxPrice,by= 0.1)

Surplus_Premium <- matrix(0, nrow = N, ncol = length(column_values))
dim(Surplus_Premium)  #Checking the dimensions of the matrix

# We will give column names to Surplus_Premium matrix
colnames(Surplus_Premium) <- paste("p=", column_values)
head(Surplus_Premium)

# We will calculate surplus:
for (p in seq(1,maxPrice,by= 0.1)) {
  index <- (p*10) - 9               # As column indexes are always integer, we will create a price-index relation.
  for (i in 1:N) {
    Surplus_Premium[i, index] <- product_2[i, 3] - p
  }
}
View(Surplus_Premium)

# Step 3: We will calculate demands for both standard and premium products ----
# Initializing arrays to store demands for each product
demand_Standard <- rep(0, length(column_values))
demand_Premium <- rep(0, length(column_values))
revenue <- rep(0, length(column_values))

# We will calculate demand_Standard, demand_Premium, and revenue for each price
for (p in seq(1,maxPrice,by= 0.1)) {
  # As the index of columns are always integer and price levels are in decimal, 
  # we will have to define a price-index relation.
  index <- (p*10) - 9
  
  # To calculate the demand for standard product, these two condition should be met:
  # Condition 1: surplus for standard product is greater than Surplus of the premium product
  # Condition 2: surplus of standard product is positive
  # We will check these condition, we will store it as a demand for standard product.
  # We will perform the same for premium product.
  demand_Standard[index] <- sum((product_2$Surplus_Standard > Surplus_Premium[, index]) 
                                & 
                                (product_2$Surplus_Standard >= 0)
                               )
  
  demand_Premium[index] <- sum((Surplus_Premium[, index] >= product_2$Surplus_Standard) 
                               & 
                               (Surplus_Premium[, index] >= 0)
                               )
  
  revenue[index] <- (baseprice * demand_Standard[index]) + (p * demand_Premium[index])
}

# Step 4: Selecting optimal price for premium product ----
# To select the optimal price, we will select the price associated with the maximum revenue.

# First, We will find maximum revenue for premium product
revenueBest <- max(revenue)

# As there is a price-index relation, 
# (which(revenue == revenueBest) will give us the index of bestprice, 
# then we will have to convert it into actual price.
priceBest <- (which(revenue == revenueBest)+9)/10

print(paste("When the standard product price is £5, 
            the optimal premium product price is: £", priceBest))

#Seeing demand for each product:
demand_Standard[which(revenue == revenueBest)]    #Standard Product demand
demand_Premium[which(revenue == revenueBest)]      # Premium Product demand

#Estimating actual demand for the population:
actual_demand_Standard <- ((demand_Standard[which(revenue == revenueBest)])/300)*45000
actual_demand_Premium <- ((demand_Premium[which(revenue == revenueBest)])/300)*45000

print(paste("When the non-premium price is £5, the optimal premium price is: £", priceBest))
