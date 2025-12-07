setwd("D:/R Book Writing Doodling/conjoint_analysis/conjoint_file_and_code")

# load library
library(readxl)
library(janitor)

# read data
# read data
segment <- read_excel("conjoint_simulator.xlsx",sheet = "Segment")

# Product Profile
profile <- read_excel("conjoint_simulator.xlsx",sheet = "Profile")

# Market Share
ms <- read_excel("conjoint_simulator.xlsx",sheet = "Market Share")

# writing function
segment_matrix = segment[,3:ncol(segment)]

# change segment_matrix to matrix
segment_mat = data.matrix(segment_matrix)

profile_matrix = profile[,2:ncol(profile)]

# transpose profile matrix and change to matrix
profile_matrix_ts = t(profile_matrix)

# matrix multiplication
market_share_matrix <- segment_mat %*% profile_matrix_ts

# parameter alpha
initial_par_alpha = c(2)

# marketshare matrix alpha
market_share_matrix_alpha = market_share_matrix^initial_par_alpha

# calculating market share segment
market_share = market_share_matrix_alpha/rowSums(market_share_matrix_alpha)

# market share brand
market_share_brand = colSums(segment$`Size(000)`*market_share)/sum(segment$`Size(000)`)

# objective - difference between real and simulation
objective = (ms$`Current share` - market_share_brand)^2

# total error objective
err_objective = sum(objective)

# writing optimization function
objective_function <- function(alpha){
  market_share_matrix_alpha = market_share_matrix^alpha
  market_share = market_share_matrix_alpha/rowSums(market_share_matrix_alpha)
  market_share_brand = colSums(segment$`Size(000)`*market_share)/sum(segment$`Size(000)`)
  
  sum((ms$`Current share` - market_share_brand)^2)
}

# Perform optimization
result <- optim(par = initial_par_alpha, fn = objective_function)

# Print the results
print(result)

# market simulation for brand 1
profile_matrix_new 
