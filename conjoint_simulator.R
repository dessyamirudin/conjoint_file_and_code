setwd("D:/R Book Writing Doodling/conjoint_analysis/conjoint_file_and_code")

# load library
library(readxl)

#------------- preparing data -----------------------------

# Segment
segment <- read_excel("conjoint_simulator.xlsx",sheet = "Segment")

# Product Profile
profile <- read_excel("conjoint_simulator.xlsx",sheet = "Profile")

# Market Share
ms <- read_excel("conjoint_simulator.xlsx",sheet = "Market Share")

# -- change data format to matrix

segment_matrix = segment[,3:ncol(segment)]
segment_mat = data.matrix(segment_matrix)

profile_matrix = profile[,2:ncol(profile)]
profile_matrix_ts = t(profile_matrix)

# matrix multiplication
initial_market_share_calc <- segment_mat %*% profile_matrix_ts

# parameter alpha
initial_par_alpha = c(2)

# marketshare matrix alpha
market_share_alpha = initial_market_share_calc^initial_par_alpha

# calculating market share segment
market_share = market_share_alpha/rowSums(market_share_alpha)

# market share brand
market_share_brand = colSums(segment$`Size(000)`*market_share)/sum(segment$`Size(000)`)

# ---- optimizing alpha ----

# objective - difference between real and simulation
objective = (ms$`Current share` - market_share_brand)^2

# total error objective
err_objective = sum(objective)

# writing optimization function
objective_function <- function(alpha){
  market_share_alpha = initial_market_share_calc^alpha
  market_share = market_share_alpha/rowSums(market_share_alpha)
  market_share_brand = colSums(segment$`Size(000)`*market_share)/sum(segment$`Size(000)`)
  
  sum((ms$`Current share` - market_share_brand)^2)
}

# Perform optimization
result <- optim(par = initial_par_alpha, fn = objective_function)

# Print the results
print(result$par)

# ----- check market share with the new parameter alpha
# marketshare matrix alpha
market_share_alpha = initial_market_share_calc^result$par

# calculating market share segment
market_share = market_share_alpha/rowSums(market_share_alpha)

# market share brand
market_share_brand = colSums(segment$`Size(000)`*market_share)/sum(segment$`Size(000)`)

# ----- simulation market share - bran 1 adding Stampel Approval from Home Goods &
# ----- money back guarantee ----

# market simulation for brand 1
profile_matrix_new = profile_matrix 
profile_matrix_new$Approved[1] = 1
profile_matrix_new$Guarantee[1] = 1

# ------ calculating new market share ----------
# transpose profile matrix and change to matrix
profile_matrix_new_ts = t(profile_matrix_new)

# matrix multiplication
initial_market_share_new <- segment_mat %*% profile_matrix_new_ts

# parameter alpha
initial_par_alpha_new = result$par

# marketshare matrix alpha
market_share_alpha_new = initial_market_share_new^initial_par_alpha_new

# calculating market share segment
market_share_new = market_share_alpha_new/rowSums(market_share_alpha_new)

# market share brand
market_share_brand_new = colSums(segment$`Size(000)`*market_share_new)/sum(segment$`Size(000)`)
