setwd("~/0. Personal/Blog/Conjoint Analysis")

# load library
library(readxl)
library(janitor)

# read data
spot_remover = read_excel("Spot Remover.xlsx",sheet = "Sheet1")

# inverse rank
spot_remover$inverse_rank = 19 - spot_remover$Rank

# cleaning column name
spot_remover <- janitor::clean_names(spot_remover)

# regression
lm_spot_remover = lm(inverse_rank ~ package_design + brand_name + price + good_housekeeping_seal_y_n + money_back_guarantee_y_n, data=spot_remover)

#Review the results
summary(lm_spot_remover) 
