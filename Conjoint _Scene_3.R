setwd("~/0. Personal/Blog/Conjoint Analysis")

# load library
library(readxl)
library(janitor)

# read data
spot_remover <- read_excel("Spot Remover.xlsx",sheet = "Sheet1")

# cleaning column name
spot_remover <- janitor::clean_names(spot_remover)

# getting dummy data
library(fastDummies)
library(tidyverse)

spot_remover_dummy <- dummy_cols(spot_remover)

spot_remover_dummy <- spot_remover_dummy %>% select(-c(package_design,brand_name,price,
                                                       good_housekeeping_seal_y_n,money_back_guarantee_y_n,rank))

# correlation
cor_spot_remover = data.frame(cor(spot_remover_dummy))
cor_spot_remover = round(cor_spot_remover)

library(openxlsx)

write.xlsx(cor_spot_remover,"cor_spot_remover.xlsx",rowNames=TRUE)

# change to factor
str(spot_remover)
spot_remover <- as.data.frame(unclass(spot_remover),stringsAsFactors = TRUE)

# change factor to numeric
factors <- sapply(spot_remover, is.factor)
spot_remover[ , factors] <- lapply(spot_remover[ , factors], as.numeric)

round(cor(spot_remover %>% select(-rank)))

# creating orthogonal design
library(conjoint)


c <- expand.grid(
  price <- c("low", "medium", "high"),
  color <- c("black", "white"),
  size <- c("small", "large"))

#change the column names to these
names(c) <- c("price", "color", "size")

design <- caFactorialDesign(data=c, type="orthogonal")

code <- caEncodedDesign(design)
encodedorthodesign <- data.frame(design, code)
print(encodedorthodesign)

# spot remover
sr <- expand.grid(
  package_design <- c("A","B","C"),
  brand_name<-c("K2R","Glory","Bissel"),
  price<-c("$1.19","$1.39","$1.59"),
  goodhousekeeping<-c("Yes","No"),
  moneyback<-c("Yes","No")
  
)

names(sr)<-c("package_design","brand_name","price","goodhousekeeping","moneyback")

sr_design <- caFactorialDesign(data=sr,type="orthogonal")
sr_code <- caEncodedDesign(sr_design)
cor(sr_code)

sr_design_fr <- caFactorialDesign(data=sr,type="fractional",cards = 18)
sr_code_fr <- caEncodedDesign(sr_design_fr)
cor(sr_code_fr)


write.xlsx(sr_design,"sr_design.xlsx")

# using package DoE
library(DoE.base)

# Define the factors and levels
factors <- c("Factor1", "Factor2", "Factor3")
levels <- list(
  c("Level1", "Level2"),
  c("Level1", "Level2"),
  c("Level1", "Level2")
)

num_runs <- 4  # Choose a power of 2 (e.g., 2, 4, 8, 16, etc.)

library(FrF2)

design_doe <- FrF2(nfactors = length(factors), resolution = num_runs, factors = factors, nlevels = levels)
