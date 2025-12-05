setwd("D:/Blog Purpose/Conjoint")

# practice 1
library(conjoint)
library(purrr)
library(tidyverse)

data(tea)

# first respondent utility
caModel(y=tprefm[1,], x=tprof) # cuma bisa 1 per satu, satu input saja
caUtilities(y=tprefm[1,], x=tprof, z=tlevn) # bisa satu per satu atau gabungan banyak
caUtilities(y=tprefm[1:10,], x=tprof, z=tlevn)
# caUtilities(y=tprefm, x=tprof, z=tlevn)
# note caUtilities create regression overall, not one by one

# top 10
caPartUtilities(y=tprefm[1,], x=tprof, z=tlevn) # dihitung satu2
# caPartUtilities calculate 100 (each respondent) regression
caPartUtilities(y=tprefm, x=tprof, z=tlevn)

# for all respondent - 100
Conjoint(y=tpref, x=tprof, z=tlevn)

# try by calculating manually
str(tprefm)
str(tprof)
# change integer to factor
tea_profile = tprof %>% purrr::map_if(is.integer,as.factor) %>% as_tibble()
# mulitply 100 times
tea_profile_100 = tea_profile
for (i in 1:99){
  tea_profile_100 = rbind(tea_profile_100, tea_profile)
}

data_tea = cbind(tea_profile_100,tpref)
result_tea_lm = lm(Y ~.,data=data_tea)
summary(result_tea_lm)

clu = caSegmentation(y=tpref, x=tprof, c=3)
# this sergment create using the response or utilities?

# profile segment 1
tprefm1 <- tprefm[clu$sclu==1,]
tpref1 <- data.frame(Y=matrix(t(tprefm1), ncol=1, nrow=ncol(tprefm1)*nrow(tprefm1), byrow=F))
Conjoint(y=tpref1, x=tprof, z=tlevn)

verbose_output_1 <- capture.output({
  Conjoint(y=tpref1, x=tprof, z=tlevn)
})

# profile segment 2
tprefm2 <- tprefm[clu$sclu==2,]
tpref2 <- data.frame(Y=matrix(t(tprefm2), ncol=1, nrow=ncol(tprefm2)*nrow(tprefm2), byrow=F))
Conjoint(y=tpref2, x=tprof, z=tlevn)

# profile segment 3
tprefm3 <- tprefm[clu$sclu==3,]
tpref3 <- data.frame(Y=matrix(t(tprefm3), ncol=1, nrow=ncol(tprefm3)*nrow(tprefm3), byrow=F))
Conjoint(y=tpref3, x=tprof, z=tlevn)

# visualizing cluster with PCA
library(factoextra)
prtutil_tea = caPartUtilities(y=tprefm, x=tprof, z=tlevn)
prtutil_tea = as.data.frame(prtutil_tea)

res.pca <- prcomp(prtutil_tea, scale = TRUE)
print(res.pca)
summary(res.pca)

eig.val<-get_eigenvalue(res.pca) # error
eig.val

library(ggpubr)

fviz_eig(res.pca, col.var="blue") #error

# using doe

# source:
# https://stats.stackexchange.com/questions/527110/how-to-calculate-average-importance-of-factors-attributes-correctly-in-conjoi