setwd("~/0. Personal/Blog/Conjoint Analysis")

library(conjoint)

data(chocolate)
data(tea)

# segmentation using conjoint
caSegmentation(y=tpref, x=tprof, c=3)
summary(clu)

num_cluster <- c(seq(2,10))
ss_in <- rep(0,9)

ss_within=data.frame(
  num_cluster,
  ss_in
)

for (i in 1:9){
  clu = caSegmentation(y=tpref,x=tprof,c=i+1)
  ss_within$ss_in[i]=clu$segm[5]
}

options(digits = 9)
ss_within$num_cluster = as.factor(ss_within$num_cluster)
ss_within$ss_in = as.numeric(ss_within$ss_in)

#----------------- plot the error -----------------------------------
library(ggplot2)
p <- ggplot(ss_within, aes(x = num_cluster, y = ss_in)) +
  geom_bar(stat = "identity",fill="blue")+
  ggtitle("Total Error Whitin Cluster x Number of Cluster")+
  xlab("Number of Cluster")+
  ylab("Total Error within Cluster")

p

# ----------------- visualizing segment using PCA -----------------------
# optimum number of clusters is 6
clu = caSegmentation(y=tpref,x=tprof,c=6)

# preparation
prtutil_tea = caPartUtilities(y=tprefm, x=tprof, z=tlevn)
prtutil_tea = as.data.frame(prtutil_tea)

# running pca
res.pca <- prcomp(prtutil_tea, scale = TRUE)
print(res.pca)
summary(res.pca)

scores <- res.pca$x
prop_var <- res.pca$sdev^2 / sum(res.pca$sdev^2)
rownames(res.pca$rotation)
screeplot(res.pca)
biplot(res.pca)

# plotting
res.pca_df <- data.frame(
  Dim1 = res.pca$x[, 1],  # Replace with the actual column names
  Dim2 = res.pca$x[, 2],
  Group = as.factor(clu$sclu)  # Replace with your actual grouping variable
)

# plotting segment from PCA value
# using ggplot
library(ggplot2)

ggplot(res.pca_df, aes(x = Dim1, y = Dim2, color = Group)) +
  geom_point(size = 3) +
  labs(title = "PCA Scatterplot", x = "Principal Component 1", y = "Principal Component 2")

# plotting PCA dim 1 and dim 2 with segment
# using facto extra
library(factoextra)
fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = as.factor(clu$sclu), # color by groups
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "convex",
             legend.title = "Groups"
)

# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
# https://www.geeksforgeeks.org/how-to-split-column-into-multiple-columns-in-r-dataframe/
# http://www.sthda.com/english/wiki/writing-data-from-r-to-excel-files-xls-xlsx

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

# create segment profile

# --------------------Segment Profile --------------
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

verbose_output_2 <- capture.output({
  Conjoint(y=tpref2, x=tprof, z=tlevn)
})

# profile segment 3
tprefm3 <- tprefm[clu$sclu==3,]
tpref3 <- data.frame(Y=matrix(t(tprefm3), ncol=1, nrow=ncol(tprefm3)*nrow(tprefm3), byrow=F))
Conjoint(y=tpref3, x=tprof, z=tlevn)

verbose_output_3 <- capture.output({
  Conjoint(y=tpref3, x=tprof, z=tlevn)
})

# profile segment 4
tprefm4 <- tprefm[clu$sclu==4,]
tpref4 <- data.frame(Y=matrix(t(tprefm4), ncol=1, nrow=ncol(tprefm4)*nrow(tprefm4), byrow=F))
Conjoint(y=tpref4, x=tprof, z=tlevn)

verbose_output_4 <- capture.output({
  Conjoint(y=tpref4, x=tprof, z=tlevn)
})

# profile segment 5
tprefm5 <- tprefm[clu$sclu==5,]
tpref5 <- data.frame(Y=matrix(t(tprefm5), ncol=1, nrow=ncol(tprefm5)*nrow(tprefm5), byrow=F))
Conjoint(y=tpref5, x=tprof, z=tlevn)

verbose_output_5 <- capture.output({
  Conjoint(y=tpref5, x=tprof, z=tlevn)
})

# profile segment 6
tprefm6 <- tprefm[clu$sclu==6,]
tpref6 <- data.frame(Y=matrix(t(tprefm6), ncol=1, nrow=ncol(tprefm6)*nrow(tprefm6), byrow=F))
Conjoint(y=tpref6, x=tprof, z=tlevn)

verbose_output_6 <- capture.output({
  Conjoint(y=tpref6, x=tprof, z=tlevn)
})

# saving the result in file
library(stringr)
library(tidyverse)

# SEGMENT 1
# extracting utility
util_1 <- as.data.frame(verbose_output_1[28:39])
colnames(util_1)[1] = "utility"
util_1[c('row', 'level_util')] <- str_split_fixed(util_1$utility,' ',2)
util_1$level_util <- trimws(util_1$level_util,which='both')
util_1[c('level','util_seg1')] <- str_split_fixed(util_1$level_util,' ',2)
util_1 <- util_1 %>% select(level,util_seg1)

# extracting factor importance
importance_1<-as.data.frame(verbose_output_1[41])
colnames(importance_1)[1]="importance"
importance_1[c('row','val')]=str_split_fixed(importance_1$importance,' ',2)
importance_1[c('price','val')]=str_split_fixed(importance_1$val,' ',2)
importance_1[c('variety','val')]=str_split_fixed(importance_1$val,' ',2)
importance_1[c('kind','val')]=str_split_fixed(importance_1$val,' ',2)
importance_1[c('aroma','val')]=str_split_fixed(importance_1$val,' ',2)
importance_1 = importance_1 %>% select(price,variety,kind,aroma)

# SEGMENT 2
# extracting utility
util_2 <- as.data.frame(verbose_output_2[28:39])
colnames(util_2)[1] = "utility"
util_2[c('row', 'level_util')] <- str_split_fixed(util_2$utility,' ',2)
util_2$level_util <- trimws(util_2$level_util,which='both')
util_2[c('level','util_seg2')] <- str_split_fixed(util_2$level_util,' ',2)
util_2 <- util_2 %>% select(level,util_seg2)

# extracting factor importance
importance_2<-as.data.frame(verbose_output_2[41])
colnames(importance_2)[1]="importance"
importance_2[c('row','val')]=str_split_fixed(importance_2$importance,' ',2)
importance_2[c('price','val')]=str_split_fixed(importance_2$val,' ',2)
importance_2[c('variety','val')]=str_split_fixed(importance_2$val,' ',2)
importance_2[c('kind','val')]=str_split_fixed(importance_2$val,' ',2)
importance_2[c('aroma','val')]=str_split_fixed(importance_2$val,' ',2)
importance_2 = importance_2 %>% select(price,variety,kind,aroma)

# SEGMENT 3
# extracting utility
util_3 <- as.data.frame(verbose_output_3[28:39])
colnames(util_3)[1] = "utility"
util_3[c('row', 'level_util')] <- str_split_fixed(util_3$utility,' ',2)
util_3$level_util <- trimws(util_3$level_util,which='both')
util_3[c('level','util_seg3')] <- str_split_fixed(util_3$level_util,' ',2)
util_3 <- util_3 %>% select(level,util_seg3)

# extracting factor importance
importance_3<-as.data.frame(verbose_output_3[41])
colnames(importance_3)[1]="importance"
importance_3[c('row','val')]=str_split_fixed(importance_3$importance,' ',2)
importance_3[c('price','val')]=str_split_fixed(importance_3$val,' ',2)
importance_3[c('variety','val')]=str_split_fixed(importance_3$val,' ',2)
importance_3[c('kind','val')]=str_split_fixed(importance_3$val,' ',2)
importance_3$val <- trimws(importance_3$val,which='both')
importance_3[c('aroma','val')]=str_split_fixed(importance_3$val,' ',2)
importance_3 = importance_3 %>% select(price,variety,kind,aroma)

# SEGMENT 4
# extracting utility
util_4 <- as.data.frame(verbose_output_4[28:39])
colnames(util_4)[1] = "utility"
util_4[c('row', 'level_util')] <- str_split_fixed(util_4$utility,' ',2)
util_4$level_util <- trimws(util_4$level_util,which='both')
util_4[c('level','util_seg4')] <- str_split_fixed(util_4$level_util,' ',2)
util_4 <- util_4 %>% select(level,util_seg4)

# extracting factor importance
importance_4<-as.data.frame(verbose_output_4[41])
colnames(importance_4)[1]="importance"
importance_4[c('row','val')]=str_split_fixed(importance_4$importance,' ',2)
importance_4[c('price','val')]=str_split_fixed(importance_4$val,' ',2)
importance_4[c('variety','val')]=str_split_fixed(importance_4$val,' ',2)
importance_4[c('kind','val')]=str_split_fixed(importance_4$val,' ',2)
importance_4[c('aroma','val')]=str_split_fixed(importance_4$val,' ',2)
importance_4 = importance_4 %>% select(price,variety,kind,aroma)

# SEGMENT 5
# extracting utility
util_5 <- as.data.frame(verbose_output_5[28:39])
colnames(util_5)[1] = "utility"
util_5[c('row', 'level_util')] <- str_split_fixed(util_5$utility,' ',2)
util_5$level_util <- trimws(util_5$level_util,which='both')
util_5[c('level','util_seg5')] <- str_split_fixed(util_5$level_util,' ',2)
util_5 <- util_5 %>% select(level,util_seg5)

# extracting factor importance
importance_5<-as.data.frame(verbose_output_5[41])
colnames(importance_5)[1]="importance"
importance_5[c('row','val')]=str_split_fixed(importance_5$importance,' ',2)
importance_5[c('price','val')]=str_split_fixed(importance_5$val,' ',2)
importance_5[c('variety','val')]=str_split_fixed(importance_5$val,' ',2)
importance_5[c('kind','val')]=str_split_fixed(importance_5$val,' ',2)
importance_5$val <- trimws(importance_5$val,which='both')
importance_5[c('aroma','val')]=str_split_fixed(importance_5$val,' ',2)
importance_5 = importance_5 %>% select(price,variety,kind,aroma)

# SEGMENT 6
# extracting utility
util_6 <- as.data.frame(verbose_output_6[28:39])
colnames(util_6)[1] = "utility"
util_6[c('row', 'level_util')] <- str_split_fixed(util_6$utility,' ',2)
util_6$level_util <- trimws(util_6$level_util,which='both')
util_6[c('level','util_seg6')] <- str_split_fixed(util_6$level_util,' ',2)
util_6 <- util_6 %>% select(level,util_seg6)

# extracting factor importance
importance_6<-as.data.frame(verbose_output_6[41])
colnames(importance_6)[1]="importance"
importance_6[c('row','val')]=str_split_fixed(importance_6$importance,' ',2)
importance_6[c('price','val')]=str_split_fixed(importance_6$val,' ',2)
importance_6[c('variety','val')]=str_split_fixed(importance_6$val,' ',2)
importance_6[c('kind','val')]=str_split_fixed(importance_6$val,' ',2)
importance_6[c('aroma','val')]=str_split_fixed(importance_6$val,' ',2)
importance_6 = importance_6 %>% select(price,variety,kind,aroma)

# combining result utility
util_all = util_1 %>% left_join(util_2,by="level") %>% 
  left_join(util_3,by="level") %>% 
  left_join(util_4,by="level") %>% 
  left_join(util_5,by="level") %>% 
  left_join(util_6,by="level")

# combining result importance
importance_all = rbind(importance_1,importance_2,importance_3,importance_4,importance_5,importance_6)

# segment characteristic using Excel
library(writexl)
write_xlsx(util_all,"util_all.xlsx")
write_xlsx(importance_all,"importance_all.xlsx")
