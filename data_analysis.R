# data analyis 
rm(list = ls())
dataset_study1 <- readRDS("~/INSTITUT Dropbox/PROKRIBUS/2_PROJECTS/2_ARCHIVE_FINALIZED_PROJECTS/Paper-Bio-Index/Paper-Bio-Index/AktuelleDateien/dataset_study1.rds")
dataset_study2 <- readRDS("~/INSTITUT Dropbox/PROKRIBUS/2_PROJECTS/2_ARCHIVE_FINALIZED_PROJECTS/Paper-Bio-Index/Paper-Bio-Index/AktuelleDateien/dataset_study2.rds")

# results for table 1 
# sample 1
prop.table(table(dataset_study1$sex))*100
prop.table(table(dataset_study1$income))*100
prop.table(table(dataset_study1$age))*100
prop.table(table(dataset_study1$education))*100
# sample 2
prop.table(table(dataset_study2$sex))*100
prop.table(table(dataset_study2$age))*100
prop.table(table(dataset_study2$education))*100



# results for table 2
library(sjPlot)
dataset_study1_items<-dataset_study1[,1:21]
tab_itemscale(dataset_study1_items)

# results for table 3
library(psych)
alpha(dataset_study1_items, check.keys =TRUE)


# results for table 4 muss in paper abgeändert werden
dataset_study1_items_reduced<-dataset_study1_items[,c(1,5,15,14,13,19,21,17)]
alpha(dataset_study1_items_reduced, check.keys =TRUE)

# results table 5
# sample 1
dataset_study1_items_reduced2<-dataset_study1_items[,c(17,5,21,1,14)]
summary(dataset_study1_items_reduced2[,c(1:5)])
sd(dataset_study1_items_reduced2[,c(1)])
sd(dataset_study1_items_reduced2[,c(2)])
sd(dataset_study1_items_reduced2[,c(3)])
sd(dataset_study1_items_reduced2[,c(4)])
sd(dataset_study1_items_reduced2[,c(5)])
alpha(dataset_study1_items_reduced2, check.keys =TRUE)

# sample 2
dataset_study2_items_reduced2<-dataset_study2[,c(1:5)]
summary(dataset_study2[,c(1:5)])
sd(dataset_study2[,c(1)])
sd(dataset_study2[,c(2)])
sd(dataset_study2[,c(3)])
sd(dataset_study2[,c(4)])
sd(dataset_study2[,c(5)])

alpha(dataset_study2_items_reduced2, check.keys =TRUE)

#table 6 
library(lavaan)
HS.model <- ' factor  =~ Attitude4  + Trust2 + Health2 + Price1_R  + Attitude1_R
 # residual correlations'

# sample 1
fit <- cfa(HS.model, data = dataset_study1_items_reduced2,  std.lv = TRUE)
fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "rmsea"))
summary(fit)
AVE(fit)
modindices(fit, sort = TRUE)

# sample 2
fit <- cfa(HS.model, data = dataset_study2_items_reduced2,  std.lv = TRUE)
fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "rmsea"))
summary(fit)
AVE(fit)
modindices(fit, sort = TRUE)

