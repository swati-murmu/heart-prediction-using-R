rm(list=ls())
gc() 
setwd('C:/Users/fabia/Documents/MBA Cal State Fullerton/ISDS 574 - Data Mining/Group Project/Alternative')

dat = read.csv('./heart_failure_clinical_records_dataset.csv', head=T, stringsAsFactors=F, na.strings='')

## check missing values & visualize ##
matrix.na = is.na(dat)
pmiss = colMeans(matrix.na)
nmiss = rowMeans(matrix.na)
plot(pmiss)

library(Amelia)
missmap(dat)

#remove time variable
dat$time = NULL

## check the distribution of continuous variables ##
par(mfrow=c(1, 2))

# Histogram & Boxplot 
hist(dat$age)
boxplot(dat$age)

# Histogram & Boxplot 
hist(dat$creatinine_phosphokinase)
boxplot(dat$creatinine_phosphokinase)

#removing outlier
which(dat$creatinine_phosphokinase > 3000)
dim(dat[c(2,53,61,73,104,135,172),])
dat = dat[dat$creatinine_phosphokinase <= 3000,]

# Histogram & Boxplot 
hist(dat$ejection_fraction)
boxplot(dat$ejection_fraction)

#removing outlier
which(dat$ejection_fraction > 65)
dim(dat[c(62,211),])
dat = dat[dat$ejection_fraction <= 65,]

# Histogram & Boxplot 
hist(dat$platelets)
boxplot(dat$platelets)

#removing outlier
which(dat$platelets > 590000)
dim(dat[c(100,104,288),])
dat = dat[dat$platelets <= 590000,]

# Histogram & Boxplot 
hist(dat$serum_creatinine)
boxplot(dat$serum_creatinine)

#removing outlier
which(dat$serum_creatinine > 8)
dim(dat[9,])
dat = dat[dat$serum_creatinine <= 8,]

# Histogram & Boxplot 
hist(dat$serum_sodium)
boxplot(dat$serum_sodium)

#removing outlier
which(dat$serum_sodium < 125)
dim(dat[c(4,18,117,187),])
dat = dat[dat$serum_sodium >= 125,]

#code to standardize numerical variables
#dat[,c(3,5,7:9)] = scale(dat[,c(3,5,7:9)])

#check frequency of age
table(dat$age)
#combining possible data entry error
which(dat$age == 60.667)
id = which(dat$age %in% c(60.667,61))
dat$age[id] = 61

library(corrplot)
par(mfrow = c(1,1))
corrplot(cor(dat), method="number", number.cex = .5)

library(dplyr)
dat %>% cor() %>% round(2) %>% View()

save(dat, file = "cleaned_Heart_failure_data.rda")
