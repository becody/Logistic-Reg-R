#reading in data:
library(haven)
library(dplyr)
install.packages("tidyverse")
install.packages("dummies")
library(dummies)
library(tidyverse)
library(forcats)
library(Hmisc)

#read in dataset and create list of factors
#list of  cat variables that contained missing values:
#INV, CC, HMOWN, RES
#contin: PHONE, POSAMT, INVBAL, CCBAL, AGE, CCPURC, HMVAL
insurance_t <-read_sas("C:\\Users\\17046\\Documents\\MSA 20\\Logistic Reg\\Homework2_LR\\insurance_t_bin.sas7bdat")

fctCol <- c('DDA', 'DIRDEP','SAV', 'ATM', 'CD', 'IRA',
            'LOC', 'INV','ILS', 'MM', 'MTG', 'CC', 'SDB', 
            'HMOWN', 'MOVED', 'INAREA', 'NSF','INS', 
            'BRANCH', 'RES',)

names(insurance_t)

df <- data.frame(insurance_t)

#turn categorical variables in the dataset into factors
insurance_t <- insurance_t %>%
  mutate_at(fctCol, as.factor)

#changing data to add missing category to categorical with missing
insurance_t = insurance_t %>% mutate_if(is.factor, fct_explicit_na, na_level= "M")
names(insurance_t)

#imputing median for continuous variables to get rid of missing
impute(insurance_t$PHONE_Bin, median)
impute(insurance_t$POS_Bin, median)
impute(insurance_t$INVBAL_Bin, median)
impute(insurance_t$CCBAL_Bin)
impute(insurance_t$AGE_Bin)
impute(insurance_t$CCPURC)
impute(insurance_t$HMVAL_Bin)

#checking levels of INV to make sure "M" category is there
levels(insurance_t$INV)

#creating a logit model with new data including M 
logit.model.w <- glm(INS ~., data=insurance_t, family = binomial(link = "logit") )
summary(logit.model.w)







































#create dummies for all factors in the data frame:

insurance_t_new <- dummy.data.frame(df, sep = ".")
names(insurance_t_new)

#create separate matrix of predictors excluding reference dummies
names(insurance_t_new)
predictvar <- (names(insurance_t_new) %in% c("INS.1",
                                             "INS.0",
                                             "DDA.0",
                                             "DIRDEP.0",
                                             "NSF.0",
                                             "SAV.0",
                                             "ATM.0",
                                             "CD.0",
                                             "IRA.0",
                                             "LOC.0",
                                             "INV.0",
                                             "ILS.0",
                                             "MM.0",
                                             "MTG.0",
                                             "CC.0",
                                             "SDB.0",
                                             "HMOWN.0",
                                             "MOVED.0",
                                             "INAREA.0",
                                             "BRANCH.19"))
predmatrix <- as.matrix(insurance_t_new)

#logistic regression model 
logit.model.w <- glm(INS.1 ~ predmatrix[,!predictvar], data=insurance_t.new, family = binomial(link = "logit") )
summary(logit.model.w)

#try to exclude reference categoriese to get fuller picture 
# of separation 

#Problematic separation categories:
# Branch 14, 15, 18, and 19  RES U  MMBAL Bin 
table(insurance_t.new$BRANCH.B14)
table(insurance_t.new$BRANCH.B15)
table(insurance_t.new$BRANCH.B16)
table(insurance_t.new$BRANCH.B18)
table(insurance_t.new$BRANCH.B19)
table(insurance_t.new$RES.U)
table(insurance_t.new$RES.R)
table(insurance_t.new$RES.R)
table(insurance_t.new$RES.U)

#Which to collapse?? 

#combining categories template: 
df$category <- as.character(df$category)
df$category[which(df$category > #)] <- "4+"
                    
table(df$category, df$target)