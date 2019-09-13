#reading in data:
library(haven)
library(dplyr)
library(dummies)
library(tidyverse)
library(forcats)
library(Hmisc)
library(glmulti)

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

#creating a logit model with new data to check for separation 
logit.model.w <- glm(INS ~., data=insurance_t, family = binomial(link = "logit") )
summary(logit.model.w)

#best subsets
glmulti.lm.out <- glmulti(INS ~.,
                        data=insurance_t,
                        level=1,
                        method='g',
                        crit="bic",
                        confsetsize = 5,
                        plotty=TRUE,
                        fitfunction = "glm",
                        family = binomial)
#gives 5 best models
glmulti.lm.out@formulas

#show result for best model
summary(glmulti.lm.out@objects[[1]])

#creating all the different models

full.model <- glm(INS ~., data=insurance_t,
                  family=binomial(link = "logit"))

empty.model <- glm(INS ~ 1, data=insurance_t,
                   family= binomial(link = "logit"))


#backwards model 
back.model <- step(full.model, direction = "backward")

#stepwise model
step.model <- step(empty.model, scope=list(lower=formula(empty.model),
                                           upper=formula(full.model)),
                   direction = "both")








































