#KT Case Study
pacman::p_load(googlesheets4, tidyverse, dplyr, ggplot2, caret, olsrr, caTools) 

#datalink-----
klink1 ='https://docs.google.com/spreadsheets/d/13tIz8ojO2kfJkj8BiPVvlenYD1FH6KLEXHNI4OJFSng/edit#gid=18263847'

#sheets_browse(klink1)
kdata <- read_sheet(klink1, sheet='goodbelly')

head(kdata)
kdata$Date = as.Date(kdata$Date)
str(kdata)
kdata$weekNo = week(kdata$Date)
names(kdata)
factorCols = c('Region', 'SalesRep','Endcap','Demo', 'Demo13','Demo45') 
kdata[, factorCols ] = lapply(kdata[, factorCols ], as.factor)
str(kdata)
modelData <- kdata %>% select(-c('Date', 'Store'))
names(modelData)
summary(modelData)

#build the model-----
model1A = lm(AvgRetailPrice ~ ., data= modelData)
summary(model1A)


#remove SalesRep, EndCap, Natural
names(modelData)
model1B = lm(AvgRetailPrice ~ . - SalesRep - Endcap - Natural, data= modelData)
summary(model1B)




#now do with sampling ----
set.seed(123)
split = caTools::sample.split(modelData$AvgRetailPrice, SplitRatio=0.75)
training_set = subset(modelData, split == TRUE)
test_set = subset(modelData, split == FALSE)

#build the 2nd model-----
model2A = lm(AvgRetailPrice ~ ., data= training_set)
summary(model2A)
#remove cols - SalesRep, Demo45, Natural
model2B = lm(AvgRetailPrice ~ . - SalesRep - Endcap - Demo45 - Natural, data= training_set)
summary(model2B)
summary(model2B)$r.squared 
help(summary.lm)
summary(model2B)$fstatistic 
summary(model2B)$coefficients
summary(model2B)$residuals
summary(model2B)$adj.r.squared

#assumptions----
#The error has a normal distribution (normality assumption).
#The errors have mean zero.
#The errors have same but unknown variance (homoscedasticity assumption).
#The error are independent of each other (independent errors assumption).
#https://cran.r-project.org/web/packages/olsrr/vignettes/residual_diagnostics.html
plot(model2B,1)
plot(model2B,2)
plot(model2B,3)
plot(model2B,4)
#there are some outliers which could be removed 
olsrr::ols_plot_resid_qq(model2B)
olsrr::ols_test_normality(model2B)
#Correlation between observed residuals and expected residuals under normality
olsrr::ols_test_correlation(model2B)
olsrr::ols_plot_resid_fit(model2B)
#Histogram of residuals for detecting violation of normality assumption.
olsrr::ols_plot_resid_hist(model2B)

#predict ----
testValue = predict(model2B, newdata= test_set, type='response')

#accuracy----
library(caret)
#MAE(predicted, original) #MSE(predicted, original) #RMSE(predicted, original) #R2(predicted, original, form = "traditional")
MAE(testValue, test_set$AvgRetailPrice)
RMSE(testValue, test_set$AvgRetailPrice)
R2(testValue, test_set$AvgRetailPrice, form='traditional')
?MAE



#end----
#library(olsrr)
modelOLSR <- lm(AvgRetailPrice ~ ., data= modelData)
k <- ols_step_all_possible(model2)
plot(k)
k
