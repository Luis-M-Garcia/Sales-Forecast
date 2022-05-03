#loading the libraries that I need
library(tidyverse)
library(readxl)
library(caret)
library(psych)
library(lubridate)
library(tseries)
library(timeDate)
library(forecast)
library(astsa)
library(seastests)
library(mvnormtest)


#importing test and train set
#data was already split but target label is not in test_set,got from  following link
#https://www.kaggle.com/datasets/sureshmecad/supplement-sales-prediction
#will make smaller dataset into validation set, it's 10% of total data
test_set <- read_csv('sales_test.csv')
sales_data <- read_csv('sales_train.csv')


#validation_set does not have order# or sales
names(test_set)
names(sales_data)
set.seed(1)
index <- createDataPartition(sales_data$Sales,p=.85,list=F)
train_set <- sales_data[index,]
validation_set <- sales_data[-index,]

#doing exploratory data analysis to see what the train_set looks like
names(train_set)
summary(train_set)

#Holiday column is an numeric, will make factor
#Discount is character, will make factor
#Store_id is numeric, will make factor
#there are 365 unique store IDs
#4 store, location, and regoin types
class(train_set$Holiday)
unique(train_set$Holiday)
train_set$Holiday <- as.factor(train_set$Holiday)  
class(train_set$Holiday)
table(train_set$Holiday)

class(train_set$Discount)
unique(train_set$Discount)
train_set$Discount <- as.factor(ifelse(train_set$Discount=='Yes',1,0))
class(train_set$Discount)
table(train_set$Discount)

train_set$Store_id <- as.factor(train_set$Store_id)
si<-train_set %>% group_by(Store_id) %>% summarize(store_nums =n())
boxplot(si$store_nums)$out
which(si$store_nums==462)
hist(si$store_nums)

train_set$Store_Type <- as.factor(train_set$Store_Type)
train_set$Location_Type <- as.factor(train_set$Location_Type)
train_set$Region_Code <- as.factor(train_set$Region_Code)
histogram((train_set$Region_Code))
histogram(train_set$Location_Type)
histogram(train_set$Store_Type)

boxplot(train_set$`#Order`)$out
boxplot(train_set$Sales)$out

#x_type variables are skewed heavily in counts
#stores have about equal amount of entries
#most sales not in holiday
#no discount most of the time, but yes/no ratio is similar
#order#s and sales have lots of variance

summary(train_set)

#will group stores by date and see how sales look
#trying to find if sales follow cyclic,trend, or seasonal pattern
##decided that time-serie is seasonal
##both years take a dip in sales after Jan, stay low Feb-April, then start going back up come May
##2019 has no data past May, for 2018, sales stay high May-July then stay down Aug-Nov and backup for Dec
###going to add 12 to 2019 months to add it to the 2018 data then switch names back to jan-dec

unique(train_set$Date)
by_day <- train_set %>% group_by(month(Date),year(Date)) %>% summarize(mean_sales = mean(Sales))
by_month <- by_day %>% arrange(`year(Date)`,`month(Date)`)
by_month

by_month %>% filter(`year(Date)`==2018) %>% 
  ggplot(aes(x = `month(Date)`, y = mean_sales)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(1,12,by=1),labels = substr(month.name,0,3)) +
  xlab('Months(2018)') + ylab('Mean Sales (2018)') + ggtitle('Mean Sales per Month in 2018') +
  theme(plot.title = element_text(hjust = 0.5))


by_month %>% filter(`year(Date)`==2019) %>% 
  ggplot(aes(x = `month(Date)`, y = mean_sales)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(1,12,by=1),labels = substr(month.name,0,3)) +
  xlab('Months(2019') + ylab('Mean Sales (2019)') + ggtitle('Mean Sales per Month in 2019') +
  theme(plot.title = element_text(hjust = 0.5))

jan_may <- month.name
append(jan_may,month.name)[1:15]
by_month %>% 
  mutate(all_months = ifelse(`year(Date)` == 2019,`month(Date)` + 12, `month(Date)`))  %>%
  ggplot(aes(x = all_months, y = mean_sales)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(1,17,by=1),labels = substr(append(jan_may,month.name)[1:17],0,3)) +
  xlab('Months') + ylab('Mean Sales') + ggtitle('Mean Sales per Month Jan 2018-May 2019') +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90))

#I will now do basic forecasting, not machine learning predictions just to see how good they are
#start by separating sales by years
#I will forecast the train data sales
#This is to show that machine learning predictions are more accurate than forecasting

mean_sales_2018 <- by_month %>% filter(`year(Date)`==2018)
mean_sales_2019 <- by_month %>% filter(`year(Date)`==2019)
#isSeasonal(sales.ts,test = 'welch',freq = 12)
#kpss.test(sales.ts, null = 'Trend')



sales.ts <- ts(by_month$mean_sales,frequency = 12, start = c(2018,1))
adf.test(sales.ts) #testing if time-series is stationary;
#was obvious it wasn't just showing what to do if uncertain
first_15_entries <- ts(by_month$mean_sales[1:15],frequency = 12, start = c(2018,1))
april_may_predictions <- hw(first_15_entries,h = 2, beta = F,initial = 'simple')
april_may_predictions$model


#sqrt(mean((sales.ts[16:17]-holt(first_15_entries,h = 2)$mean[1:2])^2))
#doing seasonal, above comment, yielded terrible results
#cannot use base HoltWinters since I do not have 2 years of data

#looking at time-series, I said it is additive, will check if assuming multiplicative better
## it was not better
mult_hw <- hw(first_15_entries,h = 2, beta = F, initial = 'simple',seasonal = 'multiplicative')
rmse_mult_hw <- sqrt(mean((sales.ts[16:17]-mult_hw$mean[1:2])^2)) 
rmse_mult_hw
rmse_hw <- sqrt(mean((sales.ts[16:17]-april_may_predictions$mean[1:2])^2))
rmse_hw
#RMSE was 4297, had to get 15 months from time series and predicts 2 months

#Now I will begin using machine learning algorithms to predict outcomes
#I will see if a linear regression outperforms Holt-Winters
#will try barebones machine learning with grouped data to see how it compares to forecast
##even with the data being super messy and only relying on the 15 points I used to forecast 16 and 17
##a simple linear regression model outperformed forecasting methods, RMSE 3788 compared to 4297
dummy_model <- by_month[1:15,]
dummy_pred <- by_month[16:17,][1:2]   #not including sales column in predicted set to show 
#that it can be used to forecast the sales
#it only needs columns that were used to try and predict sales

dummy_pred$`year(Date)` <- as.factor(dummy_pred$`year(Date)`)
dummy_pred$`month(Date)` <- as.factor(dummy_pred$`month(Date)`)
dummy_model$`month(Date)` <- as.factor(dummy_model$`month(Date)`)
dummy_model$`year(Date)` <- as.factor(dummy_model$`year(Date)`)

set.seed(1)
train_lm <- train(mean_sales~., data = dummy_model,method = 'lm')
pred_lm <- predict(train_lm,dummy_pred)
class(dummy_pred$`month(Date)`)
sqrt(mean((by_month$mean_sales[16:17]-pred_lm)^2))
mean(by_month$mean_sales[16:17])
sd(by_month$mean_sales[16:17])
#decided to not test out validation set, just leaving this forecast as is