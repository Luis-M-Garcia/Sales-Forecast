# Sales-Forecast
4-26/4-30 update
Got data from Kaggle - https://www.kaggle.com/datasets/sureshmecad/supplement-sales-prediction
Data has a two downloadables, a 'training set', and a 'test set'
I have renamed the test set into a validation set as to not get it confused with the test set I will be using for train/test split for models
#will update with metadata specifics

Forecasting 2 months of sales data from 15 months to see how it compares to machine learning models
End project of the dataset was to predict 2 months from the 17 months we had
I thus have to test out machine learning models to see which ones predicted values within the 17 months the best and use it to predict the 18th/19th month
Thus far I have shown that a simple machine learning model outperforms forecasting, even while being restricted heavily

4-30
Will call it a day for now, data was grouped by month and year, bringing us down to 17 rows for train set, grouped this way so I could use HoltWinters forecast on it
Actual train set has 160,000 rows and I will use all these rows to train my machine learning model when I update this
