# Sales-Forecast

Got data from Kaggle - https://www.kaggle.com/datasets/sureshmecad/supplement-sales-prediction  
Store_id -	    Unique id for each Store  
Store_Type -	  Type of the Store  
Location_Type -	Type of the location where Store is located  
Region_Code -	  Code of the Region where Store is located  
Date -	        Information about the Date  
Holiday -	      If there is holiday on the given Date, 1 : Yes, 0 : No  
Discount -	    If discount is offered by store on the given Date, Yes/ No  
#Orders -     	Number of Orders received by the Store on the given Day  
Sales	Total Sale for the Store on the given Day  

Data has a two downloadables, a training set, and a test set
Will load in test set but not really use it

Forecasting 2 months of sales data from 15 months to see how it compares to linear regression  
Going to group data by month and forecast like that, not store by store/day by day  
End project of the dataset was to predict 2 months from the 17 months we had , not going to do this, will simply use data to forecast  
#### will probably come back in near future and actually group by store then perform linear regression on each for each day until march and see how it compares
