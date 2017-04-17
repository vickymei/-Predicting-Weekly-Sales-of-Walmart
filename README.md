# Predicting-Weekly-Sales-of-Walmart
The Walmart data contains weekly sales for all stores and departments ranging from February 2010 to February 2011. Also since holidays can significantly affect sales, selected holiday markdown events are included in this dataset. Through proper dimension reduction and decomposition of time series, the goal of this project is to predict the weekly sales for each store and department from March 2011 to October 2012. Statistical analysis include median of historical data, stlf/SVD with ets, and stlf/SVD with ARIMA have been used for modeling. These models are evaluated based on the weighted-mean-absolute-error (WMAE) and running time.


# Conclusion
Forecast using historical median and STL with ARIMA show better results than STL with ETS. And results after applying shift does not show significant improvement: with or without applying the shift, results have nearly the same error for both ARIMA and ETS.
Moreover, simple model using median of historical data shows best performance, which is inconsistent with our initial thought. Thus, simple model using median or SVD/STL+ARIMA is recommended for this project. In conclusion, all models have WMAE ranging from 3300 to 3800.

