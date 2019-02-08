
# Customer-Purchase-Behavior-Predition
[2017 Data Mining Cup Challenge](https://www.data-mining-cup.com/reviews/dmc-2017/) - Revenue Forecast as a foundation for dynamic pricing.

### Background
Real-time automated price generation moves into focus for online retailers. Every day, category management faces the challenge of adapting thousands of products to changing market factors. This happens completely automatically when using an intelligent pricing tool. Prices reflect customer appreciation and lead to increased sales.

Our goal of is to come up with a sales forecast for a mail-order pharmacy that dynamically adapts its prices.

### Data Description
The raw data include three files
* train.csv (the data of users' behavior and the status of drug, with more than 2.7 million pieces of data)
* items.csv (the data of specific drag, with 22035 pieces of data, of which some fields have missing value)
* class.csv (mostly the same as train.csv except for the data of users' behavior, with nearly 1.2 million pieces of data)

### Feature Engineering
* Unify variables (use likelyhood encoding instead of one-hot encoding to change norminal variable into numerical variable)
* Fill missing values (Naive Bayes is used as the main idea for missing value filling)
* Create new variables (we create altogether 47 new variables containing 6 different aspects)

### Predoction Models
We use the most frequently used two models LightGBM and XGboost as the prediction model.
We build a two layers' model. The first layer uses XGBoost to predict whether a customer will buy the proucut or not. 
The second layer uses LightGBM to predict the amount of product being bought.
<img src="https://github.com/wanfb/Customer-Purchase-Behavior-Predition/blob/master/model.JPG" width = "600" height = "300" align=center/>

### Prediction Result
we can acheive the accuracy of 76.4%. The feature importance of our model can be seen as follows.
<img src="https://github.com/wanfb/Customer-Purchase-Behavior-Predition/blob/master/feature-importance.JPG" width = "600" height = "400" align=center/>

