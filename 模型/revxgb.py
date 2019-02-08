import pandas as pd
import pdb
import numpy as np
from category_encoders import *
import xgboost as xgb
#from matplotlib import pyplot
#from xgboost import plot_importance

# load data
train = pd.read_csv("train_merge.csv", sep = " ")
test = pd.read_csv("test_merge.csv", sep = " ")
train_labels = train['order']
#train.drop(['order','basket','revenue','click','lineID', 'pid', 'order_qty'], axis=1, inplace=True) 
#test.drop(['lineID', 'pid'], axis=1, inplace=True)

train_60 = train[train.day<=75]
train_30 = train[train.day > 75]
train_60_labels = train_60['order']
train_30_labels = train_30['order']
print(len(train_60[train_60["order"]==1]))
print(len(train_60[train_60["order"]==0]))
train.drop(['order','basket','revenue','click','lineID',  'order_qty','campaignIndex'], axis=1, inplace=True)
test.drop(['lineID', 'campaignIndex'], axis=1, inplace=True)
print(train.shape)
print(test.shape)
enc = TargetEncoder(cols=['group_34', 'group_12', 'content', 'unit', 'group', 'pharmForm']).fit(train,train_labels)
train = enc.transform(train)
test = enc.transform(test)

train.fillna(0)
test.fillna(0)

train_60 = train[train.day<=75]
train_30 = train[train.day > 75]
#train_60_labels = train_60['order']
#train_30_labels = train_30['order']
#test_labels = test['order']

#train.drop(['order','basket','revenue','click','lineID', 'pid', 'order_qty'], axis=1, inplace=True)
#test.drop(['lineID', 'pid'], axis=1, inplace=True)

param = {
    'max_depth':50,
    'eta':0.1,
    'silent':1,
    'objective':'binary:logistic',
    'nthread':-1,
    'n_estimatores':4,
    'scale_pos_weight':2.5
}
n_rounds=100
dtrain = xgb.DMatrix(train_60.values,train_60_labels.values)
dvalid = xgb.DMatrix(train_30.values,train_30_labels.values)

model = xgb.train(param,dtrain,n_rounds)

preds = model.predict(dvalid)
threshold = [0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
result = []
for i in range(len(threshold)):
    for j in range(len(preds)):
        if preds[j] >= threshold[i]:
            result.append(1)
        else:
            result.append(0)
    MAE = np.mean(abs(result - train_30_labels))
    print('MAE_{}'.format(threshold[i]),MAE)
    result = []
#print(preds)
#MAE = np.mean(abs(preds - train_30_labels))
#plot_importance(model)
#print('MAE:', MAE)







