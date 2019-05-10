"""
https://machinelearningmastery.com/time-series-forecasting-long-short-term-memory-network-python/


1.  Load the dataset from file.
2. Transform the dataset to make it suitable for the LSTM model, including:
  a. Transforming the data to a supervised learning problem.
  b. Transforming the data to be stationary.
  c. Transforming the data so that it has the scale -1 to 1.
3. Fitting a stateful LSTM network model to the training data.
4. Evaluating the static LSTM model on the test data.
5. report the performance of the forecasts.
"""

from pandas import DataFrame
from pandas import Series
from pandas import concat
from pandas import read_csv
from pandas import datetime
from sklearn.metrics import mean_absolute_error
from sklearn.metrics import mean_squared_error
from sklearn.preprocessing import MinMaxScaler
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from math import sqrt
from matplotlib import pyplot
import numpy

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# Importing the dataset
dataset = pd.read_csv('data/py_super.csv')

# Filter out one individual store (WaWa)

# boolean variable to tell if customer code is true or false
store_id = dataset['Customer host code']==600379764

# then use this variable to filter the dataframe
wawa5111 = dataset[store_id]
wawa5111 = wawa5111.drop(['Customer', 'Customer host code', 'Dead Net Revenue'], axis = 1)
wawa5111

# convert date column to Pandas date
wawa5111['Date'] = pd.to_datetime(wawa5111['Date'])

# create full two years
full_dates = pd.date_range(start = '1/1/2017', end = '03/17/2019', freq = 'D')
full_dates = pd.DataFrame(full_dates)
full_dates.rename(columns={0: 'Date'}, inplace=True)

# merge them together and fill NaNs
full_calendar = pd.merge(full_dates, wawa5111, how = 'left')
full_calendar['Volume'].fillna(0, inplace=True)

# 
real_dates = full_calendar['Volume']#.astype('int')

spread_dates_str = ','.join(real_dates.groupby(real_dates.cumsum()).transform('mean').astype(str).tolist())

spread_dates = pd.Series(spread_dates_str.split(',')).astype(float)

full_calendar['Spread'] = spread_dates
test = full_calendar['Date'] = pd.to_datetime(full_calendar['Date'])
full_calendar['Date'] = full_calendar['Date'].dt.date
 
# now model

# date-time parsing function for loading the dataset
def parser(x):
	return datetime.strptime('201'+x, '%Y-%m')

# frame a sequence as a supervised learning problem
def timeseries_to_supervised(data, lag=1):
	df = DataFrame(data)
	columns = [df.shift(i) for i in range(1, lag+1)]
	columns.append(df)
	df = concat(columns, axis=1)
	df.fillna(0, inplace=True)
	return df

# create a differenced series
def difference(dataset, interval=1):
	diff = list()
	for i in range(interval, len(dataset)):
		value = dataset[i] - dataset[i - interval]
		diff.append(value)
	return Series(diff)

# invert differenced value
def inverse_difference(history, yhat, interval=1):
 #  return yhat + history[-interval]
  return yhat + history[interval]

# scale train and test data to [-1, 1]
def scale(train, test):
	# fit scaler
	scaler = MinMaxScaler(feature_range=(-1, 1))
	scaler = scaler.fit(train)
	# transform train
	train = train.reshape(train.shape[0], train.shape[1])
	train_scaled = scaler.transform(train)
	# transform test
	test = test.reshape(test.shape[0], test.shape[1])
	test_scaled = scaler.transform(test)
	return scaler, train_scaled, test_scaled

# inverse scaling for a forecasted value
def invert_scale(scaler, X, value):
	new_row = [x for x in X] + [value]
	array = numpy.array(new_row)
	array = array.reshape(1, len(array))
	inverted = scaler.inverse_transform(array)
	return inverted[0, -1]

# fit an LSTM network to training data
def fit_lstm(train, batch_size, nb_epoch, neurons):
	X, y = train[:, 0:-1], train[:, -1]
	X = X.reshape(X.shape[0], 1, X.shape[1])
	model = Sequential()
	model.add(LSTM(neurons, batch_input_shape=(batch_size, X.shape[1], X.shape[2]), stateful=True))
	model.add(Dense(1))
	model.compile(loss='mean_squared_error', optimizer='adam')
	for i in range(nb_epoch):
		model.fit(X, y, epochs=1, batch_size=batch_size, verbose=0, shuffle=False)
		model.reset_states()
	return model

# make a one-step forecast
def forecast_lstm(model, batch_size, X):
	X = X.reshape(1, 1, len(X))
	yhat = model.predict(X, batch_size=batch_size)
	return yhat[0,0]

# dataset rename to match example
series = full_calendar.drop(['Volume'], axis = 1)

# transform data to be stationary
raw_values = series.Spread
diff_values = difference(raw_values, 1)

# transform data to be supervised learning
supervised = timeseries_to_supervised(diff_values, 1)
supervised_values = supervised.values

# split data into train and test-sets
train, test = supervised_values[0:730], supervised_values[730:]

# transform the scale of the data
scaler, train_scaled, test_scaled = scale(train, test)

# fit the model

"""
Tune parameters further once working

 Batch Size: 1
 Epochs: 100
 Neurons: 1

"""

lstm_model = fit_lstm(train_scaled, 1, 100, 1)
# forecast the entire training dataset to build up state for forecasting
train_reshaped = train_scaled[:, 0].reshape(len(train_scaled), 1, 1)
lstm_model.predict(train_reshaped, batch_size=1)

# walk-forward validation on the test data
predictions = list()
for i in range(len(test_scaled)):
	# make one-step forecast
	X, y = test_scaled[i, 0:-1], test_scaled[i, -1]
	yhat = forecast_lstm(lstm_model, 1, X)
	# invert scaling
	yhat = invert_scale(scaler, X, yhat)
	# invert differencing
	yhat = inverse_difference(raw_values, yhat, len(test_scaled)+1-i)
	# store forecast
	predictions.append(yhat)
	expected = raw_values[len(train) + i + 1]
	print('Day=%d, Predicted=%f, Expected=%f' % (i+1, yhat, expected))
    

# report performance
mae = mean_absolute_error(raw_values[731:], predictions)
rmse = sqrt(mean_squared_error(raw_values[731:], predictions))
print('Test RMSE: %.3f' % rmse)
# line plot of observed vs predicted
pyplot.plot(raw_values[731:])
pyplot.plot(predictions)

"""
Running the example prints the expected and predicted values for each of the 12 months in the test dataset.

The example also prints the RMSE of all forecasts. The model shows an RMSE of 71.721 monthly shampoo sales, 
which is better than the persistence model that achieved an RMSE of 136.761 shampoo sales.

Random numbers are used in seeding the LSTM, and as a result, you may have a different result from a 
single run of the model. We cover this further in the next section.
"""
