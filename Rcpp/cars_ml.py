import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns 
from sklearn.model_selection import train_test_split
# load mtcars dataset using pandas
cars = pd.read_csv('mtcars.csv')

# one-hot encode categorical variables of cars
cars = pd.get_dummies(cars, columns=['cyl', 'vs', 'am', 'gear', 'carb'])

# split dataset intoX_train, X_test, y_train, y_test
X = cars.drop(['mpg'], axis=1)
y = cars['mpg']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=123)

# fit random forest regression model
from sklearn.ensemble import RandomForestRegressor
model = RandomForestRegressor(n_estimators=100, max_depth=10, random_state=123)

model.fit(X_train, y_train)
print(model.score(X_test, y_test))





