import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
import logging
from sklearn.datasets import make_regression
from sklearn import ensemble
from sklearn.inspection import permutation_importance
#from sklearn.metrics import mean_squared_error
from sklearn.model_selection import train_test_split, GridSearchCV


logging.basicConfig(filename = "boosting_reg.log", level=logging.INFO, 
                    filemode='w')

X, y = make_regression(n_samples = 500, n_features = 20, noise = 50, 
                       random_state= 42)
logging.info(f"X shape:{X.shape}, y shape: {y.shape}")


# Split data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2)

boost_reg = ensemble.GradientBoostingRegressor()

parameters = {
    'n_estimators': [400, 500],
    'max_depth': [4, 10],
    'min_samples_split': [5, 6],
    'learning_rate': [0.01, 0.001]
}

# Create a grid search object
cv = GridSearchCV(boost_reg, param_grid = parameters, 
                  scoring='neg_mean_squared_error', cv = 5, verbose = 1,
                  n_jobs = 20)

cv.fit(X_train, y_train)

model =  cv.best_estimator_

# log the scores of the model
logging.info(f"Train score: {model.score(X_train, y_train)}")
logging.info(f"Test score: {model.score(X_test, y_test)}")

feat_imp = permutation_importance(
    model, X_test, y_test, n_repeats=10, random_state=42, n_jobs=20
)

plt.boxplot(feat_imp.importances.T, vert=False)
plt.title("Permutation Importances (test set)")
plt.savefig("permutation_importance.png")