from collections import defaultdict
from visualizations import X, y
import matplotlib.pyplot as plt
import seaborn as sns 
import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import SVC
from sklearn.neighbors import KNeighborsClassifier
from sklearn.model_selection import cross_val_score, RepeatedStratifiedKFold, train_test_split
from sklearn.model_selection import RandomizedSearchCV,  GridSearchCV
from sklearn.metrics import balanced_accuracy_score
from sklearn.preprocessing import StandardScaler
from xgboost import XGBClassifier, XGBRegressor
# from mlxtend.regressor import StackingCVRegressor
from sklearn.ensemble import StackingClassifier
from sklearn.linear_model import LogisticRegressionCV
from sklearn.discriminant_analysis import QuadraticDiscriminantAnalysis as QDA
import torch
from skorch import NeuralNetClassifier
import torch.optim as optim

 
if torch.cuda.is_available():
    print("CUDA is available, things will be fast")
device = "cuda" if torch.cuda.is_available() else "cpu"


# grid search before stacking
X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size = .33, random_state = 42, stratify=y
)

sc = StandardScaler()
X_train_std = sc.fit_transform(X_train)
X_test_std = sc.transform(X_test)


class Net(torch.nn.Module):
    def __init__(self, num_units = 10, dropout = 0.5):
        super(Net, self).__init__()
        self.fc = torch.nn.Sequential(torch.nn.Linear(20, num_units),
            torch.nn.ReLU(inplace=True),
            torch.nn.Dropout(dropout),
            torch.nn.Linear(num_units, 10),
            torch.nn.Linear(10, 2),
            torch.nn.Softmax(dim=-1))
    def forward(self, x):
        x = x.to(torch.float32)
        return self.fc(x)


nn = NeuralNetClassifier(Net, batch_size = 32, train_split = False, optimizer__lr = 0.001, max_epochs = 1,
                         optimizer__momentum = 0.9, device = device,
                         optimizer = optim.SGD)

def pipeline(X_train_std, X_test_std, y_train, y_test, model, param_grid, gridsearch = False, cv = 5, 
             scoring_fit = 'balanced_accuracy', scoring_test = balanced_accuracy_score,
             do_probabilities = False, n_iter = 10):
    if gridsearch:
        gs = GridSearchCV(estimator = model, param_grid = param_grid, cv = cv, scoring = scoring_fit,
                          n_jobs = -1, verbose = 2)
    else:
        gs = RandomizedSearchCV(estimator = model, param_distributions= param_grid, cv = cv, scoring = scoring_fit, 
                      n_jobs = -1, verbose = 2, n_iter = n_iter) # verbose = 2 also shows score
    fitted_model = gs.fit(X_train_std, y_train)
    best_model = fitted_model.best_estimator_
    if do_probabilities:
        pred = fitted_model.predict_proba(X_test_std)
    else:
        pred = fitted_model.predict(X_test_std)
    score = scoring_test(y_test, pred)
    return [best_model, pred, score]

# Models for which hyperparameters should be tuned
models_to_train = [nn, XGBClassifier(n_jobs = -1, tree_method = "hist"), RandomForestClassifier(n_jobs = -1)]

# Defining the hyperparameters to optimize
grid_parameters = [
    {# fully connected NN
    'max_epochs':[10, 20],
	'optimizer__lr': [0.1, 0.01],
	'module__num_units': [50, 100],
	'module__dropout': [0.1, 0.5],
    'optimizer__momentum': [0.9]
     },
    { # XGBoost
        'n_estimators': [400, 700, 1000],
        'colsample_bytree': [0.7, 0.8],
        'max_depth': [10, 15,20,25],
        'reg_alpha': [1.1, 1.2],
        'reg_lambda': [1.1, 1.2],
        'subsample': [0.7, 0.8]
    },
    { # Random Forest
        'max_depth':[3, 5, 10, 13],
        'n_estimators':[100, 200, 400, 600, 900],
        'max_features':[2, 4, 6, 8, 10]
    }
]



models_pred_scores = []
for i, model in enumerate(models_to_train):
    params = grid_parameters[i]
    res = pipeline(X_train_std, X_test_std, y_train, y_test, model, params, n_iter = 10, gridsearch=False)
    models_pred_scores.append(res)
    
    
    
base_models = [('random_forest', models_pred_scores[2][0]),
               ('xgboost', models_pred_scores[1][0]),
               ('svm', SVC()), 
               ('fully_connected_NN', models_pred_scores[0][0]),
               ('knn', KNeighborsClassifier(n_neighbors=15))]

meta_model = LogisticRegressionCV()
stacking_model = StackingClassifier(estimators=base_models, 
                                    final_estimator=meta_model, 
                                    passthrough=True, 
                                    cv=5,
                                    verbose=2, 
                                    n_jobs=-1)



# dictionary for models in stack
models_dict = {'random_forest': models_pred_scores[2][0],
               'xgboost': models_pred_scores[1][0],
               'svm': SVC(), 
               'fully_connected_NN': models_pred_scores[0][0],
               'knn': KNeighborsClassifier(n_neighbors=15, n_jobs = -1),
               'stacked': stacking_model
        }


def evaluate(model, X_train_std, y_train):
    cv = RepeatedStratifiedKFold(n_splits=5, n_repeats=2, random_state = 69)
    scores = cross_val_score(model, X_train_std, y_train, scoring = 'balanced_accuracy', cv = cv, 
                             verbose = True, n_jobs = -1, error_score='raise')
    return scores
    
model_scores = defaultdict()


for name, model in models_dict.items():
    print(f'Evaluating {name}')
    model_scores[name] = evaluate(model, X_train_std, y_train)



scores = pd.DataFrame(model_scores)
scores["ID"] = scores.index

scores_long = scores.melt(id_vars = "ID", var_name = "model", value_name = "score")

scores.to_csv("scores.csv")
scores_long.to_csv("scores_long.csv")


