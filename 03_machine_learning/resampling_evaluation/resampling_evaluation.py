from solution import Model, cost_function, extract_city_area_information
import numpy as np
from icecream import ic
from sklearn.model_selection import train_test_split
from memory_profiler import profile
from sklearn.model_selection import KFold
from tqdm import tqdm
from collections import namedtuple
import time
from sdv.single_table import CTGANSynthesizer, GaussianCopulaSynthesizer
from sdv.metadata import SingleTableMetadata
import pandas as pd
import os
from typing import Tuple

SEED = 42
N_FOLDS = 10
CTGAN_PATH = './ctgan_synthesizer.pkl'
GCS_PATH = './gcs_synthesizer.pkl'
DATA_PATH = './synthetical_data.csv'
UNDERSAMPLING = True


cv = KFold(n_splits = N_FOLDS)

@profile
def cv_eval(X: np.ndarray, y:np.ndarray, name:str) -> namedtuple:
    loss_tup = namedtuple('loss_evaluation', ['mean', 'all_folds'])
    loss_lst = []
    for train_idx, val_idx in tqdm(cv.split(X), total = N_FOLDS, desc = f"{name} CV"):
        X_train, X_val = X[train_idx, :2], X[val_idx, :2]
        y_train, y_val = y[train_idx], y[val_idx]
        
        AREA_val = X[val_idx, 2].astype(bool)
        
        
        gpr = Model()
        gpr.fitting_model(y_train, X_train)
        y_pred, _, _, = gpr.make_predictions(X_val, AREA_val)
        
        # calculate cost metric
        loss_lst.append(cost_function(y_val, y_pred, AREA_val))
    return loss_tup(np.mean(loss_lst), loss_lst)

@profile
def ctgansampler(X: np.ndarray, y:np.ndarray, 
                 nrows:int, n_epochs:int) -> Tuple[np.ndarray, np.ndarray]:
    if os.path.exists(DATA_PATH):
        artificial_dat = np.loadtxt(DATA_PATH, delimiter=',', skiprows=1)
        return artificial_dat[:, :3], artificial_dat[:, 3]
        
    else:
        if os.path.exists(CTGAN_PATH):
            ctgan = CTGANSynthesizer.load(
                filepath = CTGAN_PATH
            )
        else:
            train = pd.DataFrame(np.c_[X, y], 
                         columns = ['lon', 'lat', 'AREA', 'y'])
            ic('pandas')
            metadata = SingleTableMetadata()
            metadata.detect_from_dataframe(data=train)
            meta_dict = metadata.to_dict()
            ic(meta_dict)
            ctgan = CTGANSynthesizer(
                metadata=metadata,
                enforce_rounding=False, 
                epochs = n_epochs, 
                verbose = True, 
                cuda = True
            )
            
            ctgan.fit(train)
            ctgan.save(filepath=CTGAN_PATH)
        
        artificial_dat = ctgan.sample(num_rows = nrows)
        ic(artificial_dat.shape)
        ic(type(artificial_dat))
        
        artificial_dat.to_csv(DATA_PATH, index = False)
        
        return artificial_dat.to_numpy()[:, :3], artificial_dat.to_numpy()[:, 3]
        

@profile
def copulasampler(X: np.ndarray, y:np.ndarray, 
                 nrows:int) -> Tuple[np.ndarray, np.ndarray]:
    if os.path.exists(DATA_PATH):
        artificial_dat = np.loadtxt(DATA_PATH, delimiter=',', skiprows=1)
        return artificial_dat[:, :3], artificial_dat[:, 3]
        
    else:
        if os.path.exists(GCS_PATH):
            gcs = CTGANSynthesizer.load(
                filepath = GCS_PATH
            )
        else:
            train = pd.DataFrame(np.c_[X, y], 
                         columns = ['lon', 'lat', 'AREA', 'y'])
            ic('pandas')
            metadata = SingleTableMetadata()
            metadata.detect_from_dataframe(data=train)
            meta_dict = metadata.to_dict()
            ic(meta_dict)
            gcs = GaussianCopulaSynthesizer(
                metadata=metadata,
                enforce_rounding=False
            )
            
            gcs.fit(train)
            gcs.save(filepath=GCS_PATH)
        
        artificial_dat = gcs.sample(num_rows = nrows)
        ic(artificial_dat.shape)
        ic(type(artificial_dat))
        
        artificial_dat.to_csv(DATA_PATH, index = False)
        
        return artificial_dat.to_numpy()[:, :3], artificial_dat.to_numpy()[:, 3]
        
    
    
    


def main():
    start = time.time()
    train_x = np.loadtxt('train_x.csv', delimiter=',', skiprows=1)
    train_y = np.loadtxt('train_y.csv', delimiter=',', skiprows=1)
    test_x = np.loadtxt('test_x.csv', delimiter=',', skiprows=1)
    ic(f'{train_x.shape}, {test_x.shape}')

    x_scrambled = train_x.copy()
    np.random.seed(SEED)
    x_scrambled[:, 2] = np.random.permutation(x_scrambled[:, 2])
    
    x_synth, y_synth = ctgansampler(train_x, train_y, train_x.shape[0], 10000)
    #x_synth, y_synth = copulasampler(train_x, train_y, train_x.shape[0])

    loss_dct = {}
    
    loss_dct['train'] = cv_eval(train_x, train_y, "Training")
    loss_dct['permuted_train'] = cv_eval(x_scrambled, train_y, "Permuted Training")
    loss_dct['synthetic_train'] = cv_eval(x_synth, y_synth, "Synthetic Training")
    
    end = time.time()
    
    ic(f'loss evaluation: {loss_dct}')
    ic(f'Execution time: {end - start:.2f} seconds')
    
    with open('resampled_scores_jan.txt', 'a') as f:
        if UNDERSAMPLING:
            f.write(f"""loss evaluation for scale = {Model().scale} 
                    with undersampling: {loss_dct}\n""")
        else:
            f.write(f"""loss evaluation for scale = {Model().scale} 
                    using all data: {loss_dct}\n""")

if __name__ == "__main__":
    main()