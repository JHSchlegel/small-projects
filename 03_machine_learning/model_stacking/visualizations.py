
from sklearn.datasets import make_classification
import pandas as pd


X, y = make_classification(n_samples=50000, 
                           n_features=20, 
                           n_informative=15, 
                           n_redundant=5,
                           n_clusters_per_class=5,
                           class_sep=0.7,
                           flip_y=0.03,
                           n_classes=2)


if __name__ == "__main__":
    scores = pd.read_csv("scores.csv")
    scores_long = pd.read_csv("scores_long.csv")
    import matplotlib.pyplot as plt
    import seaborn as sns 
    import numpy as np
    import pandas as pd
    print(type(X))
    data = pd.DataFrame.from_records(X)
    data["y"] = y
    sns.pairplot(data.iloc[:1000, list(range(10))+ [-1]], kind = "scatter", diag_kind = "kde", hue = "y", palette = "muted",
                plot_kws = {"alpha": .7})
    plt.savefig("pairsplot.png")
    plt.show()
    
    print(scores_long)
    sns.boxplot(data = scores_long, x = "model", y = "score")
    sns.set(rc={'figure.figsize': (10, 8)})
    sns.set_style("whitegrid")
    sns.despine()
    plt.savefig("score_boxplot.png")
    plt.show()
