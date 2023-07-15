import pandas as pd
import numpy as np
import seaborn as sns 
import matplotlib.pyplot as plt




fide = pd.read_csv("fide_ratings.csv")

fide = fide.drop(fide.columns[0], axis = 1)

fide["classic_rank"] = fide["classic_rating"].rank(ascending=False)
fide["blitz_rank"] = fide["blitz_rating"].rank(ascending=False)
fide["rapid_rank"] = fide["rapid_rating"].rank(ascending=False)
fide["rank_sum"] = fide["blitz_rank"] + fide["classic_rank"] + fide["rapid_rank"]

sns.scatterplot(x = fide["birth_year"], y = fide["rank_sum"])
plt.show()

fide_wide = fide[["player", "rapid_rating", "classic_rating", "blitz_rating"]]
fide_long = fide_wide.melt(id_vars =  ["player"], var_name = "category")

sns.boxplot(data = fide_long, x = "category", y = "value")
plt.show()








