import requests
from bs4 import BeautifulSoup as bs
from selenium import webdriver
# from selenium.webdriver.chrome.service import Service
# service only available for later versions
from selenium.webdriver.chrome.options import Options
from webdriver_manager.chrome import ChromeDriverManager
import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd
import time
import numpy as np
import regex as re


url = 'https://ratings.fide.com/'


options = Options()
options.add_experimental_option("detach", True) # don't close driver after code is finished

# Initialize the Chrome driver
driver = webdriver.Chrome(ChromeDriverManager().install(),
                          options = options)

driver.set_window_position(2000, 0)


# Open the FIDE website
driver.get(url)
driver.maximize_window()


# Example: Scrape player names and ratings from the FIDE Top 100 list
player_names = []
player_classical_ratings = []
player_rapid_ratings = []
player_blitz_ratings = []
player_nationality = []
player_sex = []
player_byears = []

# Find the table rows containing player information
rows = driver.find_elements_by_xpath("//table/tbody/tr")

# Extract player names and ratings from each row
for row in rows:  # Exclude header row
    cols = row.find_elements_by_tag_name("td")
    player_names.append(cols[1].text)
    player_nationality.append(cols[2].text)
    player_classical_ratings.append(int(cols[3].text))




links = [item.get_attribute("href") for item in driver.find_elements_by_xpath("//table/tbody/tr/td/a[@href]")]

for link in links:
    driver.get(link)
    blitz = driver.find_elements_by_xpath("//div[@class='profile-top-rating-data profile-top-rating-data_blue']")
    player_blitz_ratings.append(blitz[0].text)
    rapid = driver.find_elements_by_xpath("//div[@class='profile-top-rating-data profile-top-rating-data_red']")
    player_rapid_ratings.append(blitz[0].text)
    byear = driver.find_elements_by_xpath("(//div[@class = 'profile-top-info__block__row__data'])[4]")
    player_byears.append(byear[0].text)
    sex = driver.find_elements_by_xpath("(//div[@class = 'profile-top-info__block__row__data'])[5]")
    player_sex.append(sex[0].text)
    time.sleep(np.random.uniform(low = 2.0, high = 10.0))
    

player_blitz_ratings = [int(re.findall(r'\d+', s)[0]) for s in player_blitz_ratings]
player_rapid_ratings = [int(re.findall(r'\d+', s)[0]) for s in player_rapid_ratings]
player_byears = [int(s) for s in player_byears]
player_classical_ratings = [int(s) for s in player_classical_ratings]




fide_ratings = pd.DataFrame(data = {"player": player_names, "nationality": player_nationality, 
                                    "classic_rating": player_classical_ratings,
                                    "rapid_rating": player_rapid_ratings,
                                    "blitz_rating": player_blitz_ratings,
                                    "sex": player_sex,
                                    "birth_year": player_byears})

fide_ratings.to_csv("fide_ratings.csv")


nat = fide_ratings["nationality"].value_counts(ascending=False)

# Creating a bar plot of player ratings

ax = sns.barplot(x=nat.index, y = nat.values, order = nat.index, color = "#D3D3D3")
sns.set(rc={'figure.figsize': (10, 8)})
sns.set_style("whitegrid")
sns.despine()
plt.xlabel('Nationality')
plt.ylabel('Count')
plt.title('Nationality of the FIDE Top 100 Classical Chess Players')
ax.bar_label(ax.containers[0])
plt.xticks(rotation = 45)
plt.savefig("nationality_chess_players.png", dpi = 1800)
plt.show()



