import os
from selenium import webdriver
from matplotlib import pyplot as plt
import seaborn as sns
import pandas as pd
import numpy as np
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as EC
from webdriver_manager.chrome import ChromeDriverManager


class Transfer(webdriver.Chrome):
    def __init__(self, driver_path=r"/usr/bin/google-chrome", teardown=False):
        self.driver_path = driver_path
        isExist = os.path.exists(self.driver_path)
        print(isExist)
        self.teardown = teardown
        os.environ['PATH'] += self.driver_path
        super(Transfer, self).__init__()
        self.implicitly_wait(15)
        self.maximize_window()

transfer = Transfer()


# Transfermarkt url
url = "https://www.transfermarkt.ch/spieler-statistik/wertvollstemannschaften/marktwertetop"

options = Options()
#options.add_experimental_option("detach", True) # don't close driver after code is finished
# change language to english
#options.add_argument("--lang=en")

# Initialize the Chrome driver
driver = webdriver.Chrome(ChromeDriverManager().install(),
                          options = options)


# Open the Transfermarkt website
driver.get(url)
driver.maximize_window()
driver.implicitly_wait(10)
# try except block in case cookies pop-up appears
try:
    ## Wait until the cookies pop-up appears and click accept
    # 1. wait until frame is available and switch to it
    WebDriverWait(driver, 20).until(
        EC.frame_to_be_available_and_switch_to_it((By.XPATH, '//*[@id="sp_message_iframe_764218"]'))
        )
    # 2. wait until accept button is clickable and click it
    WebDriverWait(driver, 20).until(
        EC.element_to_be_clickable((By.XPATH, "//button[@title='Zustimmen']"))
        ).click()
    print("Cookies accepted")
except:
    print("No cookies pop-up") 