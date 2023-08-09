import time
import numpy as np
import pandas as pd
import logging
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as EC
from webdriver_manager.chrome import ChromeDriverManager
import football.constants as const

class Transfer(webdriver.Chrome):
    def __init__(self, headless = False, teardown = False):
        self.teardown = teardown
        options = Options()
        options.add_argument("--window-position=2000,0")
        # if headless is True, then run in headless mode i.e. without browser
        if headless:
            options.add_argument("--headless")
        super(Transfer, self).__init__(ChromeDriverManager().install(),
                                       options = options)
        self.implicitly_wait(15)
        self.maximize_window()
        
        
    def land_first_page(self):
        """Go to Booking.com
        """
        self.get(const.BASE_URL)
        
        
    # to close context manager use __exit__ keyword
    def __exit__(self, exc_type, exc_val, exc_tb):
        # if teardown quit, else leave driver open
        if self.teardown:
            logging.info("Exiting...")
            time.sleep(5)
            self.quit()
            
            
    def close_popup(self):
        """Close popup if it appears
        """
        # try except block in case cookies pop-up appears
        try:
            time.sleep(np.random.uniform(1, 3))
            ## Wait until the cookies pop-up appears and click accept
            # 1. wait until frame is available and switch to it
            WebDriverWait(self, 20).until(
                EC.frame_to_be_available_and_switch_to_it(
                    (By.XPATH, '//*[@id="sp_message_iframe_764218"]')
                    )
                )
            # 2. wait until accept button is clickable and click it
            WebDriverWait(self, 20).until(
                EC.element_to_be_clickable(
                    (By.XPATH, "//button[@title='Zustimmen']")
                    )
                ).click()
            logging.info("Cookies accepted")
        except:
            logging.info("No cookies pop-up") 
    def get_report(self):
        link_elements = self.find_elements_by_xpath(
            "//td[contains(@class, 'no-border-links hauptlink')]/a"
            )
        links = [str(link.get_attribute('href')) for link in link_elements]
        clubs = [link.text for link in link_elements]
        leagues = [league.text for league in self.find_elements_by_xpath(
            '//*[@id="yw1"]/table/tbody/tr[1]/td[4]/a'
            )
        ]
        logging.info(leagues)
        page = {}
        for i, link in enumerate(links):
            time.sleep(np.random.uniform(3, 5))
            players_links = [str(link.get_attribute('href')) for link in
                             self.find_elements_by_xpath(
                "//td[@class = 'hauptlink']/a"
            )]
            
            player_names = [link.text for link in
                             self.find_elements_by_xpath(
                "//td[@class = 'hauptlink']/a"
            )]
            WebDriverWait(self, 20).until(
                EC.element_to_be_clickable(
                    (By.XPATH, "//td[@class = 'no-border-links hauptlink']/a")
                    )
                )
            time.sleep(np.random.uniform(3, 5))
            self.get(link)
            #link.click()

            nrs_elements = self.find_elements_by_xpath(
                "//td/div[contains(@class,'tm-shirt-number')]"
                )
            positions_elements = self.find_elements_by_xpath(
                "//table[contains(@class,'inline-table')]/tbody/tr[2]/td"
                )
            values_elements = self.find_elements_by_xpath(
                "//td[contains(@class, 'rechts hauptlink')]"
                )
            players = {player.text:[nr.text, position.text, value.text] for 
                       player, nr, position, value in 
                       zip(
                           players_elements, nrs_elements, 
                           positions_elements, values_elements
                           )
                       }
            # print([player.text for player in players_elements])
            # print([nr.text for nr in nrs_elements])
            # print([position.text for position in positions_elements])
            # print([value.text for value in values_elements])
            page[clubs[i]] = players
            if i == 1:
                logging.info(page[clubs[i]])
            time.sleep(np.random.uniform(1, 3))
            self.get(const.BASE_URL)
            # self.back()
            logging.info(f"Finished {clubs[i]}")
        logging.info("Finished page")
        return page
    def click_next(self):
        pages = {}
        pages[str(1)] = self.get_report()
        for i in range(1, const.N_PAGES):
            # wait until the page is clickable using EC
            WebDriverWait(self, 20).until(
                EC.element_to_be_clickable(
                    (By.XPATH, "//li[contains(@class,'tm-pagination__list-item')]/a")
                    )
                )
            page_number = self.find_element_by_xpath(
            f"//*[@id='yw1']/div[2]/ul/li[{i+1}]/a"
            )
            time.sleep(np.random.uniform(3,5))
            self.get(str(page_number.get_attribute('href')))
            pages[str(i+1)] = self.get_report()
            logging.info(f"Finished page {i+1}")
        return pages
    # TODO: add method to save data to json file
    # TODO: add class to convert json file to pandas dataframe and unnest wider
    # TODO: add method to click on each player and extract certain characteristics