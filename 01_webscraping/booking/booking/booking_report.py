# This file is going to include parse the website and extract
# all the data that we will need
import time 
import numpy as np
from selenium.webdriver.remote.webdriver import WebDriver


class BookingReport:
    def __init__(self, driver:WebDriver):
        if not isinstance(driver, WebDriver):
            raise Exception("driver is not a Booking instance")
        else:
            self.driver = driver
    def get_titles(self):
        """Extract hotel titles
        """
        time.sleep(np.random.uniform(2.5, 3.5))
        deal_boxes = self.driver.find_elements_by_class_name(
            'fcab3ed991 a23c043802'
        )
        collection = []
        print(deal_boxes)
        print(len(deal_boxes))
        print([box.text for box in deal_boxes])
        print([box.get_attribute("innerHTML").strip() for box in deal_boxes])
        print([box.get_attribute("innerText").strip() for box in deal_boxes])
        
        #collection.append([hotel_name, hotel_price, hotel_score])
        return [box.get_attribute("innerHTML").strip() for box in deal_boxes]