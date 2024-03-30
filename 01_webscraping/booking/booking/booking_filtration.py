# This file will include all the functions that
# will be used to filter the data

from selenium.webdriver.remote.webdriver import WebDriver
import time
import numpy as np
class BookingFiltration:
    def __init__(self, driver:WebDriver):
        if not isinstance(driver, WebDriver):
            raise Exception("driver is not a Booking instance")
        else:
            self.driver = driver
            
    def apply_star_rating(self, *star_values):
        """Apply star rating filtration
        """
        star_filtration_box = self.driver.find_elements_by_xpath("//*[contains(@id, 'filter_group_class_')]//div[@class = 'a1b3f50dcd be36d14cea b2fe1a41c3 db7f07f643 d1764ea78b']")
        print(len(star_filtration_box))
        # select all the child elements of the star_filtration_box
        # star_child_elements = star_filtration_box.find_elements_by_css_selector("*")
        for star_value in star_values:
            for star_element in star_filtration_box:
                if star_element.text == f"{star_value} stars":
                    star_element.click()
                    time.sleep(np.random.uniform(0.5, 1.5))
                    print(f"{star_value} stars selected")
    def sort_price_lowest_first(self):
        """Sort price from lowest to highest
        """
        element = self.driver.find_element_by_xpath(
            "//span[@class='e57ffa4eb5'][normalize-space()='Sort by: Top picks for long stays']"
        )
        element.click()
        time.sleep(np.random.uniform(0.5, 1.5))
        lowest_first = self.driver.find_element_by_xpath("//span[normalize-space()='Price (lowest first)']")
        lowest_first.click()
        print("Sorted by lowest price")
                    