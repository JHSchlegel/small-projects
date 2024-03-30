import time
import numpy as np
import pandas as pd
import os
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as EC
from webdriver_manager.chrome import ChromeDriverManager
import booking.constants as const
from booking.booking_filtration import BookingFiltration
from booking.booking_report import BookingReport


class Booking(webdriver.Chrome):
    def __init__(self, headless = False, teardown = False):
        self.teardown = teardown
        options = Options()
        # if headless is True, then run in headless mode i.e. without browser
        if headless:
            options.add_argument("--headless")
        super(Booking, self).__init__(ChromeDriverManager().install(), options = options)
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
            print("Exiting...")
            time.sleep(5)
            self.quit()
            
            
    def close_popup(self):
        """Close popup if it appears
        """
        try:
            time.sleep(np.random.uniform(1, 3))
            # wait until popup is available and switch to it
            popup = WebDriverWait(self, 15).until(
                EC.presence_of_element_located(
                    (By.XPATH, "//button[@aria-label='Dismiss sign-in info.']")
                    ))
            popup.click()
            print("Popup closed")
        except Exception as e:
            print("No popup")
            
            
    def set_currency(self, currency = "CHF"):
        """Change currency to the one specified

        Args:
            currency (string, optional): Name of currency. Defaults to None.
        """
        time.sleep(np.random.uniform(1, 3))
        currency_element = self.find_element_by_xpath(
            '//*[@id="b2indexPage"]/div[2]/div/header/nav[1]/div[2]/span[1]/button'
        )
        currency_element.click()
        selected_currency_element = self.find_element_by_xpath(f"//button[@data-testid = 'selection-item' and contains(.,'{currency}')]")
        print(f"Switched to {currency}")
        selected_currency_element.click()
        
        
    def set_destination(self, destination):
        """Search for destination

        Args:
            destination (string): Name of destination
        """
        time.sleep(np.random.uniform(1, 3))
        search_field = self.find_element_by_xpath("//input[@id=':rc:']")
        # first clear field in case there is some text
        search_field.clear()
        search_field.send_keys(destination)
        first_result = self.find_element_by_xpath(
            '//*[@id="indexsearch"]/div[2]/div/form/div[1]/div[1]/div/div/div[2]/ul/li[1]/div/div/div'
        )
        print(f"Search for {destination}")
        time.sleep(np.random.uniform(1, 3))
        first_result.click()
    def set_dates(self, check_in_date, check_out_date):
        """Set check-in and check-out dates

        Args:
            check_in_date (string): date of check-in
            check_out_date (string): date of check-out
        """
        time.sleep(np.random.uniform(1, 3))
        check_in_element = self.find_element_by_css_selector(
            f'span[data-date="{check_in_date}"]'
        )
        print(f"Check-in date: {check_in_date}")
        check_in_element.click()
        
        check_out_element = self.find_element_by_css_selector(
            f'span[data-date="{check_out_date}"]'
        )
        print(f"Check-out date: {check_out_date}")
        check_out_element.click()
        
        
    def set_adults(self, n_adults):
        """Set number of adults

        Args:
            n_adults (int): Number of adults
        """
        selection = self.find_element_by_xpath("//div[@class='d67edddcf0']")
        selection.click()
        # first set adults count to zero by clicking on minus sign,
        # then click n_adults times on plus button
        
        
        while True:
            try:
                time.sleep(np.random.uniform(0.9, 1.4))
                adults_value = self.find_element_by_xpath(
                    '//*[@id="indexsearch"]/div[2]/div/form/div[1]/div[3]/div/div/div/div/div[1]/div[2]/span'
                )
                adults_value = int(adults_value.text)
                
                minus_button = self.find_element_by_xpath(
                '//*[@id="indexsearch"]/div[2]/div/form/div[1]/div[3]/div/div/div/div/div[1]/div[2]/button[1]'
                )
                
                if adults_value == 1:
                    break
                else:
                    minus_button.click()
            except:
                print("Couldn't find adults value")
                break
            
        plus_button = self.find_element_by_xpath(
            '//*[@id="indexsearch"]/div[2]/div/form/div[1]/div[3]/div/div/div/div/div[1]/div[2]/button[2]'
        )
        # subtract 1 since we already have one adult by default
        for i in range(n_adults-1):
            plus_button.click()
            time.sleep(np.random.uniform(0.5, 1.5))
        print(f"Number of adults: {n_adults}")
    
    def click_search(self):
        """Click search button
        """
        search_button = self.find_element_by_xpath(
            '//*[@id="indexsearch"]/div[2]/div/form/div[1]/div[4]/button/span'
        )
        search_button.click()
        print("Search button clicked")
        
    def apply_filtrations(self, *star_values):
        """Apply star rating and sort by lowest price
        """
        time.sleep(np.random.uniform(5, 10))
        filtration = BookingFiltration(driver = self) 
        filtration.apply_star_rating(*star_values)
        # filtration.sort_price_lowest_first()
        
    def get_links(self):
        """Get all the hotel links
        """
        time.sleep(np.random.uniform(5, 10))
        # get all the hotel elements
        hotel_elements = self.find_elements_by_xpath('//a[@class = "e13098a59f"]')
        # get all the links
        #hotel_links = [hotel_element.get_attribute('href') for hotel_element in hotel_elements]
        print(len(hotel_elements))
        return hotel_elements
    
    def get_report(self):
        links = self.get_links()
        print(links)
        names = []
        for link in links:
            time.sleep(np.random.uniform(1, 3))
            # wait until link can be clicked
            WebDriverWait(self, 15).until(
                EC.element_to_be_clickable(
                    (By.XPATH, '//a[@class = "e13098a59f"]')
                    ))
            # self.find_element_by_xpath(f'//a[@href = {link}]').click()
            link.click()
            # go back to previous page
            time.sleep(np.random.uniform(1, 3))
            # close the newly opened tab
            self.close()
        return None
            
        # hotel_boxes = self.find_element_by_xpath(
        #     '//*[@id="bodyconstraint-inner"]/div[2]/div/div[2]/div[3]/div[2]/div[2]'#'search_results_table'
        #     )
        # print(self.find_elements_by_class_name('fcab3ed991 a23c043802'))
        # report = BookingReport(driver = self)
        # print(report.get_titles())
    

    