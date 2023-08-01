# This file is going to include parse the website and extract
# all the data that we will need
import time 
import numpy as np
from selenium.webdriver.remote.webelement import WebElement


class BookingReport:
    def __init__(self, boxes_section_element: WebElement):
        self.boxes_section_element = boxes_section_element
        self.deal_boxes = self.get_deal_boxes()
    def get_deal_boxes(self):
        """Extract all the deals
        """
        return self.boxes_section_element.find_elements_by_class_name(
            'd20f4628d0'
        )
    
    def get_titles(self):
        """Extract hotel titles
        """
        return [box.find_element_by_class_name(
            'a4225678b2'
        ).get_attribute('innerHTML') for box in self.deal_boxes]