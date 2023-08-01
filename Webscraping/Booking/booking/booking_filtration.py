# This file will include all the functions that
# will be used to filter the data

from selenium.webdriver.remote.webdriver import WebDriver

class BookingFiltration:
    def __init__(self, driver:WebDriver):
        if not isinstance(driver, WebDriver):
            raise Exception("driver is not a Booking instance")
        else:
            self.driver = driver
    def apply_star_rating(self):
        star_filtration_box = self.driver.find_element_by_id("filter_group_class_:rac:")
        print(star_filtration_box)
        star_child_elements = star_filtration_box.find_elements_by_css_selector("*")
        print(len(star_child_elements))
        # for star_value in star_values:
        #     for star_element in star_child_elements:
        #         if str(star_element.get_attribute("innerHTML")).strip() == f"{star_value} stars":
        #             star_element.click()