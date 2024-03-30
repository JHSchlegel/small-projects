import datetime
import logging
print(f"Initializing Webdriver at local time: {datetime.datetime.now().strftime('%H:%M:%S')}")

# set up logging
logging.basicConfig(filename = "transfermarkt.log", level = logging.INFO)
