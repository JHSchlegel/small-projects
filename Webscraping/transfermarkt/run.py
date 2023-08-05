
from football.football import Transfer
from selenium.common.exceptions import StaleElementReferenceException
import json

# with Booking(teardown = True) as bot:
# if browser should be closed in the end
try:
    with Transfer(teardown = False) as bot:
        bot.land_first_page()
        bot.close_popup()
        pages = bot.click_next()
        with open("pages.json", "w") as f:
            json.dump(pages, f)
except StaleElementReferenceException as e:
    print("Stale element reference exception occured.")
    pass
except Exception as e:
    print("Error occured.")
    print(e)