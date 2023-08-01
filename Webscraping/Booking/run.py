from booking.booking import Booking

# with Booking(teardown = True) as bot:
# if browser should be closed in the end
with Booking(teardown = True) as bot:
    bot.land_first_page()
    bot.close_popup()
    bot.set_currency(currency = 'CHF')
    bot.set_destination('New York')
    bot.set_dates(check_in_date = '2023-08-01', check_out_date = '2023-08-23')
    bot.set_adults(3)
    bot.click_search()
    bot.apply_filtrations()
    # bot.apply_filtration(
    #     min_rating=9.0,
    #     max_price=300,
    #     free_cancellation=True,
    #     breakfast_included=False,
    #     five_star=False
    # )
    # bot.report_results()
    # bot.save_results()