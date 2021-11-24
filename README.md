# R files for scraping Sherdog.com fighter and event pages

Three different webscraping scripts:
* Fighter pages
* Event pages
* Stats from fighter pages

Besides the webscrapers, there are three other scripts:

* Event checker, which checks missing EventIDs to see if the scraper timedout and retries grabbing the page information
* Data cleaning script that does some reorganizing, merging, and recoding of the scraped event file
* Elo score calculator for each fighter based on their entire Sherdog fight history
