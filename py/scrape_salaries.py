"""
Run me with:

    python -m py.scrape_salaries

By default:
    - A directory called data/ with subdirectory cache/ must exist in the working directory.
    - The resulting CSV of all salary data will be saved as data/forbidden-all.csv.

"""

import pandas as pd

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

from io import StringIO
from typing import List

from tqdm import tqdm

import dask.dataframe as dd

# TODO: Function checks that cache directory exists
# TODO: Function optionally empties cache directory
# TODO: Function checks DB for max pages and populates cache itself

def scrape_my_professor(cache: List[int], cache_path: str, payload: str) -> pd.DataFrame:

    while len(cache) > 0:
        for page in tqdm(cache, desc="Scraping UW salaries"):
            try:
                with webdriver.Firefox() as driver:
                    driver.get(f"https://madison.com/uw-salary-database/html_e37ff025-9a87-5a31-91ea-b2eb50aba4cb.html#2018-uw-salaries/?view_20_page={page}&view_20_sort=field_107%7Cdesc&view_20_per_page=1000&view_20_filters={payload}")
                    try:
                        table = WebDriverWait(driver, 10).until(
                            EC.presence_of_element_located((By.XPATH, '//*[@id="view_20"]/div[2]/table'))
                        )
                        table_html = table.get_attribute("outerHTML")
                        df = pd.read_html(StringIO(table_html))[0]
                        df.to_csv(f"{cache_path}{page}.csv", index = False)
                        cache.remove(page)
                    finally:
                        driver.quit()
            except Exception as ex:
                print(ex)
                continue
        print(cache)

    print("Done scraping. Processing results...")

    return dd.read_csv(f"{cache_path}*.csv").compute()

def main():

    cache = [
        i + 1
        for i in range(214)
    ]

    payload = "[%7B%22operator%22:%22is%20not%20blank%22,%22field%22:%22field_115%22%7D]"

    results = scrape_my_professor(
                    cache=cache,
                    cache_path="data/cache/forbidden",
                    payload=payload
                )

    results.to_csv(
        "data/forbidden-all.csv",
        lineterminator='\n',
        quoting=1,
        quotechar='"',
        index = False
        )

if __name__ == "__main__":
    main()