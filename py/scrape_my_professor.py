import pandas as pd

from selenium import webdriver
from selenium.webdriver.common.by import By

from io import StringIO

from tqdm import tqdm

import dask.dataframe as dd
from dask.diagnostics import ProgressBar

def main():

    cache = [
        i + 1
        for i in range(114)
    ]

    while len(cache) > 0:
        for page in tqdm(cache):
            try:
                with webdriver.Firefox() as driver:
                    driver.get(f"https://madison.com/uw-salary-database/html_e37ff025-9a87-5a31-91ea-b2eb50aba4cb.html#2018-uw-salaries/?view_20_page={page}&view_20_filters=%5B%7B%22field%22:%22field_106%22,%22operator%22:%22is%22,%22value%22:%22UW%20Madison%22%7D%5D&view_20_sort=field_107%7Cdesc&view_20_per_page=1000")
                    table = driver.find_element(By.XPATH, '//*[@id="view_20"]/div[2]/table')
                    table_html = table.get_attribute("outerHTML")
                    df = pd.read_html(StringIO(table_html))[0]
                    df.to_csv(f"../../data/cache/forbidden{page}.csv", index = False)
                    cache.remove(page)
            except Exception as ex:
                print(ex)
                continue
        print(cache)

    forbidden = dd.read_csv("../../data/cache/forbidden*.csv")

    with ProgressBar():
        all_forbidden = forbidden.compute()

    all_forbidden.to_csv(f"../../data/forbidden.csv", index = False)

if __name__ == "__main__":
    main()
