import pandas as pd

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

from io import StringIO

from tqdm import tqdm

import dask.dataframe as dd
from dask.diagnostics import ProgressBar

def main():

    cache = [
        i + 1
        for i in range(214)
    ]

    while len(cache) > 0:
        for page in tqdm(cache):
            try:
                with webdriver.Firefox() as driver:
                    driver.get(f"https://madison.com/uw-salary-database/html_e37ff025-9a87-5a31-91ea-b2eb50aba4cb.html#2018-uw-salaries/?view_20_page={page}&view_20_sort=field_107%7Cdesc&view_20_per_page=1000&view_20_filters=[%7B%22operator%22:%22is%20not%20blank%22,%22field%22:%22field_115%22%7D]")
                    try:
                        table = WebDriverWait(driver, 10).until(
                            EC.presence_of_element_located((By.XPATH, '//*[@id="view_20"]/div[2]/table'))
                        )
                        table_html = table.get_attribute("outerHTML")
                        df = pd.read_html(StringIO(table_html))[0]
                        df.to_csv(f"data/cache/forbidden{page}.csv", index = False)
                        cache.remove(page)
                    finally:
                        driver.quit()
            except Exception as ex:
                print(ex)
                continue
        print(cache)

    forbidden = dd.read_csv("data/cache/forbidden*.csv")

    with ProgressBar():
        all_forbidden = forbidden.compute()

    all_forbidden.to_csv(f"data/forbidden-all.csv", index = False)

if __name__ == "__main__":
    main()