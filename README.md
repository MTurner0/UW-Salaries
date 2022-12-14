# University of Wisconsin Salary Analysis

This project was completed in partial fulfillment of the requirements for [STAT 679](https://krisrs1128.github.io/stat679_notes/).
It has two main components:

1. A web-scraping module (written in Python, found in the `py` folder) to extract information about all University of Wisconsin employee salaries from 2017 - 2021 from the [UW Salary Database](https://madison.com/uw-salary-database/html_e37ff025-9a87-5a31-91ea-b2eb50aba4cb.html) on [madison.com](madison.com).

2. A [Shiny app](https://shiny.rstudio.com) that can be run to interact with and visualize this dataset (written in R, found in the `R` folder).

## To run:

**The salary data is not stored on this repository.**
The `py.scrape_salaries` module must be run to collect this data.

The following steps can be used within the cloned repository to collect the salary data and run the Shiny app.

```
mkdir data
mkdir data/cache

python -m pip install -r requirements.txt
python -m py.scrape_salaries

cd R
R -e "shiny::runApp()"
```