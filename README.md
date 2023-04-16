# USA 2020-Election Fundraising and Expense Analysis

**Objective:**

To evaluate the patterns and trends of election campaign funding and individual contributions during the U.S. 2020 election, including comparisons of candidates, party affiliations, state-wise funding and expenses for 50 states.

**Introduction:**

The given R code provides an extensive analysis of the U.S. Election Campaign funding and individual contributions during the 2020 election period. The code analyzes various datasets and answers the following key questions:

* Which candidates raised the most campaign funds?

* How does the finance situation compare between Joe Biden and Donald Trump?

* Which committees raised the most funds from individual contributors?

* What is the state-wise distribution of fundraising?

**Data Source:**

Though initially acquired over 10GB size of data but later only able to use a sample of 2 million datasets for cleaning, analyzing, and visualization using various collaborative filters and interactive maps.

Election Commission of USA website:   https://www.fec.gov/data/browse-data/?tab=bulk-data

* Data set 1: All candidates

* Data set 2: Contributions by Individuals (itcont_2020_20200830_20200908)

* Data set 3: Committee master


**Methodology:**

The code uses multiple datasets, such as All Candidate Funding amount data, Individual Contribution transaction data, and Committee master detailed affiliation data. It employs data wrangling techniques using R packages like dplyr, ggplot2, plotly, lubridate, and usmap for data cleaning, manipulation, and visualization. The code performs the following operations:

* Import and clean the data, removing duplicates and correcting data types.
* Join multiple datasets based on Committee ID and Candidate ID to create a comprehensive dataset.
* Conduct exploratory data analysis (EDA) on the combined dataset.
* Identify and visualize the top 10 candidates and committees that raised the most funds and individual contributions, respectively.
* Compare the financial situations of Joe Biden and Donald Trump using bar plots.
* Analyze state-wise fundraising data and trends over time.

**Results:**

The analysis reveals the following findings:

* The top 10 candidates who raised the most campaign funds were identified, with their funding amounts visualized using a horizontal bar plot.
* A side-by-side comparison of Biden and Trump's campaign finances was presented in a bar plot, detailing total raised amounts, total spent, committee contributions, cash on hand, total individual contributions, and debt owed.
* The top 10 committees raising the most individual contributions were identified and visualized using a horizontal bar plot.
* State-wise fundraising data was analyzed, identifying the total and average amounts raised per state, presented in a choropleth map. A separate map was generated to visualize day-to-day funding trends across the states.

**Conclusion:**

This report provides an in-depth analysis of the U.S. 2020 election campaign funding and individual contributions. The results reveal key patterns and trends, enabling better understanding of campaign finances, fundraising distribution across states, and the financial dynamics between different candidates and parties. Such insights could potentially be useful for future campaign strategizing and resource allocation.
