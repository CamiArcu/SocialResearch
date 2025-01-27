# Public spending on education as a share of GDP - Data package

This data package contains the data that powers the chart ["Public spending on education as a share of GDP"](https://ourworldindata.org/grapher/total-government-expenditure-on-education-gdp?v=1&csvType=full&useColumnShortNames=false) on the Our World in Data website. It was downloaded on January 27, 2025.

### Active Filters

A filtered subset of the full data was downloaded. The following filters were applied:

## CSV Structure

The high level structure of the CSV file is that each row is an observation for an entity (usually a country or region) and a timepoint (usually a year).

The first two columns in the CSV file are "Entity" and "Code". "Entity" is the name of the entity (e.g. "United States"). "Code" is the OWID internal entity code that we use if the entity is a country or region. For normal countries, this is the same as the [iso alpha-3](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3) code of the entity (e.g. "USA") - for non-standard countries like historical countries these are custom codes.

The third column is either "Year" or "Day". If the data is annual, this is "Year" and contains only the year as an integer. If the column is "Day", the column contains a date string in the form "YYYY-MM-DD".

The final column is the data column, which is the time series that powers the chart. If the CSV data is downloaded using the "full data" option, then the column corresponds to the time series below. If the CSV data is downloaded using the "only selected data visible in the chart" option then the data column is transformed depending on the chart type and thus the association with the time series might not be as straightforward.

## Metadata.json structure

The .metadata.json file contains metadata about the data package. The "charts" key contains information to recreate the chart, like the title, subtitle etc.. The "columns" key contains information about each of the columns in the csv, like the unit, timespan covered, citation for the data etc..

## About the data

Our World in Data is almost never the original producer of the data - almost all of the data we use has been compiled by others. If you want to re-use data, it is your responsibility to ensure that you adhere to the sources' license and to credit them correctly. Please note that a single time series may have more than one source - e.g. when we stich together data from different time periods by different producers or when we calculate per capita metrics using population data from a second source.

## Detailed information about the data


## Public spending on education as a share of GDP
Total general government expenditure on education (all levels of government and all levels of education), given as a share of GDP.
Last updated: November 4, 2024  
Next update: November 2025  
Date range: 1870–2023  
Unit: %  


### How to cite this data

#### In-line citation
If you have limited space (e.g. in data visualizations), you can use this abbreviated in-line citation:  
World Bank (2024); Tanzi & Schuknecht (2000) – processed by Our World in Data

#### Full citation
World Bank (2024); Tanzi & Schuknecht (2000) – processed by Our World in Data. “Public spending on education as a share of GDP” [dataset]. World Bank, “World Bank Education Statistics (EdStats)”; Tanzi & Schuknecht, “Public Expenditure on Education OECD” [original data].
Source: World Bank (2024), Tanzi & Schuknecht (2000) – processed by Our World In Data

### What you should know about this data

### Sources

#### World Bank – World Bank Education Statistics (EdStats)
Retrieved on: 2024-11-04  
Retrieved from: https://datacatalog.worldbank.org/search/dataset/0038480/education-statistics  

#### Tanzi & Schuknecht – Public Expenditure on Education OECD
Retrieved on: 2017-09-30  
Retrieved from: https://link.springer.com/article/10.1023%2FA%3A1017578302202?LI=true  

#### Notes on our processing step for this indicator
**Historical expenditure data:**

Historical data in this dataset is based on a wide array of sources, reflecting a comprehensive approach to data collection across different time periods and regions. However, the diverse nature of these sources leads to inconsistencies, as methodologies and data quality vary between sources. For instance, older sources like the League of Nations Statistical Yearbook or Mitchell's 1962 data may use different metrics or collection methods compared to more modern sources like the OECD Education reports or UN surveys. This variance in source material and methodology means that direct comparisons across different years or countries might be challenging, necessitating careful interpretation and cross-reference for accuracy. The dataset serves as a rich historical repository but also underscores the complexities and challenges inherent in compiling and harmonizing historical data from multiple, diverse sources.

**Recent estimates:**

General government expenditure on education (current, capital, and transfers) is expressed as a percentage of GDP. It includes expenditure funded by transfers from international sources to government. General government usually refers to local, regional and central governments.

World Bank variable id: SE.XPD.TOTL.GD.ZS

Original source: UNESCO Institute for Statistics (UIS). UIS.Stat Bulk Data Download Service. Accessed October 24, 2022.


    