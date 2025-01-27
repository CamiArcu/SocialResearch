# Literacy rate - Data package

This data package contains the data that powers the chart ["Literacy rate"](https://ourworldindata.org/grapher/cross-country-literacy-rates?v=1&csvType=full&useColumnShortNames=false) on the Our World in Data website. It was downloaded on January 27, 2025.

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


## Literacy rate
Percentage of the population aged 15 and above who can read and write a short, simple statement on their everyday life.
Last updated: November 4, 2024  
Next update: November 2025  
Date range: 1475–2023  
Unit: %  


### How to cite this data

#### In-line citation
If you have limited space (e.g. in data visualizations), you can use this abbreviated in-line citation:  
World Bank (2024); Various sources (2018) – processed by Our World in Data

#### Full citation
World Bank (2024); Various sources (2018) – processed by Our World in Data. “Literacy rate” [dataset]. World Bank, “World Bank Education Statistics (EdStats)”; Various sources, “Cross-country literacy rates” [original data].
Source: World Bank (2024), Various sources (2018) – processed by Our World In Data

### What you should know about this data

### Sources

#### World Bank – World Bank Education Statistics (EdStats)
Retrieved on: 2024-11-04  
Retrieved from: https://datacatalog.worldbank.org/search/dataset/0038480/education-statistics  

#### Various sources – Cross-country literacy rates
Retrieved on: 2018-04-18  
Retrieved from: https://data.worldbank.org/indicator/SE.ADT.LITR.ZS  

#### Notes on our processing step for this indicator
**Recent estimates:**

Percentage of the population between age 25 and age 64 who can, with understanding, read and write a short, simple statement on their everyday life. Generally, ‘literacy’ also encompasses ‘numeracy’, the ability to make simple arithmetic calculations. This indicator is calculated by dividing the number of literates aged 25-64 years by the corresponding age group population and multiplying the result by 100.

World Bank variable id: UIS.LR.AG25T64

Original source: UNESCO Institute for Statistics

**Historical literacy data:**

The historical estimates in this long-run cross-country dataset were derived from a blend of diverse sources, each contributing to different time periods. For data before 1800, the dataset relies on the work of Buringh and Van Zanden (2009), which offers insights into literacy through the lens of manuscript and book production in Europe from the sixth to the eighteenth centuries. For the years 1820 and 1870 (excluding the United States), it incorporates data from Broadberry and O'Rourke's "The Cambridge Economic History of Modern Europe." The United States data comes from the National Center for Education Statistics. Additionally, global estimates for the period 1820-2000 are drawn from van Zanden and colleagues’ "How Was Life?: Global Well-being since 1820," an OECD publication. For historical estimates specific to Latin America, the dataset uses the Oxford Latin American Economic History Database (OxLAD). Each source follows a consistent conceptual definition of literacy, although discrepancies among sources are acknowledged, necessitating cautious interpretation of year-to-year changes. The dataset also includes instances where specific sources were preferred, such as opting for OxLAD data over the World Bank for Paraguay in 1982 due to significant differences in literacy rate estimates.


    