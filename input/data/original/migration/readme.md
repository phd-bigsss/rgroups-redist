# Share of the population that were born in another country - Data package

This data package contains the data that powers the chart ["Share of the population that were born in another country"](https://ourworldindata.org/grapher/migrant-stock-share?v=1&csvType=full&useColumnShortNames=false) on the Our World in Data website. It was downloaded on May 12, 2025.

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


## Share of the population that was born in another country
Immigrants as a share of the total population.
Last updated: March 12, 2025  
Next update: March 2027  
Date range: 1990–2024  
Unit: %  


### How to cite this data

#### In-line citation
If you have limited space (e.g. in data visualizations), you can use this abbreviated in-line citation:  
United Nations Department of Economic and Social Affairs (2024) – with minor processing by Our World in Data

#### Full citation
United Nations Department of Economic and Social Affairs (2024) – with minor processing by Our World in Data. “Share of the population that was born in another country” [dataset]. United Nations Department of Economic and Social Affairs, “International Migrant Stock 2024 - POP/DB/MIG/Stock/Rev.2024” [original data].
Source: United Nations Department of Economic and Social Affairs (2024) – with minor processing by Our World In Data

### How is this data described by its producer - United Nations Department of Economic and Social Affairs (2024)?
International migrant stock as percentage of the total population by age and sex and by region, country or area of destination, 1990-2024

### Source

#### United Nations Department of Economic and Social Affairs – International Migrant Stock
Retrieved on: 2025-03-12  
Retrieved from: https://www.un.org/development/desa/pd/content/international-migrant-stock  

#### Notes on our processing step for this indicator
- Regional aggregates (such as continents, income groups and the worldwide aggregate) are based on [our definition of the continents](https://ourworldindata.org/world-region-map-definitions#our-world-in-data) and the [World Bank definition of income groups](https://ourworldindata.org/grapher/world-bank-income-groups), unless otherwise indicated. They might differ from the definitions used by the United Nations.


    