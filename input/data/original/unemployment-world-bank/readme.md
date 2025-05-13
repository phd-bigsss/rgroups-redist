# Unemployment rate - Data package

This data package contains the data that powers the chart ["Unemployment rate"](https://ourworldindata.org/grapher/unemployment-rate?v=1&csvType=full&useColumnShortNames=false) on the Our World in Data website. It was downloaded on May 12, 2025.

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


## Unemployment, total (% of total labor force) (modeled ILO estimate)
Last updated: January 24, 2025  
Next update: January 2026  
Date range: 1991–2023  
Unit: modeled ILO estimate  


### How to cite this data

#### In-line citation
If you have limited space (e.g. in data visualizations), you can use this abbreviated in-line citation:  
International Labour Organization (via World Bank) (2025) – processed by Our World in Data

#### Full citation
International Labour Organization (via World Bank) (2025) – processed by Our World in Data. “Unemployment, total (% of total labor force) (modeled ILO estimate)” [dataset]. International Labour Organization (via World Bank), “World Development Indicators” [original data].
Source: International Labour Organization (via World Bank) (2025) – processed by Our World In Data

### How is this data described by its producer - International Labour Organization (via World Bank) (2025)?
Unemployment refers to the share of the labor force that is without work but available for and seeking employment.

Limitations and exceptions: The criteria for people considered to be seeking work, and the treatment of people temporarily laid off or seeking work for the first time, vary across countries. In many cases it is especially difficult to measure employment and unemployment in agriculture. The timing of a survey can maximize the effects of seasonal unemployment in agriculture. And informal sector employment is difficult to quantify where informal activities are not tracked.

There may be also persons not currently in the labour market who want to work but do not actively "seek" work because they view job opportunities as limited, or because they have restricted labour mobility, or face discrimination, or structural, social or cultural barriers. The exclusion of people who want to work but are not seeking work (often called the "hidden unemployed" or "discouraged workers") is a criterion that will affect the unemployment count of both women and men.

However, women tend to be excluded from the count for various reasons. Women suffer more from discrimination and from structural, social, and cultural barriers that impede them from seeking work. Also, women are often responsible for the care of children and the elderly and for household affairs. They may not be available for work during the short reference period, as they need to make arrangements before starting work. Further, women are considered to be employed when they are working part-time or in temporary jobs, despite the instability of these jobs or their active search for more secure employment.

Statistical concept and methodology: The standard definition of unemployed persons is those individuals without work, seeking work in a recent past period, and currently available for work, including people who have lost their jobs or voluntarily left work. In addition, persons who did not look for work but have an arrangement for a future job are also counted as unemployed. Still, some unemployment is unavoidable—at any time, some workers are temporarily unemployed between jobs as employers look for the right workers and workers search for better jobs. The labor force or the economically active portion of the population serves as the base for this indicator, not the total population.

The series is part of the "ILO modeled estimates database," including nationally reported observations and imputed data for countries with missing data, primarily to capture regional and global trends with consistent country coverage. Country-reported microdata is based mainly on nationally representative labor force surveys, with other sources (e.g., household surveys and population censuses) considering differences in the data source, the scope of coverage, methodology, and other country-specific factors. Country analysis requires caution where limited nationally reported data are available. A series of models are also applied to impute missing observations and make projections. However, imputed observations are not based on national data, are subject to high uncertainty, and should not be used for country comparisons or rankings. For more information: https://ilostat.ilo.org/resources/concepts-and-definitions/ilo-modelled-estimates/

Notes from original source: Given the exceptional situation, including the scarcity of relevant data, the ILO modeled estimates and projections from 2020 onwards are subject to substantial uncertainty.

### Source

#### International Labour Organization (via World Bank) – World Development Indicators
Retrieved on: 2025-01-24  
Retrieved from: https://datacatalog.worldbank.org/search/dataset/0037712/World-Development-Indicators  


    