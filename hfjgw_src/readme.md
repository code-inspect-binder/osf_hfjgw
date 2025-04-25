
# Replication Materials for "Living on the Edge. Neighborhood Boundaries and the Spatial Dynamics of Violent Crime"

These files contain the code and data to replicate the results presented in Legewie (2018) "Living on the Edge. Neighborhood Boundaries and the Spatial Dynamics of Violent Crime" with the statistical programming language [R](http://www.r-project.org). 

Update Aug 2022: The replication package was updated in August 2022 to remove BoundaryDetection and colourschemes as dependencies and ensure compatibility with recent versions of R.

## Code files

The replication materials include the following code files with approximate runtime, CPU and memory requirements in parentheses. 

- `areal_wombling.r` Functions to calculate boundary values using areal wombling
- `00-utils.r`: Utility functions
- `01a-chicago-crime-data.r`: Download and recode Chicago crime data (2.5 hours, 2 CPUs, 6.2 GB memory)
- `01b-recode-census-2000.r`: Recode 2000 census data (2 hours, 2 CPUs, 12 GB memory)
- `01b-recode-census-2010.r`: Recode 2010 census data (7 hours, 2 CPUs, 32 GB memory)
- `01c-physical-boundaries.r`: Create measures for physical and admin boundaries (4 hours, 2 CPUs, 7 GB memory)
- `01c-recode-ccahs.r`: Recode CCAHS data (10 min, 2 CPUs, 1 GB memory)
- `01d-plausible-values.r`: Create plausible values for block group measures (10 min, 2 CPUs, 2 GB memory)
- `02a-analysis.r`: All analysis (35 min, 2 CPUs, 4 GB memory)

## Data files

The analyses are based on data from multiple sources. Most data is provided as part of the replication package and available online.

1. 2000 and 2010 TIGER shapefiles for Cook county on the census block and block group level ('cook-census-block-TIGER-2000.rds', 'cook-census-block-TIGER-2010.rds', 'cook-census-block-group-TIGER-2010.rds')
2. Data from the 2000 Decennial Census Summary File I and III ('cook-census-block-sf1-2000.rds', 'cook-block-group-sf1-2000.rds' and 'cook-block-group-sf3-2000.rds')
3. Data from the 2010 Decennial Census Summary File I ('cook-census-block-sf1-2010.rds' and 'cook-block-group-sf1-2010.rds')
4. Data from the 5-year estimate from the American Community Survey, 2007-2011 ('cook-block-group-acs5-2011.rds')
5. Shapefiles for Chicago parks and elementary school district from the Chicago open data website (folder 'chicago-parks-2012' and 'cps-elementary-school-boundaries')
6. 2010 TIGER/Line Shapefiles, Current All Lines Shapefile, and the 2010 TIGER/Line Shapefiles, Current Topological Faces (folder 'chicago-lines-2010' and 'chicago-faces-2010')

The only restricted data source is the Chicago Community Adult Health Study (CCAHS), 2001-2003. Access to CCAHS can be requested at ICPSR [here](https://www.icpsr.umich.edu/icpsrweb/RCMD/studies/31142).

## API Token

Downloading incident-level crime data in `01a-chicago-crime-data.r` requires an API token. The token can be obtained from https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-present/ijzp-q8t2. The token has to be set to the `app_token` variable at the beginning of the code file `01a-chicago-crime-data.r`. 

## Required software

R (version 3.6.3)

R packages: 

- tidyverse (version 1.3.0)
	- forcats (version 0.5.1)
	- stringr (version 1.4.0)
	- tibble (version 3.1.2)
	- purrr (version 0.3.4)
	- readr (version 1.3.1)
	- dplyr (version 1.0.7)
	- ggplot2 (version 3.3.4)
- sp (version 1.3-1)
- spdplyr (version 0.1.3)
- lubridate (version 1.7.4)
- httr (version 1.4.4)
- car (version 3.0-2)
- matrixStats (version 0.54.0)
- rgeos (version 0.4-1)
- MASS (version 7.3-51.5)
- stargazer (version 5.2.2)
- spdep (version 0.7-9)
- maptools (version 0.9-4)
- haven (version 2.2.0)
- glue (version 1.6.2)
- multiwayvcov (version 1.2.3)
- lmtest (version 0.9-36)
- scales (version 1.1.1)
- Hmisc (version 4.5-0)

## References

Legewie, Joscha. 2018. "Living on the Edge. Neighborhood Boundaries and the Spatial Dynamics of Violent Crime." Demography
