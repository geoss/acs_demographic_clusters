# Studying Neighborhoods With Uncertain Census Data


## Aim
This repository contains all the data and code to create an geodemographic classification of the United States like [ESRI Tapestry](http://www.esri.com/data/esri_data/tapestry) or [Nielsen PRIZM](http://www.claritas.com/MyBestSegments/Default.jsp?ID=70&&pageName=Learn%2BMore&menuOption=learnmore).  However unlike these commercial systems which use proprietary methods and data the inputs and software used to create this classification are all open source.  The methods used to create this repository are outlined in detail in the paper [here](https://www.tandfonline.com/doi/full/10.1080/00045608.2015.1052335).  

## Data
All of the used to construct the typology including all input and output data is online at [openICPSR](http://doi.org/10.3886/E41329V1).  In the [openICPSR data repository](http://doi.org/10.3886/E41329V1) for this project.  The data repository contains:
* **Input Data** folder contains all of the input Data from the American Community Survey.  A detailed list of the 136 input variables is below.
* The **Validation Data** are extracts from two freely available data sets, [crime in Chicago](http://gis.chicagopolice.org/CLEARMap_crime_sums/startPage.htm#) and [campaign contributions by individuals](http://www.fec.gov/finance/disclosure/ftpdet.shtml). The specific files that we used in the paper URL are on the [openICPSR repository](http://doi.org/10.3886/E41329V1).
* The **Output Data** folder contains a shapefile for the entire US containing the 10, 55, and 250 class level of the classification (variables names "X10, "X55", and "cluster")

### Maps
An ESRI Shapefile containing all input data and the output classes for *all* US Census Tracts is available in the **Output Data** folder on the [openICPSR data  repository](http://doi.org/10.3886/E41329V1).

### Input Variables

| Description | ACS Variable ID |
|-------------|-----------------|
| Income Gini Index | B19083001 |
| Renter-Occupied Housing Units Paying Cash Rent Median Gross Rent | B25064001 |
| Owner-Occupied Housing Units Lower Value Quartile | B25076001 |
| Owner-Occupied Housing Units Median Value | B25077001 |
| Owner-Occupied Housing Units Upper Value Quartile | B25078001 |
| Percent Male Under 5 Years | B01001003 |
| Percent Male 5 To 9 Years | B01001004 |
| Percent Male 10 To 14 Years | B01001005 |
| Percent Male 15 To 17 Years | B01001006 |
| Percent Male 18 And 19 Years | B01001007 |
| Percent Male 20 Years | B01001008 |
| Percent Male 21 Years | B01001009 |
| Percent Male 22 To 24 Years | B01001010 |
| Percent Male 25 To 29 Years | B01001011 |
| Percent Male 30 To 34 Years | B01001012 |
| Percent Male 35 To 39 Years | B01001013 |
| Percent Male 40 To 44 Years | B01001014 |
| Percent Male 45 To 49 Years | B01001015 |
| Percent Male 50 To 54 Years | B01001016 |
| Percent Male 55 To 59 Years | B01001017 |
| Percent Male 60 And 61 Years | B01001018 |
| Percent Male 62 To 64 Years | B01001019 |
| Percent Male 65 And 66 Years | B01001020 |
| Percent Male 67 To 69 Years | B01001021 |
| Percent Male 70 To 74 Years | B01001022 |
| Percent Male 75 To 79 Years | B01001023 |
| Percent Male 80 To 84 Years | B01001024 |
| Percent Male 85 Years And Over | B01001025 |
| Percent Female Under 5 Years | B01001027 |
| Percent Female 5 To 9 Years | B01001028 |
| Percent Female 10 To 14 Years | B01001029 |
| Percent Female 15 To 17 Years | B01001030 |
| Percent Female 18 And 19 Years | B01001031 |
| Percent Female 20 Years | B01001032 |
| Percent Female 21 Years | B01001033 |
| Percent Female 22 To 24 Years | B01001034 |
| Percent Female 25 To 29 Years | B01001035 |
| Percent Female 30 To 34 Years | B01001036 |
| Percent Female 35 To 39 Years | B01001037 |
| Percent Female 40 To 44 Years | B01001038 |
| Percent Female 45 To 49 Years | B01001039 |
| Percent Female 50 To 54 Years | B01001040 |
| Percent Female 55 To 59 Years | B01001041 |
| Percent Female 60 And 61 Years | B01001042 |
| Percent Female 62 To 64 Years | B01001043 |
| Percent Female 65 And 66 Years | B01001044 |
| Percent Female 67 To 69 Years | B01001045 |
| Percent Female 70 To 74 Years | B01001046 |
| Percent Female 75 To 79 Years | B01001047 |
| Percent Female 80 To 84 Years | B01001048 |
| Percent Female 85 Years And Over | B01001049 |
| Percent White Alone | B02001002 |
| Percent Black Alone | B02001003 |
| Percent American Indian Alone | B02001004 |
| Percent Asian Alone | B02001005 |
| Percent Hispanic Latino | B03001003 |
| Percent Not Citizen | B05001006 |
| Percent Less Than High School Graduate | B07009002 |
| Percent High School Graduate | B07009003 |
| Percent Some College Associate Degree | B07009004 |
| Percent Bachelor Degree | B07009005 |
| Percent Graduate Professional Degree | B07009006 |
| Percent Different House Year Ago Same City | B07204004 |
| Percent Different House Year Ago Elsewhere | B07204007 |
| Percent No Car | B08014002 |
| Percent Workers 16 Years And Over Public Transportation (Excluding Taxicab) | B08301010 |
| Percent Commute 5-9 minutes | B08303003 |
| Percent Commute 10-14 minutes | B08303004 |
| Percent Commute 15-19 minutes | B08303005 |
| Percent Commute 20-24 minutes | B08303006 |
| Percent Commute 25-29 minutes | B08303007 |
| Percent Commute 30-34 minutes | B08303008 |
| Percent Commute 35-39 minutes | B08303009 |
| Percent Commute 40-44 minutes | B08303010 |
| Percent Commute 45-59 minutes | B08303011 |
| Percent Commute 60-89 minutes | B08303012 |
| Percent Commute 90 More minutes | B08303013 |
| Percent Children In Single Female HH | B09005005 |
| Percent Households Family Households Married-Couple Family | B11001003 |
| Percent Male Male Household | B11009003 |
| Percent Female Female Household | B11009005 |
| Percent Only English | B16001002 |
| Percent Spanish | B16001003 |
| Percent Spanish Low English | B16001005 |
| Percent of Households With Income Less Than \$10,000 | B19001002 |
| Percent of Households With Income Less \$10,000 To \$14,999 | B19001003 |
| Percent of Households With Income Less \$15,000 To \$19,999 | B19001004 |
| Percent of Households With Income Less \$20,000 To \$24,999 | B19001005 |
| Percent of Households With Income Less \$25,000 To \$29,999 | B19001006 |
| Percent of Households With Income Less \$30,000 To \$34,999 | B19001007 |
| Percent of Households With Income Less \$35,000 To \$39,999 | B19001008 |
| Percent of Households With Income Less \$40,000 To \$44,999 | B19001009 |
| Percent of Households With Income Less \$45,000 To \$49,999 | B19001010 |
| Percent of Households With Income Less \$50,000 To \$59,999 | B19001011 |
| Percent of Households With Income Less \$60,000 To \$74,999 | B19001012 |
| Percent of Households With Income Less \$75,000 To \$99,999 | B19001013 |
| Percent of Households With Income Less \$100,000 To \$124,999 | B19001014 |
| Percent of Households With Income Less \$125,000 To \$149,999 | B19001015 |
| Percent of Households With Income Less \$150,000 To \$199,999 | B19001016 |
| Percent of Households With Income Less \$200,000 Or More | B19001017 |
| Percent of Households With Cash Public Assistance Or Food Stamps Snap | B19058002 |
| Percent of Households With Retirement Income | B19059002 |
| Percent Housing Units Vacant | B25002003 |
| Percent Occupied Housing Units Renter Occupied | B25003003 |
| Percent Housing Units that are 1 Unit Detached Structures | B25024002 |
| Percent Housing Units that are 1 Unit Attached Structures | B25024003 |
| Percent Housing Units that have 2 Units in Structure | B25024004 |
| Percent Housing Units that have 3 Or 4 Units in Structure | B25024005 |
| Percent Housing Units that have 5 To 9 Units in Structure | B25024006 |
| Percent Housing Units that have 10 To 19 Units in Structure | B25024007 |
| Percent Housing Units that have 20 To 49 Units in Structure | B25024008 |
| Percent Housing Units that have 50 Or More Units in Structure | B25024009 |
| Percent Housing Units that are Mobile Homes | B25024010 |
| Percent Housing Units Built 2005 Or Later | B25034002 |
| Percent Housing Units Built 2000 To 2004 | B25034003 |
| Percent Housing Units Built 1939 Or Earlier | B25034010 |
| Percent of all workers employed in Agriculture Forestry Fishing And Hunting And Mining | C24050002 |
| Percent of all workers employed in Construction | C24050003 |
| Percent of all workers employed in Manufacturing | C24050004 |
| Percent of all workers employed in Wholesale Trade | C24050005 |
| Percent of all workers employed in Retail Trade | C24050006 |
| Percent of all workers employed in Transportation And Warehousing And Utilities | C24050007 |
| Percent of all workers employed in Information | C24050008 |
| Percent of all workers employed in Finance And Insurance And Real Estate And Rental And Leasing | C24050009 |
| Percent of all workers employed in Professional Scientific And Management And Administrative And Waste Management Services | C24050010 |
| Percent of all workers employed in Educational Services And Health Care And Social Assistance | C24050011 |
| Percent of all workers employed in Arts Entertainment And Recreation And Accommodation And Food Services | C24050012 |
| Percent of all workers employed in Other Services Except Public Administration | C24050013 |
| Percent of all workers employed in Public Administration | C24050014 |
| Percent of all workers employed in Management Business Science And Arts Occupations | C24050015 |
| Percent of all workers employed in Service Occupations | C24050029 |
| Percent of all workers employed in Sales And Office Occupations | C24050043 |
| Percent of all workers employed in Natural Resources Construction And Maintenance Occupations | C24050057 |
| Percent of all workers employed in Production Transportation And Material Moving Occupations | C24050071 |
| Population Density | B01003001 |
| Percent of the Population Living in Group Quarters | B26001001 |
