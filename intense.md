---
Title: Intense rainfall events in R
output: html_document
---

# Intense rainfall events in R

Tyler Mitchell\
2022-03-30

## Introduction
<font size="4">
I am interested in tree radial growth responses to intense precipitation. Here, we define intense precipitation as any daily precipitation value that is \>2.0 standard deviations above the long-term mean [1]. This classification technique is related to other measures of precipitation intensity, which is an area of continued research interest [e.g., 2, 3, 4].

A member of my doctoral committee suggested that it could be helpful to share a standardized method to calculate intense rain events (IREs), so here we are!

## Data inputs

A daily summary weather station .csv file from NOAA's Climate Data Online portal [5], preferably with a high percentage of data coverage.

## Process

``` r
require("tidyverse")
# read climate data ----
data <- read.csv("2917741.csv")
head(data)
##       STATION             NAME LATITUDE LONGITUDE ELEVATION       DATE PRCP
## 1 USC00317097 RANDLEMAN, NC US 35.82197 -79.78975     246.9 1940-01-01  0.0
## 2 USC00317097 RANDLEMAN, NC US 35.82197 -79.78975     246.9 1940-01-02  0.0
## 3 USC00317097 RANDLEMAN, NC US 35.82197 -79.78975     246.9 1940-01-03  0.0
## 4 USC00317097 RANDLEMAN, NC US 35.82197 -79.78975     246.9 1940-01-04  0.0
## 5 USC00317097 RANDLEMAN, NC US 35.82197 -79.78975     246.9 1940-01-05  1.3
## 6 USC00317097 RANDLEMAN, NC US 35.82197 -79.78975     246.9 1940-01-06  0.0
# convert NOAA's DATE into three separate variables using tidyr ----
data <- tidyr::separate(data, DATE, sep="-", into = c("year", "month", "day"))
data$year <- as.numeric(data$year)
data$month <- as.numeric(data$month)
data$day <- as.numeric(data$day)

# remove missing precipitation observations ----
data <- data[!is.na(data$PRCP),]

# subset months of interest for intense rainfall calculation, here: july (7) ----
selected.month <- 7
data <- data[data$month == selected.month,]

# standard-normal (z) scores for precipitation and selection of intensity threshold ----
data$z <- scale(data$PRCP)
threshold <- 2.00
data$ire <- ifelse(data$z > threshold, 1, 0)

# retain precipitation amounts from intense rainfall event (ire) selection ----
data$ireppt <- ifelse(data$ire == 1, data$PRCP, 0)

# summarize intense rainfall events by year ----
ire.frequency <- setNames(aggregate(data$ire ~ data$year, FUN = sum),
                   c("year", "ire.frequency"))

ire.precipitation <- setNames(aggregate(data$ireppt ~ data$year, FUN = sum),
                              c("year", "ire.precipitation"))

ire <- merge(ire.frequency, ire.precipitation, by = "year")
ire
##    year ire.frequency ire.precipitation
## 1  1940             0               0.0
## 2  1941             1              37.6
## 3  1942             2              51.8
## 4  1943             3              85.6
## 5  1944             4             202.4
## 6  1945             4             113.6
## 7  1946             4             175.8
## 8  1947             0               0.0
## 9  1948             3              95.7
## 10 1949             0               0.0
## 11 1950             3             116.7
## 12 1951             1              48.8
## 13 1952             0               0.0
## 14 1953             1              24.1
## 15 1954             1              22.6
## 16 1955             1              24.1
## 17 1956             2              56.4
## 18 1957             2              51.5
## 19 1958             2              64.2
## 20 1959             1              41.1
## 21 1960             1              42.2
## 22 1961             1              27.9
## 23 1962             1              28.2
## 24 1963             0               0.0
## 25 1964             1              53.1
## 26 1965             5             220.2
## 27 1966             0               0.0
## 28 1967             2              91.9
## 29 1968             2              90.5
## 30 1969             1              83.8
## 31 1970             2              60.2
## 32 1971             0               0.0
## 33 1972             0               0.0
## 34 1973             2              59.4
## 35 1974             0               0.0
## 36 1975             8             354.7
## 37 1976             1              25.4
## 38 1977             2              64.5
## 39 1978             1              24.6
## 40 1979             1              49.8
## 41 1980             0               0.0
## 42 1981             2             120.9
## 43 1982             0               0.0
## 44 1983             0               0.0
## 45 1984             4             123.5
## 46 1985             3             106.4
## 47 1986             0               0.0
## 48 1987             0               0.0
## 49 1988             2              58.9
## 50 1989             2              95.2
## 51 1990             0               0.0
## 52 1991             1              28.4
## 53 1992             1              32.5
## 54 1993             2              48.2
## 55 1994             5             177.1
## 56 1995             3              89.4
## 57 1996             1              29.0
## 58 1997             2             112.5
## 59 1998             0               0.0
## 60 1999             1              25.7
## 61 2000             1              29.5
## 62 2001             1              38.4
## 63 2002             1              32.0
## 64 2003             2              80.0
## 65 2004             1              41.9
## 66 2005             3             117.9
## 67 2006             1              26.2
## 68 2007             0               0.0
## 69 2008             1              30.2
## 70 2009             0               0.0
## 71 2010             1              29.2
## 72 2011             2             122.7
## 73 2012             1              71.6
## 74 2014             3             106.2
## 75 2015             1              39.1
## 76 2016             1             101.6
## 77 2017             0               0.0
## 78 2018             1              51.6
## 79 2019             2              65.8
## 80 2020             0               0.0
## 81 2021             1              24.4


# another workflow using the lubridate package and pipes to calculate all years 
# and months at once. calculates standard normal (z) scores based on ALL months distribution ----
require("lubridate")
## Loading required package: lubridate
## 
## Attaching package: 'lubridate'
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
require("tidyverse")
data <- read.csv("2917741.csv")

# remove missing precipitation observations ----
data <- data[!is.na(data$PRCP),]

# standard-normal (z) scores for precipitation and selection of intensity threshold ----
data$z <- scale(data$PRCP)
threshold <- 2.00
data$ire <- ifelse(data$z > threshold, 1, 0)

# retain precipitation amounts from intense rainfall event (ire) selection ----
data$ireppt <- ifelse(data$ire == 1, data$PRCP, 0)

data <- data %>% 
  group_by(year = year(DATE), month = month(DATE)) %>% 
  summarise_if(is.numeric, sum)

data[c(1:2,6,8:9)]
## # A tibble: 970 × 5
## # Groups:   year [82]
##     year month  PRCP   ire ireppt
##    <dbl> <dbl> <dbl> <dbl>  <dbl>
##  1  1940     1  74.9     1   36.6
##  2  1940     2  78.5     0    0  
##  3  1940     3  68.8     0    0  
##  4  1940     4  65.6     0    0  
##  5  1940     5 198.      2  136. 
##  6  1940     6  86.9     0    0  
##  7  1940     7  70.3     0    0  
##  8  1940     8 205.      3  138. 
##  9  1940     9  41.4     1   23.6
## 10  1940    10  21.1     0    0  
## # … with 960 more rows
```
</font>

## References

[1] Mitchell, T. J., Knapp, P. A., & Patterson, T. W. (2020). The importance of infrequent, high-intensity rainfall events for longleaf pine (Pinus palustris Mill.) radial growth and implications for dendroclimatic research. Trees, Forests and People, 1, 100009.

[2] Post, A. K., & Knapp, A. K. (2020). The importance of extreme rainfall events and their timing in a semi‐arid grassland. Journal of Ecology, 108(6), 2431-2443.

[3] Griffin‐Nolan, R. J., Slette, I. J., & Knapp, A. K. (2021). Deconstructing precipitation variability: Rainfall event size and timing uniquely alter ecosystem dynamics. Journal of Ecology, 109(9), 3356-3369.

[4] Moustakis, Y., Papalexiou, S. M., Onof, C. J., & Paschalis, A. (2021). Seasonality, intensity, and duration of rainfall extremes change in a warmer climate. Earth's Future, 9(3), e2020EF001824.

[5] <https://www.ncdc.noaa.gov/cdo-web/results>
