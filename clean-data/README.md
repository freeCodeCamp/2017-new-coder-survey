# Cleaning freeCodeCamp's 2017 New Coder Survey Data

Code to help clean and format the 2017 New Coder Survey by freeCodeCamp.

**Table of Contents**

- [Introduction](#introduction)
- [Prerequisites for data cleaning](#prerequisites-for-data-cleaning)
- [Run script to reproduce clean data](#run-script-to-reproduce-clean-data)
- [Read in combined data after cleaning](#read-in-combined-data-after-cleaning)

## Introduction

The survey data is cleaned and the metadata (i.e. the data dictionary) is in
the `datapackage.json` file, which follows closely to the specifications
described by the [data packages format][datapkg]. The format was generated with
the help of [Data Packagist][packagist].

The cleaning script `clean-data-2017.R` should be run with the working
directory in R being the root of this repository.

[datapkg]: http://specs.frictionlessdata.io/simple-data-format/
[packagist]: http://datapackagist.openknowledge.io/

## Prerequisites for data cleaning

- [R][r] (>= 3.3.3)
- [dplyr][dplyr] (>= 0.5.0)

[r]: https://www.r-project.org/
[dplyr]: https://cran.r-project.org/package=dplyr

## Download survey data

The raw survey data will be downloaded into the `raw-data/` directory.

```
git clone https://github.com/freeCodeCamp/2017-new-coder-survey.git
cd 2017-new-coder-survey/clean-data
```

## Run script to reproduce clean data

The clean and combined data will appear in the `clean-data/` directory.

```
make cleaning
```

## Read in combined data after cleaning

From within R, you can run the following to read in the clean data.

```r
library(dplyr)
setwd("clean-data") # Change this accordingly
survey <- read.csv("2017-fCC-New-Coders-Survey-Data.csv",
                   stringsAsFactors = FALSE) %>% tbl_df()
```
