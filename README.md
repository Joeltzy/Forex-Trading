# Project Summary
This project contains part of the code required for Assignment 2 of the NUS module: ST3233 Time Series Analysis. The topic question was to use predictive analytics to see if we can accurately predict Forex Market Direction.

## Built With
The file included is written in R programming language and was done on RStudio. RStudio is a free and open-source integrated development environment for R, a programming language for statistical computing and graphics. RStudio is available for download [here](https://www.rstudio.com/products/rstudio/download/).

## Running the Project
1. Download the file forex_trading.R and open it using RStudio
2. Download the dataset Euro.US.m1 2018 csv. 
3. Not all the dependencies used in the project is included in RStudio by default. Hence, Click on `Tools` and add packages in RStudio and add the missing packages before running the code:
```
suppressMessages(library(fpp))
library_list = c("ggplot2", "forecast", "Metrics", "modeest", "keras", "data.table", "quantmod", "TTR",
                 "e1071", "corrplot", "pROC", "FSelector", "kernlab", "klaR", "Boruta", "dplyr", "anytime", "knitr")
lapply(library_list, require, character.only = TRUE)
```

