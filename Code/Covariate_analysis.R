library(tidyverse)
library(readxl)
library(zoo)
library(plotly)
library(stringr)
library(xlsx)

data <- read_csv("Data/CRF_ colistin pk in ICU_20220829.csv", encoding = 'utf-8', skip = 2)
?read_csv
head(data)
data <- xlsx::read.xlsx("Data/CRF_ colistin pk in ICU_20220829.xlsx", sheetIndex = 1, encoding = 'utf-8', skip = 2)
head(data)
