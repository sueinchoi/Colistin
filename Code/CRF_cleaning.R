library(tidyverse)
library(readxl)
library(zoo)
library(plotly)
library(stringr)

files <- list.files('Data')
files

data <- read_excel('Data/CRF_ colistin pk in ICU_20220704.xlsx', sheet = 1, skip =1)
head(data)

colnames(data)

colnames(data)[c(5, 16, 19, 22, 26, 28, 40)] <- c("Date", "Charson", "Bacteremia", "Survival", "중환자실재원일수", "BT", "사망여부")

data <- data %>%
    select(-2, -3, -39, -40)
colnames(data)[1] <- "ID"

covariate_host <- data %>%
    select(1, 4, 5, 6, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19)
covariate_efficacy <- data %>%
    select(1, 2, 3, 20:33)
dosing_event <- data %>%
    select(1, 2, 3, 34)

head(covariate_host)
covariate_host_tidy <- covariate_host %>%
    filter(!is.na(ID)) %>%
    separate('기저질환', paste0('comorb_', c(1:10)), sep = ",") %>%
    select(-comorb_9, comorb_10) 

head(covariate_host_tidy)
a <- c(NA, '2', NA, '4')
sum(is.na(a))
covariate_host_tidy

sumna <- function(x){
    sum(is.na(x))
}
covariate_host_tidy %>%
    summarise_at(vars(5:14), sumna) %>%
    data.frame()
head(covariate_host_tidy)
covariate_host_tidy %>%