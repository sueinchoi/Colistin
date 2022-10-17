library(tidyverse)
library(readxl)
library(zoo)
library(plotly)
library(stringr)

data <- read_excel("Data/CRF_ colistin pk in ICU_20220829.xlsx", sheet = 1, skip = 2)
head(data)
colnames(data)
data_basic <- data %>%
    select(ID, Sex, Age, Height, Weight, BSA, SOFA, APACHE, Comorbidity, CKD, HD, CRRT, ECMO, 18) %>%
    mutate(ID = ifelse(is.na(ID), na.locf(ID), ID)) %>%
    group_by(ID) %>%
    slice(1) %>%
    ungroup()

head(data_basic)

data_basic %>%
    summary()

data_basic %>%

data_basic <- data_basic %>%
    mutate(BSA_c = 0.008883*Weight^0.444*Height^0.663) %>%
    as.data.frame()

data_basic_join <- data_basic %>%
    select(-Comorbidity, -HD, -CRRT, -ECMO, -BSA)

data_basic_join %>%
    summary()
# Height median : 166, Weight median: 60, Age median : 68, Charson_index median : 4, BSA median : 1.609


nonmem_data <- read_csv('Data_tidy/nonmem_cmsAB.csv')

head(nonmem_data)

nonmem_data_join1 <- nonmem_data %>%
    left_join(data_basic_join, by = "ID") %>%
    mutate(BSA = round(BSA_c, 2)) %>%
    select(-BSA_c) %>%
    as.data.frame() 

na.locf.fun <- function(x){
    ifelse(is.na(x), na.locf(x), x)
}
head(data_lab)
nonmem_data_join1 %>%
    full_join(data_lab, by = c("ID", "TIME")) %>%
    as.data.frame() %>%
    mutate(MDV = ifelse(is.na(MDV), 1, MDV)) %>%
    arrange(ID, TIME, CMT) %>%
    mutate_at(vars(Sex, Age, Height, Weight, SOFA, APACHE, CKD, Charson_index, BSA, Hct, Cr, albumin, CRP, Volume_cum), na.locf.fun) %>%
    select(-Day, -Input, -Output, -Volume) %>%
    write.csv('Data_tidy/nonmem_covariates.csv', na = ".", row.names = FALSE)

colnames(data)
data_lab <- data %>%
    select(ID, Day, Hct, Cr, albumin, CRP, Input, Output) %>%
    mutate(Volume = Input - Output) %>%
    mutate(ID = ifelse(is.na(ID), na.locf(ID), ID)) %>%
    mutate(TIME = Day * 24) %>%
    select(ID, TIME, everything()) %>%
    group_by(ID) %>%
    mutate(Volume_cum = cumsum(Volume)/1000) %>%
    ungroup()

data_lab %>%
    group_by(ID) %>%
    summarise(sum(!is.na(Procalcitonin))) %>%
    as.data.frame()

head(nonmem_data)

head(data_lab)
