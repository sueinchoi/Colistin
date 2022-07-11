library(tidyverse)
library(readxl)
library(zoo)
library(plotly)
library(stringr)

files <- list.files('Data')
files

data <- read_excel('Data/CRF_ colistin pk in ICU_20220704.xlsx', sheet = 1, skip =1)

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
    select(-comorb_9, -comorb_10) 

a <- c(NA, '2', NA, '4')

sumna <- function(x){
    sum(is.na(x))
}
covariate_host_tidy %>%
    summarise_at(vars(5:14), sumna) %>%
    data.frame()

head(covariate_host_tidy)

colnames(covariate_host_tidy)[c(2, 3, 13, 14, 15, 16, 17, 18, 20, 21)] <- c("Sex", "Age", "CKD", "HD", "CRRT", "ECMO", "Infection_site", "Bacteria_name", "Colistin_MIC", "Anti_sensitivity")

covariate_host_tidy_comorb <- covariate_host_tidy %>%
    mutate_at(vars(starts_with('comorb_')), str_replace_all, " ", "") %>%
    mutate_at(vars(starts_with('comorb_')), str_replace_all, "h/o", "") %>%
    select(1, starts_with('comorb_'))

organism <- str_split(covariate_host_tidy$Anti_sensitivity, "\n", n = 2) %>%
    map_chr(1)

covariate_host_tidy_anti <- covariate_host_tidy %>%
    select(-starts_with('comorb_')) %>%
    mutate(Organism = organism_tidy) %>%
    select(-Anti_sensitivity)


covariate_host_tidy_anti %>%
    mutate(ID = as.factor(ID), Sex = factor(Sex, levels = c(0, 1), labels = c("Female", "Male"))) %>%
    mutate_at(vars(CKD:ECMO, Bacteremia), as.factor) %>%
    as.data.frame() %>%
    summary()

table(covariate_host_tidy_anti$Infection_site)
table(covariate_host_tidy_anti$Bacteria_name)
table(covariate_host_tidy_anti$Colistin_MIC)
table(covariate_host_tidy_anti$Organism)

organism_tidy <- covariate_host_tidy_anti$Organism %>%
    str_split(": ", n = 2, simplify = TRUE) %>%
    as.data.frame() %>%
    rename(blank = 1, organism = 2) %>%
    mutate(organism = ifelse(organism == "", NA, organism)) %>%
    pull(organism)


covariate_host_tidy_comorb %>%
    gather('number', 'comorb', -c(ID)) %>%
    arrange(ID) %>%
    drop_na() %>%
    select(-number) %>%
    group_by(comorb) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    arrange(comorb) %>%
    as.data.frame() %>%
    mutate(comorb = str_replace_all(comorb, "r/o", ""))

# 폐렴의 예후에 맞게 어느 정도 grouping 가능할지?


 ## Effect dadta ###

head(covariate_efficacy)


labs <- covariate_efficacy %>%
    select(ID, Day, CRP, Cr, Hct, albumin, Procalcitonin) %>%
    mutate(ID = na.locf(ID, fromLast = F), ID = as.factor(ID))

# 튀는 값 확인

labs %>%
    summary()

labs_fun <- function(i){
labs %>%
    left_join(covariate_efficacy_survmark, by = "ID") %>%
    ggplot(aes(x = Day, y = {{ i }}, col = as.factor(Decision))) +
        geom_line() +
        facet_wrap(vars(ID), scales = "free_y")
}

labs_fun(CRP)
labs_fun(Cr)
labs_fun(Hct)
labs_fun(albumin)
labs_fun(Procalcitonin)


covariate_efficacy_survival <- covariate_efficacy %>%
    select(ID, Day, starts_with('치료성공여부'), Survival) %>%
    mutate(ID = na.locf(ID, fromLast = F), ID = as.factor(ID)) %>%
    rename('Success_clinical' = 3, 'Success_micro' = 4)

covariate_efficacy_survival %>%
    filter(!(is.na(Survival) & is.na(Success_clinical) & is.na(Success_micro))) %>%
    as.data.frame() %>%
    head(30)
    
covariate_efficacy_survmark <- covariate_efficacy_survival %>%
    filter(!is.na(Survival)) %>%
    mutate(Decision = ifelse(Survival == '생존' | Survival == '모름', 0, 1)) %>%
    as.data.frame() %>%
    head(30) %>%
    select(ID, Decision)
 