library(tidyverse)
data <- read_csv('Data_tidy/nonmem_all_removed.csv', na = ".")
data %>%
    head(5)
data <- data %>%
    mutate(RATE = ifelse(MDV == 1, AMT/DUR, NA))

cmt <- data %>%
    pull(CMT) %>%
    unique()
cmt

for(i in 1:(length(cmt)-1)){
    data %>%
    filter(CMT == cmt[6] | CMT == cmt[i]) %>%
    select(-ADDL_diff, -DATETIME) %>%
    write.csv(paste0('Data_tidy/nonmem_', cmt[i], '.csv'), row.names = F, na = ".")
}


data <- read_csv('Data_tidy/nonmem_covariates.csv', na = ".")
head(data)
library(zoo)
data_cmt6 <- data %>%
    filter(CMT %in% c(3, 5)) %>%
    group_by(ID, TIME) %>%
    summarise(DV = sum(DV)) %>%
    ungroup() %>%
    mutate(CMT = 6) %>%
    full_join(data, by = c("ID", "TIME", "CMT", "DV")) %>%
    arrange(ID, TIME, CMT) %>%
    mutate_at(vars(MDV, TAD:eGFRM), function(x){ifelse(is.na(x), na.locf(x), x)}) 

data_cmt6_1 <- data_cmt6 %>%
    filter(is.na(CMT)) %>%
    mutate(CMT = 1)
data_cmt6_2 <- data_cmt6 %>%
    filter(is.na(CMT)) %>%
    mutate(CMT = 2)
data_cmt6_3 <- data_cmt6 %>%
    filter(is.na(CMT)) %>%
    mutate(CMT = 3)
data_cmt6_4 <- data_cmt6 %>%
    filter(is.na(CMT)) %>%
    mutate(CMT = 4)
data_cmt6_5 <- data_cmt6 %>%
    filter(is.na(CMT)) %>%
    mutate(CMT = 5)
data_cmt6_6 <- data_cmt6 %>%
    filter(is.na(CMT)) %>%
    mutate(CMT = 6)
data_cmt6_1
data_cmt6 %>%
    rbind(data_cmt6_1, data_cmt6_2, data_cmt6_3, data_cmt6_4, data_cmt6_5, data_cmt6_6) %>%
    filter(!is.na(CMT)) %>% 
    arrange(ID, TIME, CMT) %>%
    filter(CMT %in% c(1, 6)) %>%
    mutate(CMT = ifelse(CMT == 6, 3, CMT)) %>%
    write.csv(paste0('Data_tidy/nonmem_covariates_combined.csv'), row.names = F, na = ".")


data <- c("a1", "a1", "a2", "b1", "a3")
?as.factor
data <- factor(data, levels = unique(data)) %>% as.numeric
data
levels(data) <- 1:4
data
