library(tidyverse)
library(readxl)

data <- read_excel('Data/colistin_pk_result.xlsx', range ='C48:L62', sheet = 1)
data <- data %>%
    slice(-1)
head(data)
analyte <- "CMS_A"
result <- c()
for(i in 1:length(colnames(data))){
    if(i %% 2 == 1) {
        result <- result
        } else {
        result_pre <- data.frame(ID = i/2, Value = pull(data[, i]))
        result <- rbind(result, result_pre)
        }
}

result_all <- result %>%
    drop_na() %>%
    mutate(CMT = analyte) %>%
    rbind(result_all)

result_all <- result_all %>%
    mutate(Value = round(as.numeric(Value), 2))
head(result_all)
unique(result_all$CMT)
result_all

data <- read_excel("Data/colistin_pk_result.xlsx", range = "D6:S21", sheet = 2) %>%
    mutate_all(as.numeric)
head(data)
data <- data %>%
    slice(-1)
head(data)

analyte <- "Colistin_A"
data <- gather(data, key = "ID", value = "Value") %>%
    mutate(ID = parse_number(ID), CMT = analyte) %>%
    rename(Value = 2)
head(data)
data_all <- data

data_all <- rbind(data_all %>% filter(CMT != "Colistin_A"), data)
data_all$CMT %>% unique()
head(data_all)
data_all %>%
    filter(CMT == "CMS_a")

data_conc_all <- rbind(result_all, data_all)
data_conc_all

colnames(data_conc_all)

data_conc_tidy <- data_conc_all %>%
    group_by(ID, CMT) %>%
    mutate(ROW = 1:n()) %>%
    ungroup() %>%
    filter(!(ID == 19 & Value == 0)) %>%
    spread(CMT, Value, fill = NA, convert = TRUE) %>%
    mutate(CMS = CMS_A + CMS_B) %>%
    drop_na()

data_conc_tidy_new <- data_conc_tidy %>%
    filter(!(ID == 19 & Value == 0)) 

write.csv(data_conc_tidy, "Data_tidy/Concentration_tidy.csv", row.names = F)
