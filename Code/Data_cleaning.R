library(tidyverse)
library(readxl)

data <- read_excel('Data/colistin_pk_result.xlsx', range ='O6:X20', sheet = 1)
data <- data %>%
    slice(-1, -2)
head(data)
result <- c()
for(i in 1:length(colnames(data))){
    if(i %% 2 == 1) {
        result <- result
        } else {
        result_pre <- data.frame(ID = i/2, Value = pull(data[, i]))
        result <- rbind(result, result_pre)
        }
}
result_cms_a <- result %>%
    drop_na() %>%
    mutate(CMT = 'CMS_A')

result_cms_b <- result %>%
    drop_na() %>%
    mutate(CMT = 'CMS_B')
result_cms_b

result_b <- result %>%
    drop_na() %>%
    mutate(CMT = 'Colistin_B')

length(colnames(data))
data[, 2]



data <- read_excel("Data/colistin_pk_result.xlsx", range = "D50:S65", sheet = 2) %>%
    mutate_all(as.numeric)
head(data)
data <- data %>%
    slice(-1)
head(data)
data[1, ] <- 0
data <- gather(data, key = "ID", value = "CMS_A") %>%
    mutate(ID = parse_number(ID))

