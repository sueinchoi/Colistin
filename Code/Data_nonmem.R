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
?write.csv
