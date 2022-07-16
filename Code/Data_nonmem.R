library(tidyverse)
data <- read_csv('Data_tidy/nonmem_all.csv')

cmt <- data %>%
    pull(CMT) %>%
    unique()
cmt
for(i in 2:length(cmt)){
    data %>%
    filter(CMT == cmt[1] | CMT == cmt[i]) %>%
    write.csv(paste0('Data_tidy/nonmem_', cmt[i], '.csv'), row.names = F)
}

