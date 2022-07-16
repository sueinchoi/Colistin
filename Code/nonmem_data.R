library(tidyverse)
data <- read_csv('Data_tidy/nonmem_all.csv')
head(data)

data %>%
    filter(CMT %in% c('DEPOT', 'CMS')) %>%
    write.csv('Data_tidy/nonmem_cms.csv', row.names=F)
