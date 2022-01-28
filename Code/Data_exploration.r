library(tidyverse)
library(readxl)
library(NonCompart)
library(lubridate)
library(zoo)

########### Dosing info ########

Dose <- c(300, 210, 190, 300, 250)


########### PK data #########
free_colistinA <- read_excel('Data/colistin_pk_1st.xlsx', range = 'A6:J20')
free_colistinB <- read_excel("Data/colistin_pk_1st.xlsx", range = "M6:V20")
Total_colistinA <- read_excel("Data/colistin_pk_1st.xlsx", range = "A29:J43")
Total_colistinB <- read_excel("Data/colistin_pk_1st.xlsx", range = "M29:V43")
Total_CMS <- read_excel("Data/colistin_pk_1st.xlsx", range = "A69:J83")


cleanfun <- function(dataframes) {
        data_list <- list()
        param_name <- deparse(substitute(dataframes))
        for(i in 1:(ncol(dataframes)/2)) {
            i2 <- 2*i
            data_list[[i]] <- dataframes[, c(i2-1, i2)]
        }
        #map_df(data_list, ~as.data.frame(.x), .id = "id")
        data <- bind_rows(data_list, .id = "ID") 
        colnames(data) <- c('ID', 'Time', 'DV')
        data %>%
            mutate(TYPE = param_name)
        }

free_colistinA_clean <- cleanfun(free_colistinA)
free_colistinB_clean <- cleanfun(free_colistinB)
Total_colistinA_clean <- cleanfun(Total_colistinA)
Total_colistinB_clean <- cleanfun(Total_colistinB)
Total_CMS_clean <- cleanfun(Total_CMS)

Total_data <- bind_rows(free_colistinA_clean, free_colistinB_clean, Total_colistinA_clean, Total_colistinB_clean, Total_CMS_clean) %>%
                filter(Time != 0)

########## Sampling time ###########

sampling <- read_csv('Data/samplingtime.csv', col_names = T, col_types = c('c', 'n', 'n', rep('T', 13))) %>% as.data.frame() %>%
    mutate_at(vars(TIME6:TIME13), ymd_hm)

# Dosing info로 추후 업데이트 예정 
starttime <- sampling %>%
    gather('SEQ', 'TIME', -c('ID', 'DOSE', 'INT')) %>%
    arrange(ID, DOSE, TIME) %>%
    group_by(ID) %>%
    slice(1) %>%
    mutate(FIRSTTIME = TIME)

sampling_clean <- sampling %>%
    gather("SEQ", "TIME", -c("ID", "DOSE", "INT")) %>%
    arrange(ID, DOSE, TIME) %>%
    left_join(starttime, key = c('ID', 'DOSE', 'INT')) %>%
    mutate(FIRSTTIME = na.locf(FIRSTTIME), INT = int_length(interval(FIRSTTIME, TIME))/3600) %>%
    filter(!is.na(TIME))

Total_data_real <- Total_data %>% 
    mutate(REALTIME = rep(sampling_clean$INT, 5)) %>%
    mutate_at(vars(c(Time, DV)), as.numeric) %>%
    mutate(DV = DV/1000) %>%      #Unit change to mg/L
    spread(key = 'TYPE', value = 'DV') %>%
    mutate(Total_colistin = Total_colistinA + Total_colistinB, Total_free_colistin = free_colistinA + free_colistinB) %>%
    gather(key = 'TYPE', value = 'DV', -c('ID', 'Time', 'REALTIME')) %>%
    mutate(SS = ifelse(REALTIME > 48, 1, 0))

Total_data_real %>%    
    filter(TYPE %in% c("Total_colistin"), SS == 0) %>%
    ggplot(aes(x = Time, y = DV, col = TYPE)) +
    geom_point() +
    geom_line() +
    facet_grid(ID ~.) +
    theme_classic()

a <- 'Total_free_colistin'
a <- 'Total_CMS'
Total_data_real %>%
    mutate(Time = as.numeric(Time), DV = as.numeric(DV)) %>%
    filter(TYPE == a) %>%
    ggplot(aes(x = Time, y = DV, col = ID)) +
    geom_point() +
    geom_line() +
    facet_grid(. ~ SS, scales = "free_x") +
    theme_classic() +
    ggtitle(a) +
    labs(x = "Time (hr)",
        y = "Concentration (mg/L)")

Total_data_real %>%
    mutate(SS = ifelse(REALTIME > 48, 1, 0)) %>%
    filter(SS == 1) %>%
    split(.$TYPE) %>%
    map(~as.data.frame(.x)) %>%
    map_df(~tblNCA(.x, key = "ID", colTime = "REALTIME", colConc = "DV", dur = 0.5, dose = 300, adm = "Infusion", timeUnit = "h", concUnit = "ug/mL", R2ADJ = -1), .id = "TYPE") %>%
    group_by(TYPE) %>%
    summarize_all(mean, na.rm = T) %>%
    select(TYPE, CMAX, LAMZHL, AUCLST)

Total_data_real %>%
    filter(TYPE == 'Total_colistin') %>%
    as.data.frame() %>%
    tblNCA(key = "ID", colTime = "REALTIME", colConc = "DV", dur = 1, dose = 300, adm = "Infusion", timeUnit = "h", concUnit = "ug/mL", R2ADJ = -1)
