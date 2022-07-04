library(tidyverse)
library(readxl)
library(NonCompart)
library(lubridate)
library(zoo)
library(plotly)

########### Dosing info ########

Dose <- c(300, 210, 190, 300, 250)


########## PK data ##########

data <- read_excel('Data/CRF2.xlsx')

data_time <- data %>%
    mutate(DATETIME = str_replace(DATETIME, '오후', 'pm'), DATETIME = str_replace(DATETIME, '오전', 'am')) %>%
    mutate(DATETIME = parse_date_time(DATETIME, '%y.%m.%d %p %H:%M:%S'))

head(data_time)


########## Tidy data for TIME calculation ###########

data_time_tidy <- data_time %>%
    mutate(CMT = factor(CMT, levels = c(1, 2, 3, 4, 5), labels = c("CMS_A", "CMS_B", "Colistin_A", "Colistin_B", "CMS"))) %>%
    select(ID, DATETIME, SS, NTAD, DV, MDV, CMT) %>%
    group_by(ID) %>%
    mutate(START = min(DATETIME)) %>%
    ungroup() %>%
    mutate(TIME = time_length(interval(START, DATETIME), "hour"))%>%
    group_by(ID, SS) %>%
    mutate(SSTART = min(DATETIME)) %>%
    ungroup() %>%
    mutate(TAD = time_length(interval(SSTART, DATETIME), "hour")) %>%
    as.data.frame() 

data_time_tidy <- data_time_tidy %>%
    mutate(DATETIME = if_else(TIME > 2500, update(DATETIME, year = 2020), DATETIME)) %>%
    mutate(TIME = time_length(interval(START, DATETIME), "hour"))

data_time_tidy_ss <- data_time_tidy %>%
    mutate(SS = ifelse(TIME < 36 & ID != 19, 0, 1)) %>%
    mutate(SSTART = if_else(ID == 19 & TIME > 12, ymd_hms('2022-01-27 19:00:00'), SSTART)) %>%
    mutate(TAD = time_length(interval(SSTART, DATETIME), "hour"))   

data_time_tidy_ss %>%
    filter(ID == 2)
########## Plot #########

plot_ss <- data_time_ss_clean %>%
    filter(MDV == 0) %>%
    filter(SS == 1) %>%
    ggplot(aes(x = TAD, y = DV, col = as.factor(ID))) +
    geom_point() +
    geom_line() +
    facet_wrap(vars(CMT), scales = "free_y")

ggplotly(p = plot_ss)

data_time_ss_clean <- data_time_tidy_ss %>%
    filter(!(ID == 7 & TAD == 24 & SS ==0)) %>%
    filter(!(ID == 17 & TAD == 12 & SS == 1)) %>%
    filter(!(ID == 19 & TAD == 11)) %>%
    filter(!(ID == 19 & TAD == 0 & SS == 1))


# Dosing info로 추후 업데이트 예정 

cleanfun <- function(dataframes) {
    data_list <- list()
    param_name <- deparse(substitute(dataframes))
    for (i in 1:(ncol(dataframes) / 2)) {
        i2 <- 2 * i
        data_list[[i]] <- dataframes[, c(i2 - 1, i2)]
    }
    # map_df(data_list, ~as.data.frame(.x), .id = "id")
    data <- bind_rows(data_list, .id = "ID")
    colnames(data) <- c("ID", "Time", "DV")
    data %>%
        mutate(TYPE = param_name)
}

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
