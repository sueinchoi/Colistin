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


# 3 days ahead for starting dose in ID 19 #

data_time <- data_time %>%
    mutate(DATETIME = if_else(ID == 19 & MDV == 1, DATETIME - days(3), DATETIME), ADDL = ifelse(ID == 19 & MDV == 1, 18, NA)) 
data_time_tidy <- data_time %>%
    mutate(CMT = ifelse(is.na(CMT), 0, CMT)) %>%
    mutate(CMT = factor(CMT, levels = c(0, 1, 2, 3, 4, 5), labels = c("DEPOT", "CMS_A", "CMS_B", "Colistin_A", "Colistin_B", "CMS"))) %>%
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
    arrange(ID, TIME, CMT) %>%
    filter(ID == 7)


    
########## Plot #########

data_time_ss_clean <- data_time_tidy_ss %>%
    arrange(ID, TIME, CMT) %>%
    filter(!(ID == 7 & TAD == 24 & SS ==0 & MDV == 0)) %>%
    filter(!(ID == 17 & TAD == 12 & SS == 1 & MDV == 0)) %>%
    filter(!(ID == 19 & TAD == 11 & MDV == 0)) %>%
    filter(!(ID == 19 & TAD == 0 & SS == 1 & MDV == 0)) %>%
    filter(!(ID == 20 & DV == 3140)) %>%
    filter(!(ID == 15 & DV == 4440)) %>%
    filter(!(ID == 16 & MDV == 0 & TAD %in% c(2, 4) & SS == 1)) %>%
    filter(!(ID == 14 & MDV == 0 & TAD == 2.5 & SS == 1 & CMT == 'CMS_A')) %>%
    filter(!(ID == 5 & MDV == 0 & TAD == 24 & SS == 1 & CMT == 'CMS_A')) %>%
    filter(!(ID == 3 & MDV == 0 & TAD == 8 & SS == 1 & CMT == 'CMS_A')) %>%
    filter(!(ID == 15 & MDV == 0 & TAD == 1.5 & SS == 0 & CMT == 'CMS_B')) %>%
    filter(!(ID == 16 & MDV == 0 & TAD == 4 & SS == 0 & CMT == 'CMS_B')) %>%
    filter(!(ID == 5 & MDV == 0 & TAD == 24 & SS == 1 & CMT == 'CMS_A')) %>%
    filter(!(ID == 5 & MDV == 0 & TAD == 4 & SS == 1 & CMT == 'CMS_B')) %>%
    filter(!(ID == 11 & MDV == 0 & TAD == 4 & SS == 1 & CMT == 'Colistin_A')) %>%
    filter(!(ID %in% c(4, 5) & MDV == 0 & TAD == 1.5 & SS == 0 & CMT %in% c('Colistin_A', 'Colistin_B')))

plot_ss <- data_time_ss_clean %>%
    filter(MDV == 0, CMT != 'CMS') %>%
    filter(SS == 0) %>%
    ggplot(aes(x = TAD, y = DV, col = as.factor(ID))) +
    geom_point() +
    geom_line() +
    facet_wrap(vars(CMT), scales = "free_y") +
    scale_y_continuous(trans = "log10")

ggplotly(p = plot_ss)


plot_indiv <- data_time_ss_clean %>%
    filter(MDV == 0, CMT == 'Colistin_B') %>%
    filter(SS == 0) %>%
    ggplot(aes(x = TAD, y = DV, col = as.factor(ID))) +
    geom_point() +
    geom_line() +
    facet_wrap(vars(ID), scales = "free") +
    scale_y_continuous(trans = "log10")

ggplotly(p = plot_indiv)
head(data_time_ss_clean)

arrange <- data_time_ss_clean %>%
    select(ID, TIME, AMT, II, ADDL, DUR, DV, MDV, CMT, TAD, SS, NTAD, DATETIME) 

# ID 1번은 dosing 시간이 불확실해서 12시간 앞으로 함 #
data <- arrange %>%
    mutate(DATETIME = if_else(ID == 1 & SS == 1, DATETIME - hours(12), DATETIME)) %>%
    mutate(TIME = ifelse(SS==1 & ID == 1, TIME - 12, TIME))

data %>%
    filter(MDV == 1)

dosedata <- data %>%
    filter(MDV == 1) %>%
    group_by(ID) %>%
    mutate(Time_diff = TIME - lag(TIME), II = ifelse(is.na(II), 0, II), II_diff = II - lag(II),  addl_diff = abs(Time_diff / II_diff) - 1) %>% ungroup() %>%
    as.data.frame() %>%
    select(ID, TIME, AMT, II, ADDL, DUR, MDV, CMT, SS, ends_with('diff'))

fulldose <- c(1, 2, 3, 4, 5, 6, 7, 11, 16, 18)
diffdose <- setdiff(1:21, fulldose)

doseupdated <- dosedata %>%
    filter(ID %in% fulldose) %>%
    filter(addl_diff >= 1) %>%
    mutate(ADDL_diff = round(addl_diff)) %>%
    select(ID, TIME, ADDL_diff, MDV) %>%
    right_join(data, by = c('ID', 'TIME', 'MDV')) %>%
    mutate(ADDL = ifelse(is.na(ADDL_diff), ADDL, ADDL_diff)) %>%
    arrange(ID, TIME, CMT) %>%
    select(-ADDL_diff)

doseupdated_all <- doseupdated %>%
    filter(ID %in% diffdose, MDV == 1) %>%
    mutate(ADDL_diff = case_when(II == 8 ~ 21,
                            II == 12 ~ 14,
                            TRUE ~ NA_real_)) %>%
    select(ID, TIME, ADDL_diff, MDV) %>%
    right_join(doseupdated, by = c('ID', 'TIME', 'MDV')) %>%
    mutate(ADDL = ifelse(is.na(ADDL_diff), ADDL, ADDL_diff)) %>%
    arrange(ID, TIME, CMT) %>%
    mutate(CMT_num = as.numeric(CMT)) 
write.csv(doseupdated_all, 'Data_tidy/nonmem_all.csv', na = ".", row.names = F)

doseupdated_all %>%
    filter(CMT_num == 6 | CMT == 1) %>%
    write.csv('Data_tidy/Nonmem_CMS.csv', na = ".", row.names = F)


data_3 <- arrange %>%
    filter(MDV == 1) %>%
    pull(ID) %>%
    table() %>%
    as.data.frame() %>%
    filter(Freq == 3) %>%
    pull(".") 
?na.locf
arrange %>%
    filter(ID %in% data_3 & MDV ==1) %>%
    group_by(ID) %>%
    mutate(DIFF = TIME - lag(TIME), II = ifelse(is.na(II), na.locf(II), II)) %>%
    slice(2, 3) %>%
    ungroup() %>%
    select(ID, TIME, DIFF, II) %>%
    mutate(ADDL_pre = DIFF/II - 1)

write.csv('Data_tidy/nonmem_data.csv', na = ".", row.names = F)


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
