library(tidyverse)
library(readxl)
library(NonCompart)
library(lubridate)
library(zoo)
library(plotly)
library(IQRtools)
library(gtsummary)
data <- read_csv('Data_tidy/nonmem_covariates.csv')
head(data)

colnames(data)

data <- data %>%
    mutate(DV = as.numeric(DV), TAD = as.numeric(TAD))

data %>%
    filter(MDV == 0) %>%
    pull(TIME) %>%
    max()


#### Exploratory plot by compartment ####

# Compartment number
i <- 3

# Individual plot
data %>%
    filter(CMT == i, MDV == 0, SS == 0) %>%
    IQRggplot(aes(x = TIME, y = DV)) +
    geom_line() +
    geom_point() +
    facet_wrap(vars(ID)) +
    scale_y_continuous(trans = 'log10')

data %>%
    filter(CMT == i, MDV == 0, SS == 1, TAD >= 0) %>%
    IQRggplot(aes(x = TAD, y = DV)) +
    geom_line() +
    geom_point() +
    facet_wrap(vars(ID)) +
    scale_y_continuous(trans = "log10")

# All individual plots by steady state
data %>%
    filter(CMT == i, MDV == 0, TAD >= 0) %>%
    IQRggplot(aes(x = TAD, y = DV, col = as.factor(ID))) +
    geom_line() +
    geom_point() + 
    facet_wrap(vars(as.factor(SS)))

# Time-binning for mean calculation
data <- data %>%
    mutate(BINTIME = cut(TAD, breaks = c(0, 0.5, 1, 2, 4, 6, 8, 12, 24, Inf), right = F, labels = c(0, 0.5, 1, 2, 4, 6, 8, 12, 24))) %>%
    mutate(BINTIME = as.numeric(as.character(BINTIME)))

# Mean-sd plot by steady state
data %>%
    filter(MDV == 0, TAD >= 0) %>%
    group_by(BINTIME, SS, CMT) %>%
    summarise(mean = mean(DV, na.rm = T), sd = sd(DV, na.rm = T)) %>%
    ungroup() %>%
    filter(CMT == i) %>%
    IQRggplot() +
    geom_errorbar(aes(x = BINTIME, ymax = mean + sd, ymin = mean)) +
    geom_line(aes(x = BINTIME, y = mean)) +
    facet_wrap(vars(as.factor(SS))) +
    scale_y_continuous(trans = 'log10')


#### Exploratory of covariates - EDA ####

head(data)
library(summarytools)
install.packages('SmartEDA')
library(SmartEDA)
library(DataExplorer)

data_summary <- data %>%
    mutate_at(vars(CKD, Charson_index, APACHE, SOFA, Sex), as.character) %>%
    group_by(ID) %>%
    slice(1) %>%
    ungroup() %>%
    select(1, Sex:Volume_cum) 

tbl_summary(data_summary)
stview(dfSummary(data_summary))

stview(dfSummary(data_summary %>% select(-SOFA, -APACHE, -Charson_index),
            plain.ascii = FALSE,
            style = "grid",
            graph.magnif = 0.75,
            valid.col = FALSE,
            tmp.img.dir = "/tmp"), file = "~/test2.html")


introduce(data_summary)
plot_intro(data_summary)
plot_missing(data_summary)
plot_bar(data_summary)
plot_histogram(data_summary)
plot_correlation(data_summary)
plot_density(data_summary %>% select(-c(ID, Charson_index, SOFA)))

plot_prcomp(data_summary, variance_cap = 0.9, nrow = 2L, ncol = 2L)
ExpNumStat(data_summary, Outlier = TRUE, round = 2)[, c(1, 9:10, 12:15, 18, 23)]

ExpCatStat(data_summary, Target = "Sex")
ExpCatViz(data_summary)


#### Covariate analysis - Time-varying covariates ####

head(data)

data_crp <- data %>%
    select(ID, TIME, MDV, DV, CMT, TAD, SS, CRP) 

data_crp %>%
    filter(CMT == 2) %>%
    ggplot(aes(x = TIME, y = CRP)) +
    geom_point() +
    geom_line() + 
    facet_wrap(vars(ID))
