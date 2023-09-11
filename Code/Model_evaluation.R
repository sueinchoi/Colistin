library(tidyverse)
library(plotly)
library(xpose4)

?xpose4
xpose4()
setwd("Model/Combined_model")
a <- read.table("Model/Base_model/sdtab010_c1", header = T, skip = 1)
head(a)

a %>%
  filter(MDV == 0) %>%
  plot_ly(x = ~TIME, y = ~IPRED, color = ~as.factor(ID), type = 'scatter', mode = 'markers')%>%
  add_trace(y= ~DV, color = ~as.factor(ID), name = 'DV', mode = 'line+makers')

a %>%
  filter(MDV == 0) %>%
  plot_ly(x = ~IPRE, y= ~DV, size = ~TIME, color = ~as.factor(ID))

a %>%
  filter(MDV == 0) %>%
  plot_ly(x = ~TIME, y= ~CWRES, size = ~TIME, color = ~as.factor(ID))

a %>%
  filter(MDV == 0) %>%
  ggplot(aes(x = TIME, y = IPRED)) +
  geom_point(color = 'red') +
  geom_point(aes(x=TIME, y=DV)) +
  facet_wrap(vars(CMT), scales= "free_y")


a %>%
    filter(MDV == 0) %>%
    ggplot(aes(x = IPRED, y = DV)) +
    geom_smooth() +
    geom_point(color = "red") +
    facet_wrap(vars(CMT), scales = "free")
