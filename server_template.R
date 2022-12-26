# setup ----

# 1. packages

library(deSolve)
library(plyr)
library(grid)
library(compiler)
library(shinyTime)
library(lubridate)
library(TeachingDemos)
library(rmarkdown)
library(knitr)
library(DT)
library(rsconnect)
library(tidyverse)
library(rhandsontable)
library(mrgsolve)

code <- '
$SET request=""

$PARAM TVCL=1.5, TVVC=23.4, ETA1=0, ETA2=0

$CMT CENT

$PKMODEL ncmt=1

$SIGMA 0

$MAIN
double CL = TVCL*exp(ETA1);
double V =  TVVC*exp(ETA2);

$TABLE
capture DV = (CENT/V);

'

mod <- mcode_cache("map", code)



# 1. functions

calculate_crcl <- function(age, weight, sex, scr) {
  crcl <- ((140 - age) * weight * ifelse(sex == "Female", 0.85, 1)) / (72 * scr)
  return(crcl)
}

init <- c(ETA1 = -0.3, ETA2 = 0.2)

mapbayes <- function(eta, d, ycol, m, dvcol = ycol, pred = FALSE) {
  sig2 <- as.numeric(sigma)
  eta <- as.list(eta)
  names(eta) <- names(init)
  eta_m <- eta %>%
    unlist() %>%
    matrix(nrow = 1)
  m <- param(m, eta)
  out <- mrgsim(m, data = d, output = "df")
  if (pred) {
    return(out)
  }
  # http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3339294/
  sig2j <- out[[dvcol]]^2 * sig2
  sqwres <- log(sig2j) + (1 / sig2j) * (d[[ycol]] - out[[dvcol]])^2
  nOn <- diag(eta_m %*% omega.inv %*% t(eta_m))
  return(sum(sqwres, na.rm = TRUE) + nOn)
}

# 2. constants

TVCL1 <- 20 # L
TVVC1 <- 100 # /h

omega <- cmat(0.23, -0.78, 0.62)
omega.inv <- solve(omega)
sigma <- matrix(0.0032)


DF = data.frame(
    Date = c(seq(from=Sys.Date(), by = "days", length.out = 3)),
    Time = c("00:00", "08:00", "16:00"),
    Dose = rep(5, 3),
    Dur = rep(1, 3),
    stringsAsFactors = FALSE
    )



# main ----


shiny::shinyServer(function(input, output) {
  
  
  # dosing_history_contents ----
  ## input data 
  values <- reactiveValues(data = DF)
  output$hot <- renderRHandsontable({
    rhandsontable(values$data)
  })
  DF_r <- eventReactive(input$saveBtn, {
    as.data.frame(isolate(hot_to_r(input$hot)))
  })

  output$test <- renderPrint({
    if(input$dose_type == 'csv'){
              read_csv(input$file1$datapath, col_types = cols(
                Date = col_date(),
                Time = col_time(),
                Dose = col_double(),
                Dur = col_double()
              )) %>%
                as.data.frame()
    } else {
        DF_r()
    }
   
  })


  output$csvbox <- renderPrint({
    DF
  })

  # creatinine_clearance ----
  
  output$creatinine_clearance <- renderText({
    return(calculate_crcl(input$age, input$weight, input$sex, input$scr) %>% round(digits = 2))
  })
  


  sim.data <- reactive({
    
    # prelude 3. sim.data ----
    dosedata <- if(input$dose_type == 'csv'){
              read_csv(input$file1$datapath, col_types = cols(
                Date = col_date(),
                Time = col_time(),
                Dose = col_double(),
                Dur = col_double()
                )) %>%
                as.data.frame()
              } else {
              DF_r()
              }

    dosedata_tidy <- dosedata %>%
      mutate(Date = as.character(Date), Time = str_sub(as.character(Time), 1, 5), Date_time = paste(Date, Time), Date_r = ymd_hm(Date_time))
    
    dosedata_tidy$time = interval(dosedata_tidy$Date_r[1], dosedata_tidy$Date_r) / hours(1)

    dosedata_f <- dosedata_tidy %>%
      mutate(ID = 1, cmt = 1, evid = 1, DV = NA, rate = Dose / Dur) %>%
      select(ID, time, evid, Dose, rate, cmt, DV) %>%
      rename(amt = Dose)

    
    # input: Observation
    if(input$Observations == '1'){
      obs <- data.frame(DV = input$obsc, Time = input$obsTime, Date = input$obsDate)
    } else{
      obs <- data.frame(DV = c(input$obsc1, input$obsc2), Time = c(input$obsTime1, input$obsTime2), Date = c(input$obsDate1, input$obsDate2))
    }
    
    obs_tidy <- obs %>%
      mutate(
        ID = 1, cmt = 0, evid = 0, rate = 0, amt = 0,
        Time = format(Time, "%H:%M"),
        Date = as.character(Date),
        Date_time = paste(Date, Time),
        Date_r = ymd_hm(Date_time)
      )

    
    obs_tidy$time <- interval(dosedata_tidy$Date_r[1], obs_tidy$Date_r) / hours(1)

    obs_f <- obs_tidy %>%
      select(ID, time, evid, amt, rate, cmt, DV)


    dose_con_data <- rbind(dosedata_f, obs_f) %>%
      arrange(ID, time, desc(evid))

    # Typical Values - 
    
    eGFR <- calculate_crcl(input$age, input$weight, input$sex, input$scr)
    TVCL = eGFR * TVCL1
    TVVC = input$weight * TVVC1 


    shiny::withProgress(
      message = 'Minimization in progress', 
      min = 0, max = 100, value = 99, {
        FIT <- nloptr::newuoa(init, mapbayes, ycol = "DV", m = mod, d = dose_con_data)
        print(FIT$par)
      })
    
    pdata <- dose_con_data %>% filter(evid == 1)
    pmod <- mod %>% update(end = ceiling(max(dose_con_data$time) / 24) * 24 + 24 * 3, delta = 0.05)

    pred <- mapbayes(FIT$par, ycol = "DV", pdata, pmod, pred = TRUE) %>%
                as.data.frame() %>%  
                filter(time > 0) %>% 
                rename(DV_pred = DV)

    initial <- mapbayes(c(ETA1 = 0, ETA2 = 0), ycol = "DV", pdata, pmod, pred = TRUE) %>% 
                as.data.frame() %>%
                filter(time > 0) %>% 
                rename(DV_init = DV)

    pred_initial_result <- left_join(pred, initial, by = c("ID", "time")) %>%
                          select(-ID)

    obs_pred_result <- obs_f %>%
        select(time, DV) %>%
        left_join(pred, by=c("time"))

    colistin_pk_plot <- pred_initial_result %>%
      gather(key = "Pred", value = "value", -time) %>%
      ggplot() +
      geom_line(aes(x = time, y = value, color = Pred, linetype = Pred), alpha = 0.8, lwd = 1) +
      geom_point(
        data = obs_pred_result,
        aes(x = time, y = DV, fill = "Observed concentration"),
        color = "red",
        size = 4, alpha = 0.5
      ) +
      scale_linetype_manual(" ",
        values = c(
          "DV_pred" = 1,
          "DV_init" = 4
        ),
        labels = c("Predicted individual concentration", "Population concentration")
      ) +
      scale_color_manual(" ",
        values = c(
          "DV_pred" = "firebrick",
          "DV_init" = "darkgreen"
        ),
        labels = c("Predicted individual concentration", "Population concentration")
      ) +
      scale_fill_manual(" ",
        values = c("Observed concentration" = "red")
      ) +
      # geom_hline(yintercept = c(50, 200), color = 'red') +
      labs(
        x = "Time (hour)", y = "Colistin concentration (ng/mL)",
        title = "Concentration curve of colistin",
        color = " "
      ) +
      theme_bw()

    sim_data_output <- list(
      param_eta = FIT$par,
      table1 = dose_con_data,
      table2 = tibble(`CL(L/h)` = TVCL*(exp(FIT$par[1])),
                      `V(L)` = TVVC*(exp(FIT$par[2]))),
      plot1 = colistin_pk_plot
    )
    
    return(sim_data_output)
  })
  
  output$outputtable1 <- renderTable({
        sim_data_output <- sim.data()
        return(sim_data_output$table1)
  })

  output$outputtable2 <- renderTable({
    sim_data_output <- sim.data()
    return(sim_data_output$table2)
  })

  # plot 1 ----
  output$plotCONC <- renderPlot({
    sim_data_output <- sim.data()
    return(sim_data_output$plot1)
  }, res = 100)
  
  # plot 2 ----
  output$plotCONC2 <- renderPlot({
    sim_data_output <- sim.data()
    
    dose_con_data <- sim_data_output$table1
    last_time_dose <- dose_con_data %>% filter(evid == 1) %>% pull(time) %>% max()

    pmod <- mod %>% update(end = 24*8, delta = 0.1)

    new_dose <- c(ID = 1, time = last_time_dose + input$tau, ii = input$tau, amt = input$amt, addl = 24*8/input$tau, evid = 1, rate = input$amt / input$dur, cmt = 1, DV = NA)

    pdata <- rbind(dose_con_data %>% filter(evid == 1) %>% mutate(addl = 0, ii = 0), new_dose)




    pred <- mapbayes(c(ETA1 = 0, ETA2 = 0), ycol = "DV", pdata, pmod, pred = TRUE) %>%
      as.data.frame() %>%
      filter(time > 0)
    
    colistin_dose_adjustment_plot <- pred %>% 
      as_tibble() %>% 
      ggplot(aes(time, DV)) + 
      geom_line(alpha = 0.5) +
      geom_hline(yintercept = input$ul, color = 'red') +
      geom_hline(yintercept = input$ll, color = 'blue') +
      geom_vline(xintercept = last_time_dose, color = 'red', alpha = 0.3) +
      labs(x = 'Time (hour)', y = 'Colistin concentration (ng/mL)',
           title = 'Concentration of colistin') +
      theme_bw()

    colistin_dose_adjustment_plot
  })
  # end ----  
})
