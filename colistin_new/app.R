# 1. package for ui ----

library(shinydashboard)
library(shinyTime)
library(lubridate)
library(shinythemes)
library(rhandsontable)


# 2. packages for server ----

library(lubridate)
library(DT)
library(rsconnect)
library(tidyverse)
library(mrgsolve)
library(NonCompart)
library(purrr) # for pmap_dbl()
library(knitr) 
# setup ----


code <- '
$SET request=""

$PARAM @as_object

list(TVCL = 19, 
     TVV1 = 100,
     TVV2 = 100, 
     TVQC = 10,
     TVK13 = 0.01,
     TVCLMA = 3,
     TVQM = 1, 
     TVK15 = 0.001,
     TVCLMB = 2
     )

$CMT CENT PERI MCENTA MPERIA MCENTB

$OMEGA @labels ETA1 ETA2 ETA3 ETA4 ETA5 ETA6 ETA7 ETA8 ETA9

0.23 -0.78 0.62 0 0 0 0 0 0 

$MAIN

double CL = TVCL*exp(ETA1);
double V1 =  TVV1*exp(ETA2);
double V2 =  TVV2*exp(ETA3);
double QC =  TVQC*exp(ETA4);
double K13 =  TVK13*exp(ETA5);
double CLMA =  TVCLMA*exp(ETA6);
double QM =  TVQM*exp(ETA7);
double K15 =  TVK15*exp(ETA8);
double CLMB =  TVCLMB*exp(ETA9);
double V3 = V1;
double V4 = V2;
double V5 = V1;

double K10 = CL/V1-K13-K15;
double K12 = QC/V1;
double K21 = QC/V2;
double K30 = CLMA/V3;
double K34 = QM/V3;
double K43 = QM/V4;
double K50 = CLMB/V5;

$ODE

dxdt_CENT = - K12*CENT - K10*CENT + K21*PERI - K13*CENT - K15 * CENT;
dxdt_PERI = K12*CENT - K21*PERI;
dxdt_MCENTA = K13*CENT - K30*MCENTA - K34*MCENTA + K43*MPERIA;
dxdt_MPERIA = K34*MCENTA - K43*MPERIA;
dxdt_MCENTB = K15*CENT - K50*MCENTB;


$TABLE
capture DV1 = (MCENTA/V3*1000);
capture DV2 = (MCENTB/V5*1000);
capture DV= DV1 + DV2;
'


mod <- mcode_cache("map", code)

# 1. functions

calculate_crcl <- function(age, weight, sex, scr) {
  crcl <- ((140 - age) * weight * ifelse(sex == "Female", 0.85, 1)) / (72 * scr)
  return(crcl)
}

sigma <- matrix(0.0032)


# 2. constants

CLCR_exp <- 1
Albumin_exp <- 1
AGE_exp <- 1
WT_exp <- 1
TVCL_base <- 19
TVV1_base <- 200

omega <- dmat(0.23, -0.78, 0.62, 0, 0, 0, 0, 0, 0)
sigma <- matrix(0.0032)


tblNCA2 <- function(concData, key = "Subject", 
                    colTime = "Time", colConc = "conc", 
                    down = "linear", t1 = 0, t2 = 0, ...){
  # tblNCA() and data calculation
  input_tbl <- tblNCA(as.data.frame(concData), 
                      key, colTime, colConc, down = down, ...) %>% # calculation
    as_tibble() %>% mutate_all(as.character) %>% 
    left_join(concData %>% 
                as_tibble() %>% mutate_all(as.character) %>% 
                group_by_(.dots = key) %>% # grouping by keys (Subject)
                summarise_(x = sprintf('list(%s)', colTime), 
                           y = sprintf('list(%s)', colConc)))
  
  # calculation of IntAUC()
  output <- input_tbl %>% 
    mutate(Res = do.call(c, input_tbl %>% select(-x, -y) %>% apply(1, list))) %>% 
    mutate(iAUC = pmap_dbl(.l = list(x, y, Res), 
                           .f = ~IntAUC(x = as.numeric(..1), y = as.numeric(..2), 
                                        t1, t2, 
                                        Res = ..3, down = down))) %>% 
    select(-x, -y, -Res) %>% 
    mutate_at(vars(b0:iAUC), as.numeric) # character -> number
  return(output)
}

tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
)

# 3. UI main ----

ui <- shiny::navbarPage(
  
  # Title page ----
  title = "Colistin TDM",
  theme = shinytheme("spacelab"),
  # selected = 'dosa', #icon = "prescription",
  
  # Chapter 1. Patient Info `pinfo` ----
  
  tabPanel(
    # icon = icon("user-plus"),
    icon = icon("address-card"),
    title = "Patient Info",
    tabName = "pinfo",
    h2("Patient Information",
       align = "center",
       style = "color:#dbdbdb; font-weight:bold"
    ),
    tags$hr(),
    column(width = 2),
    column(
      width = 4,
      box(
        title = "Patient ID", width = NULL, solidHeadner = TRUE, status = "primary",
        textInput("pid", "", value = "003866", width = NULL, placeholder = NULL)
      ),
      
      # tags$hr(),
      
      box(
        title = "Information", status = "primary", width = NULL, solidHeader = TRUE,
        numericInput("scr", "Serum Creatinine (mg/dL)", 0.9,
                     min = NA, max = NA, step = 0.1,
                     width = NULL
        ),
        radioButtons("sex", ("Gender"),
                     inline = TRUE,
                     choices = list("Male" = "Male", "Female" = "Female"), selected = NULL, choiceName = "Male"
        ),
        numericInput("age", "Age (year)", 40, min = NA, max = NA, step = 1),
        numericInput("weight", "Weight (kg)", 70, min = NA, max = NA, step = 0.1),
        numericInput("albumin", "Albumin level", 2, min = NA, max = NA, step = 0.1)
      )
    ),
    column(
      width = 4,
      box(
        width = NULL,
        title = "Creatinine Clearance (mL/min)",
        background = "maroon",
        verbatimTextOutput("creatinine_clearance")
      ),
      p("**Cockcroff-Gault Equation")
    ),
    column(width = 2)
  ),
  
  # Chapter 5. PK profile 2 `main2` ----
  
  tabPanel(
    icon = icon("line-chart"),
    title = "Simulation",
    tabName = "sim",
    column(
      width = 3,
      box(
        width = NULL, status = "warning", solidHeader = TRUE, title = "",
        sliderInput("LDOSE", "Loading dose (mg)", 1000,
                    min = 0, max = 2000, step = 250, ticks = TRUE,
                    width = NULL
        )
      ),
      box(
        width = NULL, status = "warning", solidHeader = TRUE, title = "",
        sliderInput("DOSE", "Maintenance dose (mg)", 1000,
                    min = 0, max = 2000, step = 250, ticks = TRUE,
                    width = NULL
        )
      ),
      box(
        width = NULL, status = "warning", solidHeader = TRUE, title = "",
        radioButtons("INT", "Interval (hr)",
                      inline = TRUE,
                      choices = list(
                        "6hr" = "6", 
                        "8hr" = "8",
                        "12hr" = "12",
                        "24hr" = "24"
                        ), 
                      selected = "8"
        )
      ),
      box(
        width = NULL, status = "warning", solidHeader = TRUE, title = "",
        sliderInput("DUR", "Infusion duration", 1,
                    min = 0.5, max = 4, step = 0.5,
                    width = NULL
        )
      ),
      tags$hr(),
      box(
        width = NULL, solidHeader = TRUE, title = "", status = "primary",
        numericInput("trough", "trough levels (mg/L)", 2,
                    min = 0, max = 20, step = 0.1
        ),
        numericInput("peak", "peak levels (mg/L)", 10,
                    min = 0, max = 50, step = 1
        ),
        radioButtons("sim", "Simulation pattern",
                     inline = TRUE,
                     choices = list("Population" = "pop", 
                                    "Individual" = "ind"), 
                     selected = NULL, 
                     choiceName = "pop"
        )
      )
    ),
    column(
      width = 9,
      box(
        width = NULL, status = "primary", title = "Colistin concentration",
        plotOutput("plotsim", height = "500px")
      ),
      box(
        width = NULL, status = "primary", title = "",
        tableOutput("nca")
      )
    )
  )
)



# main ----


server <- shiny::shinyServer(function(input, output) {
  
  
  # creatinine_clearance ----
  
  output$creatinine_clearance <- renderText({
    return(calculate_crcl(input$age, input$weight, input$sex,input$scr) %>% round(digits = 2))
  })
  
  
  evt <- reactive({
    e1 <- ev(amt = input$LDOSE, ii = 24, rate = input$LDOSE/1)
    e2 <- ev(amt = input$DOSE, ii = as.numeric(input$INT), addl = 240/as.numeric(input$INT), rate = input$DOSE/input$DUR)
    
    seq(e1, e2)
  })

  
  sim.data <- reactive({
    CLCR <- calculate_crcl(input$age, input$weight, input$sex, input$scr)
    AGE <- input$age
    WT <- input$weight
    Albumin <- input$albumin
    TVCL = TVCL_base*CLCR**CLCR_exp*Albumin**Albumin_exp
    TVV1 = TVV1_base*(AGE/68)**AGE_exp*(WT/60)**WT_exp
    
    
    mod <- param(mod, list(TVCL = TVCL,
                           TVV1 = TVV1))
    
    if(input$sim == "pop"){
     
      mrgsim_e(mod, evt(), end = 240) %>%
        as.data.frame()
      
    } else {
      mod %>%
        ev(evt()) %>%
        mrgsim(nid = 100, end = 240) %>%
        as.data.frame()
    }


    })
  



  
  output$nca <- renderTable({
    if(input$sim == "pop") {
      endtime <- 240 - as.numeric(input$INT)
      nca_24 <- sim.data() %>%
        slice(-1) %>%
        filter(time >= 0 & time <= 24) %>%
        tblNCA(key = "ID", colTime = "time", colConc = "DV", dose = input$DOSE, adm = "Infusion", dur = input$DUR, doseUnit = "mg", timeUnit = "h", concUnit = "ng/mL", down = "Linear", R2ADJ = 0, MW = 0) %>%
        select(CMAX, TMAX, LAMZHL, AUCLST, AUCIFO) 
      nca_ss <- sim.data() %>%
        filter(time <= 240 & time >= 240 - as.numeric(input$INT)) %>%
        tblNCA2(key = "ID", colTime = "time", colConc = "DV", dose = input$DOSE, adm = "Infusion", dur = input$DUR, doseUnit = "mg", timeUnit = "h", concUnit = "ng/mL", down = "Linear", R2ADJ = 0, MW = 0, t1 = endtime, t2 = 240) %>%
        select(CMAX, iAUC) %>%
        rename('CMAXss' = CMAX,
               'AUCss' = iAUC) 
      cbind(nca_24, nca_ss)
    } else {
      NULL
    }

  })
  
  # plot 2 ----
  output$plotsim <- renderPlot({
    
    if(input$sim == "pop"){
      sim.data() %>%
        ggplot() +
        geom_line(aes(x = time, y = DV)) +
        geom_hline(aes(yintercept  = input$trough), color = "#268bd2") +
        geom_hline(aes(yintercept  = input$peak), color = "#cb4b16") +
        theme_classic() +
        labs(x = "Time (hr)",
             y = "Concentration (ng/mL)") +
        theme(axis.title = element_text(face = "bold", size = 14))
      
    } else {
      
      VPC <- sim.data() %>%
        group_by(time) %>%
        summarise(per95 = quantile(DV, 0.95), per05 = quantile(DV, 0.05), med = median(DV))
      
      VPC %>%
        gather("value", key = "Parameter", -time) %>%
        mutate(Parameter = factor(Parameter, levels = c("per95", "med", "per05"))) %>%
        ggplot() +
        geom_line(aes(x = time, y = value, linetype = Parameter)) +
        scale_linetype_manual(values = c("dashed", "solid", "dashed"), label = c("95% percentile", "median", "5% percentile"), limits = c("per95", "med", "per05")) +
        theme_bw() +
        geom_ribbon(data = VPC, aes(x = time, ymax = per95, ymin = per05), fill = "skyblue", alpha = 0.5) +
        theme(legend.position = "bottom",
              axis.title = element_text(size = 13),
              axis.text = element_text(size = 12),
              title = element_text(size = 12)) +
        geom_hline(aes(yintercept = input$trough), color = "#268bd2") +
        geom_hline(aes(yintercept = input$peak), color = "#cb4b16") 
    }

    
  })
  # end ----  
})


# Run the application 
shinyApp(ui = ui, server = server)
