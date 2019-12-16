library(shiny)
library(shinythemes)
library(tidyverse)
library(forcats)
library(dslabs)


dat <- read.csv("finaldata.csv") 
datfinal <- dat %>% filter(dat$"totaldays" > 0 & dat$"totaldays" < 100000)

histvariables <- c("rated9_10",
                   "recommend_definitely",
                   "pain_always",
                   "training_definitely",
                   "communicated_always",
                   "respect_always",
                   "helped_always",
                   "emotionalsupport",
                   "total_medicare_payment",
                   "total_medicare_standard_payment",
                   "total_charge",
                   "medicare_return_rate",
                   "medicare_payment_pbeneday",
                   "medicare_standard_payment_pbeneday",
                   "charge_pbeneday",
                   "totaldays",
                   "total_live_discharges",
                   "discharges_pbeneday",
                   "age",
                   "male_pct",
                   "advantage_pct",
                   "medicaid_pct",
                   "benefgr180days_pct",
                   "benefgr60days_pct",
                   "benefls7days_pct",
                   "calcavglos",
                   "cancer_pct",
                   "dementia_pct",
                   "stroke_pct",
                   "heart_pct",
                   "resp_pct",
                   "otherdx_pct",
                   "home_health_pct",
                   "snf_pct",
                   "social_services_pct",
                   "site_home_pct",
                   "site_alf_pct",
                   "site_nonsnf_pct",
                   "site_snf_pct",
                   "site_inpthospital_pct",
                   "site_inpthospice_pct"
)

canc <- datfinal %>% filter(cancer_pct != "NA")
stro <- datfinal %>% filter(stroke_pct != "NA")
deme <- datfinal %>% filter(dementia_pct != "NA")
hear <- datfinal %>% filter(heart_pct != "NA")
reps <- datfinal %>% filter(resp_pct != "NA")
othe <- datfinal %>% filter(otherdx_pct != "NA")

ui <- fluidPage(theme = shinythemes::shinytheme("journal"),
                
                #HEADING
                titlePanel("Shiny App for BST 260 Final Project -- Predicting Hospice Performance"),
                p("The data used was created using data available from", a("Medicare Hospice Compare", href="https://www.medicare.gov/index")),
                
                tabsetPanel(
                  
                  #TAB 1: DEMOGRAPHIC
                  tabPanel("Demographic Data",
                           fluidRow(
                             column(8,
                                    plotOutput("sexL")
                             )
                           ),
                           fluidRow(
                             column(12, p("___________________________________________________________________________________________________________________"))
                           ),
                           fluidRow(
                             column(2,
                                    plotOutput("cancerP")
                             ),
                             column(2,
                                    plotOutput("strokeP")
                             ),
                             column(2,
                                    plotOutput("dementiaP")
                             ),
                             column(2,
                                    plotOutput("heartP")
                             ),
                             column(2,
                                    plotOutput("respP")
                             ),
                             column(2,
                                    plotOutput("otherP")
                             )
                           ),
                           fluidRow(
                             column(12, p("___________________________________________________________________________________________________________________"))
                           ),
                           fluidRow(
                             column(8,
                                    plotOutput("daysVregion")
                             )
                           )
                  ),
                  #TAB 2 QUALITY
                  tabPanel("Hospice Quality Data Analysis",
                           fluidRow(
                             column(4,
                                    selectInput(inputId = "qregion",
                                                label = "Select a Region:",
                                                choices = unique(c(as.character(datfinal$region))),
                                                selected = "ne"), 
                                    
                             )),
                           fluidRow(
                             column(8,
                                    plotOutput("ratingVdischarge")
                             )
                           )),
                  
                  #TAB 3: DISCHARGE
                  tabPanel("Hospice Discharge Data Analysis",
                           
                           fluidRow(
                             column(4,
                                    selectInput(inputId = "dregion",
                                                label = "Select a Region:",
                                                choices = unique(c(as.character(datfinal$region))),
                                                selected = "ne")
                             )
                           ),
                           fluidRow(
                             column(8,
                                    plotOutput("dischargeVcancer"))
                           )
                  ),
                  #TAB 4: PAYMENT
                  tabPanel("Payment per Beneficiary-Day Data Analysis",
                           fluidRow(
                             column(4,
                                    selectInput(inputId = "pregion",
                                                label = "Select a Region:",
                                                choices = unique(c(as.character(datfinal$region))),
                                                selected = "ne")
                             )
                           ),
                           fluidRow(
                             column(8,
                                    plotOutput("paymentVhome"))
                           )
                  ),
                  #TAB 5: HISTOGRAM
                  tabPanel("Histograms",
                           fluidRow(
                             column(4,
                                    selectInput(inputId = "histvar",
                                                label = "Select a Variable:",
                                                choices = as.list(histvariables))
                             )
                           ),
                           fluidRow(
                             column(8,
                                    plotOutput("varhist"))
                           )
                  )
                ))

server <- function(input, output) {
  
  #OUTPUT 1: DEMOGRAPHIC
  
  ##Sex
  output$sexL <- renderPlot({
    datfinal %>%
      ggplot(aes(x = male, y = female)) +
      geom_point(alpha = 0.2) +
      geom_abline(intercept = 0, slope = 1, col = "red") +
      
      ggtitle(paste("Sex Comparison")) + 
      theme(plot.title = element_text(size = 19, face = "bold", color = "blue4", hjust=0.3), 
            axis.title = element_text(size = 13, face = "bold", color = "blue4"),
            axis.title.x = element_text(vjust=-1),
            axis.title.y = element_text(vjust=1.5),
            axis.text = element_text(size = 12.5, color = "blue4"),
            legend.text = element_text(size = 12.5, color = "blue4"))
  })
  
  ##Cancer
  output$cancerP <- renderPlot({
    datfinal %>%
      ggplot(aes(x = cancer_pct)) +
      geom_histogram(alpha = 0.2) +
      geom_vline(xintercept = mean(canc$cancer_pct), col = "red") +
      ggtitle(paste("% Cancer Diagnosis")) + 
      theme(plot.title = element_text(size = 10, face = "bold", color = "blue4", hjust=0.3), 
            axis.title = element_text(size = 13, face = "bold", color = "blue4"),
            axis.title.x = element_text(vjust=-1),
            axis.title.y = element_text(vjust=1.5),
            axis.text = element_text(size = 12.5, color = "blue4"),
            legend.text = element_text(size = 12.5, color = "blue4"))
  })
  ##Stroke
  output$strokeP <- renderPlot({
    datfinal %>%
      ggplot(aes(x = stroke_pct)) +
      geom_histogram(alpha = 0.2) +
      geom_vline(xintercept = mean(stro$stroke_pct), col = "red") +
      ggtitle(paste("% Stroke Diagnosis")) + 
      theme(plot.title = element_text(size = 10, face = "bold", color = "blue4", hjust=0.3), 
            axis.title = element_text(size = 13, face = "bold", color = "blue4"),
            axis.title.x = element_text(vjust=-1),
            axis.title.y = element_text(vjust=1.5),
            axis.text = element_text(size = 12.5, color = "blue4"),
            legend.text = element_text(size = 12.5, color = "blue4"))
  })
  
  ##Dementia
  output$dementiaP <- renderPlot({
    datfinal %>%
      ggplot(aes(x = dementia_pct)) +
      geom_histogram(alpha = 0.2) +
      geom_vline(xintercept = mean(deme$dementia_pct), col = "red") +
      ggtitle(paste("% Dementia Diagnosis")) + 
      theme(plot.title = element_text(size = 10, face = "bold", color = "blue4", hjust=0.3), 
            axis.title = element_text(size = 13, face = "bold", color = "blue4"),
            axis.title.x = element_text(vjust=-1),
            axis.title.y = element_text(vjust=1.5),
            axis.text = element_text(size = 12.5, color = "blue4"),
            legend.text = element_text(size = 12.5, color = "blue4"))
  })
  
  ##Heart Dz
  output$heartP <- renderPlot({
    datfinal %>%
      ggplot(aes(x = heart_pct)) +
      geom_histogram(alpha = 0.2) +
      geom_vline(xintercept = mean(hear$heart_pct), col = "red") +
      ggtitle(paste("% Heart Disease Diagnosis")) + 
      theme(plot.title = element_text(size = 10, face = "bold", color = "blue4", hjust=0.3), 
            axis.title = element_text(size = 13, face = "bold", color = "blue4"),
            axis.title.x = element_text(vjust=-1),
            axis.title.y = element_text(vjust=1.5),
            axis.text = element_text(size = 12.5, color = "blue4"),
            legend.text = element_text(size = 12.5, color = "blue4"))
  })
  
  ##Respiratory Dz
  output$respP <- renderPlot({
    datfinal %>%
      ggplot(aes(x = resp_pct)) +
      geom_histogram(alpha = 0.2) +
      geom_vline(xintercept = mean(reps$resp_pct), col = "red") +
      ggtitle(paste("% Respiratory Disease Diagnosis")) + 
      theme(plot.title = element_text(size = 10, face = "bold", color = "blue4", hjust=0.3), 
            axis.title = element_text(size = 13, face = "bold", color = "blue4"),
            axis.title.x = element_text(vjust=-1),
            axis.title.y = element_text(vjust=1.5),
            axis.text = element_text(size = 12.5, color = "blue4"),
            legend.text = element_text(size = 12.5, color = "blue4"))
  })
  
  ##Other Dx
  output$otherP <- renderPlot({
    datfinal %>%
      ggplot(aes(x = otherdx_pct)) +
      geom_histogram(alpha = 0.2) +
      geom_vline(xintercept = mean(othe$otherdx_pct), col = "red") +
      ggtitle(paste("% Other Diagnosis")) + 
      theme(plot.title = element_text(size = 10, face = "bold", color = "blue4", hjust=0.3), 
            axis.title = element_text(size = 13, face = "bold", color = "blue4"),
            axis.title.x = element_text(vjust=-1),
            axis.title.y = element_text(vjust=1.5),
            axis.text = element_text(size = 12.5, color = "blue4"),
            legend.text = element_text(size = 12.5, color = "blue4"))
  })
  
  ##Days v Region    
  output$daysVregion <- renderPlot({
    datfinal %>%
      ggplot(aes(region, totaldays)) +
      geom_jitter(width = 0.1, alpha = 0.2) +
      xlab("Region") +
      ylab("Total Days") +
      scale_y_discrete(limits = c(0,50000)) +
      ggtitle(paste("Total Days vs. Region")) + 
      theme(plot.title = element_text(size = 19, face = "bold", color = "blue4", hjust=0.3), 
            axis.title = element_text(size = 17, face = "bold", color = "blue4"),
            axis.title.x = element_text(vjust=-1),
            axis.title.y = element_text(vjust=1.5),
            axis.text = element_text(size = 12.5, color = "blue4"),
            legend.text = element_text(size = 12.5, color = "blue4"))
  })
  
  #OUTPUT 2: QUALITY
  output$ratingVdischarge <- renderPlot({
    datfinal %>%
      filter(region == input$qregion) %>%
      ggplot() +
      geom_boxplot(aes(x = total_live_discharges, y = rated9_10, group = total_live_discharges)) +
      xlab("Total Live Discharges") +
      scale_x_continuous() +
      ylab("Percent Overall Rating 9+/10") +
      ggtitle(paste("Overall Hospice rating vs. Total Live Discharges for Region",input$qregion)) + 
      theme(plot.title = element_text(size = 19, face = "bold", color = "blue4", hjust=0.3), 
            axis.title = element_text(size = 17, face = "bold", color = "blue4"),
            axis.title.x = element_text(vjust=-1),
            axis.title.y = element_text(vjust=1.5),
            axis.text = element_text(size = 12.5, color = "blue4"),
            legend.text = element_text(size = 12.5, color = "blue4"))
  })
  
  #OUTPUT 3: DISCHARGE
  output$dischargeVcancer <- renderPlot({
    datfinal %>%
      filter(region == input$dregion) %>%
      ggplot() +
      geom_line(aes(x = cancer_pct, y = total_live_discharges)) +
      xlab("Percent Cancer") +
      scale_x_continuous() +
      ylab("Total Live Discharges") +
      ggtitle(paste("Total Live Discharges vs. % Beneficiaries with Cancer for Region",input$dregion)) + 
      theme(plot.title = element_text(size = 19, face = "bold", color = "blue4", hjust=0.3), 
            axis.title = element_text(size = 17, face = "bold", color = "blue4"),
            axis.title.x = element_text(vjust=-1),
            axis.title.y = element_text(vjust=1.5),
            axis.text = element_text(size = 12.5, color = "blue4"),
            legend.text = element_text(size = 12.5, color = "blue4"))
  })
  
  #OUTPUT 4: PAYMENT
  output$paymentVhome <- renderPlot({
    datfinal %>%
      filter(region == input$pregion) %>%
      ggplot() +
      geom_line(aes(x = home_health_pct, y = discharges_pbeneday)) +
      xlab("Percent Receiving Home Health Visits") +
      scale_x_continuous() +
      ylab("Payment per Beneficiary-Day") +
      ggtitle(paste("Payment / Beneficiary-Day vs. % Receiving Home Health Visits for Region",input$pregion)) + 
      theme(plot.title = element_text(size = 19, face = "bold", color = "blue4", hjust=0.3), 
            axis.title = element_text(size = 17, face = "bold", color = "blue4"),
            axis.title.x = element_text(vjust=-1),
            axis.title.y = element_text(vjust=1.5),
            axis.text = element_text(size = 12.5, color = "blue4"),
            legend.text = element_text(size = 12.5, color = "blue4"))
  })
  
  #OUTPUT 5: HISTOGRAMS
  
  output$varhist <- renderPlot({
    
    if(input$histvar == "rated9_10"){data <- datfinal$rated9_10}
    if(input$histvar == "recommend_definitely"){data <- datfinal$recommend_definitely}
    if(input$histvar == "pain_always"){data <- datfinal$pain_always}
    if(input$histvar == "training_definitely"){data <- datfinal$training_definitely}
    if(input$histvar == "communicated_always"){data <- datfinal$communicated_always}
    if(input$histvar == "respect_always"){data <- datfinal$respect_always}
    if(input$histvar == "helped_always"){data <- datfinal$helped_always}
    if(input$histvar == "emotionalsupport"){data <- datfinal$emotionalsupport}
    if(input$histvar == "total_medicare_payment"){data <- datfinal$total_medicare_payment}
    if(input$histvar == "total_medicare_standard_payment"){data <- datfinal$total_medicare_standard_payment}
    if(input$histvar == "total_charge"){data <- datfinal$total_charge}
    if(input$histvar == "medicare_return_rate"){data <- datfinal$medicare_return_rate}
    if(input$histvar == "medicare_payment_pbeneday"){data <- datfinal$medicare_payment_pbeneday}
    if(input$histvar == "medicare_standard_payment_pbeneday"){data <- datfinal$medicare_standard_payment_pbeneday}
    if(input$histvar == "charge_pbeneday"){data <- datfinal$charge_pbeneday}
    if(input$histvar == "totaldays"){data <- datfinal$totaldays}
    if(input$histvar == "total_live_discharges"){data <- datfinal$total_live_discharges}
    if(input$histvar == "discharges_pbeneday"){data <- datfinal$discharges_pbeneday}
    if(input$histvar == "age"){data <- datfinal$age}
    if(input$histvar == "male_pct"){data <- datfinal$male_pct}
    if(input$histvar == "advantage_pct"){data <- datfinal$advantage_pct}
    if(input$histvar == "medicaid_pct"){data <- datfinal$medicaid_pct}
    if(input$histvar == "benefgr60days_pct"){data <- datfinal$benefgr60days_pct}
    if(input$histvar == "benefgr180days_pct"){data <- datfinal$benefgr180days_pct }
    if(input$histvar == "benefls7days_pct"){data <- datfinal$benefls7days_pct}
    if(input$histvar == "calcavglos"){data <- datfinal$calcavglos}
    if(input$histvar == "cancer_pct"){data <- datfinal$cancer_pct}
    if(input$histvar == "dementia_pct"){data <- datfinal$dementia_pct}
    if(input$histvar == "stroke_pct"){data <- datfinal$stroke_pct}
    if(input$histvar == "heart_pct"){data <- datfinal$heart_pct}
    if(input$histvar == "resp_pct"){data <- datfinal$resp_pct}
    if(input$histvar == "otherdx_pct"){data <- datfinal$otherdx_pct}
    if(input$histvar == "home_health_pct"){data <- datfinal$home_health_pct}
    if(input$histvar == "snf_pct"){data <- datfinal$snf_pct}
    if(input$histvar == "social_services_pct"){data <- datfinal$social_services_pct}
    if(input$histvar == "site_home_pct"){data <- datfinal$site_home_pct}
    if(input$histvar == "site_alf_pct"){data <- datfinal$site_alf_pct}
    if(input$histvar == "site_nonsnf_pct"){data <- datfinal$site_nonsnf_pct}
    if(input$histvar == "site_snf_pct"){data <- datfinal$site_snf_pct}
    if(input$histvar == "site_inpthospital_pct"){data <- datfinal$site_inpthospital_pct}
    if(input$histvar == "site_inpthospice_pct"){data <- datfinal$site_inpthospice_pct}
    
    
    hist(as.numeric(data))
    
  })
  
}

shinyApp(ui = ui, server = server)


