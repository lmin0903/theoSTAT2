rm(list=ls())
library(fresh)
library(readr)
library(markdown)
library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(evd)
library(ggforce)
library(readxl)
options(scipen=500)

#생명표 불러들이기
data1<-read.csv("생명표_2010.csv", header=TRUE, fileEncoding = "utf-8", skip=1) %>%
  select(-contains(c("전체", "총생존년수")))
colnames(data1)<-c("age", "ex.bar_M", "ex.bar_F", "qx_M", "qx_F", "lx_M", "lx_F", "Lx_M", "Lx_F", "dx_M", "dx_F")
data1 <- data1[-nrow(data1),] #100세 이상 삭제
data1$age <- seq(0,99) #0세부터 99세까지
data2 <- data1


#life.table 출력하기

life.table <- function(data, sex){
  if(sex==1){
    df <- data %>% select(contains(c("age","_M")))
  }
  if(sex==2){
    df <- data %>% select(contains(c("age","_F")))
  }
  colnames(df) <- c("age", "ex.bar", "qx", "lx", "Lx", "dx")
  return(df)
}

#####function 정의######
pv.life <- function(data,x,i,n,m,b){
  r <- log(1+i*0.01)
  lx <- data[,4]
  
  a <- c()
  for (j in 1:n-1){
    a[j] <- (lx[x+1+j]/lx[x+1])*exp(-r*j)
  }
  ax <- 1/2 + sum(a) + (1/2)*(lx[x+1+n]/lx[x+1])*exp(-r*n)
  
  A <- c()
  for (j in 1:m){
    A[j] <- (lx[x+1+j]/lx[x+1])*exp(-r*j)
  }
  Ax <- 1 - r*(1/2 + sum(A) + (lx[x+1+m]/lx[x+1])*(exp(-r*(m+1))/(1-exp(-r))))
  
  px <- b*(Ax/ax)/12
  return(px)
}


pv.annual <- function(data,x,i,n,m,b){
  r <- log(1+i*0.01)
  lx <- data[,4][-1]
  Tx <- as.integer(data[,2][-1])
  b <- b*12
  
  f <- function(t) {
    if (x+t > 100) { 
      return(lx[100]/lx[x] * exp(-r*t))
    } else {
      return(lx[x+t]/lx[x] * exp(-r*t))
    }
  }
  
  s <- function(start, end) {
    s <- 0
    for (t in start:end) {
      s <- s + f(t)
    }
    return(s)
  }
  
  integration <- function(start, end) {
    (1/2)*f(start) + s(start, end) + (1/2)*f(end)
  }
  
  annuity <- b * integration(m-x, Tx[x]) 
  px <- annuity/integration(0, n)
  return(px/12) 
}


##### UI 정의 #####
header<-dashboardHeader(title = "생명/연금 보험료 2조")

sidebar<-dashboardSidebar(
  sidebarMenu(
    menuItem("Life Insurance", tabName="life", icon=icon("person-falling-burst")),
    menuItem("Annuity", tabName="annuity", icon = icon("won-sign")),
    hr(style = "border-color: gray;"),
    div(
      style = "margin-left: 27px;", # 여백을 추가하는 CSS 스타일
      withMathJax(
        HTML('<h5 style="color: #2E3440; font-weight: bold;"> ※ Important Terminology ※</h5>')
      )),
    div(
      style = "margin-left: 8px;",
      helpText('\\(P_{x}\\) : \\((x, x+1)\\)세 사이의 남(여)인구', style = "color: #2E3440;"),
      helpText('\\(D_{x}\\): \\((x, x+1)\\)세 사이의 사망자수', style = "color: #2E3440;"),
      helpText('\\( q_x = m_x / (1 + m_x /2) \\)', style = "color: #2E3440;"),
      helpText('\\( l_x = l_0 \\Pi_{t=0}^{x-1} (1-q_t) \\)', style = "color: #2E3440;"),
      helpText('\\( L_x = ( l_x + l_{x+1}) /2 \\)', style = "color: #2E3440;"),
      helpText('\\( \\bar{e}_x = \\Sigma_{t=0}^{\\infty} L_{x+t} / l_x \\)', style = "color: #2E3440;"))
    
  )
)

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E",
    olive = "#56cc9d",
    purple = "#6cc3d5",
    orange = "#ffce67",
    blue = "#78c2ad",
    red = "#f3969a"
  ),
  adminlte_sidebar(
    width = "230px",
    dark_bg = "#eff4fa",
    dark_hover_bg = "#6cc3d5",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)



body <- dashboardBody(
  use_theme(mytheme),
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #6cc3d5}")),
  tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #6cc3d5}")),
  tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #6cc3d5}")),
  tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #6cc3d5}")),
  tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #6cc3d5}")),
  tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: #6cc3d5}")),
  tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: #6cc3d5}")),
  
  tabItems(
    #첫번째 탭: life
    tabItem(tabName="life",
            fluidRow(
              column(9,
                     h3('Output'),
                     fluidRow(
                       column(12,
                              valueBoxOutput("interest.r1", width=6),
                              valueBoxOutput("payment.n1", width=6)),
                       column(12,
                              valueBoxOutput("provide.m1", width=6),
                              valueBoxOutput("benfit.b1", width=6))),
                     
                     valueBoxOutput("premium.m1", width=12),
                     
                     h3("Life Table"),
                     fluidRow(
                       column(12,
                              DT::dataTableOutput("data1")))
                     ),

              
              column(3,
                     h4('Client Information'),
            radioButtons("sex1", "Sex",    
                         choices=list("Male"=1, "Female"=2), selected=1),
            tags$hr(),
            sliderInput("age1", "Age",       
                         min = 0, max = 100, value = 50, step = 1),
            tags$hr(),
            h4('Product Information'),
            numericInput("interest1", "Interest rate (%)",       
                         min = 1, max = 100, value = 5, step = 1),
            tags$hr(),
            sliderInput("nn1", "Payment Expiration (N)",       
                         min = 1, max = 100, value = 20, step = 1),
            tags$hr(),
            sliderInput("mm1", "Provide Age (M)",       
                         min = 20, max = 100, value = 20, step = 1),
            tags$hr(),
            numericInput("mr1", "Benefit (B, unit: 10,000 won)",       
                         value = 10000, step = 1)
            )
            )
    )
    ,
    #두번째 탭: annuity
    tabItem(tabName="annuity",
            fluidRow(
              column(9,
                     h3('Output'),
                     fluidRow(
                       column(12,
                              valueBoxOutput("interest.r2", width=6),
                              valueBoxOutput("payment.n2", width=6)),
                       column(12,
                              valueBoxOutput("provide.m2", width=6),
                              valueBoxOutput("benfit.b2", width=6))),
                     
                  
                     valueBoxOutput("premium.m2", width=12),
                     
                     h3("Life Table"),
                     fluidRow(
                       column(12,
                              DT::dataTableOutput("data2")))
                     ),
              
              column(3,
                     h4('Client Information'),
                     radioButtons("sex2", "Sex",    
                                  choices=list("Male"=1, "Female"=2), selected=1),
                     tags$hr(),
                     sliderInput("age2", "Age",       
                                 min = 0, max = 100, value = 50, step = 1),
                     tags$hr(),
                     sliderInput("retage2", "Retirement Age (R)",       
                                 min = 0, max = 100, value = 60, step = 1),
                     tags$hr(),
                     h4('Product Information'),
                     numericInput("interest2", "Interest rate (%)",       
                                  min = 1, max = 100, value = 2, step = 1),
                     tags$hr(),
                     sliderInput("nn2", "Payment Expiration (N)",       
                                 min = 1, max = 100, value = 30, step = 1),
                     tags$hr(),
                     sliderInput("mm2", "Provide Age (M)",       
                                 min = 20, max = 100, value = 70, step = 1),
                     tags$hr(),
                     numericInput("mr2", "Monthly Receipt (B, unit: 10,000 won)",       
                                  value = 100, step = 1))
    )
  )
)
)


ui<-dashboardPage(header, sidebar, body)


##### 서버 정의 #####
server <- function(input, output, session) {
  
  output$interest.r1 <- renderValueBox({
    valueBox(
      paste0(input$interest1, "%"), "INTEREST RATE PER YEAR(i)", icon = icon("percent"),
      color = "olive"
    )
  })
  
  output$interest.r2 <- renderValueBox({
    valueBox(
      paste0(input$interest2, "%"), "INTEREST RATE PER YEAR(i)", icon = icon("percent"),
      color = "olive"
    )
  })
  
  output$payment.n1 <- renderValueBox({
    valueBox(
      input$nn1, "PAYMENT EXPIRATION(N)", icon = icon("calendar-check"),
      color = "purple"
    )
  })
  
  output$payment.n2 <- renderValueBox({
    valueBox(
      input$nn2, "PAYMENT EXPIRATION(N)", icon = icon("calendar-check"),
      color = "purple"
    )
  })
  
  output$provide.m1 <- renderValueBox({
    valueBox(
      input$mm1, "PROVIDE AGE(M)", icon = icon("hand-holding-dollar"),
      color = "orange"
    )
  })
  
  output$provide.m2 <- renderValueBox({
    valueBox(
      input$mm2, "PROVIDE AGE(M)", icon = icon("hand-holding-dollar"),
      color = "orange"
    )
  })
  
  output$benfit.b1 <- renderValueBox({
    valueBox(
      format(input$mr1, big.mark=","), "BENEFIT(B)", icon = icon("file-invoice-dollar"),
      color = "blue"
    )
  })
  
  output$benfit.b2 <- renderValueBox({
    valueBox(
      format(input$mr2, big.mark=","), "MONTHLY ANNUITY(B)", icon = icon("file-invoice-dollar"),
      color = "blue"
    )
  })
  
  output$premium.m1 <- renderValueBox({
    valueBox({
      if(input$sex1==1){
        format(round(pv.life(life.table(data1, 1), input$age1, input$interest1, input$nn1, input$mm1, input$mr1)*10000, -1), big.mark = ",")
      }
      else if(input$sex1==2){
        format(round(pv.life(life.table(data1, 2), input$age1, input$interest1, input$nn1, input$mm1, input$mr1)*10000, -1), big.mark = ",")
      }
    }, 
    h4("MONTHLY PREMIUM PRICE"), icon = icon("won-sign"), color = "red")       
  })
  
  output$premium.m2 <- renderValueBox({
    valueBox({
      if(input$sex2==1){
        format(round(pv.annual(life.table(data1, 1), input$age2, input$interest2, input$nn2, input$mm2, input$mr2)*10000, -1), big.mark = ",")
      }
      else if(input$sex2==2){
        format(round(pv.annual(life.table(data1, 2), input$age2, input$interest2, input$nn2, input$mm2, input$mr2)*10000, -1), big.mark = ",")
      }
    }, 
    h4("MONTHLY PREMIUM PRICE"), icon = icon("won-sign"), color = "red")       
  })
  
  
  
  output$data1 <- DT::renderDataTable({
    filtered_data <- life.table(data1[data1$age >= input$age1,], input$sex1)
    
    if (nrow(filtered_data) >= 7) {
      top_rows <- head(filtered_data, 3)
      middle_rows <- data.frame(age=c("..."), ex.bar=c("..."), qx=c("..."), lx=c("..."), Lx=c("..."), dx=c("..."))
      bottom_rows <- tail(filtered_data, 3)
    } else {
      top_rows <- filtered_data
      middle_rows <- NULL
      bottom_rows <- NULL
    }
    
    combined_data <- rbind(top_rows, middle_rows, bottom_rows)
    colnames(combined_data) <- c("나이 (age)", "기대여명 (ex.bar)", "사망확률 (qx)", "생존자수 (lx)", "정지인구 (Lx)", "사망자수 (Dx)")
    rownames(combined_data) <- NULL
    
    DT::datatable(
      combined_data,
      options = list(paging = FALSE, searching = FALSE)
    )
  })
  
  
  output$data2 <- DT::renderDataTable({
    filtered_data <- life.table(data1[data2$age >= input$age2,], input$sex2)
    
    if (nrow(filtered_data) >= 7) {
      top_rows <- head(filtered_data, 3)
      middle_rows <- data.frame(age=c("..."), ex.bar=c("..."), qx=c("..."), lx=c("..."), Lx=c("..."), dx=c("..."))
      bottom_rows <- tail(filtered_data, 3)
    } else {
      top_rows <- filtered_data
      middle_rows <- NULL
      bottom_rows <- NULL
    }
    
    combined_data <- rbind(top_rows, middle_rows, bottom_rows)
    colnames(combined_data) <- c("나이 (age)", "기대여명 (ex.bar)", "사망확률 (qx)", "생존자수 (lx)", "정지인구 (Lx)", "사망자수 (Dx)")
    rownames(combined_data) <- NULL
    
    DT::datatable(
      combined_data,
      options = list(paging = FALSE, searching = FALSE)
    )
  })

  

  
}


shinyApp(ui = ui, server = server)