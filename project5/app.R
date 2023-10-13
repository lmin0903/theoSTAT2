
library(shiny)
library(tidyverse)
library(markdown)
library(dplyr)
library(stats)
library(ggrepel)
library(knitr)
library(shinydashboard)
library(shinythemes)
library(dashboardthemes)
library(lubridate)
options(scipen=999)



itdata <- read.csv("유통수익률.csv")
samsung <- read.csv("samsung.csv")
ktag <- read.csv("KT_G.csv")
kakao <- read.csv("kakao.csv")
naver <- read.csv("naver.csv")
kogas <- read.csv("kogas.csv")

samsung <- samsung[,c(1,5)]
ktag <- ktag[,c(1,5)]
kakao <- kakao[,c(1,5)]
naver <- naver[,c(1,5)]
kogas <- kogas[,c(1,5)]

itdata$Date <- format(as.Date(itdata$Date), format = "%Y/%m")
samsung$Date <- format(as.Date(samsung$Date),format = "%Y/%m")
ktag$Date <- format(as.Date(ktag$Date),format = "%Y/%m")
kakao$Date <- format(as.Date(kakao$Date),format = "%Y/%m")
naver$Date <- format(as.Date(naver$Date),format = "%Y/%m")
kogas$Date <- format(as.Date(kogas$Date),format = "%Y/%m")

df <- samsung %>% select("Date","Close") %>% merge(ktag,by = "Date", suffixes = c("_samsung","_kt")) %>% 
  merge(kakao,by = "Date")  %>%
  merge(naver,by = "Date", suffixes = c("_kakao","_naver")) %>%
  merge(kogas,by = "Date") %>%
  merge(itdata,by="Date") %>% arrange("Date") 

df <- df %>% rename('Close_gas' = 'Close') 

data1 <- df %>% mutate(t = seq(0,nrow(df)-1,1),
                      samsung = (Close_samsung-lag(Close_samsung))/lag(Close_samsung), 
                      kt.g = (Close_kt-lag(Close_kt))/lag(Close_kt),
                      kakao = (Close_kakao-lag(Close_kakao))/lag(Close_kakao),
                      naver = (Close_naver-lag(Close_naver))/lag(Close_naver),
                      kor.gas = (Close_gas-lag(Close_gas))/lag(Close_gas),
                      deposit = (it*0.01+1)^(1/12)-1) %>% 
                      na.omit() %>%
                      select(-it,-contains("Close")) %>% 
                      select(t,deposit,everything())






##### UI 정의 #####
header <- dashboardHeader(title = "Optimal Portfolio")

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("Investment", tabName="inv",
               numericInput("V0", h5("Initial Investment Amount", style="color:white;"),       
                            min = 0, max = 100000, value = 1000, step = 1),
               numericInput("istar", h5("Target rate of return (year)", style="color:white;"),       
                            min = 0, max = 1, value = 0.05, step = 0.01),
               numericInput("mm", h5("Investment period (month)", style="color:white;"),       
                            min = 1, max = 60, value = 60, step = 1),
               numericInput("nn", h5("Sample data size (month)", style="color:white;"),       
                            min = 1, max = 60, value = 60, step = 1)
             ),
    
    menuItem("Portfolio", tabName="port",
             sliderInput("s0", h5("deposit", style="color:white;"), min=0, max=1, value=c(0,1)),
             sliderInput("s1", h5("Samsung", style="color:white;"), min=0, max=1, value=c(0,1)),
             sliderInput("s2", h5("KT&G", style="color:white;"), min=0, max=1, value=c(0,1)),
             sliderInput("s3", h5("Kakao", style="color:white;"), min=0, max=1, value=c(0,1)),
             sliderInput("s4", h5("Naver", style="color:white;"), min=0, max=1, value=c(0,1)),
             sliderInput("s5", h5("Korea Gas", style="color:white;"), min=0, max=1, value=c(0,1))
             )
  )
)


body <- dashboardBody(

  valueBoxOutput("mean", width=4),
  valueBoxOutput("sd", width=4),
  valueBoxOutput("SR", width=4),
  
  br(),
  br(),
  fluidRow(
    column(8, box(title="Time-Series Plot", plotOutput('Plot1', height=500), width=12)
    ),
    fluidRow(
      column(4, br(), br(),
             valueBoxOutput("ex_m", width = 12),
             valueBoxOutput("ex_s", width = 12),
             valueBoxOutput("re_m", width = 12),
             valueBoxOutput("re_s", width = 12)
      )
  )
 )
)

ui <- dashboardPage(header, sidebar, body, shinyDashboardThemes(theme="blue_gradient"))




shape.ratio <- function(theta, r, i){  
  theta <- c(1-sum(theta), theta)
  theta1 <- matrix(theta, nrow=nrow(r), ncol=length(theta), byrow = T)
  rp <- rowSums(r*theta1)
  rp.bar <- mean(rp)
  sp <- sd(rp)
  r.star <- (1+i)^(1/12)-1
  Z <- (rp.bar-r.star)/sp
  return(Z)
} 

outputs <- function(train,test,theta,i,v0){
  
  r.star <- (1+i)^(1/12)-1
  date <- test$Date
  train1 <- data.matrix(train[c(-1,-3)])
  test1 <- data.matrix(test[c(-1,-3)])
  n1 <- nrow(train1)
  n2 <- nrow(test1)
  
  theta <- c(1-sum(theta), theta)
  mat1 <- matrix(theta, nrow=n1, ncol=length(theta), byrow = T)
  mat2 <- matrix(theta, nrow=n2, ncol=length(theta), byrow = T)
  rpt <- rowSums(train1*mat1)
  rpt.star <- rowSums(test1*mat2)
  rp.bar <- mean(rpt)
  sp <- sd(rpt)
  z <- (rp.bar-r.star)/sp
  
  ex.mean <- 12*rp.bar
  ex.sd <- sqrt(12)*sp
  real.mean <- mean(rpt.star)*12
  real.sd <- sd(rpt.star)*sqrt(12)
  vt.star <- v0*cumprod(1+rpt.star)
  
  result <- list(theta = theta,
                 sharpe = z%>%round(4),
                 month = c(rp.bar,sp)%>%round(4), 
                 expected = c(ex.mean,ex.sd)%>%round(4),
                 realized = c(real.mean,real.sd)%>%round(4),
                 vt = data.frame(t = date, vt.star = vt.star),
                 name = colnames(train1))
  return(result)
} 




#### Server ####
server <- function(input, output, session) {
  
  out <- reactive({
    n <- input$nn
    train <- data1[1:n,] 
    m <- input$mm
    test <- data1[(n+1):(n+m),]
    a0 <- input$s0
    a1 <- c(input$s1,input$s2, input$s3, input$s4, input$s5)
    
    r <- train[c(-1,-3)]
    i <- input$istar
    j <- ncol(r)-1
    
    ui <- rbind(rep(-1, j), diag(j))
    ui <- c(t(cbind(ui, -ui)))
    ui <- matrix(ui, ncol=j, byrow = T)
    ci <- c(a0-1, a1)
    ci <- ci*c(1,-1)
    
    ci2 <- matrix(ci+0.1, ncol=1)
    theta0 <- MASS::ginv(ui) %*% ci2
    # ui %*% theta0 - ci > 0
    res <- constrOptim(theta0, f=shape.ratio, grad=NULL, ui=ui, ci=ci, i=i, r=r, control=list(fnscale=-1))
    theta <- res$par[,1]

    result <- outputs(train,test,theta,i,input$V0)
    return(result)
  })

  
  output$mean <- renderValueBox({
    valueBox({
      out <- out()
      out$month[1]
    }, 
    h4("Mean", style="color:white;"), color="blue", icon=icon("list-ul"))       
  })
  
  output$sd <- renderValueBox({
    valueBox({
      out <- out()
      out$month[2]
    }, 
    h4("Standard Deviation", style="color:white;"), color="blue", icon=icon("list-ul"))       
  })
  
  output$SR <- renderValueBox({
    valueBox({
      out <- out()
      out$sharpe
    },
    h4("Sharpe Ratio", style="color:white;"), color="navy")       
  })
  
  
  output$Plot1 <- renderPlot({
    out <- out()
    if(!is.null(out)){
      t_min <- min(out$vt$t)
      t_max <- max(out$vt$t)
      label1 <- out$vt[nrow(out$vt)/5,1]
      label2 <- out$vt[nrow(out$vt)/5*2,1]
      label3 <- out$vt[nrow(out$vt)/5*3,1]
      label4 <- out$vt[nrow(out$vt)/5*4,1]
      plot <- out$vt %>% ggplot() + geom_line(aes(x=t,y=vt.star, group=1)) + 
                            labs(title=paste0("Optimal Portfolio Value  (period : ", t_min, ' ~ ', t_max, ")"), x='date', y='Vt') +
                            theme(plot.title=element_text(size=15), axis.text.x = element_text(size=12)) +     
                            scale_x_discrete(breaks=c(label1, label2, label3, label4)) +
                            ylab("Portfolio Value  (단위 : 만원)") 
      plot
    }
  })
  
  output$ex_m <- renderValueBox({
    valueBox({
      out <- out()
      out$expected[1]
    },
    h4("annualized expected mean"), color="teal", icon=icon("percentage"))       
  })
  
  output$ex_s <- renderValueBox({
    valueBox({
      out <- out()
      out$expected[2]
    },
    h4("annualized expected sd"), color="teal", icon=icon("percentage"))       
  })
  
  output$re_m <- renderValueBox({
    valueBox({
      out <- out()
      out$realized[1]
    },
    h4("annualized realized mean"), color="aqua", icon=icon("signal"))       
  })
  
  output$re_s <- renderValueBox({
    valueBox({
      out <- out()
      out$realized[2]
    },
    h4("annualized realized sd"), color="aqua", icon=icon("signal"))       
  })
  
}

shinyApp(ui = ui, server = server)


