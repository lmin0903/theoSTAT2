#setwd("C:/Users/jeekyung/Downloads")

library(readr)
library(markdown)
library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(lubridate)
library(evd)
library(ggforce)
library(readxl)
library(spsComps)


##### UI 정의 #####
header<-dashboardHeader(title = "손해보험료 계산 - 2조")

sidebar<-dashboardSidebar(
  ## 파일 업로드
  #fileInput("file1", "데이터를 업로드하세요",
  #          multiple = FALSE,
  #          accept = c("text/csv",
  #                     "text/comma-separated-values,text/plain",
  #                     ".csv")),
  
  ## 데이터 선택
  selectInput("file1", "데이터를 선택하세요",
              choices = c("car_insurance","school_fire"),
              selected = "car_insurance"),
  
  
  tags$hr(),
  
  
  ## 본인부담금 A, 보상한도 B 입력
  numericInput("a", "본인부담금을 입력하세요.", value = 0, min = 0, max = 200000, step = 100000),
  numericInput("b", "보상한도액을 입력하세요",
               min = 0, max = 1000000000, value = 500000, step = 500000),
  
  #shinyWidgets::autonumericInput(
  #  "c",label="천단위 테스트", value = 1000000,
  #  digitGroupSeparator = ","),
  
  
  tags$hr(),
  
  
  ## Claim Size Distribution(Pareto,Frechet,log-Norma)
  radioButtons("mdl","활용할 분포를 선택하세요",
               choices = c(`Pareto`="pa",
                           `Frechet`="fr",
                           `log-Normal`="lnorm",
                           `log-logistic`="loglogis"),
               selected = "pa"),
  
  
  actionButton("goButton", "Go!")
)


body<-dashboardBody(
  
  style = "background-color: black;",
  tags$style(HTML(".white-icon { color: white; }")),
  
  #추정값 표시
  valueBoxOutput("premium"),
  valueBoxOutput("rsquare"),
  valueBoxOutput("loss"),
  
  fluidRow(
    box(title = "Data", width = 4,
        dataTableOutput("output_data")),
    box(title = "histogram & pdf", width = 8,
        plotOutput("histogram", height = 600))
  )
  
  
)


ui<-dashboardPage(header, sidebar, body, skin="black")


##### BODY #####
###### OLS -----
# 파레토분포(Pareto) 히스토그램 & pdf
options(scipen = 999)

qq_pareto<-function(data,n){
  myL<-seq(1,150,by=1); p<-Yr<-myR<-c()
  for(i in 1:length(myL)){
    p<-cumsum(data$count)/(n+1)
    Yr<-log(1+data$x/myL[i])
    R<-summary(lm(Yr~I(-log(1-p))+0))$r.squared
    myR[i]<-R
  }
  cand<-data.frame(myL, myR)
  lambda<-cand$myL[which.max(myR)]
  
  data <- data %>% mutate(p = cumsum(count)/(n+1),
                          theo = -log(1-p),
                          samp = log(1 + x/lambda))
  myLM<-lm(samp~theo+0, data=data)
  alpha=1/myLM$coef[1]
  pdf<-function(x){alpha*(lambda^alpha)*((lambda+x)^(-alpha-1))}
  myR2<-summary(myLM)$r.squared
  myplt <- data %>% ggplot(aes(x,count)) +
    geom_bar(stat = "identity", fill = "royalblue") + theme_minimal() +
    geom_line(aes(x=x,y=n*pdf(x)), color = "indianred1", linewidth=1.3)
  mylst <- list(draw_plt=myplt, R2=myR2, alpha=alpha, lambda=lambda)
  return(mylst)
}

# 프레셰 hist
qq_frechet<-function(data,n){
  data <- data %>% mutate(p = cumsum(count)/(n+1),
                          theo = -log(-log(p)),
                          samp = log(x))
  myLM<-lm(samp~theo, data=data)
  myR2<-summary(myLM)$r.squared
  # 모수가 c랑 tau
  mu<-myLM$coef[1]
  sig<-myLM$coef[2]
  c=exp(mu/sig)
  tau=1/sig
  pdf<-function(x){c*tau*exp(-c/(x^tau))/(x^(tau+1))}
  myplt <- data %>% ggplot(aes(x,count)) +
    geom_bar(stat = "identity", fill = "royalblue") + theme_minimal() +
    geom_line(aes(x=x,y=n*pdf(x)), color = "indianred1", linewidth=1.3)
  mylst <- list(draw_plt=myplt, R2=myR2, c=c, tau=tau)
  return(mylst)
}

# 로그노말 hist
qq_lognorm<-function(data,n){
  data <- data %>% mutate(p = cumsum(count)/(n+1),
                          theo = qnorm(p),
                          samp = log(x))
  myLM<-lm(samp~theo, data=data)
  myR2<-summary(myLM)$r.squared
  mu=myLM$coef[1]
  sig=myLM$coef[2]
  pdf<-function(x){(1/(x*sig*sqrt(2*pi)))*exp(-0.5*((log(x)-mu)/sig)^2)}
  myplt <- data %>% ggplot(aes(x,count)) +
    geom_bar(stat = "identity", fill = "royalblue") + theme_minimal() +
    geom_line(aes(x=x,y=n*pdf(x)), color = "indianred1", linewidth=1.3)
  mylst <- list(draw_plt=myplt, R2=myR2, mu=mu, sig=sig)
  return(mylst)
}

# 로그로지스틱 hist
qq_loglogis<-function(data,n){
  data <- data %>% mutate(p = cumsum(count)/(n+1),
                          theo = log(p/(1-p)),
                          samp = log(x))
  myLM<-lm(samp~theo, data=data)
  myR2<-summary(myLM)$r.squared
  mu<-myLM$coef[1]
  sig<-myLM$coef[2]
  alpha<-1/sig
  lambda<-exp(mu)
  pdf<-function(x){alpha * x^(alpha-1) * lambda^(-alpha) / (1+(x/lambda)^alpha)^2}
  myplt <- data %>% ggplot(aes(x,count)) +
    geom_bar(stat = "identity", fill = "royalblue") + theme_minimal() +
    geom_line(aes(x=x,y=n*pdf(x)), color = "indianred1", linewidth=1.3)
  mylst <- list(draw_plt=myplt, R2=myR2, alpha=alpha, lambda=lambda)
  return(mylst)
}

# school_fire barchart
data_bar<-function(data,n){
  data$x <- factor(data$x, levels = c("50", "100", "1000","5000","10000","100000"))
  myplt <- data %>% ggplot(aes(x,count)) +
    geom_bar(stat = "identity", fill = "royalblue") +
    theme_minimal()
  return(myplt)
}



###estimate function 
iw.coef <- function(coef){
  mu <- coef[[1]]
  sigma <- coef[[2]]
  tau<- 1/sigma 
  c<- exp(mu*tau)
  return(list(tau=tau,c=c))
}

estimator <- function(data,dist,n){
  
  if(dist=="lnorm"){
    result<- list(mu=qq_lognorm(data,n)$mu,sigma=qq_lognorm(data,n)$sig,r2=qq_lognorm(data,n)$R2)
    
  } else if(dist=="pa"){
    result<-list(lambda=qq_pareto(data,n)$lambda,alpha=qq_pareto(data,n)$alpha,r2=qq_pareto(data,n)$R2)
    
  } else if(dist=="fr"){
    result <- list(tau=qq_frechet(data,n)$tau,c=qq_frechet(data,n)$c,r2=qq_frechet(data,n)$R2)
    
  } else if(dist=="loglogis"){
    result<-list(lmabda=qq_loglogis(data,n)$lambda,alpha=qq_loglogis(data,n)$alpha,r2=qq_loglogis(data,n)$R2)
  }
  
  return(result) 
}



d.pareto<- function(x,lambda,alpha){
  value <- 1-(lambda/(lambda+x))^alpha
  return(value)
}

d.iweibull<- function(x,tau,c){
  value <- exp(-c/(x^tau))
  return(value)
}

d.lognormal<- function(x,mu,sigma){
  value <- pnorm((log(x)-mu)/sigma)
  return(value)
}

d.loglogi<- function(x,lambda,alpha){
  value <- 1-(1/(1+(x/lambda)^alpha))
  return(value)
}



#### 보험료 계산 -----
Ey.pareto<-function(data,a,b,n){
  lambda <- estimator(data,"pa",n)[[1]]
  alpha <- round(estimator(data,"pa",n)[[2]],3)
  f<- function(x) {(alpha*(lambda^alpha)*((lambda+x)^(-alpha-1)))*x}
  Fab<-d.pareto(a+b,lambda,alpha)
  Fa<-d.pareto(a,lambda,alpha)
  Ey<-integrate(f,a,a+b)$value+b*(1-Fab)-a*(Fab-Fa)
  return(Ey)
}

Ey.iw<-function(data,a,b,n){
  tau <- estimator(data,"fr",n)[[1]]
  c <- estimator(data,"fr",n)[[2]]
  f<- function(x) {((c*tau*exp(-c/x^tau))/x^(tau+1))*x}
  Fab<-d.iweibull(a+b,tau,c)
  Fa<-d.iweibull(a,tau,c)
  Ey<-integrate(f,a,a+b)$value+b*(1-Fab)-a*(Fab-Fa)
  return(Ey)
}

Ey.norm<-function(data,a,b,n){
  mu <- estimator(data,"lnorm",n)[[1]]
  sigma <- estimator(data,"lnorm",n)[[2]]
  f<-function(x) {(1/(x*sqrt(2*pi)*sigma)*exp((-1/2)*((log(x)-mu)/sigma)^2))*x}
  Fab<-pnorm(a+b,mu,sigma)
  Fa <-pnorm(a,mu,sigma)
  Ey<-integrate(f,a,a+b)$value+b*(1-Fab)-a*(Fab-Fa)
  return(Ey)
}

Ey.loglogi<-function(data,a,b,n){
  lambda <- estimator(data,"loglogis",n)[[1]]
  alpha <- estimator(data,"loglogis",n)[[2]]
  
  f<-function(x) {(alpha*x^(alpha-1)*lambda^(-alpha)/(1+(x/lambda)^alpha)^2)*x}
  Fab<-d.loglogi(a+b,lambda,alpha)
  Fa<-d.loglogi(a,lambda,alpha)
  Ey<-integrate(f,a,a+b)$value+b*(1-Fab)-a*(Fab-Fa)
  return(Ey)
}





###### 보험료 결과 출력 -----
result <- function(data,a,b,model,n){
  if(model=="pa"){price<-Ey.pareto(data,a,b,n)}
  else if(model=="fr"){price<-Ey.iw(data,a,b,n)}
  else if(model=="lnorm"){price<-Ey.norm(data,a,b,n)}
  else if(model=="loglogis"){price<-Ey.loglogi(data,a,b,n)}
  return(price)
  
}

result2 <- function(data,model,n){
  if(model=="pa"){hist<-qq_pareto(data,n)}
  else if(model=="fr"){hist<-qq_frechet(data,n)}
  else if(model=="lnorm"){hist<-qq_lognorm(data,n)}
  else if(model=="loglogis"){hist<-qq_loglogis(data,n)}
  return(hist)
  
}



##### 서버 정의 #####
server <- function(input, output, session) {
  
  #data<-reactive({
  #  infile<-input$file1
  #  
  #  if(is.null(infile)){
  #    return(NULL)
  #  }
  
  
  #  df<-read.csv(input$file1$datapath, header=T, fileEncoding="cp949")
  #  df<-df[1:50,]
  #  colnames(df)<-c("x", "count")
  #  df<-df%>%mutate(x=as.numeric(x))
  
  #  return(df)
  
  #})
  
  datatable<-eventReactive(input$goButton,{
    selected_data <- input$file1
    
    if (selected_data == "car_insurance") {
      df<-read.csv("car_insurance.csv", header=T, fileEncoding="cp949")
      df<-df[1:50,]
      names(df)<-c("x","count")
      #n<-8443
      df<-df%>%mutate(x=as.numeric(x))
      return(df)
    } else if (selected_data == "school_fire"){
      df<-read.csv("school_fire.csv", fileEncoding="cp949")
      names(df)<-c("x","n08","n09","n10","n11","n12","n13","n14","n15","n16","n17","count")
      df<-df[1:6,1:12]
      df<-df%>%mutate(x=as.numeric(x))
      df<-df[,c("x","count")]
      return(df)
    }
  })
  
  N<-eventReactive(input$goButton,{
    if (input$file1 == "car_insurance") {
      n<-8443
      return(n)
    } 
    else if (input$file1 == "school_fire"){
      n<-986
      return(n)
    }
  })
  
  
  output$output_data <- renderDataTable({
    datatable()
  })
  
  myhist<-eventReactive(input$goButton,{
    selected_data <- input$file1
    data.df<-datatable()
    n<-N()
    
    if (selected_data == "car_insurance") {
      result2(data.df,input$mdl,n)
    } else if (selected_data == "school_fire"){
      data_bar(data.df)
    }
    
  }
  
  )
  
  output$histogram<-renderPlot({
    myhist()},
    
    height=600, width = 780
  )
  
  #문제되는 부분 ------
  myprice<-eventReactive(input$goButton,{
    data.df<-datatable()
    if(input$file1 == "car_insurance"){
      En<-8443 / 271306
    }
    else if (input$file1 == "school_fire"){
      En<-986/(5*20761)
    }
    n<-N()
    
    valueBox(
      paste(format(round(result(data.df, (input$a)/10000, (input$b)/10000, input$mdl,n)*En,3)*10000,big.mark=","),'원'),
      "Premium", icon = icon("money-bill",class="white-icon"),
      color = "black"
    )
  })
  
  
  myrsquare<-eventReactive(input$goButton,{
    data.df <- datatable()
    n<-N()
    valueBox(
      round(estimator(data.df,input$mdl,n)$r2,3),
      subtitle = "R-square", icon = icon("lightbulb",class="white-icon"),
      color = "black"
    )
  })
  
  
  myloss<-eventReactive(input$goButton,{
    data.df <- datatable()
    n<-N()
    if(input$file1 == "car_insurance"){
      En<-8443 / 271306
      damage<-259699
      lossrate<-round(damage/(result(data.df, (input$a)/10000, (input$b)/10000, input$mdl,n)*En*271306)*100,2)
    }
    else if (input$file1 == "school_fire"){
      En<-986/(5*20761)
      damage<-1109823
      lossrate<-round(damage/(result(data.df, (input$a)/10000, (input$b)/10000, input$mdl,n)*En*105263)*100,2)
    }
    
    valueBox(
      lossrate,
      subtitle = "Loss rate", icon = icon("percent",class="white-icon"),
      color = "black"
    )
  })
  
  
  
  output$premium <- renderValueBox({
    myprice()
  })
  
  output$rsquare <- renderValueBox({
    myrsquare()
  })
  
  output$loss <- renderValueBox({
    myloss()
  })
  
}


shinyApp(ui = ui, server = server)