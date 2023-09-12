#setwd("~/Desktop/2023-2/이론통계학II/HW1")

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


##### UI 정의 #####
header<-dashboardHeader(title = "수요예측모형 - 2조")

sidebar<-dashboardSidebar(
  ## 파일 업로드
  fileInput("file1", "박스오피스 자료를 업로드하세요",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
  
  dateInput("start", "개봉일을 입력하세요(연-월-일)",
            value = "2023-09-13"),
  
  ## header 있으면 체크표시
  #checkboxInput("header", "Header 있음", TRUE),
  
  ## 공휴일 파일 업로드
  fileInput("file2", "휴일 정보를 업로드하세요(CSV)",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
  
  tags$hr(),
  
  ## 예측 모형 선택 (Bass/Logistic/Gumbel)
  radioButtons("mdl","활용할 확산 모형을 선택하세요",
               choices = c(`Bass Model`="bs",
                           `Logistic Model`="lg",
                           `Gumbel Model`="gb",
                           `Exponential Model`="epn"),
               selected = "bs"),
  
  ## 추정 방법 선택 (OLS/QQ Plot/MLE)
  radioButtons("mth","추정 방법을 선택하세요",
               choices = c(OLS="ols",
                           `Q-Q Plot`="qq"),
                           #MLE="mle"
               selected = "ols"),
  
  tags$hr(),
  
  ## 공휴일/주말 쪼개기
  numericInput("weekend", "주말/공휴일 보정 기준을 선택하세요",
               value = 2),
  
  ## 수요를 예측할 시점 선택 (slider 형태, Prediction Horizon)
  sliderInput("bins", "예측할 시점을 선택하세요",
              min = 1, max = 30, value = 10, step = 1,
              post = "시점 뒤"),
  br(),
  actionButton("goButton", "Go!")
)


body<-dashboardBody(
  
  #추정값 표시
  valueBoxOutput("mhat"),
  valueBoxOutput("phat"),
  valueBoxOutput("qhat"),
  
  # 그래프 표시
  fluidRow(
    box(title = "Time Series Plot", width = 8,
        plotOutput("plot1", height = 600)),
    box(title = "Predicted Values", width = 4,
        dataTableOutput("pred"))
  )
  
)


ui<-dashboardPage(header, sidebar, body)


##### BODY #####
###### OLS -----
bass<-function(s,y,data){
  mylm <- lm(s~y+I(y^2))
  a<-mylm$coef[1]
  b<-mylm$coef[2]
  c<-mylm$coef[3]
  m<-(-b-sqrt(b^2-4*a*c))/(2*c)
  p<-a/m
  q<-(-m)*c
  par<-list(m=m, p=p,  q=q)
  return(par)
}

logis<-function(s,y,data){
  mylm<-lm(s~y+I(y^2)+0)
  m<-(-mylm$coef[1])/mylm$coef[2]
  p<-0
  q<-mylm$coef[1]
  mu<-data$t[which.max(mylm$fitted.values)]
  sigma<-1/q
  par<-list(m=m, p=p, q=q, mu=mu, sig=sigma)
  return(par)
}

gumbel<-function(s,y,data){
  mylm<-lm(s~y+I(y*log(y))+0)
  m<-exp(-mylm$coef[1]/mylm$coef[2])
  p<-0
  q<-(-mylm$coef[2])
  mu<-data$t[which.max(mylm$fitted.values)]
  sigma<-1/q
  par<-list(m=m, p=p, q=q, mu=mu, sig=sigma)
  return(par)
}

expn<-function(s,y,data){
  mylm<-lm(s~y)
  p<- -mylm$coef[2]
  m<- mylm$coef[1] / p
  q<-0
  mu<-data$t[which.max(mylm$fitted.values)]
  sigma<-1/p
  par<-list(m=m, p=p, q=q, mu=mu, sig=sigma)
  return(par)
}


###### q-q plot-----
qq_bass<-function(s,y,Xr){
  mylm<-lm(s~y+I(y^2))
  a<-mylm$coef[1]
  b<-mylm$coef[2]
  c<-mylm$coef[3]
  m.ini<-(-b-sqrt(b^2-4*a*c))/(2*c)
  myM<-seq(m.ini-100000, m.ini+100000, by = 1000)
  myR2<-c(); Ur<-c(); k<-c(); c<-c()
  for(i in 1:201){
    Ur<-y/(myM[i]+1);k<-2*a/myM[i]+b;c<-(b-a/myM[i])/myM[i]
    suppressWarnings({quan<-(log((1-c*Ur)/(1-Ur)))/k})
    R2<-summary(lm(Xr~quan+0))$r.squared
    myR2[i]<-R2
  }
  cand<-data.frame(myM,myR2)
  m<-cand$myM[which.max(myR2)]
  par<-list(m=m, p=a/m, q=(-m)*c, R2=max(myR2))
  return(par)
}

qq_logis<-function(s,y,Xr){
  mylm<-lm(s~y+I(y^2)+0)
  m.ini<-(-mylm$coef[1])/mylm$coef[2]
  myM<-seq(m.ini-100000, m.ini+100000, by = 1000)
  myR2<-c(); Ur<-c()
  for(i in 1:201){
    Ur<-y/(myM[i]+1)
    suppressWarnings({quan<-log(Ur/(1-Ur))})
    R2<-summary(lm(Xr~quan))$r.squared
    myR2[i]<-R2
  }
  cand<-data.frame(myM,myR2)
  m<-cand$myM[which.max(myR2)]
  U<-y/(m+1)
  suppressWarnings({mylm2<-lm(Xr~log(U/(1-U)))})
  par<-list(m=m, p=0, q=1/mylm2$coef[2], mu=mylm2$coef[1], sig=mylm2$coef[2], R2=max(myR2))
  return(par)
}

qq_gumb<-function(s,y,Xr){
  mylm<-lm(s~y+I(y*log(y))+0)
  m.ini<-exp(-mylm$coef[1]/mylm$coef[2])
  myM<-seq(m.ini-100000, m.ini+100000, by = 1000)
  myR2<-c(); Ur<-c()
  for(i in 1:201){
    Ur<-y/(myM[i]+1)
    suppressWarnings({quan<-(-log(-log(Ur)))})
    R2<-summary(lm(Xr~quan))$r.squared
    myR2[i]<-R2
  }
  cand<-data.frame(myM,myR2)
  m<-cand$myM[which.max(myR2)]
  y<-y[y<m]
  U<-y/(m+1)
  Quan<-(-log(-log(U)))
  suppressWarnings({mylm2<-lm(1:length(U)~Quan)})
  par<-list(m=m, p=0, q=1/mylm2$coef[2], mu=mylm2$coef[1], sig=mylm2$coef[2], R2=max(myR2))
  return(par)
}

qq_exp<-function(s,y,Xr){
  mylm<-lm(s~y)
  m.ini<-(-mylm$coef[1])/mylm$coef[2]
  myM<-seq(m.ini-100000, m.ini+100000, by = 1000)
  myR2<-c(); Ur<-c()
  for(i in 1:201){
    Ur<-y/(myM[i]+1)
    suppressWarnings({quan<-(-log(1-Ur))})
    R2<-summary(lm(Xr~quan))$r.squared
    myR2[i]<-R2
  }
  cand<-data.frame(myM,myR2)
  m<-cand$myM[which.max(myR2)]
  y<-y[y<m]
  U<-y/(m+1)
  Quan<-(-log(1-U))
  suppressWarnings({mylm2<-lm(1:length(U)~Quan)})
  par<-list(m=m, p=1/mylm2$coef[2], q=0, mu=mylm2$coef[1], sig=mylm2$coef[2], R2=max(myR2))
  return(par)
}




###### mle -----
#bass_m <- function(s,y,data){
#  t <- c(1:length(s))
#  f <- function(par){
#    m <- par[1]
#    p <- par[2]
#    q <- par[3]
#    e <- exp(-(p+q)*t)
    #fx <- (1-e)/(1+(q/p)*e)
    #fx <- (e+(q/p)*e)/(1+(q/p)*e)
#    y1 <- m*(1-e)/(1+(q/p)*e)
#    fx <- (p+q*y1/m)*(m-y1)
#    sum(log(fx))
#  }
#  optim1 <- optim(par=c(bass(s,y,data)[c(1,2,3)]), f, control=list(fnscale=-1))
#  m <- optim1$par[1]	
#  p <- optim1$par[2]
#  q <- optim1$par[3]
#  par <- list(m=m, p=p, q=q)
#  return(par) 
#}


#logis_m <- function(s,y,data){
#  x <- c(1:length(s))
#  f <- function(par){
#    mu <- par[1]
#    sigma <- par[2]    
#    z <- (x-mu)/sigma
#    fx <- exp(-z)/(sigma*(1+exp(-z))^2)
#    sum(log(fx))
#  }
#  optim1 <- optim(par=c(logis(s,y,data)[c(4,5)]), f, control=list(fnscale=-1))
#  mu <- optim1$par[1]	
#  sigma <- optim1$par[2]
#  p <- 0
#  q <- 1/sigma
#  t <- length(s)
#  m <- y[t]*(1+exp(-q*(t-mu)))
#  par<-list(m=m, p=p, q=q, mu=mu, sig=sigma)
#  return(par)   
#}

#gumbel_m <- function(s,y,data){
#  x <- c(1:length(s))
#  f <- function(par){
#    mu <- par[1]
#    sigma <- par[2]    
#    z <- (x-mu)/sigma
#    fx <- exp(-z-exp(-z))/sigma
#    sum(log(fx))
#  }
#  optim1 <- optim(par=c(gumbel(s,y,data)[c(4,5)]), f, control=list(fnscale=-1))
#  mu <- optim1$par[1]	
#  sigma <- optim1$par[2]
#  p <- 0
#  q <- 1/sigma
#  t <- length(s)
#  m <- y[t]*exp(-exp(-q*(t-mu)))
#  par<-list(m=m, p=p, q=q, mu=mu, sig=sigma)
#  return(par)   
#}

#expn_m<-function(s,y,data){
#  t <- c(1:length(s))
#  f <- function(par){
#    m <- par[1]
#    p <- par[2]
#    mu <- par[3]
#    y1 <- m*(1-exp(-p*(t-mu)))
#    fx <- p*(m-y1)
#    sum(log(fx))
#  }
#  optim1 <- optim(par=c(expn(s,y,data)[c(1,2,4)]), f, control=list(fnscale=-1))
#  m <- optim1$par[1]   
#  p <- optim1$par[2]   
#  mu <- optim1$par[3]
#  sigma <- 1/p
#  q <- 0 
#  par<-list(m=m, p=p, q=q, mu=mu, sig=sigma)
#  return(par)
#}



###### m,p,q 결과 출력 -----
result <- function(data, method, model){
  if(method=="ols"){
    if(model=="bs"){res<-bass(data$St, data$Yt, data)}
    else if(model=="lg"){res<-logis(data$St, data$Yt, data)}
    else if(model=="gb"){res<-gumbel(data$St, data$Yt, data)}
    else if(model=="epn"){res<-expn(data$St, data$Yt, data)}
  }
  else if(method=="qq"){
    if(model=="bs"){res<-qq_bass(data$St, data$Yt, data$t)}
    else if(model=="lg"){res<-qq_logis(data$St, data$Yt, data$t)}
    else if(model=="gb"){res<-qq_gumb(data$St, data$Yt, data$t)}
    else if(model=="epn"){res<-qq_exp(data$St, data$Yt, data$t)}
  }
  #else if(method=="mle"){
  #  if(model=="bs"){res<-bass_m(data$St, data$Yt, data)}
  #  else if(model=="lg"){res<-logis_m(data$St, data$Yt, data)}
  #  else if(model=="gb"){res<-gumbel_m(data$St, data$Yt, data)}
  #  else if(model=="epn"){res<-expn_m(data$St, data$Yt, data)} 
  #}
  return(res)
  
}


###### predict -----

pred<-function(par,model,step,dat){
  tm<-c(1:step); m<-par$m; pred_res<-c(); n<-nrow(dat)
  if(model=="bs"){
    myYt<-c();myYt[1]<-dat$Yt[1]
    mylm<-lm(St~Yt+I(Yt^2),data=dat)
    pred_res[1]<-predict(mylm, newdata = list(Yt=myYt[1]))
    for(i in 2:(step+n)){
      myYt[i]<-myYt[i-1]+pred_res[i-1]
      pred_res[i]<-predict(mylm, newdata = list(Yt=myYt[i]))
    }
    pred_res<-pred_res[(n+1):(step+n)]
  }
  else if(model=="lg"){
    mu<-par$mu; sig<-par$sig
    g<-function(t){exp(-t-exp(-t))}
    for(t in (n+1):(n+step)){pred_res[t-n]<-m*g((t-mu)/sig)/sig}
  }
  else if(model=="gb"){
    mu<-par$mu; sig<-par$sig
    g<-function(t){exp(-t)/((1+exp(-t))^2)}
    for(t in (n+1):(n+step)){pred_res[t-n]<-m*g((t-mu)/sig)/sig}
  }
  else if(model=="epn"){
    sig<-par$sig
    g<-function(t){exp(-t)}
    for(t in (n+1):(n+step)){pred_res[t-n]<-m*g(t/sig)/sig}
  }
  return(data.frame(After=tm, `Predicted Attendance`=round(pred_res,4)))
}



##### 서버 정의 #####
server <- function(input, output, session) {
  
  data<-reactive({
    infile<-input$file1
    
    if(is.null(infile)){
      return(NULL)
    }
    
    
    df<-read_csv(input$file1$datapath, 
                 col_types = cols(날짜 = col_date(format = "%Y.%m.%d")))
    colnames(df)<-c("date", "s", "y")
    df <- df %>% mutate(t=1:nrow(df))
    df <- df %>% mutate(day=wday(df$date, label = T))
    
    holiday <- input$file2
    
    if(!is.null(holiday)){
      holiday<-read_csv(holiday$datapath,
                        col_types = cols(`Start date` = col_date(format = "%Y-%m-%d"), Subject = col_skip()))
      holiday <- holiday %>% `names<-`(c("date")) %>% mutate(idx=T)
      df<- df %>% left_join(holiday, by="date")
      df$idx[df$day %in% c("Sat", "Sun")]<-T
      df<-df %>% mutate(St = ifelse(is.na(idx)!=T, s/input$weekend, s))
      df<-df %>% mutate(repnum = ifelse(is.na(idx)!=T, input$weekend,1))
      df<-as.data.frame(lapply(df,rep,df$repnum))
      #df<-df %>% mutate(St2 = ifelse(df$idx==T,df$St/input$weekend, df$St))
      #df$St<-df$St2
      df<-df %>% mutate(Yt = cumsum(St))
      df<-df[df$date >= as.Date(input$start),]
    }
    
    return(df)
    
  })
  
  predtable<-eventReactive(input$goButton,{
    data.df<-data()
    par<-result(data.df, input$mth, input$mdl)
    pred.df<-pred(par, input$mdl, input$bins, data.df)
  })
  
  myplot1<-eventReactive(input$goButton,{
    data.df<-data(); n<-nrow(data.df)
    par<-result(data.df, input$mth, input$mdl)
    pred.df<-pred(par, input$mdl, input$bins, data.df)
    predicted<-pred.df$`Predicted.Attendance`
    mydf<-data.frame(t=1:(n+input$bins), S=c(data.df$St, predicted))
    mydf %>% ggplot(aes(x=t, y=S, fill = t<=n)) +
      geom_col(show.legend = F) + theme_minimal() +
      ggtitle("Predicted Daily Attendance S(t)") +
      scale_y_continuous(name=expression(S[t]))+
      scale_x_continuous(name="t") +
      theme(plot.title = element_text(size=15, face="bold")) +
      coord_fixed(ratio = 2,) +
      facet_zoom(horizontal = F, xlim=c(n-5,n+input$bins), ylim=c(0,mean(sort(data.df$St)[1:15])),zoom.size = 0.5)
  }
  
  )
  
  mymhat<-eventReactive(input$goButton,{
    data.df<-data()
    valueBox(
      format(round(result(data.df,input$mth, input$mdl)$m,3),big.mark=","),
      "m (market potential)", icon = icon("cart-plus"),
      color = "purple"
    )
  })
  
  myphat<-eventReactive(input$goButton,{
    data.df <- data()
    valueBox(
      round(result(data.df,input$mth, input$mdl)$p,3),
      subtitle = "p (coef. of Innovation)", icon = icon("lightbulb"),
      color = "orange"
    )
  })
  
  myqhat<-eventReactive(input$goButton,{
    data.df <- data()
    valueBox(
      round(result(data.df,input$mth, input$mdl)$q,3),
      "q (coef. of Immitation)", icon = icon("users"),
      color = "olive"
    )
  })
  
  
  output$pred <- renderDataTable({
    predtable()
  })
  
  
  output$plot1<-renderPlot({
    myplot1()},
    
    height=600, width = 780
  )
  
  output$mhat <- renderValueBox({
    mymhat()
  })
  
  
  output$phat <- renderValueBox({
    myphat()
  })
  
  output$qhat <- renderValueBox({
    myqhat()
  })
  
}


shinyApp(ui = ui, server = server)