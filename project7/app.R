library(shiny)
library(shinydashboard)
library(tidyverse)
options("scipen" = 100)
library(readxl)
library(MASS)
library(survival)
library(randomForest)
library(data.table)
library(mltools)
library(xgboost)
library(nnet)
library(NeuralNetTools)
library(ROCR)
library(DT)
library(caret)
library(e1071)
library(kknn)
library(class)
library(gam)
#setwd("C:/Users/서민지/Desktop/통계 대학원/2023-2/이론통계학2/Project 7/app")

header <- dashboardHeader(title = "Life Insurance 2조")


sidebar <- dashboardSidebar(
  width = 250,
  
  sidebarMenu(id="type",
              menuItem("Insurance Cancellation Probability", tabName = "acc", icon = icon("id-card")),
              
              br(),
              
              fileInput('file', label=h4('파일을 업로드하세요.'),
                        accept=c('text/csv', 
                                 'text/comma-separated-values,text/plain', 
                                 '.csv')),
              
              #가입자 이름
              textInput("name", h4("가입자 이름"), value="이민주"),
              
              #가입연령
              numericInput("age", h4("가입 나이"), value=25),
              
              #보험료
              numericInput("premium",h4("보험료"),value = 30000),
              
              #분석방법
              selectInput("a.method", h4("분석방법"), 
                          choices = c("GLM","GAM","CoxPHM","LDA","KNN",
                                      "RandomForest","SVM","XGBoost","NeuralNetwork"))
              
  )
)

body <- dashboardBody(
  
  fluidRow(
    column(6,
           valueBoxOutput("p", width=12),
           br(),
           DT::dataTableOutput("table")),
    
    column(3,
           
           #현재시점
           dateInput("current.date", h4("현재시점"), value = "2001-06-30", format = "yyyy-mm-dd", startview = "year", weekstart = 0, language = "kr"),
           
           #무만기 여부
           radioButtons("x8", h4("무만기 여부"), 
                        choices = list("Yes"= 1,
                                       "No" = 0),  selected = 0),
           #계약일자 : 만기일자
           conditionalPanel(
             condition = "input.x8 == 0",
             dateRangeInput("dates", h4("계약일자 to 지급만기일자"), start = "2000-02-08", end = as.character(Sys.Date()), format = "yyyy-mm-dd", startview = "year", weekstart = 0, language = "kr", separator = " to ")
           ),
           
           # 계약일자
           conditionalPanel(
             condition = "input.x8 == 1",
             dateInput("cont.date", h4("계약일자"), value = "2000-02-08", format = "yyyy-mm-dd", startview = "year", weekstart = 0, language = "kr")
           ),

           sliderInput("x3", h4("납입기간(년)"),       
                       min = 0, max = 50, value = 10, step = 1),
           
           numericInput("pay.num",h4("최종납입횟수"),value = 30)
           ),
    
    column(3,
           radioButtons("x6", h4("부활 유무"), 
                        choices = list("Yes"= 1,
                                       "No" = 0),  selected = 0),
           numericInput("unpaid.num", h4("미납횟수"), value = 1),
           selectInput("x4", h4("수금방법"), 
                       choices = list("방문" = 1,
                                      "자동이체" = 2,
                                      "지로" = 3,
                                      "직납" = 4,
                                      "카드납" = 5)),
           
           selectInput("x2", h4("납입방법"), 
                       choices = list("월납" = 1,
                                      "3월납" = 2,
                                      "6월납" = 3,
                                      "연납" = 4)),
           
           selectInput("x9", h4("납입형태"), 
                       choices = list("전기납" = 1,
                                      "단기납" = 0)),
           
           selectInput("x7", h4("상품종류"), choices = list(
             건강보험 = list("암보험"=11,"CI보험"=17,"간병보험"=19),
             사망보험 = list("정기보험"=23,"종신보험"=34),
             의료실비보험 = list("의료실비보험"=55),
             기타보험 = list("상해보험"=42,"어린이보험"=46,"태아보험"=48,"유병자보험"=49)
           ), selectize = FALSE) 
           )
    ))



ui <- dashboardPage(header, sidebar, body)

###############################################################################

## function

train.data <- read_csv("train1106.csv")

## data setting
data.setting <- function(df){
  result <- df%>%
    mutate(delta=as.factor(delta), x2=as.factor(x2),
           x4=as.factor(x4), x6=as.factor(x6), x7=as.factor(x7), x8=as.factor(x8), x9=as.factor(x9),
           x12=as.factor(x12), x13=as.factor(x13), x14=as.factor(x14))
  return(result)
}

data.setting(train.data) -> train.df

fac.level <- list(x2=levels(train.df$x2), x4=levels(train.df$x4), x6=levels(train.df$x6), x7=levels(train.df$x7),
                  x8=levels(train.df$x8), x9=levels(train.df$x9), x12=levels(train.df$x12), x13=levels(train.df$x13), x14=levels(train.df$x14))

## Cox PHM용 데이터 -----
aaa <- read_excel("insurance.data.xls", sheet = 2,
                  col_names = c("delta", paste0("x",1:11)), skip = 1) %>%
  mutate(period = interval(x7, as.Date("2001-06-30")) %/% months(1),
         totpay = ifelse(x2==4, x9*12,
                         ifelse(x2==3, x9*6,ifelse(x2==2, x9*3, x9))),
         diff = period - totpay,
         maintmth = ifelse(diff>2, totpay + 3,
                           ifelse((diff==1 | diff ==2), diff + totpay, period))) %>%
  dplyr::select(delta, x1, x2, x3, x6, maintmth, period, diff, totpay) %>%
  mutate(x2=as.factor(x2), x6=as.factor(x6))


#cox 외 필요한 함수들 ----
##납입방법 input 시 납입간격 output하는 function
pay.interval <- function(a){
  if(a == 1){result <- 1}
  else if(a == 2){result <- 3}
  else if(a == 3){result <- 6}
  else {result <- 12}
  return(result)
}

## 총경과월수 function
total.passed <- function(a,b,c){
  if ((as.Date(a) + years(b)  >= as.Date(c))){
      result <- interval(as.Date(a), as.Date(c)) %/% months(1)}
  else {result <- b * 12}
  return(result)
}


## 총계약월수 function
cont.month <- function(a,b){
  result <- interval(as.Date(a), as.Date(b)) %/% months(1)
  return(result)
  }

## 해지예측여부 function
cancel.fn <- function(a,b,c){
  #a: 총경과월수, b: 납입간격, c: 미납횟수 
  이상적횟수 <- floor(a / b) + 1
  최대미납허용횟수 <- 이상적횟수 - floor((이상적횟수-1)/3) - 1
  
  if(c > 최대미납허용횟수){result<-1}
  else{result<-0}
  return(result)
}

## 미납여부 function
unpaid.fn <- function(a){
  if(a>0){result<-1}
  else{result<-0}
  return(result)
}

# 기대여명으로 실제만기일자 구하는 function
life.table <- read.csv("lifetable2001.csv", fileEncoding = "euc-kr", skip=1) %>% dplyr::select(1:2) %>% `colnames<-`(c("age","기대여명"))

life.fn <- function(a,b,c){
  #a: 가입연령, b: 계약일자, c: 현재시점
  현연령 <- a + interval(as.Date(b),as.Date(c)) %/% years(1)
  실제만기년도 <- floor(life.table[현연령+1,"기대여명"])+year(as.Date(c))
  실제만기일자 <- as.Date(paste0(substr(실제만기년도,1,4),substr(b,5,10)))
  return(실제만기일자)
}

##가입연령층 계산하는 function
age_range<-function(a){
  if(a<20){result<-1}
  else if(a<30){result<-2}
  else if(a<40){result<-3}
  else if(a<50){result<-4}
  else if(a<65){result<-5}
  else {result<-6}
  return(result)
}


## input data 전처리 function
data.setting2 <- function(df){
  
  result<-df%>%
    mutate(x2=factor(x2, levels = fac.level[[1]]), x4=factor(x4, levels = fac.level[[2]]),
           x6=factor(x6, levels = fac.level[[3]]), x7=factor(x7, levels = fac.level[[4]]),
           x8=factor(x8, levels = fac.level[[5]]), x9=factor(x9, levels = fac.level[[6]]),
           x12=factor(x12, levels = fac.level[[7]]), x13=factor(x13, levels = fac.level[[8]]),
           x14=factor(x14, levels = fac.level[[9]]))
  
  return(result)
}



###############################################################################


# model fitting
# glm
glm_logit <- glm(delta ~ x1 + x3 + log1p(x5) + x6 + x12 + x13 + log1p(x15) + x13:log1p(x15) + x6:log1p(x15) + x3:x13 + x3:x6 + log1p(x5):x13,
                  family = binomial(link = "logit"), data = train.df)

# gam
cloglog = function(x) log(-log(1-x))
gam <- gam(delta ~ x1 + log1p(x5) + x12 + x13 + log1p(x15), train.df, family=binomial(link=cloglog))
# print(summary(gam))
# print(head(gam$fitted.values))
# print(head(predict(gam, newdata=train.df, type = 'response')))

# cox----
cox <- coxph(Surv(totpay, delta) ~ x1 + x2 + x3 + x6 + maintmth + period + diff + x6:maintmth, data = aaa)
# print(summary(cox))
# S <- basehaz(cox)
# lm <- lm(log(hazard)~log(time), data = S)
# alpha <- summary(lm)$coefficients[1]
# beta <- summary(lm)$coefficients[2]

# lda
ld <- lda(delta~., data=train.df)

# scaling data
coln <- which(colnames(train.df) %in% c("delta","x2","x4","x6","x7","x8","x9","x12","x13","x14"))
data.scale <- train.df
max.train <- apply(train.df[,-coln], 2, max)
min.train <- apply(train.df[,-coln], 2, min)
# print("max")
# print(max.train)
# print("min")
# print(min.train)
min_max <- function(x){return((x-min(x))/(max(x)-min(x)))}
data.scale[,-coln] <- as.data.frame(lapply(data.scale[,-coln], min_max))

# knn data
data2 <- data.scale %>% mutate(delta=ifelse(delta==1, "yes", "no"))
best_k <- 7
kn_fit <- knn(train=data2[,-1], cl=data2$delta, test=data2[,-1], k=best_k, prob=TRUE)

# svm
svm_fit <- svm(delta~., data=train.df, gamma=0.1, cost=5, probability=TRUE)
# print(summary(svm_fit))


# one-hot encoding
data.one<-one_hot(as.data.table(train.df[,-1]))
data.xgb <- xgb.DMatrix(data=as.matrix(data.one),label=as.numeric(as.character(train.df$delta)))
# xgb
xgb_model =xgb.train(
  data=data.xgb,
  max.depth = 5, eta = 0.01, nthread = 2, nrounds = 2, objective = "binary:logistic",
  subsample = 0.8, min_child_weight = 1, verbose=0
)


# rf
rf_model <- randomForest(delta ~ ., data=train.df, mtry = floor(sqrt(15)), ntree = 500, importance = T)

# nn
nn_model <- nnet(delta ~. , data=data.scale, size = 2, decay = 5e-04)


# newdata에 적용하는 function
p.function<-function(newdata){
  set.seed(123)
  coln <- which(colnames(newdata) %in% c("x2","x4","x6","x7","x8","x9","x12","x13","x14"))
  newdata.scale <- newdata
  newdata.scale[,-coln] <- (newdata.scale[,-coln]-min.train)/(max.train-min.train)

  newdata.xgb <- one_hot(as.data.table(rbind(newdata, train.df[,-1])))
  newdata.xgb <- newdata.xgb[1,]
  newdata.xgb <- as.matrix(newdata.xgb)

  # glm
  glm_pred <- predict(glm_logit, newdata=newdata, type = 'response')
  #print(glm_pred)

  # gam
  #print(predict(gam, newdata=head(data), type="response"))
  gam_pred <- predict(gam, newdata=newdata, type = 'response')
  # print(gam_pred)

  # cox
  #predict_cox <- 1 - predict(cox, newdata=newdata, type="survival")
  # Ht <- exp(alpha+beta*log(newdata$x15))
  # Ht.delta <- exp(alpha+beta*log((newdata$x15+3)))
  # St<- exp(-Ht)
  # St.delta<-exp(-Ht.delta)
  # mu.cox <- exp(predict(cox, newdata, type = 'lp'))
  # cox_pred <- 1-(St.delta/St)^mu.cox
  #print(paste("cox: ",cox_pred))

  # lda
  ld_hat <- predict(ld, newdata)
  lda_pred <- ld_hat$posterior[,2]
  #print(lda_pred)

  # knn
  set.seed(123)
  kn_fit <- knn(train=data2[,-1], cl=data2$delta, test=newdata.scale, k=best_k, prob=TRUE)
  knn_pred <- 1-attr(kn_fit,"prob")
  #print(paste("knn: ",knn_pred))

  # svm
  svm_hat <- predict(svm_fit, newdata, probability = TRUE)
  svm_pred <- attr(svm_hat, "prob")[,2]
  #print(paste("svm: ", svm_pred))

  #xgboost
  xgb_pred = predict(xgb_model, newdata = newdata.xgb, reshape=T)

  #rf
  rf_pred <- predict(rf_model, newdata, type="prob")[,2]
  #print(paste("rf: ",rf_pred))

  #nn
  nn_pred<-predict(nn_model,newdata=newdata.scale)
  #print(paste("nn: ", nn_pred))


  p <- list(GLM=glm_pred, GAM=gam_pred,
            LDA=lda_pred, KNN=knn_pred, SVM=svm_pred,
            RandomForest=rf_pred, NeuralNetwork=nn_pred, XGBoost=xgb_pred)
  return(p)
}

# cox p.function -----
p.function2<-function(newdata){
  newdata$delta=0
  p <- 1 - exp(-predict(cox, newdata = newdata, type="expected"))
  p[is.na(p)] <- 0
  return(p=list(CoxPHM=p))
}




# Server

server <- function(input, output, session){

  out <- reactive({
    
    if(is.null(input$file)){
      return(NULL)
    }
    
    #csv 읽기
    input.df<-read.csv(input$file$datapath); input.df <- data.setting(input.df)
    
    # 납입간격 <- pay.interval(as.numeric(input$x2))
    # 총경과월수 <- ifelse(as.numeric(input$x8) == 0, total.passed(input$dates[1], input$x3, input$current.date), total.passed(input$cont.date, input$x3, input$current.date))
    # 총계약월수 <- ifelse(as.numeric(input$x8) == 0, cont.month(input$dates[1], input$dates[2]), cont.month(input$cont.date, life.fn(input$age, input$cont.date, input$current.date)))
    
    #input data 지정
    df<-data.frame(x1 = age_range(as.numeric(input$age)), x2 = as.numeric(input$x2), x3 = as.numeric(input$x3), x4=as.numeric(input$x4),
                   x5 = as.numeric(input$premium) / pay.interval(as.numeric(input$x2)), x6=as.numeric(input$x6), x7=as.numeric(input$x7),
                   x8=as.numeric(input$x8), x9=as.numeric(input$x9), x10=round(ifelse(as.numeric(input$x8) == 0, total.passed(input$dates[1], input$x3, input$current.date), total.passed(input$cont.date, input$x3, input$current.date)) / ifelse(as.numeric(input$x8) == 0, cont.month(input$dates[1], input$dates[2]), cont.month(input$cont.date, life.fn(as.numeric(input$age), input$cont.date, input$current.date))) * 100),
                   x11=ifelse(as.numeric(input$x8) == 0, cont.month(input$dates[1], input$dates[2]), cont.month(input$cont.date, life.fn(input$age, input$cont.date, input$current.date))),
                   x12=cancel.fn(ifelse(as.numeric(input$x8) == 0, total.passed(input$dates[1], input$x3, input$current.date), total.passed(input$cont.date, input$x3, input$current.date)), pay.interval(as.numeric(input$x2)),as.numeric(input$unpaid.num)),
                   x13=unpaid.fn(as.numeric(input$unpaid.num)),
                   x14=ifelse(as.numeric(input$x8) == 0, quarter(as.Date(input$dates[1])), quarter(as.Date(input$cont.date))), x15 = as.numeric(input$pay.num)*pay.interval(as.numeric(input$x2)))
    
    #cox용 input -----
    df_cox<-data.frame(x1 = as.numeric(input$age),
                       x2 = factor(as.numeric(input$x2), levels = fac.level[[1]]),
                       x3 = as.numeric(input$x3),
                       x6 = factor(as.numeric(input$x6), levels = fac.level[[3]]),
                       period = ifelse(as.numeric(input$x8)==0,
                                       interval(input$dates[1], ymd(20010630)) %/% months(1),
                                       interval(input$cont.data, ymd(20010630)) %/% months(1)),
                       totpay = ifelse(input$x2==4,input$pay.num*12,
                                       ifelse(input$x2==3, input$pay.num*6,
                                              ifelse(input$x2==2, input$pay.num*3, input$pay.num)))) %>%
      mutate(diff = period - totpay,
             maintmth = ifelse(diff>2, totpay + 3,
                               ifelse((diff==1 | diff ==2), diff + totpay, period)))
    
    if(input$a.method=="CoxPHM"){
      res<-p.function2(df_cox)
      return(list(data=df_cox, p=res))}
    else{
      df2 <- data.setting2(df)
      res <- p.function(df2)
      return(list(data=df, p=res))}
    
    
  }
  )



  output$table <- renderDataTable({
      df<-data.frame(name = input$name,
                   x1 = input$age,
                   x2 = ifelse(as.numeric(input$x2)==1,"월납",ifelse(as.numeric(input$x2) == 2,"3월납",ifelse(as.numeric(input$x2) == 3,"6월납","연납"))), x3=paste0(input$x3, " 년"),
                   x4=ifelse(as.numeric(input$x4)==1,"방문",ifelse(as.numeric(input$x4)==2,"자동이체",ifelse(as.numeric(input$x4)==3,"지로",ifelse(as.numeric(input$x4)==4,"직납","카드납")
                   ))), x5=paste0(format(input$premium/pay.interval(as.numeric(input$x2)), big.mark=","),"원"), x6=ifelse(input$x6==1,"O","X"),
                   x7_1 = ifelse((as.numeric(input$x7) == 11) |(as.numeric(input$x7) == 17) | (as.numeric(input$x7) == 19), "건강보험",
                                 ifelse((as.numeric(input$x7)==23)|(as.numeric(input$x7)==34),"사망보험",
                                        ifelse((as.numeric(input$x7)==55),"의료실비보험",
                                               ifelse((as.numeric(input$x7) == 42)|(as.numeric(input$x7) == 46)|(as.numeric(input$x7) == 48)|(as.numeric(input$x7) == 49),"기타보험")))),
                   x7_2 = ifelse(as.numeric(input$x7) == 11,"암보험",
                                 ifelse(as.numeric(input$x7) == 17,"CI보험",
                                        ifelse(as.numeric(input$x7) == 19,"간병보험",
                                               ifelse(as.numeric(input$x7)==23,"정기보험",
                                                      ifelse(as.numeric(input$x7)==34,"종신보험",
                                                             ifelse(as.numeric(input$x7)==55,"의료실비보험",
                                                                    ifelse(as.numeric(input$x7) == 42,"상해보험",
                                                                           ifelse(as.numeric(input$x7) == 46,"어린이보험",
                                                                                  ifelse(as.numeric(input$x7) == 48,"태아보험",
                                                                                         ifelse(as.numeric(input$x7) == 49,"유병자보험")))))))))),
                   x8=ifelse(as.numeric(input$x8)==1,"O","X"),
                   x9=ifelse(input$x9==1,"전기납","단기납"), x10=paste0(round(ifelse(as.numeric(input$x8) == 0, total.passed(input$dates[1], input$x3, input$current.date), total.passed(input$cont.date, input$x3, input$current.date)) / ifelse(as.numeric(input$x8) == 0, cont.month(input$dates[1], input$dates[2]), cont.month(input$cont.date, life.fn(input$age, input$cont.date, input$current.date))) * 100), " %"),
                   x11=paste0(ifelse(as.numeric(input$x8) == 0, cont.month(input$dates[1], input$dates[2]), cont.month(input$cont.date, life.fn(input$age, input$cont.date, input$current.date))), " 개월"), x13=ifelse(input$unpaid.num > 0,"O","X"),
                   x14=paste0(ifelse(as.numeric(input$x8) == 0, quarter(as.Date(input$dates[1])), quarter(as.Date(input$cont.date))), ' 분기'), x15 = paste0(input$pay.num*pay.interval(as.numeric(input$x2)), ' 개월'))
  
    colnames(df)<-c("이름","가입연령",'납입방법','납입기간','수금방법','월보험료','부활유무','상품중분류','상품소분류','무만기 여부','납입형태',
                    '경과비율','총계약월수','미납여부','계약만기분기','최종납입기간')
    
    df <- as.data.frame(t(df)); colnames(df) <- "고객정보"
    
    DT::datatable(
      df,
      options = list(paging = FALSE, searching = FALSE, columnDefs = list(list(className = 'dt-center', targets = '_all'))))
  })
  
  
  output$p <- renderValueBox({
    valueBox({out <- out()
    round(out$p[[input$a.method]],3)},
    subtitle = paste0(input$name," 고객님의 보험 해지율 with ", input$a.method), icon = icon("file-signature"), color = "light-blue"
    )})

}

shinyApp(ui=ui, server = server)
