

library(readr)
library(markdown)
library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(dashboardthemes)
library(lubridate)
library(evd)
library(ggforce)
library(readxl)
library(cluster)
library(MASS)
library(gam)
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
library(mgcv)

options(scipen=999) 


##### UI 정의 #####
header <- dashboardHeader(title = "Project 8 - bankruptcy")

sidebar <- dashboardSidebar(
  
  selectInput("analysis", label = h4("Analysis Method", style="color:white;"),
              choices = list("GLM"=1, "GAM"=2, "Cox PHM"=3, "LDA"=4, "KNN"=5, 
                             "SVM"=6, "Random Forest"=7, "Neural Network"=8, "XGBoost"=9), selected = 1),
  textInput("name", label = h4("Company Name", style="color:white;"), value = "Corporation1"),
  
  fluidRow(
    column(6, radioButtons("industry", h5("업종", style="color:white;"), 
                           choices = list("경공업"="경공업", "중공업"="중공업", "건설업"="건설업", "도소매"="도소매", "서비스"="서비스"), selected = "경공업")),
    column(6, radioButtons("size", h5("규모", style="color:white;"), 
                           choices = list("개인"="개인", "외감"="외감", "비외감1"="비외감1", "비외감2"="비외감2", "소호"="소호"), selected = "비외감1")),
    tags$style(HTML('.radio label {color: white;}'))
    
  ),
  
  numericInput("x40", h5("기업 나이", style="color:white;"), value=2340, min=0, max=1000000, step=1),
  numericInput("x41", h5("로그매출액", style="color:white;"), value=14.5855, min=0, max=10000, step=1),
  numericInput("x42", h5("로그자산", style="color:white;"), value=15.05542, min=0, max=10000, step=1)
  
  
)


body <- dashboardBody(
  
  valueBoxOutput("BP", width = 4),
  valueBoxOutput("AZ", width = 4),
  valueBoxOutput("cf", width = 4),
  
  fluidRow(
    
    box(width=4,
        tabsetPanel(
          tabPanel("생산성, 성장성",
                   numericInput("x21", h5("총자산투자효율"), value=30.146, min=-1000, max=10000, step=1),
                   numericInput("x22", h5("매출채권증가율"), value=-49.092, min=-1000, max=10000, step=1),
                   numericInput("x23", h5("재고자산증가율"), value=39.4728, min=-1000, max=10000, step=1)
          ),
          tabPanel("수익성1",
                   numericInput("x24", h5("경영자본순이익율"), value=14.924, min=-1000, max=10000, step=1),
                   numericInput("x25", h5("금융비용/총부채비율"), value=4.187, min=-1000, max=10000, step=1),
                   numericInput("x1", h5("금융비용/총비용비율"), value=7.884, min=-1000, max=10000, step=1),
                   numericInput("x26", h5("자기자본순이익율"), value=1126.155, min=-1000, max=10000, step=1),
                   numericInput("x27", h5("자본금순이익율"), value=59.289, min=-1000, max=10000, step=1)
          ),
          tabPanel("수익성2",
                   numericInput("x28", h5("총자본경상이익율"), value=13.648, min=-1000, max=10000, step=1),
                   numericInput("x29", h5("총자본순이익율"), value=13.648, min=-1000, max=10000, step=1),
                   numericInput("x30", h5("총자본영업이익율"), value=13.828, min=-1000, max=10000, step=1),
                   numericInput("x31", h5("총자산사업이익율"), value=13.8285, min=-1000, max=10000, step=1)
          ),
        )
    ),
    
    box(width=4,
        tabsetPanel(
          tabPanel("안정성1",
                   numericInput("x2", h5("고정부채비율"), value=814.885, min=-1000, max=10000, step=1),
                   numericInput("x3", h5("고정비율"), value=760.455, min=-1000, max=10000, step=1),
                   numericInput("x4", h5("부채비율"), value=1137.238, min=-1000, max=10000, step=1),
                   numericInput("x5", h5("부채총계/자산총계비율"), value=91.917, min=-1000, max=10000, step=1),
                   numericInput("x6", h5("순부채/총자산비율"), value=91.513, min=-1000, max=10000, step=1),
                   numericInput("x7", h5("유동부채비율"), value=322.353, min=-1000, max=10000, step=1)
          ),
          tabPanel("안정성2",
                   numericInput("x8", h5("유동비율"), value=147.907, min=-1000, max=10000, step=1),
                   numericInput("x9", h5("유보액/총자산비율"), value=-15.071, min=-1000, max=10000, step=1),
                   numericInput("x10", h5("자기자본비율"), value=8.082, min=-1000, max=10000, step=1),
                   numericInput("x11", h5("차입금의존도"), value=65.863, min=-1000, max=10000, step=1),
                   numericInput("x12", h5("고정자산/차입금비율"), value=93.321, min=-1000, max=10000, step=1),
                   numericInput("x13", h5("차입금/자기자본비율"), value=814.885, min=-1000, max=10000, step=1)
          ),
          tabPanel("부채상환능력",
                   numericInput("x14", h5("고정재무비보상배율"), value=3.346, min=-1000, max=1000, step=1),
                   numericInput("x15", h5("총차입금/(총차입금+자기자본)비율"), value=89.069, min=-1000, max=1000, step=1),
                   numericInput("x16", h5("총CF/차입금비율"), value=27.958, min=-1000, max=1000, step=1),
                   numericInput("x17", h5("CF/차입금비율"), value=30.155, min=-1000, max=1000, step=1)
          ),
        )
    ),
    
    box(width=4,
        tabsetPanel(
          tabPanel("유동성",
                   numericInput("x18", h5("순운전자본/총자산비율"), value=12.481, min=-1000, max=1000, step=1),
                   numericInput("x19", h5("유동부채구성비율"), value=26.054, min=-1000, max=1000, step=1),
                   numericInput("x20", h5("현금비율"), value=1.386, min=-1000, max=1000, step=1)
          ),
          tabPanel("활동성1",
                   numericInput("x32", h5("경영자본회전율"), value=0.6795, min=-1000, max=1000, step=1),
                   numericInput("x33", h5("고정자산회전율"), value=0.9882, min=-1000, max=1000, step=1),
                   numericInput("x34", h5("매입채무회전율"), value=289.421, min=-1000, max=1000, step=1),
                   numericInput("x35", h5("매출채권회전율"), value=5.928, min=-1000, max=1000, step=1)
          ),
          tabPanel("활동성2",
                   numericInput("x36", h5("자기자본회전율"), value=51.278, min=-1000, max=1000, step=1),
                   numericInput("x37", h5("자본금회전율"), value=2.699, min=-1000, max=1000, step=1),
                   numericInput("x38", h5("재고자산회전율"), value=2.424, min=-1000, max=1000, step=1),
                   numericInput("x39", h5("총자본회전율"), value=0.621, min=-1000, max=1000, step=1)
          )
        )
    )
  )
)

ui <- dashboardPage(header, sidebar, body, shinyDashboardThemes(theme="poor_mans_flatly"))

#####BODY#####

replace.99 <- function(data) {
  for (i in 2:ncol(data)) {
    missing_rows <- which(is.na(data[,i]))
    
    if (length(missing_rows) > 0) {
      if (sum(!is.na(data[missing_rows,i]) & data[missing_rows,i] == 9999.990) > 0 | 
          sum(!is.na(data[missing_rows,i]) & data[missing_rows,i] == -9999.990) > 0) {
        
        if (sum(!is.na(data[missing_rows,i]) & data[missing_rows,i] == 9999.990) > 0) {
          quantile_value <- quantile(data[data[,i] != 9999.990, i], probs = 0.99, na.rm = TRUE)
          data[missing_rows[data[missing_rows,i] == 9999.990], i] <- quantile_value
        }
        if (sum(!is.na(data[missing_rows,i]) & data[missing_rows,i] == -9999.990) > 0) {
          quantile_value <- quantile(data[data[,i] != -9999.990, i], probs = 0.01, na.rm = TRUE)
          data[missing_rows[data[missing_rows,i] == -9999.990], i] <- quantile_value
        }
      }
    }
    else{
      if(sum(data[,i] == 9999.990) > 0 | sum(data[,i] == -9999.990) > 0){
        data[data[,i] == 9999.990,i] <- quantile(data[data[,i]!=9999.990,i], probs=0.99, na.rm=TRUE)
        data[data[,i] == -9999.990,i] <- quantile(data[data[,i]!=-9999.990,i], probs=0.01, na.rm=TRUE)
      }
    }
  }
  return(data)
}

myimputation <- function(x, k=10) {
  central.value <- function(x) {
    if (is.numeric(x)) {
      median(x,na.rm=T)
    }else if (is.factor(x)) {
      levels(x)[which.max(table(x))] 
    }else { 
      f <- as.factor(x)
      levels(f)[which.max(table(f))]
    }
  }
  dist.mtx <- as.matrix(daisy(x,stand=T))
  
  for(r in which(!complete.cases(x))){
    x[r,which(is.na(x[r,]))] <- apply(data.frame(x[c(as.integer(names(sort(dist.mtx[r,])[2:(k+1)]))), which(is.na(x[r,]))]), 2,central.value)
  }
  return(x)
}


##### 전처리 #####
df <- read_excel('2023-HW#8-과제용 (1).xls', sheet=1)

train1 <- df[is.na(df$부도여부)==FALSE,]   
train1[is.na(train1$로그매출액), c(33:40,42)] <- 0
train1 <- train1 %>% mutate(업종=as.factor(업종), 규모=as.factor(규모))
train1 <- train1[!is.na(train1$'총자산투자효율'),] 
train2 <- replace.99(train1)
train3 <- myimputation(as.data.frame(train2[,2:43])) 
train4 <- cbind(train2[,1], train3, train2[,44:47])

out_whole <- c()
for (i in 1:42){
  out_list <- train4[order(train4[,i]),] %>% filter() %>% dplyr::select(ID)
  out_list <- out_list[c(1:3, 3157:3159),]
  out_whole <- union(out_whole, out_list)
}

train5 <- train4[!train4$ID %in% out_whole,]  
train5 <- train5 %>% dplyr::select(ID, everything())
names(train5) <- c("ID", paste0("x", 1:44), "delta", "calender times")


train5$x36[train5$x36==0] <- min(train5$x36[train5$x36 > 0])
train5$x5[train5$x5==0] <- min(train5$x5[train5$x5 > 0])




##### FITTING #####

###### GLM ######
glm_fit <- glm(as.factor(delta) ~ x9 + x14 + x18 + I((1/x5)-1) + x39 + I(x27/x36) + I((x25*x4)/x36), family = binomial(link = "probit"), data = train5)


###### GAM ######
gam_fit <- mgcv::gam(as.factor(delta) ~ s(x3) + x4 + s(x8) + x9 + x13 + x19 + x22 + x23 + s(x26) + x27 + x28 + s(x30) + x37 + s(x38) + s(x40) + s(x41), family = binomial(link = "probit"), data = train5, method="REML")
#gam_fit <- gam(delta ~ x9 + x14 + x18 + I((1/x5)-1) + x39 + I(x27/x36) + I((x25*x4)/x36), family = binomial(link = "probit"), data = train5)


###### COX ######
cox_fit <- coxph(Surv(`calender times`,delta) ~ x1 + x2 + x3 + x4 + x6 + x13 + x18 + x22 + x25 + x26 + x28 + x32 + x33 + x35 + x39 + x40 + x41 + x42 + x44, data = train5)
S <- basehaz(cox_fit)
lm <- lm(log(hazard)~time, data = S)
alpha <- summary(lm)$coefficients[1]
beta <- summary(lm)$coefficients[2]
delta.t <- 365
Ht <- exp(alpha+beta*0)
Ht.delta <- exp(alpha+beta*(0+delta.t))
St <- exp(-Ht)
St.delta <- exp(-Ht.delta)


###### LDA ######
lda_fit <- lda(delta ~ x1 + x2 + x3 + x4 + x6 + x13 + x18 + x22 + x25 + x26 + x28 + x32 + x33 + x35 + x39 + x40 + x41 + x42, data = train5, cv=TRUE)


###### KNN ######
train_min <- apply(train5[,1:43], 2, min)
train_max <- apply(train5[,1:43], 2, max)
train_mm <- train5
for(i in 1:43){
  train_mm[,i] <- (train_mm[,i]-train_min[i])/(train_max[i]-train_min[i])
}
train_mm$ID <- train5$ID
x43_level <- as.character(1:5); names(x43_level) <- levels(train_mm$x43)
train_mm["x43"] <- as.factor(sapply(train_mm$x43, function(x) x43_level[[x]]))
x44_level <- as.character(1:5); names(x44_level) <- levels(train_mm$x44)
train_mm["x44"] <- as.factor(sapply(train_mm$x44, function(x) x44_level[[x]]))
train_mm$delta <- ifelse(train_mm$delta==1, "yes", "no")
train_mm$`calender times` <- train5$`calender times`
knn_fit <- knn(train=train_mm[,-c(1,46,47)], test=train_mm[,-c(1,46,47)], cl=train_mm$delta, k=51, prob=T)


###### SVM ######
svm_fit <- svm(delta ~ ., data=train5[,-c(1,47)], gamma=c(0.001,0.01,0.1,1,10), cost=c(0.1,1,3,5,10), probability=TRUE)


###### RF ######
rf_fit <- randomForest(as.factor(delta) ~ ., data=train5[,-c(1,47)], mtry= floor(sqrt(45)), importance=T)


###### NNET ######
nn_fit <- nnet(delta ~ ., data=train5[,-c(1,47)], size = 5, decay = 1)


###### XGB ######
xgb_fit <- xgboost(data=data.matrix(train5[,-c(1,46,47)]), label=train5$delta, nrounds=10)



##### SERVER #####
server <- function(input, output, session) {
  
  ###### df 처리 ######
  out <- reactive({
    df <- data.frame(x1=input$x1, x2=input$x2, x3=input$x3, x4=input$x4, x5=input$x5, x6=input$x6, x7=input$x7, x8=input$x8,
                     x9=input$x9, x10=input$x10, x11=input$x11, x12=input$x12, x13=input$x13, x14=input$x14, x15=input$x15,
                     x16=input$x16, x17=input$x17, x18=input$x18, x19=input$x19, x20=input$x20, x21=input$x21, x22=input$x22, x23=input$x23,
                     x24=input$x24, x25=input$x25, x26=input$x26, x27=input$x27, x28=input$x28, x29=input$x29, x30=input$x30, x31=input$x31,
                     x32=input$x32, x33=input$x33, x34=input$x34, x35=input$x35, x36=input$x36, x37=input$x37, x38=input$x38, x39=input$x39,
                     x40=input$x40, x41=input$x41, x42=input$x42, x43=as.factor(input$industry), x44=as.factor(input$size))
    
    score <- 0.012*input$x18 + 0.014*input$x9 + 0.033*input$x14 + 0.006*((1/input$x5)-1) + 0.999*input$x39
    
    return(list(df=df, score=score))
  })
  
  ###### Predict ######
  pr <- reactive({
    out <- out()
    df <- out$df
    df$x43 <- factor(df$x43, levels=c("건설업","경공업","도소매","서비스","중공업"))
    df$x44 <- factor(df$x44, levels=c("개인","비외감1","비외감2","소호","외감"))
    
    if(input$analysis==1){       # GLM
      pred <- predict(glm_fit, df, type = "response")[[1]]
    }
    else if(input$analysis==2){  # GAM
      pred <- predict(gam_fit, df, type = "response")[[1]]
    }
    else if(input$analysis==3){  # Cox
      mu.cox <- predict(cox_fit, df, type = 'risk')
      pred <- 1-(St.delta/St)^mu.cox
    }
    else if(input$analysis==4){  # LDA
      ld_hat <- predict(lda_fit, df)
      pred <- ld_hat$posterior[,2]
    }
    else if(input$analysis==5){  # KNN
      test_mm <- df
      test_mm <- cbind("ID"=1, test_mm)
      for(i in 1:43){
        test_mm[,i] <- (test_mm[,i]-train_min[i])/(train_max[i]-train_min[i])
      }
      test_mm["x43"] <- sapply(test_mm$x43, function(x) x43_level[[x]])
      test_mm["x44"] <- sapply(test_mm$x44, function(x) x44_level[[x]])
      kn_fit <- knn(train=train_mm[,-c(1,46,47)], cl=train_mm$delta, test=test_mm[,-1], k=51, prob=TRUE)
      pred <- 1-attr(kn_fit,"prob")
    }
    else if(input$analysis==6){  # SVM
      pred <- predict(svm_fit, df, probability = TRUE)
    }
    else if(input$analysis==7){  # RF
      pred <- predict(rf_fit, df, type="prob")[,2]
    }
    else if(input$analysis==8){  # NN
      pred <- predict(nn_fit, newdata=df, type="raw")[,1]
    }
    else if(input$analysis==9){  # XGB
      pred <- predict(xgb_fit, data.matrix(df))
    }
    
    return(pred)
  })
  
  
  ###### 부도확률 ######
  output$BP <- renderValueBox({
    valueBox({
      pr <- pr()
      round(pr, 6)
    },
    h4(paste0(input$name, "'s Bankruptcy Probability")))       
  })
  
  ###### Z score ######
  output$AZ <- renderValueBox({
    valueBox({
      out <- out()
      round(out$score, 4)
    },
    h4("Altman Z score"))       
  })
  
  ###### Altman Zone ######
  output$cf <- renderValueBox({
    valueBox({
      out <- out()
      if(out$score <= 1.81){
        "Zone I" 
      }
      else if(out$score >= 3){
        "Zone II"
      }
      else{
        "grey area"
      }
    },
    h4("Cut-off points"), color="teal")       
  })
}


shinyApp(ui = ui, server = server)



















