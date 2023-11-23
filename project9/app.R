library(shiny)
library(flexdashboard)
library(shinydashboard)
library(readxl)
library(dplyr)
library(survival)
library(survminer)
library(MASS)
library(caret)
library(ggplot2)
library(gridExtra)
library(gtsummary)
library(flexdashboard)
library(shinydashboard)
library(dashboardthemes)

# setwd("C:/Users/서민지/Desktop/통계 대학원/2023-2/이론통계학2/Project 9")

header <- dashboardHeader(title = "Heart disease attack predicion")


sidebar <- dashboardSidebar(
  
  
  sidebarMenu(id="type",
              
              fileInput('file', label=h4('Upload Data File'),
                        accept=c('text/csv', 
                                 'text/comma-separated-values,text/plain', 
                                 '.csv')),
              textInput("name", h4("Name"), value = "Seo Minji"),
              sliderInput("age", h4("Age in years"), value=26,min=0,max=100),
              radioButtons("sex", h4("Gender"), choices=list("male"=1,
                                                             'female'=0)),
              
              numericInput("height",h4("Height(cm)"),value=164),
              numericInput("weight",h4("Weight(kg)"),value=49),
              numericInput("month",h4("Month"),value=11)
              
              
  ))





body <- dashboardBody(

  # fluidRow(valueBoxOutput("p.cox",width=6),
  #          valueBoxOutput("p.alt",width=6)),
  
  fluidRow(box(column (flexdashboard::gaugeOutput("p.chart") ,width = 6),
               column(flexdashboard::gaugeOutput("p.chart2"),width = 6),
               title="Heart Disease Attack Probability",solidHeader = T ,status = "primary", height = 250 ,width = 12 )
  ),
  
  fluidRow(
    box(
      sliderInput("sbp", h3("Systolic blood pressure"), value=100, min = 0, max = 300),
      sliderInput("dbp", h3("Diastolic blood pressure"), value=80,min=0,max=300),
      sliderInput("scl", h3("Serum cholesterol"), value=200,min=0,max=600),height = 500, width = 7
    ) ,
    valueBoxOutput("bmi",width=5),valueBoxOutput("health",width=5),
    valueBoxOutput("text",width=5),valueBoxOutput('chol',width=5)
    
  ),
  
  
  #fluidRow(valueBoxOutput("bmi",width=3),
  #         valueBoxOutput("bloodpressure",width=3)),
  # 
  # fluidRow(valueBoxOutput("text",width=3),
  #          valueBoxOutput('chol',width=3))
  # 
  
)


ui <- dashboardPage(header, sidebar, body, skin = "black")


### imputation 함수 정의
myimputation<-function(x,k=10){
  
  # this function imputes x-matrix using k-nn imputataion
  
  # x : x-matrix, k: nearest k neighborhood 
  
  # central. value functions fills the missing data
  
  # if numeric -> median
  
  # if categorical -> most frequen value
  
  
  central.value <- function(x) {
    
    if (is.numeric(x)) median(x,na.rm=T)
    
    else if (is.factor(x)) levels(x)[which.max(table(x))] #mode value
    
    else { #Compute mode value after change character varible to factor
      
      f <- as.factor(x)
      
      levels(f)[which.max(table(f))]
      
    }
    
  }
  
  library(cluster)
  
  #dist.mtx has all pairwise distances in x-matrix
  
  #it uses daisy function in cluster package
  
  dist.mtx<-as.matrix(daisy(x,stand=T))
  
  for(r in which(!complete.cases(x))) x[r,which(is.na(x[r,]))] <- apply(data.frame(x[c(as.integer(names(sort(dist.mtx[r,])[2:(k+1)]))), which(is.na(x[r,]))]), 2,central.value)
  
  
  return(x)
  
}


###이상치 탐지 함수 정의
detect_outliers <- function(df, n, features) {
  outlier_indices <- c()
  
  for (col in features) {
    Q1 <- quantile(df[[col]], 0.25)
    Q3 <- quantile(df[[col]], 0.75)
    IQR <- IQR(df[[col]])
    
    outlier_step <- 1.5 * IQR
    
    outlier_list_col <- which(df[[col]] < Q1 - outlier_step | df[[col]] > Q3 + outlier_step)
    
    outlier_indices <- c(outlier_indices, outlier_list_col)
  }
  
  outlier_counts <- table(outlier_indices)
  multiple_outliers <- as.numeric(names(outlier_counts[outlier_counts > n]))
  
  return(multiple_outliers)
}

### 내장데이터 정의
# dat <- read.csv("Framingham.csv")
# 
# dat<-dat %>% mutate(month = as.factor(month), sex = as.factor(sex))
# 
# # knn으로 NA imputation
# dat_noNA <- cbind(myimputation(as.data.frame(dat[,c("sbp","dbp","scl","age","bmi","month","sex")])), dat[,c("chdfate","followup")])
# 
# 
# myOUT<-detect_outliers(dat_noNA, n=1, features = c("sbp","dbp","scl","age","bmi"))
# 
# dat_noNA <- dat_noNA[-myOUT,]
# 
# dat_all<-dat_noNA%>%
#   mutate(bp.fac = factor(ifelse(sbp>=180|dbp>=120,"high3",ifelse(sbp>=140|dbp>=90,"high2",ifelse(sbp>=130|dbp>=80,"high1",ifelse(sbp>=120&dbp<80,"elevated","normal")))),
#                          ordered = T, levels=c("normal","elevated","high1","high2","high3")),
#          bmi.fac = factor(ifelse(bmi<18.5,"underweight",ifelse(bmi<25,"healthy",ifelse(bmi<30,"overweight",ifelse(bmi<40,"obesity","severe obesity")))),ordered = T,
#                           levels=c("underweight","healthy","overweight","obesity","severe obesity")),
#          age.fac = factor(ifelse(age<35,"age3034",ifelse(age<45,"age3544",ifelse(age<55,"age4554",ifelse(age<65,"age5564","age6569"))))))
# 

dat_all <- read.csv("dat_all.csv")
dat_all$bp.fac <- factor(dat_all$bp.fac, levels=c("normal","elevated","high1","high2","high3"))
dat_all$bmi.fac <- factor(dat_all$bmi.fac, levels=c("underweight","healthy","overweight","obesity","severe obesity"))
dat_all$age.fac <- factor(dat_all$age.fac)

# centering
mean1<-apply(dat_all[c("sbp","dbp","scl","age","bmi")],2,mean)
dat_all <- dat_all %>% mutate(scl=log(scl/mean1[["scl"]]))

dat_all <- dat_all  %>%
  dplyr::select(scl,month,sex,bp.fac,bmi.fac,age.fac,chdfate,followup)

dat_all$chdfate <- as.factor(dat_all$chdfate)
dat_all$sex <- as.factor(dat_all$sex)

#factor level 지정
fac.level <- list(bp.fac = levels(dat_all$bp.fac), bmi.fac=levels(dat_all$bmi.fac), age.fac=levels(dat_all$age.fac), sex=levels(dat_all$sex))




### server에서 input data에 대한 data setting 함수
data.setting<-function(df){
  # df <- read.csv("Framingham.csv")
  
  df<-df %>% mutate(month = as.factor(month), sex = as.factor(sex))
  
  
  df<-df%>%
    mutate(bp.fac = factor(ifelse(sbp>=180|dbp>=120,"high3",ifelse(sbp>=140|dbp>=90,"high2",ifelse(sbp>=130|dbp>=80,"high1",ifelse(sbp>=120&dbp<80,"elevated","normal")))),
                           ordered = T, levels=fac.level[[1]]),
           bmi.fac = factor(ifelse(bmi<18.5,"underweight",ifelse(bmi<25,"healthy",ifelse(bmi<30,"overweight",ifelse(bmi<40,"obesity","severe obesity")))),ordered = T,
                            levels=fac.level[[2]]),
           age.fac = factor(ifelse(age<35,"age3034",ifelse(age<45,"age3544",ifelse(age<55,"age4554",ifelse(age<65,"age5564","age6569")))), levels=fac.level[[3]]))
  
  
  # centering
  df <- df %>% mutate(scl=log(scl/mean1["scl"]))
  
  
  df <- df  %>%
    dplyr::select(scl,month,sex,bp.fac,bmi.fac,age.fac)
  
  df$sex <- as.factor(df$sex)
  
  return(df)
}




### ALT(weibull simple+step)
# alt.w <- survreg(Surv(followup,as.numeric(chdfate)-1)~ ., data = dat_all, dist = 'weibull')
alt.w.step <-survreg(Surv(followup,as.numeric(chdfate)-1)~ scl+sex+bp.fac+bmi.fac+age.fac, data = dat_all, dist = 'weibull')

sigma.alt<-alt.w.step$scale

### Cox(simple+step)
cox.step<-coxph(Surv(followup, as.numeric(chdfate)-1)~scl+sex+bp.fac+bmi.fac+age.fac, data = dat_all)
# cox.step<-stepAIC(cox, direction="both", trace=0)



S <- basehaz(cox.step)
lm <- lm(log(hazard)~log(time), data = S)
alpha <- summary(lm)$coefficients[1]
beta <- summary(lm)$coefficients[2]


p.function<-function(newdata){
  mu.alt <- predict(alt.w.step,type = "lp",newdata=newdata)
  u.alt <- (log(3650)-mu.alt)/sigma.alt
  alt.p<-(1-exp(-exp(u.alt)))
  
  mu.cox <- predict(cox.step,type = 'risk',newdata=newdata)
  cox.p<-1-exp(-exp((alpha+beta*(log(3650))))*mu.cox)
  p<-list(ALT=alt.p,CoxPHM=cox.p)
  return(p)
}




server <- function(input, output, session){
  out <- reactive({
    df<-data.frame(sbp = input$sbp, dbp = input$dbp,age=input$age,scl=input$scl,
                   bmi=input$weight/((input$height)*0.01)^2, sex=input$sex, month=input$month)
    df2 <- data.setting(df)
    # print(df)
    res <- p.function(df2)
    bmi<-df$bmi
    scl<-df$scl
    dbp<-df$dbp
    sbp<-df$sbp
    return(list(data=df, p.cox=res$CoxPHM, p.alt = res$ALT ,bmi=bmi,scl=scl,dbp=dbp,sbp=sbp))
  }
  )
  
  output$p.chart <- renderGauge({
    # out <- out()
    rate <- round(out()$p.alt,3)*100
    gauge(as.numeric(rate) ,min = 0, max = 100, symbol = '%',  label= "ALT",
          gaugeSectors(
            success = c(0,15), warning = c(15,60), danger = c(60,100)
          )) 
  })
  
  output$p.chart2 <-renderGauge({
    gauge(as.numeric(round(out()$p.cox,3)*100) ,min = 0, max = 100, symbol = '%', label= "CoxPHM",
          gaugeSectors(
            success = c(0,15), warning = c(15,60), danger = c(60,100)
          )) 
  })
  
  
  output$bmi <- renderValueBox({
    valueBox({out <- out()
    round(out$bmi,1)},
    paste0(input$name,"'s BMI"),color = "purple", icon=icon("child"))
  })
  
  output$health <- renderValueBox({
    out<-out()
    if ((out$bmi)<=18.5){
      mycolor="blue"
      myicon = icon('exclamation')
      
    }
    else if (18.5<(out$bmi)&(out$bmi)<25){
      mycolor="green"
      myicon = icon('smile-wink')
    }
    else if (25<=(out$bmi)&(out$bmi)<30){
      mycolor="yellow"
      myicon = icon("exclamation-triangle")
    }
    else if (30<=(out$bmi)&(out$bmi)<40){
      mycolor="orange"
      myicon = icon("exclamation-triangle")
    }
    else{
      mycolor="red"
      myicon = icon("bomb")
    }
    valueBox({out <- out()
    if ((out$bmi)<=18.5){
      print("UNDERWEIGHT!!")
      
      
    }
    else if (18.5<(out$bmi)&(out$bmi)<25){
      print("Healthy")
      
    }
    else if (25<=(out$bmi)&(out$bmi)<30){
      print("Overweight")
      
    }
    else if (30<=(out$bmi)&(out$bmi)<40){
      print("OBESITY!!")
      
    }
    else{
      print("!!SEVERE OBESITY!!")
    }
    
    },h4("BMI Condition"),color = mycolor,icon = myicon)
  })
  output$text<-renderValueBox({
    out<-out()
    if (out$sbp >= 180 | out$dbp >=120){
      mycolor="red"
      myicon = icon("bomb")
    }
    else if (out$sbp>=140 | out$dbp>=90){
      mycolor="orange"
      myicon = icon('exclamation-triangle')
    }
    else if (out$sbp>=130 | out$dbp>=80){
      mycolor="yellow"
      myicon = icon('exclamation-triangle')
    }
    else if (out$sbp>=120 & out$dbp<80){
      mycolor="green"
      myicon = icon('exclamation-triangle')
    }
    else{
      mycolor="blue"
      myicon = icon('smile-wink')
    }
    
    valueBox({out<-out()
    if (out$sbp >= 180 | out$dbp >=120){
      print("DANGER!!")
    }
    else if (out$sbp>=140 | out$dbp>=90){
      print("TOO High")
    }
    else if (out$sbp>=130 | out$dbp>=80){
      print("High")
    }
    else if (out$sbp>=120 & out$dbp<80){
      print("Be Careful")
    }
    else {
      print("Healthy")
    }
    
    
    },h4("Blood Pressure Condition"),color=mycolor,icon = myicon)
  })
  output$chol<-renderValueBox({
    out<-out()
    if (out$scl >= 240){
      mycolor="red"
      myicon = icon("bomb")
    }
    else if (out$scl>=200&out$scl<240){
      mycolor="orange"
      myicon = icon('exclamation-triangle')
    }
    else{
      mycolor="green"
      myicon = icon('smile-wink')
    }
    
    valueBox({out<-out()
    if (out$scl >= 240){
      print("DANGER!!")
    }
    else if (out$scl>=200&out$scl<240){
      print("Caution")
    }
    else {
      print("Healthy")
    }
    
    
    },h4("Cholesterol Condition"),color=mycolor,icon = myicon )
  })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

