library(shiny)
library(bslib)
library(bsicons)
library(scales)
library(htmltools)
library(shinyWidgets)
library(readr)
library(tidyverse)
library(dplyr)
library(MASS)
library(lubridate)
library(shinyjs)
options(scipen=999)

#### BODY ----

##### MC Simulation ----
myMC<-function(S0,r,sigma,t,K,M=1000000){
  set.seed(2)
  z<-rnorm(M)
  st<-S0*exp((r-0.5*sigma^2)*t+sigma*sqrt(t)*z)
  st.call<-sapply(st-K, max, 0)
  st.put<-sapply(K-st, max, 0)
  ct<-exp(-r*t)*sum(st.call)/M
  pt<-exp(-r*t)*sum(st.put)/M
  return(list(call=ct, put=pt))
}




#### UI ----
ui <- page_navbar(title = "Option Price Calculation",
                  #includeCSS("www/bootstrap.css"),
                  useShinyjs(),
                  theme = bs_theme(version = 5, bootswatch = 'minty'),
                  ##### KOSPI ----
                  nav_panel(title = "KOSPI(2021)", 
                            layout_column_wrap(width = 1/2, height = "500px",
                                               card(fill = FALSE,
                                                    width = 400,
                                                    card_header(class = "border-primary", "시장정보"),
                                                    card_body(
                                                      height = "100px",
                                                      layout_column_wrap(
                                                        width = 1/2,
                                                        tags$h5("Riskless Interest Rate (%)"),
                                                        numericInput("r", step = 0.1, value=0.0488,label = NULL)
                                                      )),
                                                    card_body(
                                                      height = "100px",
                                                      layout_column_wrap(
                                                        width = 1/2,
                                                        tags$h5("Volatility"),
                                                        withTags(
                                                          div(class="form-group",
                                                              fieldset(disabled="",
                                                                       input(class="form-control", id="sigma", type="text", placeholder="0.1609", disabled=""))
                                                          )
                                                        )
                                                        #numericInput("sigma", step = 0.01, value=0.1609,label = NULL)
                                                      )),
                                                    card_body(
                                                      height = "100px",
                                                      layout_column_wrap(
                                                        width = 1/2,
                                                        tags$h5("Present Price(￦)"),
                                                        withTags(
                                                          div(class="form-group",
                                                              fieldset(disabled="",
                                                                       input(class="form-control", id="S0", type="text", placeholder="2998.32", disabled=""))
                                                          )
                                                        )
                                                      ))                                   
                                               ),
                                               card(fill = FALSE,
                                                    width = "400px",
                                                    card_header(class = "border-primary", "상품정보"),
                                                    card_body(
                                                      height = "100px", 
                                                      HTML('<div class="btn-group" role="group" aria-label="Basic radio toggle button group">
  <input type="radio" class="btn-check" name="btnradio" id="btnradio1_call" autocomplete="off" checked="">
  <label class="btn btn-outline-primary" for="btnradio1_call">Asian Call</label>
  <input type="radio" class="btn-check" name="btnradio" id="btnradio2_put" autocomplete="off" checked="">
  <label class="btn btn-outline-primary" for="btnradio2_put">Asian Put</label>
</div>')
                                                    ),
                                                    card_body(
                                                      height = "100px",
                                                      layout_column_wrap(
                                                        width = 1/2,
                                                        tags$h5("Exercise Price(￦)"),
                                                        numericInput("k", value=2977.65,label = NULL)
                                                      )),
                                                    card_body(
                                                      height = "100px",
                                                      layout_column_wrap(width=NULL,
                                                                         style = css(grid_template_columns = "1fr 1fr"),
                                                                         tags$h5(`for`="tInput", class="form-label", "Maturity (years)"),
                                                                         sliderInput("tInput", label = NULL, min=1, max=12, step=1, value=5)
                                                      )
                                                    )
                                               )),
                            layout_columns(fill = FALSE,
                                           value_box(theme_color = "secondary",
                                                     title = "Asian Call/Put Option Price",
                                                     value = textOutput("price"),
                                                     showcase = bsicons::bs_icon("cash-stack")
                                           )
                            )
                  ),
                  ##### MANUAL ----
                  nav_panel(title = "MANUAL", 
                            layout_column_wrap(width = 1/2, height = "500px",
                                               card(fill = FALSE,
                                                    width = 400,
                                                    card_header(class = "border-primary", "시장정보"),
                                                    card_body(
                                                      height = "100px",
                                                      layout_column_wrap(
                                                        width = 1/2,
                                                        tags$h5("Riskless Interest Rate (%)"),
                                                        numericInput("r", step = 0.1, value=0.0488,label = NULL)
                                                      )),
                                                    card_body(
                                                      height = "100px",
                                                      layout_column_wrap(
                                                        width = 1/2,
                                                        tags$h5("Volatility"),
                                                        numericInput("sigma", value=0.1609,label = NULL)
                                                      )),
                                                    card_body(
                                                      height = "100px",
                                                      layout_column_wrap(
                                                        width = 1/2,
                                                        tags$h5("Present Price(￦)"),
                                                        numericInput("S0", value=2998.32,label = NULL)
                                                      ))                                   
                                               ),
                                               card(fill = FALSE,
                                                    width = "400px",
                                                    card_header(class = "border-primary", "상품정보"),
                                                    card_body(
                                                      height = "100px", 
                                                      HTML('<div class="btn-group" role="group" aria-label="Basic radio toggle button group">
  <input type="radio" class="btn-check" name="btnradio" id="btnradio3_call" autocomplete="off" checked="">
  <label class="btn btn-outline-primary" for="btnradio3_call">Asian Call</label>
  <input type="radio" class="btn-check" name="btnradio" id="btnradio4_put" autocomplete="off" checked="">
  <label class="btn btn-outline-primary" for="btnradio4_put">Asian Put</label>
</div>')
                                                    ),
                                                    card_body(
                                                      height = "100px",
                                                      layout_column_wrap(
                                                        width = 1/2,
                                                        tags$h5("Exercise Price(￦)"),
                                                        numericInput("k", value=2977.65,label = NULL)
                                                      )),
                                                    card_body(
                                                      height = "100px",
                                                      layout_column_wrap(width=NULL,
                                                                         style = css(grid_template_columns = "1fr 1fr"),
                                                                         tags$h5(`for`="tInput", class="form-label", "Maturity (years)"),
                                                                         sliderInput("tInput", label = NULL, min=1, max=12, step=1, value=5)
                                                      )
                                                    )
                                               )),
                            layout_columns(fill = FALSE,
                                           value_box(theme_color = "secondary",
                                                     title = "Asian Call/Put Option Price",
                                                     value = textOutput("price2"),
                                                     showcase = bsicons::bs_icon("cash-stack")
                                           )
                            )
                  )
)


#### Server ----
server <- function(input, output, session) {
  
  
  shinyjs::onclick(
    "btnradio1_call",
    {
      output$price<-renderText({
        myMC(S0=2998.32, r=input$r, sigma=0.1609186, t=input$tInput, K=input$k)$call %>% round() %>% format(big.mark=",") %>% paste("원")
        
        
      })
    }
  )
  
  shinyjs::onclick(
    "btnradio2_put",
    {
      output$price<-renderText({
        myMC(S0=2998.32, r=input$r, sigma=0.1609186, t=input$tInput, K=input$k)$put %>% round() %>% format(big.mark=",") %>% paste("원")
      })
    }
  )
  
  shinyjs::onclick(
    "btnradio3_call",
    {
      output$price2<-renderText({
        myMC(S0=input$S0, r=input$r, sigma=input$sigma, t=input$tInput, K=input$k)$call %>% round() %>% format(big.mark=",") %>% paste("원")
      })
    }
  )
  
  shinyjs::onclick(
    "btnradio4_put",
    {
      output$price2<-renderText({
        myMC(S0=input$S0, r=input$r, sigma=input$sigma, t=input$tInput, K=input$k)$put %>% round() %>% format(big.mark=",") %>% paste("원")
      })
    }
  )
  
  
}

shinyApp(ui = ui, server = server)