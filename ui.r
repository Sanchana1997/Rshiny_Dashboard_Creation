
library(AMR)
library(data.table)
library(DT)
library(ggridges)
library(lubridate)
library(plotly)
library(qicharts2)
library(rintrojs)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(survival)
library(ggpubr)
library(survminer)
library(tidyverse)
library(dplyr)
library(viridis)
library(zoo)
library(gapminder)
library(gganimate)
library(transformr)
library(readr)
library(gifski)
#library(tseries) # time series analysis
library(forecast) # time series analysis
#library(prophet) # time series analysis
#library(timetk) 
#library(fpp2)




fluid_design <- function(id, w, x, y, z, t1, t2, t3, t4) {
  fluidRow(
    div(
      id = id,
      column(width = 6,
             plotlyOutput(w, width = "500px")),
      column(width = 6,
             plotlyOutput(y, width = "500px")),
      column(width = 6,
             plotlyOutput(x, width = "500px")),
      column(width = 6,
             plotlyOutput(z, width = "500px"))
      )
    )
}

fluid_design3 <- function(id, w, x, y, z, t1, t2, t3, t4) {
  fluidRow(
    div(
      id = id,
      column(width = 6,
             plotlyOutput(y, width = "500px")),
      column(width = 6,
             imageOutput(w, width = "800px")),
      column(width = 6,
             plotlyOutput(x, width = "500px")),
      column(width = 6,
             plotlyOutput(z, width = "500px"))
    )
  )
}

fluid_design2 <- function(id, x, y, z, t1, t2, t3) {
  fluidRow(div(
    id = id,
    column(width = 12,
           div(plotlyOutput(x), align = "center")),
    column(width = 6,
           plotlyOutput(y, width = "500px")),
    column(width = 6,
           plotlyOutput(z, width = "500px"))
  ))
}
fluid_design5 <- function(id, x, y, z, t1, t2, t3) {
  fluidRow(div(
    id = id,
    column(width = 12,
           div(plotlyOutput(x), align = "center")),
    column(width = 6,
           imageOutput(y)),
    column(width = 6,
           plotlyOutput(z, width = "500px"))
  ))
}
fluid_design4 <- function(id, x, y, z, t1, t2, t3) {
  fluidRow(div(
    id = id,
    column(width = 12,
           div(imageOutput(x), align = "center")),
    column(width = 6,
           plotlyOutput(y, width = "500px")),
    column(width = 6,
           plotlyOutput(z, width = "500px"))
  ))
}

fluid_design6 <- function(id, x) {
  fluidRow(div(
    id = id,
    column(width = 12,
           div(plotOutput(x), align = "center"))
    )
  )
}

ui <- fluidPage(skin = "black",
                title = "World",
                titlePanel(title = span(icon("globe", class="fa-solid"), 
                                        "GLOBAL SNAPSHOT")),
                sidebarLayout(
                  sidebarPanel(
                    id = "sidebar",
                    width = 3,
                    plotlyOutput("globe"),
                    fluidRow(
                      column(1,code("WORLD POPULATION 7.734 B")),
                      column(2,offset=5,code("TRADE (% OF GDP) 93.9%"))),
                    fluidRow(
                      column(1,code("ELECTRIC CONSUMPTION 1064 KWH PER CAPITA")),
                      column(2,offset=5,code("GREEN HOUSE EMISSION 4.59 GIGATONNES")))
                  ,
                   # imageOutput("pop1"),
                    textOutput("stat1")
                    
                  ),
                  mainPanel(
                    tags$head(
                      tags$link(rel = "stylesheet",
                                type = "text/css",
                                href = "mystyle.css"),
                      tags$style(HTML("
            code {
                display:block;
                padding:10px;
                margin:0 0 0 0px;
                margin-top:10px;
                font-size:14px;
                line-height:25px;
                word-break:break-all;
                word-wrap:break-word;
                white-space:pre-wrap;
                background-color:#c9e3cc;
                border:2px solid rgba(0,0,0,0.15);
                border-radius:4px; 
                font-family:monospace;
                font-weight:bold;
                alligh-content:center;
                color:black;
                width:130px; 
            }"))
                    ),
                    
                    useShinyjs(),
                    introjsUI(),
                    
                    # MAIN BODY ---------------------------------------------------------------
                    
                    
                    fluidRow(column(
                      width = 12,
                      introBox(
                        bsButton(
                          "social",
                          label = "SOCIAL",
                          icon = icon("globe", class = "fa-solid"),
                          style = "success"
                        ),
                        bsButton(
                          "economic",
                          label = "ECONOMIC",
                          icon = icon("wallet"),
                          style = "success"
                        ),
                        bsButton(
                          "environmental",
                          label = "ISSUES",
                          icon = icon("flask", class = "flask-box"),
                          style = "success"
                        ),
                        bsButton(
                          "modeling",
                          label = "MODELING",
                          icon = icon("signal"),
                          style = "success"
                        )
                      )
                    )),
                   
                    fluid_design5(
                      "economic_panel",
                      "box1",
                      "box2",
                      "box3",
                      "Title 1",
                      "Title 2",
                      "Title 3"
                    ),
                    fluid_design2(
                      "environmental_panel",
                      "box5",
                      "box6",
                      "box7",
                      "Title 1",
                      "Title 2",
                      "Title 3"
                    ),
                    # fluid_design(
                    #   "modeling_panel",
                    #   "box_los1",
                    #   "box_los2",
                    #   "box_los3",
                    #   "box_los4",
                    #   "Title 1",
                    #   "Title 2",
                    #   "Title 3",
                    #   "Title 4"
                    # ),
                    fluid_design6(
                      "modeling_panel",
                      "box_los1"
                    ),
                    fluid_design4(
                      "social_panel",
                      "box_pat",
                      "box_pat2",
                      "box_year",
                      "Title 1",
                      "Title 2",
                      "Title 3"
                    )
                    
                  )
                )
                )
