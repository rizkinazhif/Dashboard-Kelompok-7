library(dplyr)                   # this packages is use for data preparation (edit, remove, mutate, etc)
library(stringr)                 # all functions deal with "NA"'s and zero length vectors
library(purrr)                   # requirement packages for Functional Programming Tools
library(rlang)                   # requirement packages for Rmarkdown
library(DT)                      # interface to the JavaScript library DataTables (https://datatables.net/)
library(r2d3)                    # D3 visualization
library(shinydashboard)
library(shiny)

source("global.R", local = TRUE)

ui <- dashboardPage(title = "Kelompok 7",
                    skin = "purple",
                    
                    dashboardHeader(title = "Statistik Kejahatan"), 
                    
                    dashboardSidebar(
                      selectInput(
                        inputId = "year",
                        label = "Tahun:",
                        choices = year_list,
                        selected = "all_year",
                        selectize = F
                      ),
                      
                      sidebarMenu(
                        selectInput(
                          inputId = "crime",
                          label = "Jenis Kejahatan:",
                          choices = crime_list,
                          selected = "all_crime",
                          selectize=F))
                    ), 
                    
                    dashboardBody(
                      tabsetPanel(id = "tabs",
                                  tabPanel(title = strong("Statistik"),
                                           value = "page1",
                                           icon = icon("chart-bar"),
                                           fluidRow(valueBoxOutput("total_crime"),
                                                    valueBoxOutput("per_prov"),
                                                    valueBoxOutput("most_crime")),
                                           fluidRow(box(title = "Peta Persebaran Total Kasus Kejatahan",
                                                        status = "primary",
                                                        solidHeader = T,
                                                        width = 12,
                                                        leafletOutput("mymap"))),
                                           br(),
                                           fluidRow(box(title = "Jumlah Kejahatan yang Dilaporkan per Provinsi",
                                                        status = "primary",
                                                        solidHeader = T,
                                                        width = 4,
                                                        d3Output("group_totals2")),
                                                    box(title = "Jumlah Kejahatan Indonesia 2017-2019",
                                                        status = "primary",
                                                        solidHeader = T,
                                                        width = 4,
                                                        d3Output("group_totals3")),
                                                    box(title = "Tindak Kejahatan di Indonesia",
                                                        status = "primary",
                                                        solidHeader = T,
                                                        width = 4,
                                                        plotlyOutput("tindak_pidana")))
                                  ),
                                  tabPanel(title = strong("Data"),
                                           value = "page2",
                                           icon = icon("table"),
                                           fluidRow(box(title = "Tabel Dinamis",
                                                        status = "primary",
                                                        solidHeader = T,
                                                        width = 12,
                                                        DTOutput('table_crime'))))
                      )
                    ))