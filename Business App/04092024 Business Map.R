library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(ggplot2)
library(stringr)
library(data.table)
library(cowplot)
library(rebus)
library(shinydashboard)
library(shiny)
library(plotly)
library(leaflet)
library(fontawesome)
library(RColorBrewer)
library(scales)



## *Side note: Change library path to make sure that i can use old packages*: 
## Current library path: "C:/Users/la000062/OneDrive - Defra/Migrated Data/R/win-library/4.0" "C:/Program Files/R/R-4.2.2/library"
## Changed to old library path: "C:/Users/la000062/OneDrive - Defra/Migrated Data/R/win-library/4.0"
## .libPaths()
## alternative: tell R to look for your library folder in a specific location using lib.loc

Business_data <- readRDS("Business Map2024-09-04.RDS")


#####################functions##########################

##1. 

Business_namevalue <- format(length(unique(Business_data$compname)), big.mark = ",")


##2.

createbusinessCN8codes <- function(flow) {
  x <- format(length(unique(Business_data$comcode[Business_data$flow == flow])), big.mark = ",")
}

##3.

createcomcount <- function(data,flow1,number) {
  com_count_import <- data %>%
    filter(flow == flow1) %>%
    unique() %>%
    count(comcode) %>%
    arrange(n) %>%
    top_n(number) 
  
}

##4.

createHS2graph <- function(data) {
  ggplotly(ggplot(data,aes(x = reorder(comcode, n), y = n, text = paste0(comma(n)))) +
             geom_bar(position = "stack", stat = "identity", fill = "#6F2DA8") +
             theme(axis.text.x = element_text(angle = 90)) +
             scale_y_continuous(labels = scales::comma_format()) +
             labs(y = "No. of businesses", x = "comcode"), tooltip = c("text"))
  
}

##5.

createmap_data <- function(data) {
  mapdata <- data %>%
    group_by(compname,HS2,flow) %>%
    select(HS2,comcode,compname,address,postcode,SICCode,flow,Period, LAT,LONG) %>%
    distinct()
  
}

##6. 

create_map_plot <- function(data1) {
  
  pal <- colorFactor(palette = c("red","blue"),domain = data1$flow)
  
  
  mymap <- leaflet(data = data1) %>%
    addTiles() %>%
    setView( lng=-2.0, lat=54.0, zoom = 6 ) %>%
    addCircleMarkers( lat = ~LAT, lng = ~LONG, popup = ~paste(compname,"<br>HS Code:", HS2,"<br>CN8:",comcode ,"<br>Trade Flow:",flow, "<br>Sector:",SICCode, "<br>Period:",Period),
                      color = ~pal(flow),
                      clusterOptions = markerClusterOptions()
    )
  
}

#7.

filter_Business_data <- function(data1, Business1, HS2_type, flow1) {
  if (Business1 == "All" & HS2_type == "All" & flow1 == "All") {
    return(data1)
  } else {
    
    if (Business1 != "All" & HS2_type == "All" & flow1 == "All") {
      filtered_data <- data1[data1$compname == Business1, ]
    }
    
    if (Business1 != "All" & HS2_type != "All" & flow1 != "All") {
      filtered_data <- data1[data1$compname == Business1 & data1$HS2 == HS2_type & data1$flow == flow1, ]
    }
    ## seems not to work at the moment
    if (Business1 != "All" & HS2_type != "All" & flow1 == "All") {
      filtered_data <- data1[data1$compname == Business1 & data1$HS2 == HS2_type, ]
    }
    
    if (HS2_type != "All" & Business1 == "All" & flow1 == "All") {
      filtered_data <- data1[data1$HS2 == HS2_type, ]
    }
    
    if (HS2_type != "All" & Business1 == "All" & flow1 != "All") {
      filtered_data <- data1[data1$HS2 == HS2_type & data1$flow == flow1, ]
    }
    
    if (flow1 != "All" & Business1 == "All" & HS2_type == "All") {
      filtered_data <- data1[data1$flow == flow1, ]
    }
    
    return(filtered_data)
  }
}


#Building the app

## Overview tab
## General summary tab - covers all data within the app - general statistics 
## Splitting out by preference (The map + tables - allowing user to pick their preferences i.e. HS code, Business etc.)

#Part 1- structure and how it looks

#Setting up the sidebar with app pages

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "overview"),
    menuItem("App", tabName= "app")
  ) #close sidebar menu
) #close sidebar

#Populating the pages of the app
body <- dashboardBody(
  tabItems(
    tabItem(tabName="overview",
            h3(strong("Welcome to the UK Business Details App")),
            h5("This app has been produced by the Trade Analysis, Project Delivery and Support (TAPS) team to present the business details information published by HMRC in a more accessible format."),
            box(width="100%", height="100%",
                h4(strong("User guide")),
                h5(strong("Information:")),
                h5("This app plots the names and addresses of UK importers and exporters trading products with non-EU countries and GB importers and exporters trading products with EU countries from 2022 until May 2024 onto a map. "),
                h5("It aims to help support analysis on tariffs and Rules of Origin by identifying which businesses trade which product (which may help with supply chain analysis) and where these businesses are located in the UK (which may help with regional analysis)." ),
                h5("It uses the data published by HMRC here: https://www.uktradeinfo.com/trade-data/latest-bulk-datasets/bulk-datasets-archive/#importer-details" ),
                
                h5(strong("Important notes and caveats:")),
                h5("HMRC make this data available, however there are many important caveats. For example, HMRC do not provide details of the following for legal reasons:"),
                h5("- Quantities or values of transactions "),
                h5("- Traders' customers "),
                h5("- VAT or EORI details of traders "),
                h5("- Countries where the goods moved to or from "),
                h5("- Traders who have asked us to remove details of their trading activity "),
                
                h5("They also have to withhold some data to protect sensitive national and business interests."),
                h5("Some businesses have incomplete HS codes, or incomplete or mistyped addresses, which limits how we can plot them."),
                h5("There is also the caveat that the trade may be recorded at the 'head office' address." ),
                
                
                h5(strong("How to use:")),
                
                h5("App built by: Trade Policy Analysis, November 2023."),
                h5(strong("If you have any questions, please contact: Katie Earl or Louise Anokye.")))
            
    ),  #close tab item for overview tab
    
    
    tabItem(tabName="app",
            
            fluidPage(width = "100%", height = "100%",
                      column(width = 12,
                             
                             tabsetPanel(
                               tabPanel("Summary",
                                        box(status='primary', solidHeader=TRUE, width="100%", height="100%",
                                            fluidRow(width = "100%",
                                                     
                                                     column(width = 12,
                                                            valueBoxOutput("number_business"),
                                                            valueBoxOutput("number_imports"),
                                                            valueBoxOutput("number_exports")
                                                     ),
                                                     
                                                     column(width = 6,
                                                            h4(strong(textOutput("HS2_import_title"))),
                                                            textOutput("HS2_importtext"),
                                                            plotlyOutput("importgraph")
                                                     ),
                                                     
                                                     column(width = 6,
                                                            h4(strong(textOutput("HS2_export_title"))),
                                                            textOutput("HS2_exporttext"),
                                                            plotlyOutput("exportgraph")
                                                     )
                                                     
                                                     
                                            ))),
                               tabPanel("Map",
                                        box(status='primary', solidHeader=TRUE, width="100%", height="100%",
                                            fluidRow(width = "100%",
                                                     
                                                     column(width = 2,
                                                            uiOutput("Business_UI")
                                                     ),
                                                     column(width = 2,
                                                            uiOutput("HS2_UI") 
                                                     ),
                                                     column(width = 2,
                                                            uiOutput("flow_UI") 
                                                     ),
                                                     column(width = 12,
                                                            leafletOutput("map", width = "100%", height = "1000px"))
                                                     
                                            )
                                            
                                        )),
                               tabPanel("Business Tables",
                                        fluidPage(width = "100%", height = "100%",
                                                  
                                                  DT::dataTableOutput("table")
                                        )
                               )
                               
                             )
                      ) # close column
            )# close fluidpage
            
    ) #close tabitem
  )# close tabitems
)# close dashboard body


# Part 2- What to display, e.g. workings
server <- function(input, output, session){ #workings and  calcs start here
  
  output$Business_UI <- renderUI({
    
    selectInput(inputId = "Business",
                label = "Choose Business",
                choices = c("All",sort(unique(Business_data$compname),decreasing = FALSE)), ## default selected
                multiple = F,
                selected = "All")
    
  })
  
  output$HS2_UI <- renderUI({
    option2 <- c("All",sort(unique(Business_data$HS2)))
    if(!is.null(input$Business)){
      if(input$Business == "All"){
        option2 <- c("All",sort(unique(Business_data$HS2)))
      }else{
        option2 <- c("All",sort(unique(Business_data$HS2[which(Business_data$compname == input$Business)])))  
      }
    }
    selectInput(inputId = "HS2",
                label = "Choose HS2",
                choices = option2, ## default selected
                multiple = F,
                selected = "All")
    
    
  })
  
  output$flow_UI <- renderUI({
    option3 <- c("All",sort(unique(Business_data$flow)))
    if(!is.null(input$HS2))
    {
      if(input$Business == "All"){
        option3 <- c("All",sort(unique(Business_data$flow)))
      }else{
        option3 <- c("All",sort(unique(Business_data$flow[which(Business_data$compname == input$Business & Business_data$HS2 == input$HS2)]))) 
        
      }
    }
    selectInput(inputId = "flow",
                label = "Choose flow",
                choices = option3,
                multiple = F,
                selected = "All") 
    
  })
  
  
  
  ## First Value box shows number of businesses in dataset
  
  output$number_business <- renderValueBox({
    
    valueBox(value = Business_namevalue,
             subtitle = "Number of businesses",
             color = "purple")
    
  })
  
  
  ## second box shows the number of CN8 codes - imports
  
  
  output$number_imports <- renderValueBox({
    
    valueBox(value = createbusinessCN8codes("imports"),
             subtitle = "Number of CN8 codes (imports)",
             color = "purple")
    
  })
  
  ## third box shows the number of CN8 codes - Exports
  
  output$number_exports <- renderValueBox({
    
    valueBox(value = createbusinessCN8codes("exports"),
             subtitle = "Number of CN8 codes (exports)",
             color = "purple")
    
  })
  
  
  ## The following title will appear for the comcode import graph
  
  output$HS2_import_title <- renderText({
    
    
    paste0(
      
      "Top 10 comcode codes by imports")
  })
  
  
  ## The following title will appear for the comcode export graph
  
  output$HS2_export_title <- renderText({
    
    
    paste0(
      
      "Top 10 comcode codes by exports")
  })
  
  
  output$HS2_importtext <- renderText({
    
    ## The following text will accompany the comcode graph for top 10 imports
    
    paste0(
      "The graph below presents the top 10 imports by comcode."
    )
    
  })
  
  ## The following text will accompany the comcode graph for top 10 exports
  
  output$HS2_exporttext <- renderText({
    
    paste0(
      "The graph below presents the top 10 exports by comcode."
    )
    
  })
  
  ## This creates the interactive comcode graph that shows the Top 10 exports by comcode
  
  #graph - Top comcode codes by flow (exports)
  
  ## This creates the interactive comcode graph that shows the Top 10 imports by comcode
  
  
  #Top 10 comcode codes by flow (imports)
  
  com_count_import <- createcomcount(Business_data,"imports",10) 
  
  
  #Top 10 comcode codes by flow (exports)
  
  com_count_export <- createcomcount(Business_data,"exports",10)
  
  
  #graph - Top 10 comcode codes by flow (imports)
  
  output$importgraph <- renderPlotly({
    
    createHS2graph(com_count_import)
    
  })
  
  #graph - Top 10 comcode codes by flow (exports)
  
  output$exportgraph <- renderPlotly({
    
    createHS2graph(com_count_export)
  })
  
  
  
  ### functions to create reactive maps ###
  
  
  business_test <- reactive({
    
    filter_Business_data(createmap_data(Business_data), input$Business, input$HS2, input$flow)
    
  })
  
  ### maps ###
  
  output$map <- renderLeaflet({
    req(length(input$Business)> 0 | length(input$HS2)> 0 | length(input$flow)> 0)
    
    create_map_plot(business_test())
    
  })
  
  ## tables
  
  ## This creates the tables generated in the tabs
  
  output$table <- DT::renderDataTable({
    
    businesstable <- business_test() %>%
      select(HS2,comcode,compname,address,postcode,flow, Period)
    
    
    names(businesstable)[names(businesstable) == "comcode"] <- "CN8"
    names(businesstable)[names(businesstable) == "compname"] <- "Company"
    names(businesstable)[names(businesstable) == "address"] <- "Address"
    names(businesstable)[names(businesstable) == "postcode"] <- "Postcode"
    names(businesstable)[names(businesstable) == "flow"] <- "Flow"
    
    DT::datatable(businesstable,
                  rownames = F, filter= 'top',
                  extensions = 'Buttons',
                  options = exprToFunction(list(searching = TRUE, paging = FALSE, dom = 'Bfrtip', scrollX = TRUE,scrollY="800",
                                                options = list(pageLength = 1000),
                                                buttons = list(list(extend = 'csv',
                                                                    filename=paste0("Businessdetails",Sys.Date())),
                                                               list(extend = 'excel',
                                                                    filename=paste0("Businessdetails",Sys.Date())
                                                               )))))
    
  })
  
  
  
} #workings and  calcs end here

## App interface

ui <- dashboardPage( skin="purple",
                     dashboardHeader(title=" UK Business Details App"), sidebar, body) 

#Run it
shinyApp(ui, server)

