#DEFENSE SPENDING
library(edgebundleR)
library(FactoMineR)
library(factoextra)
library(ca)
library(shinythemes)
library(openintro)
library(plotly)
library(shinycssloaders)
library(networkD3)
library(visNetwork)
library(gtrendsR)
library(leaflet)
library(highcharter)
library(DT)
library(xts)
require(devtools)
library(dplyr)
library(profileR)
library(sunburstR)
library(quantmod)
library(fontawesome)
library(zoo)
library(RColorBrewer)
library(geosphere)
library(networkD3)
library(d3heatmap)
library(htmlwidgets)
library(shinyjqui)
library(d3r)
library(shinyalert)
library(rvest)


map.data <- read.csv("map.data.csv")
export <- read.csv("Export.csv")
dd <- read.csv("Sunburst Data.csv")
d6 <- read.csv("travel.csv")
tivdata <- read.csv("TIV.csv")
rownames(tivdata)<- tivdata$Supplier
tivdata$Supplier <- NULL

#WORLD BANK HEADLINES
headlines = read_html("https://www.google.com/search?q=world+bank&source=lnms&tbm=nws&sa=X&ved=0ahUKEwj2g87I5LDcAhWywVkKHeXBDh4Q_AUICigB&biw=1440&bih=826") %>%
  html_nodes(".r") %>% 
  html_text()
headlines

headlines <- data.frame(headlines)
colnames(headlines) <- c("World Bank Google News Headlines")

#IFC HEADLINES
headlines1 = read_html("https://www.google.com/search?biw=1440&bih=826&tbm=nws&ei=SX5TW_2CPYj65gLdoKCoDQ&q=international+finance+corporation&oq=International+Fina&gs_l=psy-ab.3.0.0l10.4219.6276.0.7207.18.12.0.6.6.0.155.1014.8j3.11.0....0...1c.1.64.psy-ab..1.17.1069....0.KpIPktHLtpI") %>%
  html_nodes(".r") %>% 
  html_text()
headlines1

headlines1 <- data.frame(headlines1)
colnames(headlines1) <- c("IFC Google News Headlines")


ui <- fluidPage(theme = shinytheme("cosmo"),
  # Application title
  navbarPage(title = "Spending",
               #tags$a(tags$img(src = "origin.logo.png", height = 30, width = 50)),

  tabPanel("United States Defense Spending",icon = icon("fighter-jet"),
           br(),
           fluidRow(align = "center",
                tags$img(src = "origin.logo.png", height = 345*.75, width = 605*.75)
           ),
           br(),
           fluidRow(align = "center",
                    h1("United States Defense Spending Tracker")
             
           ),
           br(),
           br(),
           fluidRow(align = "center",
                    useShinyalert()
           ),
           br(),
           br(),
           h4("This tracker represents data regarding US Defense, the US Armed Forces, and the IFC World Bank. This data was gathered from a variety of public sources, including Bloomberg, Gapminder, a large scale survey, Twitter APIs, and more. The primary goal of this page is to showcase different data visualization examples and techniques. If you’d like to see additional visuals or functionality added, please submit a request by email to andrewcmcmanus@gmail.com and I will be sure to add. Please note, all visuals are interactive. Hovering over them will offer more detail on specific data points."),
           br(),
           h4("The below chart provides a snapshot of deaths in war per 100,000 people versus the number of armed forces personnel per 100,000 people for various different countries. Burundi (the largest point high off in the right) has both the highest death toll and number of personnel in the armed forces for the given date range. Israel and Cambodia (close to the x-axis by the 6 point mark) provide examples of countries that have a relatively high number of armed forces personnel but a low death count. The United States finds itself closer to the bottoms of both the x and y-axes."),
           fluidRow(align = "center",
           withSpinner(highchartOutput(outputId = "comparisonspend", height = 600, width = 900))
           ),
           br(),
           h4("The below two graphs show the states spending the most on defense in 2009 both by percent GDP and in US Dollars. With its proximity to Washington DC, it comes as no surprise that Virginia has the highest spending in both categories (13.9% of GDP and $65.9 Billion)."),
           fluidRow(splitLayout(cellWidths = c("50%","50%"),
                withSpinner(highchartOutput(outputId = "TopSpendingGDP", height = 400, width = 675)),
                withSpinner(highchartOutput(outputId = "TopSpendingBillions", height = 400, width = 675))
                )
           ),
           br(),
           h4("The below maps allow for state comparison. The darker the shade of blue, the higher that state spends on defense. Use the dropdown to toggle between the map showing spending by percent of GDP, and the map measuring spend in billions of dollars."),
           selectInput(inputId = "dropdown", label = "Select a map:", choices = c("% of GDP Spending","Spending in Dollars (billions)")),
           fluidRow(align = "center",
                withSpinner(plotlyOutput(outputId = "map1", height = 800, width = 1000))
           ),
           h4("While the United States had a slight decline in spending in 2014 and 2015, that turned around in 2016 and spending has only increased since. It's projected that spending will continue to increase by approximately $154 Billion."),
           fluidRow(align = "center",
                withSpinner(highchartOutput(outputId = "timelapse", height = 400, width = 1000))
           ),
           br(),
           # br(),
           # h3("Country Spending"),
           # h4("The below heatmap shows defense spending by various countries in 2017. This visual provides more clarity on how US military spending compares to other countries."),
           # br(),
           # withSpinner(d3heatmapOutput(outputId = "heatmap",height = "700px", width = "1000px")),
           # br(),
           br()
  ),
  
  tabPanel("Massachusetts Defense Spending",icon = icon("signal"),
           fluidRow(align = "center",
                    tags$img(src = "mass.logo.png", height = 1200*.21, width = 1200*.21)
           ),
           br(),
           br(),
           fluidRow(align = "center",
                    h1("Massachusetts Defense Spending Tracker")
                    
           ),
           br(),
           h4("Massachusetts spent $10.641 Billion on defense contractors in 2009. Raytheon raked in the lion's share of that at $4.5 Billion, followed by General Electric ($1.8 Billion), MIT ($1.7 Billion) and General Dynamics ($1.2 Billion). The remaining $9.2 Billion was dispersed across a handful of smaller contractors. The below chart also shows what types of defense systems budgets are allocated towards, and furthermore whether or not those resources are used for training purposes, building the armory in the United States, or supplying troops on the front lines internationally."),
           fluidRow(align = "center",
                  h3("Massachusetts Defense Contractor Spending")
           ),
           fluidRow(align = "center",
                  withSpinner(sankeyNetworkOutput(outputId = "massoutput", height = 600, width = 1100))
           ),
           br(),
           h4("Massachusetts spending increased by $8.4 Billion from 2002 - 2009. While this is a large increase, it is far below the increase seen by the top spender, Virginia, which increased spending by $20.6 Billion over the same period. For reference, the bottom spender, Wyoming appears on the same chart, which saw a slight increase in spending from $0.1 - $0.2 Billion, but then a decrease back to $0.1 Billion in 2009."),
           fluidRow(align = "center",
                  withSpinner(plotlyOutput(outputId = "linechart", height = 600, width = 1200))
           ),
           br(),
           h4("With a handful of highly paid defense contractors, Massachusetts focuses spending primarily on equipment and technology development, research, and the national guard/reserve. Only a small portion of funding (13%) is going towards active duty. The army is the division that receives most funding, although the Navy and Air Force are not far behind."),
           fluidRow(splitLayout(cellWidths = c("50%","50%"),
                                withSpinner(highchartOutput(outputId = "piechart1", height = "100%", width = "100%")),
                                withSpinner(highchartOutput(outputId = "piechart2", height = "100%", width = "90%"))
                    )
           ),
           fluidRow(align = "center",
                    withSpinner(highchartOutput(outputId = "piechart3", height = "100%", width = "100%"))       
           ),
           br(),
           br(),
           br(),
           h4("The below map shows the top defense spending locations in Massachusetts. The size of the points indicates the number of defense dollars each location receives. Bedford ($2.3 Billion) and Andover ($2.2 Billion) stand out as the two locations that receive the most state funding. This comes as no surprise given that Bedford is home to DOD’s Command, Control, Communications, and Intelligence Federally Funded Research and Development Center (FFRDC) and a Department of Veterans Affairs (DVA) R&D unit. Andover is also where Raytheon (Massachusetts' highest paid contractor) develops Patriot missiles at the Air Defense Center."),
           fluidRow(align = "center",
           withSpinner(plotlyOutput(outputId = "map2", height = 500, width = 800))
           ),
           br(),
           br()
          
    ),
    navbarMenu("Modeling/Machine Learning",icon = icon("certificate"),
      
        tabPanel("Profile Analysis",icon = icon("users"),
               
               h3("Profile Analysis via Multidimensional Scaling (PAMS)"),
               h4('PAMS is a process that is often used for determining different segments or profiles from multiple columns of data that describe individuals. Below, you may upload a CSV file of data to conduct PAMS without using R or another programming language.'),
               br(),
               h4('Click the "Download" button for access to a sample data set that can be uploaded into the page below and used to run an example PAMS. This data set contains information regarding the skills, experience, and preferences of approximately 820 individuals in the armed forces. Uploading it back into the page and running the analysis will provide a graph that shows profiles based on how these individuals can be grouped or segmented. The table below the graph will show what percentage of the population falls into each profile. You can also use the fields below the "Create Chart" button to make live updates to the chart, including naming the profiles, adding a title, and changing the y-axis limits to adjust the zoom.'),
               titlePanel("Run a Profile Analysis"),
               sidebarLayout(
                 # Sidebar with a slider and selection inputs
                 sidebarPanel(
                   
                   #Upload Excel Grid
                   fileInput(inputId = 'data', 'Upload CSV File',
                             accept=c('.csv')),
                   actionButton("go","Create Chart"),
                   br(),
                   br(),
                   h4("When you fill in the fields below, the graph will automatically update."),
                   textInput(inputId="plottitle",label="Title your graph:"),
                   textInput ( 'segment1', 'Indicate name for Profile 1:', value=""),
                   textInput ( 'segment2', 'Indicate name for Profile 2:', value=""),
                   textInput ( 'segment3', 'Indicate name for Profile 3:', value=""),
                   br(),
                   h4("Change Y-Axis Limits:"),
                   numericInput(inputId = "min", label = "Minimum",value = -100),
                   numericInput(inputId = "max", label = "Maximum",value = 100),
                   tags$style(type="text/css",
                              ".shiny-output-error { visibility: hidden; }",
                              ".shiny-output-error:before { visibility: hidden; }"
                   )
                 ),
                 
                 # Visual Output
                 mainPanel(
                   h5("To test out the page, click below to download a sample data set and try uploading it in the left hand sidebar."),
                   downloadButton("downloadData", "Download"),
                   br(),
                   wellPanel(h4('Visual')),
                   highchartOutput(outputId = "plot",  width = "100%", height= 500),
                   DT::dataTableOutput(outputId = "Dimensions")
                   # downloadButton("downloadPlot", "Download Visual")
                 )
               )
               
        )
        
        # tabPanel("Clustering Techniques",icon = icon("share-alt"),
        #          
        #          h3("Clustering:"),
        #          br(),
        #          h4("Different clustering methods can be used to define groups within a particular data set. In the example below, an agglomerative hierarchical cluser analysis was performed on the data set to determine different groups within a military data set that held test scores for individuals in different divisions of the military. The results were then organized into a radial network."),
        #          br(),
        #          h4("Of the tests that were included in the data set, there was: a field knowledge test, a logic test, and a physical test. Scores for all tests were evaluated on a scale of 0 - 100."),
        #          br(),
        #          h4("The below visual shows that test takers typically fell into one of three groups. This prompted further analysis of the data set where I looked to find a way in which the data could fall into three groups and noticed that different divisions of the military appeared to perform differently on the three different tests (with the exception of a few outliers)."),
        #          br(),
        #          fluidRow(align = "center",
        #               radialNetworkOutput(outputId = "radnet",height = 800, width = 1100)
        #          ),
        #          br(),
        #          br(),
        #          br()
        #          
        # )
        
      ),
  
      tabPanel("Military Maps & Linguistic Skills",icon = icon("map-marker"),
               
               h3("Military Recruitment Map:"),
               br(),
               h4("The below shows all military applications submitted across the United States over the course of a few months. Each point represents a person, and clicking on an individual point will display information regarding the city they are located in, the division they are applying for a new role in (Army/Navy/Marines/Air Force) and the number of years of experience they have. When using the search bar in the data table, the map will automatically update to reflect the filtered results."),
               br(),
               fluidRow(
                 column(
                   width = 6,
                   DT::dataTableOutput(outputId = "MilitaryData")
                 ),
                 column(
                   width = 6,
                   withSpinner(leaflet::leafletOutput(outputId = "MilitaryMap", height = 500))
                 )
               ),
               br(),
               br(),
               h3("Military Placement and Language Skills"),
               br(),
               h4("The below sunburst diagram helps to illustrate what percentage of each US military division is stationed overseas and what percentage is stationed in the United States. Further breakdowns help to show what percentage of service members are multilingual. The diagram shows that there are more service members stationed overseas that are multilingual than stationed in the US. This comes as no surprise given that many are stationed according to their linquistic skills."),
               h5("Note: This visual is draggable. You may move it around the page."),
               fluidRow(align = "center",
                  jqui_draggable(sunburstOutput(outputId = "sunburst", height = 600, width = 600))
               ),
               br(),
               br(),
               h3("Military Placement Map"),
               h4("The below map shows a sample of military placements for those now stationed overseas. The map shows were some service men and women completed training in the United States and where they were sent overseas."),
               br(),
               fluidRow(align = "center",
                    leafletOutput(outputId = "flowmap", height = 700, width = 1300)
               ),
               br(),
               br()
               
      ),
  
    navbarMenu("The World Bank and IFC Online",icon = icon("bank"),
    tabPanel("IFC/World Bank Social Network",icon = icon("connectdevelop"),
             
             h3("The World Bank and IFC Social Network:"),
             br(),
             h4("The below social network was created using Twitter conversation around the IFC and World Bank on Tuesday, April 17th. The network shows all users who were interacting with the @worldbank and @ifc_org Twitter handles. Hovering over any point will show you who that user is, who they connected to, and key metrics regarding their connections."),
             br(),
             h4("NOTE: Please provide the network a moment or two to stabilize. This could take up to 2.5 minutes. Once it has stabilized, feel free to use the drop down or click on nodes to begin looking at connections. You can also change the size of the visual by clicking on the bottom right hand corner and dragging to your preferred size."),
             br(),
             withSpinner(jqui_draggable(jqui_resizable(visNetworkOutput(outputId = "network",width = "100%", height = 700)))),
             br(),
             br()
             
           
      ),
      tabPanel("IFC/World Bank News Headlines",icon = icon("align-justify"),

               h3("The World Bank and IFC Top News Headlines:"),
               br(),
               h4("The headlines page allows users to see the top headlines on Google News for both the IFC and World Bank. The tables below will automatically update to present the most up-to-date headlines to help users stay up to speed on how these two groups are being mentioned in the news."),
               br(),
               br(),
               tags$img(src = "blue.png", width = "100%", height = 10),
               br(),
               br(),
               fluidRow(align = "center",
                        h3("Top Headlines for the IFC and World Bank")
               ),
               br(),
               br(),
               fluidRow(splitLayout(align = "center",
                        dataTableOutput("headlines1", width = "60%"),
                        dataTableOutput("headlines2", width = "60%")

                        )
               ),
               br(),
               br(),
               tags$img(src = "blue.png", width = "100%", height = 10),
               br(),
               br(),
               br(),
               br(),
               br()

      ),
    tabPanel("IFC/World Bank Hits on Google",icon = icon("google"),

             h3("Searches for the World Bank and IFC on Google:"),
             br(),
             h4('This page allows users to search for specific terms and view how often these terms have been searched on Google. Simply type in a single term (this term may consist of multiple words), select a date range to look for, and click "GO." The date range will default to January 1, 2018 to today. You may change the date range to anything you would like, although please remember that if the term you are searching is more obscure and the date range is short, the site may not return results.'),
             br(),
             br(),
             fluidRow(align = "center",
                      h3('Enter a search:')
             ),
             br(),
             br(),
             fluidRow(align = "center",
                     textInput(inputId = "trends", label = "Enter search terms here (Note: Please use a comma between seperate search terms)"),
                     dateInput('date1',label = 'Start date: yyyy-mm-dd',value = "2018-01-01"),
                     dateInput('date2',label = 'End date: yyyy-mm-dd',value = Sys.Date()),
                     actionButton("stupid","Go")
             ),
             br(),
             br(),
             fluidRow(align = "center",
                      h3('Graphs will populate below after clicking "GO"')
             ),
             br(),
             fluidRow(align = "center",
                      withSpinner(highchartOutput(outputId = "trends1", height = 600, width = 1100))
             ),
             br(),
             br(),
             fluidRow(align = "center",
                      withSpinner(highchartOutput(outputId = "relatednews", height = 600, width = 1100))
             ),
             br(),
             br(),
             br(),
             br()

      )
    
    )
  
  )
  
)


server <- function(input,output){


    # Show a modal when the button is pressed
    shinyalert('New Updates!', 'As of December 10th, new updates were made to the dashboard. Check out each tab to see new visualizations that offer different interactive capabilities.', type = "info")
  

  output$TopSpendingGDP <- renderHighchart({
    
    top.gdp <- read.csv("1.Top 10 by Spending of GDP.csv")
    top.gdp$State <- factor(top.gdp$State, levels = c("Virginia","Hawaii","Alaska","Alabama","District of Columbia",
                                                      "Kentucky","Maryland","Arizona","Mississippi","Missouri"))
    
   
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_add_series(data = top.gdp, "column", hcaes(x = State, color = State, y = PercentofGDP)) %>%  
      hc_tooltip(crosshairs = T, borderWidth = 5, sort = T, shared = T, table = T, pointFormat = paste('<b>: {point.PercentofGDP}%</b>')) %>%
      hc_title(text = "Top 10 States by Spending of GDP",margin = 20,
               style = list( useHTML = TRUE)) %>%
      hc_xAxis(title = list(text = "State"), categories = top.gdp$State) %>%
      hc_yAxis(title = list(text = "Percent"), max = 100)
    
  })
  
  output$TopSpendingBillions <- renderHighchart({
    
    top.billions <- read.csv("2.Top 10 Recipients Defense Doctor (B).csv")
    top.billions$State <- factor(top.billions$State, levels = c("Virginia","California","Texas","Florida","Maryland",
                                                                "Georgia","Pennsylvania","Arizona","Massachusetts","North Carolina"))
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_add_series(data = top.billions, "column", hcaes(x = State, color = State, y = PercentofGDP)) %>%  
      hc_tooltip(crosshairs = T, borderWidth = 5, sort = T, shared = T, table = T, pointFormat = paste('<b>: {point.PercentofGDP}</b>')) %>%
      hc_title(text = "Top 10 States by Spending in Billions",margin = 20,
               style = list( useHTML = TRUE)) %>%
       hc_xAxis(title = list(text = "State"), categories = top.billions$State) %>%
       hc_yAxis(title = list(text = "Percent"), max = 70)
    
  })

  output$map1 <- renderPlotly({
    

    if(input$dropdown == "% of GDP Spending"){
    map.data2 <- read.csv("4.All States Defense Spending (%GOP).csv")
    map.data2$State <- state2abbr(map.data2$State)
    
    map.data2$hover <- paste0("State: ",map.data2$State,"<br>",
                              "% of GDP: ",map.data2$Spending,"%", "<br>")
    
    l <- list(color = toRGB("white"), width = 2)
    
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    
     map <- plot_geo(map.data2, locationmode = 'USA-states') %>%
      add_trace(
        z = ~Spending, locations = ~State, text = ~hover,
        color = ~Spending, colors = 'Blues'
      ) %>%
      layout(
        title = "Defense Spending (% of GDP)",titlefont = list(size = 25),
        margin = list(t = 130),
        geo = g
      ) %>% colorbar(title = "Spending (% of GDP)") 
    
    map
    
    }
    
    if(input$dropdown == "Spending in Dollars (billions)"){
      
      map.data1 <- read.csv("3.All States Defense Spending (B).csv")
      map.data1$State <- state2abbr(map.data1$State)
      
      map.data1$hover <- paste0("State: ",map.data1$State,"<br>",
                                "Spending: ","$",map.data1$Spending," Billion", "<br>")
      
      l <- list(color = toRGB("white"), width = 2)
      
      g <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showlakes = TRUE,
        lakecolor = toRGB('white')
      )
      
      
      map <- plot_geo(map.data1, locationmode = 'USA-states') %>%
        add_trace(
          z = ~Spending, locations = ~State, text = ~hover,
          color = ~Spending, colors = 'Blues'
        ) %>%
        layout(
          title = "Defense Spending (in billions)",titlefont = list(size = 25),
          margin = list(t = 130),
          geo = g
        ) %>% colorbar(title = "Spending (in billions)") 
      
      map
       
    }
    
    map
    
  })
  
  output$timelapse <- renderHighchart({
    
    k <- read.csv("10.Current.Projected Spending.csv")

    highchart() %>%
      hc_chart(type = "column") %>%
      hc_add_series(data = k, "column", hcaes(x = Year, color = Color, y = Spending)) %>%  
      hc_tooltip(crosshairs = T, borderWidth = 5, sort = T, shared = T, table = T, pointFormat = paste('<b>: ${point.Spending}B</b>')) %>%
      hc_title(text = "US Defense Spending 2013 - 2023 (2019+ projected)",margin = 20,
               style = list( useHTML = TRUE)) %>%
      hc_xAxis(title = list(text = "State"), categories = k$Year) %>%
      hc_yAxis(title = list(text = "Percent"), max = 1500)
    
    
    
  })
  
  output$piechart1 <- renderHighchart({
    
    pie1data <- read.csv("5.Mass by type.csv")
    
    
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_add_series_labels_values( labels = paste("Type: ",pie1data$Type," "), values = pie1data$Percent) %>%
      hc_tooltip(crosshairs = T, borderWidth = 5, sort = T, shared = T, table = T, pointFormat = paste('<b>{point.percentage:.1f}%</b>')) %>%
      hc_title(text = "Distribution of Spend",margin = 20,
               style = list(useHTML = TRUE))
    
  })

  output$piechart2 <- renderHighchart({
    
    pie2data <- read.csv("7.Mass Distribution.csv")
    
   
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_add_series_labels_values( labels = paste("Type: ",pie2data$Distribution," "), values = pie2data$Percent) %>%
      hc_tooltip(crosshairs = T, borderWidth = 5, sort = T, shared = T, table = T, pointFormat = paste('<b>{point.percentage:.1f}%</b>')) %>%
      hc_title(text = "Distribution of Spend",margin = 20,
               style = list(useHTML = TRUE))
    
  })
  
  output$piechart3 <- renderHighchart({
    
    pie3data <- read.csv("6.Mass by service.csv")
    
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_add_series_labels_values( labels = paste("Type: ",pie3data$Type," "), values = pie3data$Percent) %>%
      hc_tooltip(crosshairs = T, borderWidth = 5, sort = T, shared = T, table = T, pointFormat = paste('<b>{point.percentage:.1f}%</b>')) %>%
      hc_title(text = "Type of Spend per Service Division",margin = 20,
               style = list(useHTML = TRUE))
    
  })
  
  output$massoutput <- renderSankeyNetwork({
    
    nodes = data.frame("name" = 
                         c("Massachusetts Contractor Budget ($10.641 Billion)",	
                           "Raytheon ($4.5 Billion)",	
                           "General Electric ($1.8 Billion)",
                           "MIT ($1.7 Billion)",
                           "General Dynamics ($1.2 Billion)",
                           "Charles Stark Draper Labs ($437.2 Million)",
                           "General Dynamics Information Tech ($238.4 Million)",
                           "MITRE ($325.9 Million)", 
                           "BAE Systems ($178.6 Million)",
                           "Northrop Grumman ($142.7 Million)",	
                           "Brighton Marine Health Center ($121.3 Million)",
                           "Electronic Warfare",
                           "Precision Weapons",
                           "Missile Defense",
                           "Command and Control",
                           "Sensors and Imaging",
                           "Euipment",
                           "Cyber",
                           "Mission Support",
                           "Training",
                           "Domestic Armory",
                           "International Use"))# Node 3
    links = as.data.frame(matrix(c(
      
      0, 1, 4.5,
      0, 2, 1.8,
      0, 3, 1.7,
      0, 4, 1.2,
      0, 5, .4372,
      0, 6, .2384,
      0, 7, .3259,
      0, 8, .1786,
      0, 9, .1427,
      0, 10, .1213,
      2,11,1,
      2,12,.2,
      1,13,2.1,
      2,14,.3,
      3,15,1.7,
      4,17,.4,
      2,18,.4,
      5,12,.4372,
      4,11,.7,
      6,12,.2,
      1,14,2.4,
      10,15,.1213,
      8,16,.1786,
      9,17,.0427,
      7,18,.2,
      7,13,.1259,
      9,19,.1,
      11,19,1.68,
      12,20,.68,
      12,21,.2,
      13,20,2.25,
      14,21,1,
      14,20,1.7,
      15,19,.9,
      15,20,.9,
      16,21,.16,
      17,20,.4,
      17,19,.1,
      18,19,.31,
      18,20,.31
      
    ),# The third number is the value of the node
    byrow = TRUE, ncol = 3))
    links
    names(links) = c("source", "target", "value")
    sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",
                  fontSize= 12, nodeWidth = 30)
    
  })
  library(networkD3)
  output$linechart <- renderPlotly({
  
  d <- read.csv("8.Mass Prime Contracy Awards.csv")
  d$Year <- as.factor(d$Year)
  d$Year <- factor(d$Year, levels = c("2002","2003","2004","2005",
                                      "2006","2007","2008","2009","2010"))
  d
  plot_ly(data = d, x = ~Year, y = ~Spending, type = "scatter",mode = "lines", name = "Massachusetts") %>%
    layout(title = "Massachusetts Spending from 2003 - 2009 (in billions)",font = list(size = 15),
           titlefont = list(size = 20), xaxis = list(titlefont = list(size = 18),title = "Year"),
           yaxis = list(title = "Spending (in billions)", range = c(0,40),titlefont = list(size = 18)),
           margin = list(t = 130, r = 130)) %>%
    add_trace(y = ~Spending.Virginia, name = "Virginia (top spender)") %>%
    add_trace(y = ~Spending.Wyoming, name = "Wyoming (bottom spender)")
  
  
  })
  
  output$map2 <- renderPlotly({
  map.data <- read.csv("mapping.data.csv") 
  
 
  Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoiZHJld20xMTkyIiwiYSI6ImNqN255cmJ6YjNjOGQzMmt5eGg3M3hpN3MifQ.uzZ55tHI7E25njySudCmlA')
  lon1<- -71.076370
  lat1<- 42.365084
  

  k <- plot_mapbox(data = map.data,color = "green" ,size = ~Spending2, x = ~Longitude, y = ~Latitude, mode = "markers", 
                   text = ~paste0("Location: ",Location,"<br>",
                                 "Spending:  ","$",Spending," Billion","<br>"),
                   hoverlabel = list(bgcolor = "#767FFF", bordercolor = "white", font = list(color = "black",family = "Times New Roman"))) %>%
    layout(title = "Top Military Spending Locations (in billions)",titlefont = list(size = 20),
      mapbox = list(style = "dark",
                         zoom = 7,
                         center = list(lon = ~lon1,
                                       lat = ~lat1)),
           margin = list(l = 25, r = 25,
                         b = 25, t = 125)
    )
  k
  
  })
  
  output$comparisonspend <- renderHighchart({
    army <- read.csv("army2.csv")
    str(army)
    
    hchart(army, "scatter", hcaes(label = Country,name = Country,x = ArmedForcesPersonel,
                                  color = Country, y = DeathsinWar, size = ArmedForcesPersonel),
           tooltip = list(pointFormat = "Country: {point.Country} <br> 
                          Deaths in War: {point.DeathsinWar} <br>
                          Armed Forces Personel: {point.ArmedForcesPersonel}")) %>% 
    hc_chart(backgroundColor = "transparent") %>% 
      hc_title(text = "Armed Forces Personal and Deaths in War") %>% 
      hc_subtitle(text = "Data provided by Gap Minder") %>% 
      hc_tooltip(useHTML = TRUE, headerFormat = "") %>% 
      hc_size(height = 600) %>%
      hc_yAxis(title = list(text = "Deaths in War (per 100,000)", min = 0, max = 45)) %>%
      hc_xAxis(title = list(text = "Armed Forces Personel (per 100,000)", size = 15)) %>%
      hc_tooltip(crosshairs = F)
      })
  

  output$network <- renderVisNetwork({
    
    nodes <- read.csv("Nodes.csv")
    nodes$title <- paste0("<p>","METRICS:","<br>",
                          "User:", nodes$user, "<br>",
                          "Followers:", nodes$followers_count,"<br>",
                          "Modularity:", nodes$modularity, "<br>",
                          "Eigencentrality:",nodes$eigencentrality,"<br>",
                          "Indegree:", nodes$indegree,"<br>",
                          "Outdegree:", nodes$outdegree, "<br>")
    

    nodes$title <- as.character(nodes$title)
    nodes$color <- NULL
    
    edges <- read.csv("Edges.csv")  
    
    visNetwork(nodes, edges) %>% 
      visOptions(highlightNearest = list(enabled = T, hover = T), nodesIdSelection = list(enabled = F, selected = T)) %>%
      visPhysics(stabilization = FALSE, solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -150)) %>%
      visEdges(arrows ="to",length = 1500) %>%
      visEvents(type = "once", startStabilizing = "function() {
                this.moveTo({scale:0.1})}") %>%
      visInteraction(navigationButtons = TRUE)%>%
      visLegend(enabled = T)
    
})
  
  output$MilitaryData <- DT::renderDataTable({
    
    DT::datatable(
      map.data, rownames = F
    )
    
  })
  
  output$MilitaryMap <- renderLeaflet({
   
    
    l <- map.data %>% 
      dplyr::slice(#using this line to have the map show what specific rows are highlighted on the map
        as.integer(
          input$MilitaryData_rows_all #This shows you what rows have been selected (based on the search bar)
        )
      )
    
    pal <-colorFactor(
      palette = c('red', 'blue', 'green', 'pink'),
      domain = l$Division
    )
    
    leaflet() %>%
      addTiles() %>% #addTiles puts the actualy map graphic down. You can get tiles from Google Maps and more, but Openstreet map is the default
      setView(lng = -98.877098, lat = 40.311139, zoom = 4) %>% # Add default OpenStreetMap map tiles
      addCircles(data = l,
                 lng= ~Longitude, lat = ~Latitude, color = pal(l$Division),
                 radius = ~Years.of.Experience,
                 popup = paste("City:",l$City,"<br>",
                               "Division:",l$Division,"<br>",
                               "Years of Experience",l$Years.of.Experience)
                 ) %>%
      leaflet::addLegend("bottomright", pal = pal, values = map.data$Division)
  })

  output$downloadData <- downloadHandler(
    filename = "Export.csv",
    content = function(file) {
      write.csv(export, file, row.names = FALSE)
    }
  )
  
  K <- eventReactive(input$go,{
    
    data <- read.csv(input$data$datapath, header = T,fileEncoding = "latin1")
    data <- data[complete.cases(data),]
    x <- pams(data, 3)
    a <- x$dimensional.configuration
    a2 <- x$weights.matrix
    b <- as.data.frame(a)
    b$journey.phase <- rownames(b)
    b$journey.phase <- gsub("."," ",b$journey.phase, fixed = T)
    b$Profile.1 <- b$Dimension1
    b$Profile.2 <- b$Dimension2
    b$Profile.3 <- b$Dimension3
    b2 <- as.data.frame(a2)
    return(b)
    
  })
  
  
  p <- eventReactive(input$go,{
    data <- read.csv(input$data$datapath, header = T)
    data <- data[complete.cases(data),]
    x <- pams(data, 3)
    b2 <- as.data.frame(x$weights.matrix)
    b2 <- b2[,1:3]
    maxFun<-function(x){max(na.omit(x))} 
    #NEED A FUNCTION THAT WILL TELL WHICH COLUMN OWNS THE WINNER COLUMN
    b2$MAX <- apply(b2, 1, maxFun)
    b2 <- filter(b2, weight1 != 0)
    b2$MAX <- max.col(b2[1:3])
    Profile1 <- round(nrow(filter(b2, MAX == 1))/nrow(b2)*100,0)
    Profile2 <- round(nrow(filter(b2, MAX == 2))/nrow(b2)*100,0)
    Profile3 <- round(nrow(filter(b2, MAX == 3))/nrow(b2)*100,0)
    if(sum(Profile1,Profile2,Profile3) > 100){
      Profile1 <- Profile1 - 1
    }
    else{}
    Profiles <- c(Profile1,Profile2,Profile3)
    Names <- c("Series 1 (blue)","Series 2 (black)","Series 3 (green)")
    Profiledf <- data.frame(Names,Profiles)
    str(Profiledf)
    colnames(Profiledf) <- c("Profile","Percent of Population")
    return(Profiledf)
    
  })
  
  output$Dimensions <- renderDataTable(
    
    p(),
    options = list( selection = "none", autoWidth = T,bLengthChange = 0, bFilter = 0, bInfo = 0,
                    bPaginate = 0, bSortable = 0, bOrderable = 0, rownames = F)
    
    
  )
  
  output$plot<- renderHighchart({
    
    highchart() %>%
      hc_add_series(data = K(), name = input$segment1, "line", hcaes(x = journey.phase, color = c("#db3c00"), y = Profile.1)) %>%
      hc_add_series(data = K(), name = input$segment2, "line", hcaes(x = journey.phase, color = c("#008BFF"), y = Profile.2)) %>%
      hc_add_series(data = K(), name = input$segment3, "line", hcaes(x = journey.phase, color = c("#B1B9C0"), y = Profile.3)) %>%
      hc_tooltip(crosshairs = T, borderWidth = 5, sort = T, shared = T, pointFormat = paste('<b> {point.y},   </b>')) %>%
      hc_plotOptions(
        series = list(
          showInLegend = T), column = list(colorByPoint = T)) %>%
      hc_yAxis(title = list(text = "Profile Score"), max = input$max, min = input$min) %>%
      hc_xAxis(title = list(text = "Question Asked"), categories = K()$journey.phase) %>%
      hc_title(text = input$plottitle)
    
    
  })
  
  output$sunburst <- renderSunburst({
    
    add_shiny(sunburst(dd,legend = F))
    
  })
  
  output$flowmap <- renderLeaflet({
    
    flows <- gcIntermediate(d6[,4:5], d6[,6:7], sp = TRUE, addStartEnd = TRUE)
    flows$counts <- d6$counts
    flows$origins <- d6$origins
    flows$destinations <- d6$destinations
    hover <- paste0(flows$origins, " to ", 
                    flows$destinations, ': ', 
                    as.character(flows$counts))
    
    pal <- colorFactor(brewer.pal(4, 'Set2'), flows$origins)
    
    leaflet() %>%
      addProviderTiles('CartoDB.Positron') %>%
      addPolylines(data = flows, weight = ~counts, label = hover, 
                   group = ~origins, color = ~pal(origins)) %>%
      addLayersControl(overlayGroups = unique(flows$origins), 
                       options = layersControlOptions(collapsed = FALSE))
    
    
  })
  
  output$heatmap <- renderD3heatmap({

      d1 <- d3heatmap::d3heatmap(tivdata, scale = "column", colors = "Blues")
      d1
      
  })
  
  output$headlines1 <- renderDataTable(
    
    headlines, 
    options = list( selection = "none", autoWidth = T,bLengthChange = 0, bFilter = 0, bInfo = 0,
                    bPaginate = 0, bSortable = 0,searching = F, bOrderable = 0, rownames = F)
    
  )
  
  output$headlines2 <- renderDataTable(
    
    headlines1, 
    options = list( selection = "none", autoWidth = T,bLengthChange = 0, bFilter = 0, bInfo = 0,
                    bPaginate = 0, bSortable = 0,searching = F, bOrderable = 0, rownames = F, Filter = F)
    
    
  )
  
  observeEvent(input$stupid, {
    
    
    #Google Trends Graph
    testgt<-gtrends(c(input$trends),time= paste0(input$date1," ",input$date2) ,geo="", gprop='web',category=0,hl='en-US')
   
    output$trends1 <- renderHighchart({
      
      q <- testgt$interest_over_time
      q$date <- substr(q$date, 1,10)
      
      highchart() %>%
        hc_add_series(data = q, "line", hcaes(x = date, color = keyword, y = hits)) %>%
        hc_plotOptions(
          series = list(
            showInLegend = F), column = list(colorByPoint = T)) %>%
        hc_yAxis(title = list(text = "Number of Searches"), max = 100) %>%
        hc_xAxis(title = list(text = "Date"), categories = q$date) %>%
        hc_title(text = "Interest Over Time")
      
    })
    
    
    related <- as.data.frame(testgt$related_queries)
    related <- related%>%
      filter(related_queries=="top") %>%
      arrange(desc(subject))%>% top_n(15,subject)
    
    
    output$relatednews <- renderHighchart({
      
      related$subject <- as.numeric(related$subject)
      related$value <- factor(related$value, levels = related$value[order(related$subject)])  
      related$color <- c("#008BFF")
      
      highchart() %>%
        hc_add_series(data = related, "column", hcaes(x = value, color = color, y = subject), 
                      name = "Number of searches") %>%
        hc_plotOptions(
          series = list(
            showInLegend = F,
            pointFormat = "{point.y}%"
          ), column = list(colorByPoint = T)
        ) %>%
        hc_yAxis(title = list(text = "Count"),
                 #lables = list(format("{value}%"),
                 max = 100) %>%
        hc_xAxis(title = list(text = "Related Searches"),categories = related$value) %>%
        hc_title(text = "Top Related Searches")
      
    })
    
  })
  
  output$radnet <- renderRadialNetwork({
    
    veterentest <- read.csv("clust.csv")
    
    
    veteran2 <- dist(veterentest)
    
    hc_mtcars2  <- hclust(veteran2, method = "complete")
    Armed.Forces <- hc_mtcars2
    radialNetwork(as.radialNetwork(Armed.Forces),linkColour = "#ccc", nodeColour = "#cccccc")
   
    
  })

}

shinyApp(ui = ui, server = server)

  