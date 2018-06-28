library(shiny)
library(tidyverse)
library(plotly)
library(shinydashboard)
library(lubridate)
library(scales)
# 
# Notes on the UI
# Major sections are created by code > "insert section"
# Minor sections have a comment hash and four dashes
# Minor comments just get the comment hash

demographics <- read_rds("./www/mergeddemographics.rds")
dates <- read_rds("./www/dates.rds")
datesgathered <- read_rds("./www/datesgathered.rds")
dlong <- read_rds("./www/dlong.rds")



# Define the UI -----------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "HACSL Dashboard"),
  
  # Make a sidebar with the right tabs ----
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "Home", icon = icon("cog")),
      menuItem("Program Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Client Demographics", tabName = "demographics", icon = icon("th")),
      menuItem("Housing and Lease", tabName = "housing", icon = icon("home")),
      menuItem("Time to Housing", tabName = "duration", icon = icon("calendar"))#,
     # menuItem("Map of Housing", tabName = "map", icon = icon("map-marker"))
    )
  ),
  
  # Start of the actual body ----
  dashboardBody(

    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "dashboard.css")
    ),
    
    # Tabs begin ----
    tabItems(
      
      # Home tab ----
      tabItem(tabName = "Home",
              fluidRow(
                column(12,
                       img(src="SLC Housing Dashboard.png", class="welcomeImage"),
                       h3("Welcome to the HACSL DataVis Dashboard", align = "center"),
                       p("The following dashboard is an at-a-glance informational resource to explore housing placement for 
                         The Housing Authority of the County of Salt Lake. Data presented here are aggregated, de-identified program-level 
                         data from The Housing Authority of the County of Salt Lake. The following funding sources are included in the 
                         information presented:  HACSL Continuum of Care and Salt Lake County HARP Program.")
                         )
              )
      ),
      
      # Overview tab ----
      tabItem(tabName = "overview",
              fluidRow(
                column(12,
                       h3("Program Overview"),
                       p("Data presented here are aggregated, de-identified program-level data from The Housing Authority of the 
                         County of Salt Lake. The following funding sources are included in the information presented:  HACSL Continuum 
                         of Care and Salt Lake County HARP Program."
                       )
                )
              ),
              br(),

              fluidRow(
                column(12, 
                       valueBoxOutput("Individuals"), 
                       valueBoxOutput("Families"), 
                       valueBoxOutput("Percent")
                )
              ),
              br(),
              fluidRow(
                column(7,
                       h4("Individuals Served by Entry Date"),
                       plotlyOutput("servedbymonth")
                       
                ), 
                column(5,
                       h4("Days to Housing"),
                       plotlyOutput("Vouchertime")
                )
              )
      ),
      
      # Demographics tab ----
      tabItem(tabName = "demographics",
              
              fluidRow(
                column(12,
                       p("The following charts and graphs show detailed demographic data for households served by HACSL.")
                )
              ),
              
              # Demographics sliders ----
              fluidRow(
                     column(4,
                       h4("Demographics:"),
                       selectInput(
                         inputId = "clienttype", "Household Composition:",
                         choice = c("All", unique(as.character(demographics$client_type)))
                       ),
                       selectInput(
                         inputId = "gender", "Gender:",
                         c("All", unique(as.character(demographics$Gender)))
                       ),
                       selectInput(
                         inputId = "ethnicity", "Ethnicity:",
                         c("All", unique(as.character(demographics$Ethnicity)))
                       ),
                       selectInput(
                         inputId = "race", "Race:",
                         c("All", unique(as.character(demographics$Race)))
                       )
                     ),
                     column(4,
                         h4("Number of Households"),
                         plotlyOutput("clientplot")
                         
                     ), 
                     
                     column(4, 
                         h4("Number of Households by Gender"),
                         plotlyOutput("client_genderplot")
                              
                         ), 
                     
                     br()
                     

              ),
              
              
              # Demographics plots ----
              
                     fluidRow(
                       column(12, 
                       h4("Number of Households Over Time"),
                       plotlyOutput("client_timeplot")
                     )
                     ), 
              
                     br(),
                     
                     fluidRow(
                       column(12,
                       h4("Number of Households by Race"),
                       plotlyOutput("client_raceplot")
                     )
                     ),
              
                    br(),

                    fluidRow(
                       column(12,
                       h4("Number of Households by Ethnicity"),
                       plotlyOutput("client_ethnplot")
                )
              ),                            
                     
                     br(),
                     
                     fluidRow(
                       column(12, 
                       h4("Number of Households by Income Level"),
                       plotlyOutput("client_incomeplot")
                     )
                     ),
                     
                     br(),
                     
                     fluidRow(
                       column(12, 
                       h4("Number of Households With X Number of Children"),
                       plotlyOutput("client_kidplot")
                     )
                     )
              
      ),
      
      # Housing tab ----
      tabItem(tabName = "housing",
              
              fluidRow(
                column(12,
                       p("The following charts and graphs show the detailed housing and lease data for households served by HACSL.")
                )
                       
              ),
              
              # Housing sliders ----
              fluidRow(column(3,
                                h4("Demographics:"),
                                selectInput(
                                  inputId = "clienttypeh", "Household Composition:",
                                  choice = c("All", unique(as.character(demographics$client_type)))
                                ),
                                selectInput(
                                  inputId = "genderh", "Gender:",
                                  c("All", unique(as.character(demographics$Gender)))
                                ),
                                selectInput(
                                  inputId = "ethnicityh", "Ethnicity:",
                                  c("All", unique(as.character(demographics$Ethnicity)))
                                ),
                                selectInput(
                                  inputId = "raceh", "Race:",
                                  c("All", unique(as.character(demographics$Race)))
                                )
              ), 
                        column(9, 
                                 h4("Frequency Chart of Total Rent"),
                                 plotlyOutput("rentplot"),
                                     
                                  br(),
                                     
                                  h4("Frequency Chart of Amount Tenant Pays Toward Rent"),
                                  plotlyOutput("rentbytenantplot"), 
                               
                                  br()

                              
                              
              )
            ),
              
              fluidRow(
                column(12, 
                  h4("Scatterplot of Rent"),
                  plotlyOutput("rentscatterplot")
                )
              ), 
              
                br(),
              
              fluidRow(
                column(12, 
                  h4("Number of Bedrooms"),
                  plotlyOutput("bedroomsplot")
                )
              ), 
              
                br(),
                
              fluidRow(
                column(12, 
                  h4("Box Plot of Rent by Bedroom Count"),
                  p("Each boxplot below shows the distrubtion of the data by bedroom count. The line in the center of the box is the median, 
                    the top line of the boxplot is the third quartile (75th percentile), the bottom line of the boxplot is the first quartile (25th percentile), 
                    the tallest whisker represents 1.5 times the third quartile, and the whiskers are 1.5 times the interquartile range from the top or bottom of 
                    the box to the furthest datum within that distance."),
                  plotlyOutput("bedroomsrentplot")
                )
              ),
              
                br(),
                
              fluidRow(
                column(12, 
                  h4("Box Plot of Rent by Housing Type"),
                  p("Each boxplot below shows the distrubtion of the data by housing type. The line in the center of the box is the median, 
                    the top line of the boxplot is the third quartile (75th percentile), the bottom line of the boxplot is the first quartile (25th percentile), 
                    the tallest whisker represents 1.5 times the third quartile, and the whiskers are 1.5 times the interquartile range from the top or bottom of 
                    the box to the furthest datum within that distance."),
                  plotlyOutput("typerentplot")
                )
              )

              
      ),
      
      # Duration tab ----
      tabItem(tabName = "duration",
              fluidRow(
                column(12,
                       p("The following charts and graphs describe how a person goes from being referred to HACSL
                         to finally being housed.")
                       )
                ),
              
              br(),
              
              fluidRow(
                column(12, 
                       h4("Average Number of Days Between Each Step to Housing"),
                       valueBoxOutput("Vouchertimed"), 
                       valueBoxOutput("Vouchertimedpacket"), 
                       valueBoxOutput("Vouchertimedvoucher")
                )
              ),
              
              br(), 
              
              fluidRow(
                column(12, 
                       h4("Average Number of Days to Housing"),
                       valueBoxOutput("Vouchertoleaseup"), 
                       valueBoxOutput("Referraltovouch"), 
                       valueBoxOutput("Referraltoleaseup")
                )
              ),
              
              br(),
              
              fluidRow(
                column(12,
                       h4("Every Household Processed Through HACSL Less than 50 Days"),
                       plotlyOutput("timeplot")
                )
              ),
              
              br(),
              
              fluidRow(
                column(12,
                       h4("Every Household Processed Through HACSL ALL"),
                       plotlyOutput("timetwoplot")
                )
              ),
              
              br(),
              
              fluidRow(
                column(12,
                       h4("Days to Complete Each Progress Point by Individuals Indexed by Entry Date"),
                       p("Each bar represents one individual. Each individual is indexed by the time they enter HACSL's systems. 
                         The individual assigned the first number is the individual with the earliest entry date in the data."),
                       plotlyOutput("progressdaysplot")
                )
              ),
              
              br(),
              
              fluidRow(
                column(12,
                       h4("Clients Who Have Not Been Issued a Voucher"),
                       plotlyOutput("progressdaystwoplot")
                )
              )
              
              
              )#,
      
      # Map tab ----
      # tabItem(tabName = "map",
      #         fluidRow(
      #           column(12,
      #                  h3("Map of Housing"), 
      #                  p("THIS IS AN EXAMPLE MAP! The following maps show where HACSL's vouchers are being used throughout Salt Lake County. The darker the blue, the more housing in that area."),
      #                  HTML('<center><img src="slco map example.png"><center>'),
      #                  br(), 
      #                  p("Here the map is zoomed out, but in the actual dashboard, you'll be able to do this in real time."),
      #                  HTML('<center><img src="full Utah map.png"><center>'),
      #                  br()
      #           )
      #         )
   #   )
      )
    )
  )
            
               


# Define the server ------------------------------------------------------------------
server <- function(input, output) {
  
  # Overview infographics ----
  output$Individuals <- renderValueBox({
    
    indiv <- demographics %>% 
      mutate(numindiv = TotalAdults + NumberKids) 
      
    indiv <- sum(indiv$numindiv, na.rm = TRUE)
    
    valueBox(value = prettyNum(indiv, big.mark = ","), subtitle = "Individuals Served Since January 2018" 
             , color = "navy")
  }) 
  
  output$Families <- renderValueBox({
    
    kids <- demographics %>% 
       filter(Kids==TRUE) %>% 
      count(Kids)
    
    valueBox(value = prettyNum(kids$n, big.mark = ","), subtitle = "Households Served Since January 2018", 
             color = "blue")
    
  }) 
  
  output$Percent <- renderValueBox({
    
    kidspercent <- demographics %>% 
      group_by(Kids) %>% 
      summarise(count = n()) %>% 
      mutate(percent = percent((count/sum(count)))) %>% 
      slice(2)
    
    valueBox(value = prettyNum(kidspercent$percent, big.mark = ","), subtitle = "Percent of Households with Children Served Since January 2018", 
             color = "teal")
    
  }) 
  
  output$Vouchertime <- renderValueBox({
    
    valueBox(value = prettyNum(0, big.mark = ","), subtitle = "Time to Housing: From Referral to Voucher Issued", 
             color = "yellow")
  })
  
  
  output$servedbymonth <- renderPlotly({
    plot <- demographics %>% 
      mutate(CreatedDate = mdy(CreatedDate), 
             year = year(CreatedDate),
             year_month = as.Date(cut(CreatedDate, breaks = "month"))) %>% 
      mutate(totalpeople = TotalAdults + NumberKids) %>% 
      filter(RelationshipHOH=="Head") %>%
      filter(year>2010) %>% 
      group_by(year_month) %>% 
      summarise(n = sum(totalpeople))
    
    plot_ly(plot, x= ~year_month, y= ~n, type = "scatter", mode = "lines", line = list(width = 3)) %>% 
      layout(
        xaxis = list(title = "Year Month"), 
        yaxis = list(title = "# of People")
      )
    
  })
  
  output$Vouchertime <- renderPlotly({
    
    time1 <- dates %>% 
      filter(referraltopacketdays>=0) %>% 
      summarise(`Referral to Packet` = mean(referraltopacketdays)) %>% 
      round() 
    
    
    time2 <- dates %>% 
      filter(packettoappdays>=0) %>% 
      summarise(`Packet to Application` = mean(packettoappdays)) %>%  
      round()
    
    time3 <- dates %>% 
      filter(appvouchdays>=0) %>% 
      summarise(`Application to Voucher` = mean(appvouchdays)) %>%  
      round()
    
    time4 <- dates %>% 
      filter(vouchleasedays>=0) %>% 
      summarise(`Voucher to LeaseUp` = mean(vouchleasedays)) %>%  
      round()
    
    time <- bind_cols(time1, time2)
    time <- time %>% bind_cols(time3) 
    time <- time %>% bind_cols(time4) %>%  
      gather() %>% 
      rename(Time = key, 
             mean = value) %>% 
      mutate(Time = as.character(Time))
    
    plot_ly(time, labels = ~Time, values = ~mean) %>% 
      add_pie(hole = 0.4) %>% 
      layout(showlegend = T, 
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  #  Demographics tab data and plots ----
  # Reactive demographics data that changes with the sliders on that tab
  selected.data <- reactive({
    demographics %>%
      {if(input$clienttype != 'All') filter(., client_type == input$clienttype) else .} %>%
      {if(input$gender != 'All') filter(., Gender == input$gender) else .} %>% 
      {if(input$ethnicity != 'All') filter(., Ethnicity == input$ethnicity) else .} %>% 
      {if(input$race != 'All') filter(., Race == input$race) else .} 
      
  })

  
  output$clientplot <- renderPlotly({
    plot <- selected.data() %>%
      count(client_type) %>%

    plot_ly(labels = ~client_type, values = ~n, marker = list (colors = c("#3e4c89", "#28a683"))) %>%
      add_pie(hole = 0.4) %>%
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  output$client_timeplot <- renderPlotly({
    allmonths <- demographics %>% 
      group_by(year_month) %>% 
      tally() %>% 
      select(-n)
    
    ts <- selected.data() %>% 
      group_by(year_month) %>%
      tally() %>% 
      right_join(allmonths) %>% 
      replace(is.na(.), 0)
    
    plot <- ts %>%
      plot_ly(x = ~year_month, y = ~n, group = 1, 
              type = "scatter", mode = "lines") %>%
      layout(yaxis = list(title = 'Count'), 
             xaxis = list(title = 'Year and Month'))
    
  })
  

  
  output$client_genderplot <- renderPlotly({
    plot <- selected.data() %>% 
      group_by(Gender) %>% 
      summarise(n = n()) %>% 

    plot_ly(labels = ~Gender, values = ~n, marker = list (colors = c("#2e738e", "#dce428"))) %>% 
      add_pie(hole = 0.4) %>% 
      layout(showlegend = T, 
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  

  
  output$client_raceplot <- renderPlotly({
    plot <- selected.data() %>%
      group_by(Race) %>%
      summarise(n = n()) %>%
      plot_ly(x = ~Race, y = ~n, type = "bar", marker = list(color = "#3e4c89")) %>%
      layout (
      yaxis = list(title = 'Count'), 
      xaxis = list(title = 'Race'))
  })
  
  output$client_ethnplot <- renderPlotly({
    plot <- selected.data() %>% 
      group_by(Ethnicity) %>% 
      summarise(n = n()) %>% 
      plot_ly(x = ~Ethnicity, y = ~n, type = "bar", marker = list(color = "#afdb39")) %>% 
      layout (
        yaxis = list(title = 'Count'), 
        xaxis = list(title = 'Ethnicity'))
  })
  
  output$client_incomeplot <- renderPlotly({
    plot <- selected.data() %>%
      group_by(IncomeLevel) %>%
      summarise(n = n()) %>%
      plot_ly(x = ~IncomeLevel, y = ~n, type = "bar", marker = list(color = "#28a683")) %>%
      layout (
      yaxis = list(title = 'Count'), 
      xaxis = list(title = 'Income Level'))
  })

  output$client_kidplot <- renderPlotly({
    plot <- selected.data() %>%
      mutate(NumberKids = as.factor(NumberKids)) %>% 
      group_by(NumberKids) %>% 
      summarise(n = n()) %>% 
      plot_ly (x = ~NumberKids, y = ~n, type = "bar", marker = list(color = "#441665")) %>%
      layout (
      yaxis = list (title = 'Count'), 
      xaxis = list(title = 'Number of Children'))
  })
  
  
  # Housing tab data and plots ----
  # Reactive demographics data that changes with the sliders on that tab
  housing.data <- reactive({
    demographics %>%
    {if(input$clienttypeh != 'All') filter(., client_type == input$clienttypeh) else .} %>%
    {if(input$genderh != 'All') filter(., Gender == input$genderh) else .} %>% 
    {if(input$ethnicityh != 'All') filter(., Ethnicity == input$ethnicityh) else .} %>% 
    {if(input$raceh != 'All') filter(., Race == input$raceh) else .} 
    
  })
  

  
  output$rentplot <- renderPlotly({
    plot <- housing.data() %>%
      plot_ly (x = ~TotalRent, type = "histogram", 
               marker = list (color = c("#440255", "#440e5e", "#441665", "#442a75", "#433d82",
                                        "#3e4c89", "#2e738e", "#26898d", "#28a683", "#35b679",
                                        "#46bc72", "#72cc59", "#b3dc31", "#afdb39", "#cfe12c", "#dce428", "#fae725"))) %>%
      layout(xaxis = list (title = "Total Rent $", tickformat = "$"),
      yaxis = list (title = "Count"))
  })
  

  output$rentbytenantplot <- renderPlotly({
    plot <- housing.data() %>%
      plot_ly (x = ~TenantPaid, type = "histogram", 
               marker = list (color = c("#440255", "#440e5e", "#441665", "#442a75", "#433d82",
                                        "#3e4c89", "#2e738e", "#26898d", "#28a683", "#35b679",
                                        "#46bc72", "#72cc59", "#b3dc31", "#afdb39", "#cfe12c", "#dce428", "#fae725"))) %>%
      layout(xaxis = list (title = "Amount Tenant Pays Toward Rent $",
                           tickformat = "$"),
             yaxis = list (title = "Count"))
  })
  

  output$rentscatterplot <- renderPlotly({
    plot <- housing.data() %>%
      plot_ly (x = ~TotalRent, y = ~TenantPaid,
               type = "scatter", mode = "marker",
               color =  ~TotalRent, size =  ~TotalRent) %>%
      layout (xaxis = list (title = "Total Rent $", tickformat = "$"),
               yaxis = list (title = "Amount Tenant Pays Toward Rent $",
                             tickformat = "$"))
  })
  

  output$bedroomsplot <- renderPlotly({
    plot <- housing.data() %>%
      count(BedroomSize.y) %>%
      plot_ly (x = ~BedroomSize.y, y = ~n,
               type = "bar", marker = list(color = "#3e4c89")) %>%
      layout (xaxis = list (title = "Number of Bedrooms"),
              yaxis = list (title = "Number of Households"))
  })

  
  output$bedroomsrentplot <- renderPlotly({
    plot <- housing.data() %>%
      plot_ly(x = ~BedroomSize.y, y = ~TotalRent, type = "box", marker = list(color = 'rgb(7,40,89)'),
              line = list(color = 'rgb(7,40,89)')) %>%
      layout (xaxis = list (title = "Number of Bedrooms"),
              yaxis = list (title = "Total Rent"))
  })

  
  output$typerentplot <- renderPlotly({
    plot <- housing.data() %>%
      plot_ly (x = ~Stuturetype, y = ~TotalRent, type = "box", marker = list(color = 'rgb(7,40,89)'),
               line = list(color = 'rgb(7,40,89)')) %>%
      layout (xaxis = list (title = "Type of Structure", tickfont = list (size = 7)),
              yaxis = list (title = "Total Rent"))
  })
  
  
  # Duration plots ----

  output$Vouchertimed <- renderValueBox({
    tim <- dates %>% 
      filter(referraltopacketdays>=0) %>% 
      summarise(meandays = mean(referraltopacketdays)) %>% 
      round()
    
    valueBox(value = prettyNum(tim, big.mark = ","), subtitle = "Average Number of Days from Referral to Packet Submitted", color = "navy")
    
  })
  
  output$Vouchertimedpacket <- renderValueBox({
    tim <- dates %>% 
      filter(packettoappdays>=0) %>% 
      summarise(meandays = mean(packettoappdays)) %>%  
      round()
    
    valueBox(value = prettyNum(tim, big.mark = ","), subtitle = "Average Number of Days from Packet to Application Processed", color = "blue")
  })
  
  output$Vouchertimedvoucher <- renderValueBox({
    tim <- dates %>% 
      filter(appvouchdays>=0) %>% 
      summarise(meandays = mean(appvouchdays)) %>%  
      round()
    
    valueBox(value = prettyNum(tim, big.mark = ","), subtitle = "Average Number of Days from Application to Voucher Issued", color = "teal")
    
  })
  
  output$Vouchertoleaseup <- renderValueBox({
    tim <- dates %>% 
      filter(vouchleasedays>=0) %>% 
      summarise(meandays = mean(vouchleasedays)) %>% 
      round()
    
    valueBox(value = prettyNum(tim, big.mark = ","), subtitle = "Average Number of Days from Voucher Issued to Lease Up", color = "navy")
    
  })
  
  output$Referraltovouch <- renderValueBox({
    tim <- dates %>% 
      filter(referraltovoucher>=0) %>% 
      summarise(meandays = mean(referraltovoucher)) %>%  
      round()
    
    valueBox(value = prettyNum(tim, big.mark = ","), subtitle = "Average Number of Days from Referral to Voucher Issued", color = "blue")
  })
  
  output$Referraltoleaseup <- renderValueBox({
    tim <- dates %>% 
      filter(referraltoleaseup>=0) %>% 
      summarise(meandays = mean(referraltoleaseup)) %>%  
      round()
    
    valueBox(value = prettyNum(tim, big.mark = ","), subtitle = "Average Number of Days from Referral to Lease Up", color = "teal")
    
  })
  
  output$timeplot <- renderPlotly({
    plot <- datesgathered %>% 
      group_by(referral, name) %>%
      filter(days>=0) %>%
      filter(days<50) %>%
      ungroup() %>% 
      bind_cols(g = as.factor(group_indices(., referral, name))) 
    
    plot_ly(plot, x= ~point, y= ~days, color = ~g, type = "scatter", mode = "lines", colors = "Spectral") %>% 
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Days"), 
        legend = list(orientation = "h")
      ) 
    
  })
  
  output$timetwoplot <- renderPlotly({
    plot <- datesgathered %>% 
      group_by(referral, name) %>% 
      filter(days>=0) %>% 
      ungroup() %>% 
      bind_cols(g = as.factor(group_indices(., referral, name))) 
    
    plot_ly(plot, x= ~point, y = ~days, color = ~g, type = "scatter", mode = "lines", colors = "Spectral") %>% 
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Days"), 
        legend = list(orientation = "h")
      ) 
    
  })
  
  output$progressdaysplot <- renderPlotly({
    
    plot <- dlong %>%
      filter(!in_process) %>%
      bind_cols(g = group_indices(., referral, name))
    
    
    plot_ly(plot, x = ~g, y = ~days, type = 'bar', 
            name = ~pointname, color= ~pointname) %>% 
      layout(yaxis = list(title = "Days"),
             xaxis = list(title = "Individuals Indexed by Date Entered"), 
             barmode = 'stack')
    
  })
  
  output$progressdaystwoplot <- renderPlotly({
    plot <-  dlong %>% 
      filter(in_process) %>% 
      bind_cols(g = group_indices(., referral, name)) %>% 
      mutate(g = as.factor(g))
    
    plot_ly(plot, x= ~days, y = ~g, color = ~pointname, name = ~pointname, type = "bar", orientation = 'h') %>% 
      layout(
        yaxis = list(title = "Individuals Indexed by Date Entered"), 
        xaxis = list(title = "Days Until Present"),
        barmode = 'stack',
        showlegend = T
      )
    
  })
}


# Run the app -------------------------------------------------------------
shinyApp(ui = ui, server = server)
