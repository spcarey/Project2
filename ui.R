#helper script for data
source("https://raw.githubusercontent.com/spcarey/Project2/master/helper.R")

#sidebar content 
sidebar <-  dashboardSidebar(sidebarMenu(
  menuItem("Introduction", tabName = "Intro", icon = icon("dashboard")),
  menuItem("All States", icon = icon("fa-chart-bar"), tabName = "All_States"),
  menuItem("Compare States", icon = icon("fa-chart-bar"), tabName = "Compare_States"),
  menuItem("All Data", icon = icon("fa-chart-bar"), tabName = "All_Data")
  
)
)


#body content for each 
body <- dashboardBody(tabItems(
  #Into Tab
  tabItem(tabName = "Intro",
          fluidRow(
            # A static valueBox
            valueBox(count(gun_violence_total_2017), "Incedents", icon = icon("gun")),
            valueBox(sum(gun_violence_total_2017$n_killed), "Total Deaths", icon = icon("gun")),
            valueBox(sum(gun_violence_total_2017$n_injured), "Total Wounded", icon = icon("gun"))
            
            
  )),
  #end Into Tab
  #All States
  tabItem(tabName = "All_States",
          fluidRow(
           box(
             width = 9 ,solidHeader = TRUE,
            collapsible = TRUE,
            DTOutput("table1")
           ),
          
           box(
            title = "Inputs", width = 3 ,solidHeader = FALSE,
            selectInput("Deaths_Wouded_Select", label = h3("Deaths or Wounded"), 
                        choices = list("Deaths"="Deaths", "Wounded"="Wounded"), 
                        selected = 1),
            sliderInput("population_slider", label = h3("Population Range"), min = 500000, 
                        max = 40000000, value = c(0, 40000000)),
            checkboxInput("PerCapita","PerCapita", value = FALSE),
            checkboxInput("PartyBox","Show 2016 Election Party", value = FALSE),
            downloadButton("downloadData", "Download Data"),downloadButton("exportPlot1", "Download Plot")
           )
          ),
          fluidRow(
            box(
               width = 12, solidHeader = TRUE, status = "primary",
              plotOutput("Plot1")
            )
  )),
  #end All States Tab
  #Compare States
  tabItem(tabName = "Compare_States",
          fluidRow(
            box(
              width = 9 ,solidHeader = TRUE,
              collapsible = TRUE,
              DTOutput("table2")
            ),
            
            box(
              title = "Inputs", width = 3 ,solidHeader = FALSE,
              selectInput("SelectState", label = h3("State"), 
                          choices = state_population_2017$state, 
                          selected = 1),
              selectInput("Deaths_Wouded_Select2", label = h3("Deaths or Wounded"), 
                          choices = list("Deaths"="Deaths", "Wounded"="Wounded"), 
                          selected = 1),
              downloadButton("downloadData2", "Download Data"),downloadButton("exportPlot2", "Download Plot")
            )
          ),
          fluidRow(
            box(
              width = 12, solidHeader = TRUE, status = "primary",
              plotlyOutput("Plot2")
            )
          )
  ),  

  #end All States Tab
  #Compare States
  tabItem(tabName = "All_Data",
          fluidRow(
            box(
              width = 12, solidHeader = TRUE,
              DTOutput("table3"),
              downloadButton("downloadData3", "Download Data")
            )
          )
  )
  
))


# dasboard page set up
dashboardPage(
  dashboardHeader(title = "Gun Violence in 2017"),
  sidebar,
  body  
)