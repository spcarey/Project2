
library(tidyverse)
library(lubridate)
library(DT)
library(plotly)


gun_violence_total_2017 <- read_csv("Gun_Violence_2017_total.csv")
State_2016_vote <- read_csv("2016_Election_Results.csv")
state_population_2017 <- read_csv("population_2017.csv")

gun_violence_2017_by_state <- gun_violence_total_2017 %>% 
  mutate(year = year(date), month = month(date)) %>% 
  filter(year == 2017) %>% 
  group_by(state) %>% 
  summarise(deaths = sum(n_killed), wounded = sum(n_injured))

names(state_population_2017) <-  c("state", "population")

gun_violence_2017_by_state  <- left_join(gun_violence_2017_by_state , state_population_2017, by= "state")

gun_violence_2017_by_state <- gun_violence_2017_by_state %>%  mutate(death_per_capita = deaths/population, wounded_per_capita = wounded/population)


names(State_2016_vote) <- c("state", "party")

gun_violence_2017_by_state <- left_join(gun_violence_2017_by_state, State_2016_vote , by = "state")

gun_violence_total_2017_pop <- left_join(gun_violence_total_2017, state_population_2017, by="state")

gun_violence_total_2017_pop$month <- as.factor(gun_violence_total_2017_pop$month)

gun_violence_2017_fil <- gun_violence_total_2017 %>% select(date,state, city_or_county, n_killed, n_injured)


#start Shiny Server 
shinyServer(function(input, output, session) {
  
  getData <- reactive({
    
    if(input$Deaths_Wouded_Select == "Deaths" & input$PerCapita){
      newData <- gun_violence_2017_by_state %>% filter(population >= input$population_slider[1] & population < input$population_slider[2]) %>% 
        select(state, death_per_capita, population, party)  
    } 
    else if (input$Deaths_Wouded_Select == "Wounded" & input$PerCapita) {
      newData <- gun_violence_2017_by_state %>% filter(population >= input$population_slider[1] & population < input$population_slider[2]) %>% 
        select(state, wounded_per_capita, population, party) 
    }
    else if (input$Deaths_Wouded_Select == "Deaths") {
      newData <- gun_violence_2017_by_state %>% filter(population >= input$population_slider[1] & population < input$population_slider[2]) %>% 
        select(state, deaths, population, party) 
    }
    else {newData <- gun_violence_2017_by_state %>% filter(population >= input$population_slider[1] & population < input$population_slider[2]) %>% 
      select(state, wounded , population, party) }
  })
  
  getData2 <- reactive({
  if(input$Deaths_Wouded_Select2 == "Wounded"){
    newData2 <- gun_violence_total_2017_pop %>%
      filter(state == input$SelectState) %>%
      group_by(state, month) %>%
      summarise(Wounded = sum(n_injured))
  } 
    else {newData2 <- gun_violence_total_2017_pop %>%
      filter(state == input$SelectState) %>%
      group_by(state, month) %>%
      summarise(Deaths = sum(n_killed))
    }
  
  })
  
  
#table out put
   output$table1 <-  renderDT(
    getData(), options = list(lengthChange = FALSE)
   )
   
   output$table2 <- renderDT(
     getData2(), options = list(pageLength = 12)
   )
   
   output$table3 <- renderDT(
     gun_violence_2017_fil, options = list(pageLength = 15)
   )
  
#end tableoutput
  
#Start graph output
  output$Plot1 <- renderPlot({
#get filtered data
  newData <- getData()
  str(newData)
#create plot

  if(input$PartyBox  ){
     ggplot(newData, aes(x = reorder(state,-newData[[2]]), y = newData[[2]], fill= as.factor(party))) + 
      geom_bar(stat = "identity") + 
      scale_fill_manual("legend", values = c("Republicans" = "red", "Democrats" = "blue")) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) + labs(x = "State", y = input$Deaths_Wouded_Select)
    
 
    
  } 
  else {
     ggplot(newData, aes(x = reorder(state,-newData[[2]]), y = newData[[2]], fill=state)) + geom_bar(stat = "identity") +
      theme(legend.position = "none",axis.text.x = element_text(angle = 60, hjust = 1)) + labs(x = "State", y = input$Deaths_Wouded_Select)
    
  }
 })

#end creat Plot1
#Create Plot2
output$Plot2 <- renderPlotly({
  
  newData2 <- getData2()
  
  ggplot(newData2, aes(x = month, y = newData2[[3]], group=1 )) + geom_line()

  })


#Create Download out put for current data

# Downloadable csv of selected dataset ----
output$downloadData <- downloadHandler(
  filename = function() {
    paste(Sys.time(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(getData(), file)
  }
 )
# End download csv of selected data set
# Download CSV from second tab
output$downloadData2 <- downloadHandler(
  filename = function() {
    paste(Sys.time(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(getData2(), file)
  }
)

output$downloadData3 <- downloadHandler(
  filename = function() {
    paste(Sys.time(), ".csv", sep="")
  },
  content = function(file) {
    write.csv( gun_violence_2017_fil, file)
  }
)

# Download Plot
  output$exportPlot1 = downloadHandler(
    filename = function() {paste(Sys.time(),"-plot.pdf", sep = "")},
    content = function(file) {
      ggsave(file, device = "pdf", width=11, height=8.5)
      
    }
  )
  output$exportPlot2 = downloadHandler(
    filename = function() {paste(Sys.time(),"-plot.pdf", sep = "")},
    content = function(file) {
      ggsave(file, device = "pdf", width=11, height=8.5)
      
    }
  )
})  















