


#start Shiny Server func
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
      summarise(Wounded = sum(n_injured)) %>% mutate(month = month.abb)
  } 
    else {newData2 <- gun_violence_total_2017_pop %>%
      filter(state == input$SelectState) %>%
      group_by(state, month) %>%
      summarise(Deaths = sum(n_killed)) %>% mutate(month = month.abb)
    }
  
  })
  
  getData3 <- reactive({
    if(input$Deaths_Wouded_Select2 == "Wounded"){
      newData3 <- gun_violence_total_2017_pop %>%
        filter(state == input$SelectState2) %>%
        group_by(state, month) %>%
        summarise(Wounded = sum(n_injured))%>% mutate(month = month.abb)
    } 
    else {newData3 <- gun_violence_total_2017_pop %>% 
      filter(state == input$SelectState2) %>%
      group_by(state, month) %>%
      summarise(Deaths = sum(n_killed)) %>% mutate(month = month.abb)
    }
    
  })
  
  
#table out put
   output$table1 <-  renderDT(
    getData(), options = list(lengthChange = FALSE)
   )
   
   output$table2 <- renderTable(
     getData2(), 
     align = 'l',
     striped = TRUE
   )
   
   output$table3 <- renderTable(
     getData3(), 
     align = 'l',
     striped = TRUE
   )
   
   output$table4 <- renderDT(
     gun_violence_2017_fil,colnames = c("Date","State", "City/County","Deaths","Wounded"), options = list(pageLength = 15)
   )
  
#end tableoutput
  
#Start graph output
  output$Plot1 <- renderPlot({
#get filtered data
  newData <- getData()
  
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
  newData3 <- getData3()

  #ewData4 <- rbind(newData2, newData3) %>% as.tibble()
  
  ggplot(newData2, aes(x = month, y = newData2[[3]], group=1)) + geom_line() + geom_point() + 
    geom_line(data = newData3,color = "blue", aes(x = month, y= newData3[[3]])) + 
    geom_point(data = newData3,color = "blue", aes(x = month, y= newData3[[3]])) + 
    geom_line(data=gun_violence_NatAvg_Monthly,color="red", aes(x=month,y= NatAvg))+
    geom_point(data=gun_violence_NatAvg_Monthly,color="red", aes(x=month,y= NatAvg))+
    labs(x = "Month", y=input$Deaths_Wouded_Select2, title = "States vs. National Average")
 

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
    write.csv(rbind(getData2(),getData3()), file)
  }
)

output$downloadData <- downloadHandler(
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















