library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(lubridate)
library(DT)
library(tree)
library(rpart)
library(rpart.plot)




gun_violence_total_2017 <- read_csv("https://raw.githubusercontent.com/spcarey/Project2/master/data/Gun_Violence_2017_total.csv")
State_2016_vote <- read_csv("https://raw.githubusercontent.com/spcarey/Project2/master/data/2016_Election_Results.csv")
state_population_2017 <- read_csv("https://raw.githubusercontent.com/spcarey/Project2/master/data/population_2017.csv")
state_regs <- read.csv("https://raw.githubusercontent.com/spcarey/Project2/master/data/state_regs.csv")
model_data <- read_csv("https://raw.githubusercontent.com/spcarey/Project2/master/data/model_data.csv")




#split Model Data into training and test data sets

train <- sample(1:nrow(model_data), size = nrow(model_data)*0.6)
test <- dplyr::setdiff(1:nrow(model_data), train)

#create training and test data sets
model_train <-  model_data[train, ]
model_test <- model_data[test, ]


gun_violence_2017_by_state <- gun_violence_total_2017 %>% 
  mutate(year = year(date), month = month(date)) %>% 
  filter(year == 2017 & state != "District of Columbia") %>% 
  group_by(state) %>% 
  summarise(deaths = sum(n_killed), wounded = sum(n_injured))

names(state_population_2017) <-  c("state", "population")

gun_violence_2017_by_state  <- left_join(gun_violence_2017_by_state , state_population_2017, by= "state")

gun_violence_2017_by_state <- gun_violence_2017_by_state %>%  mutate(death_per_capita = deaths/population, wounded_per_capita = wounded/population)


names(State_2016_vote) <- c("state", "party")

gun_violence_2017_by_state <- left_join(gun_violence_2017_by_state, State_2016_vote , by = "state")

gun_violence_2017_by_state <- gun_violence_2017_by_state %>% filter(state != "District of Columbia")

gun_violence_total_2017_pop <- left_join(gun_violence_total_2017, state_population_2017, by="state")

gun_violence_total_2017_pop$month <- as.factor(gun_violence_total_2017_pop$month)

gun_violence_2017_fil <- gun_violence_total_2017 %>%
  filter(state != "District of Columbia") %>%
  select(date,state, city_or_county, n_killed, n_injured)

gun_violence_NatAvg_Monthly <- gun_violence_total_2017 %>% 
  group_by(month) %>% summarize(NatAvg= sum(n_killed)/51) %>% mutate(MON = month.abb)



gun_violence_NatAvg_Monthly$MON <- factor(gun_violence_NatAvg_Monthly$MON, levels = gun_violence_NatAvg_Monthly$MON[order(gun_violence_NatAvg_Monthly$month)])

state_regs_perc <-  state_regs %>% 
  select(year,state,lawtotal) %>% 
  mutate(perc = 100*(lawtotal/133))

state_regs_perc <- state_regs_perc %>% select(state, perc)

gun_violence_2017_laws <- left_join(gun_violence_2017_fil, state_regs_perc, by="state")

State_2016_vote <- State_2016_vote %>% filter(state != "District of Columbia")

gun_violence_2017_laws <- left_join(gun_violence_2017_laws, State_2016_vote, by="state")


tree_fit_child_death <- rpart::rpart(formula = acc_death_child ~ perc  , data = model_data)
tree_fit_teen_death <-rpart::rpart(formula = acc_death_teens ~ perc  , data = model_data)
tree_fit_child_inJ <- rpart::rpart(formula = acc_inj_child ~ perc , data = model_data)
tree_fit_teen_inj  <- rpart::rpart(formula = acc_inj_teens ~ perc  , data = model_data)