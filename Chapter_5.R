#Chapter 5: Data wrangling with multiple dataframes


library(nycflights13)
glimpse(flights)
head(flights,3)
head(airlines,3)

#inner join: same logic as merge in base R: merge flights with airplines by carrier
flights_joined <- flights %>% 
  inner_join(airlines, by = c("carrier" = "carrier"))
glimpse(flights_joined)

flights_joined %>% 
  select( name, carrier, flight, origin, dest) %>% #order of names in select is same order they will be produced
  head(3)

#always good to check the rows are what you expected
nrow(flights_joined)
nrow(flights)

#inner_join, like merge, will return only matching rows. left_join will return all
#rows whether or not they match

#example joining pacific time zones with all of the data
airports_pt <- airports %>%
  filter(tz == -8)

head(airports_pt,3)

nyc_dests <- flights %>% 
  left_join(airports_pt, by = c("dest" = "faa")) #this means to match faa in airports_pt to the dest column in flights

nyc_dests %>%
  summarize(
    num_flights = n(),
    num_flights_pt = sum(!is.na(name)),
    num_flights_not_pt = sum(is.na(name))
  )


#extended example with Manny Ramiriz -----

library(Lahman)
head(Batting,3)

#subset to many ramirez
manny <- Batting %>%
  filter(playerID == "ramirma02")
nrow(manny) #21 seasons

head(manny,3)
#look at all the basic career-wide summary stats
manny %>%
  summarize(
    span = paste(min(yearID), max(yearID), sep = "-"),
    #literally creates an x-x column
    num_years = n_distinct(yearID),
    #'number of distinct"
    num_teams = n_distinct(teamID),
    BA = sum(H) / sum(AB),
    tH = sum(H),
    tHR = sum(HR),
    tRBI = sum(RBI)
  )

#we can look at consistency (by team) by just grouping this code by team:
manny %>% 
  group_by(teamID) %>%
  summarize(
    span = paste(min(yearID), max(yearID), sep = "-"), #literally creates an x-x column
    num_years = n_distinct(yearID), #'number of distinct"
    num_teams = n_distinct(teamID), 
    BA = sum(H)/sum(AB), 
    tH = sum(H), 
    tHR = sum(HR), 
    tRBI = sum(RBI)
  ) %>%
  arrange(span)


#get home runs per year
manny %>%
  group_by(yearID) %>%
  summarise(hr_total = sum(HR)) %>%
  filter(hr_total > 30) %>%
  nrow()

#we know the player ID because of a key (reference table) that exists in our workflow

Master %>%
  filter(nameLast == "Ramirez" & nameFirst == "Manny")

#if we want to look at more specific information, we can join with Master dataframe

Batting %>%
  filter(playerID == "ramirma02") %>%
  inner_join(Master,by=c('playerID' = 'playerID')) %>%
  group_by(yearID) %>%
  summarise(
    
    Age = max(yearID - birthYear), 
    num_teams = n_distinct(teamID), 
    BA = sum(H)/sum(AB), 
    tH = sum(H), 
    tHR = sum(HR), 
    tRBI = sum(RBI),
    weight = mean(weight)
    
  ) %>%
  arrange(yearID)
    
  








