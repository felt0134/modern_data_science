#Chapter 4: Data wrangling

#intoduce the grammar using presidents data ------

data("presidential")
head(presidential)

#select specific columns
select_example <- presidential %>%
  select(name,party)

#filter rows by a certain criteria
filter_example <- presidential %>%
  filter(party == 'Republican') # ==  denotes exact match or 'equal' or 'equality'

#we need quotations of Republican because it is a value. We don't need it for
#the column names because they are variable names.

#combine select and filter

select_filter_example <- presidential %>%
  filter(lubridate::year(start) > 1973 & party == "Democratic") %>%
  select(name)
  
# the %>% is called the 'pipe' operator and sequentially linked commands together.

#mutate example
library(lubridate)

#create new column estimating length of each term
my_presidents <- presidential %>%
  mutate(term.length = interval(start,end)/dyears(1))

#create new column for when they were elected
my_presidents <- my_presidents %>%
  mutate(elected = year(start) -1)

#ifelse function to account for incorrect data (Ford/Johnson starting)
my_presidents <- my_presidents %>%
  mutate(elected = ifelse(elected %in% c(1962,1973),NA,elected))

#we also want to avoid using periods in the names of anything, as this could
#mess with R's use of generic functions

#rename ot get rid of periods
my_presidents <- my_presidents %>%
  rename(term_length = term.length)

#arrange dataset by a certain metric. try descending order of term length
my_presidents <- my_presidents %>%
  arrange(desc(term_length))

#add in other grouping factors
my_presidents <- my_presidents %>%
  arrange(desc(term_length),party,elected)

#summarise the data
my_presidents %>%
  summarise(
    N = n(), 
    first_year = min(year(start)), 
    last_year = max(year(end)), 
    num_dems = sum(party == "Democratic"), 
    years = sum(term_length), 
    avg_term_length = mean(term_length)
  )


#on its own, sumamrise produces one row. That is why it is often used with group_by
#It is also suggested to use n() with each summarise for good measure

my_presidents %>%
  group_by(party) %>%
  summarise(
    N = n(), 
    first_year = min(year(start)), 
    last_year = max(year(end)), 
    num_dems = sum(party == "Democratic"), 
    years = sum(term_length), 
    avg_term_length = mean(term_length)
  )


#extended examples -----

library(Lahman)
head(Teams)
unique(Teams$name)
unique(Teams$teamID)

#look at twins inmy lifetime
twins <- Teams %>% 
  filter(name == "Minnesota Twins") %>%
  filter(yearID %in% 1992:2012) %>%
         select(yearID, teamID, W, L)

#expected wins as function of runs
twins_ben <- Teams %>% 
  select(name,yearID, teamID, W, L, R, RA) %>%
  filter(name == "Minnesota Twins" & yearID %in% 1992:2012)
twins_ben

#quick rename of R columns to RS
twins_ben <- twins_ben %>%
  rename(RS = R)

head(twins_ben)

#compute and add column for winning percentage
twins_ben <- twins_ben %>%
  mutate(WP = W/(W+L))

#compute what the 'model' would predict using a common equation
twins_ben <- twins_ben %>%
  mutate(WP_hat = 1 / (1 + (RA/RS)^2))

head(twins_ben)

#compuce expectd number of wins
twins_ben <- twins_ben %>% 
  mutate(W_hat = WP_hat * (W + L))
head(twins_ben)

plot(W_hat ~ W,data=twins_ben)

filter(twins_ben, W >= W_hat)

#use case_when to add new groups based off of conditions
twins_ben <- twins %>% 
  mutate(
    gm = case_when(
      yearID == 2004 ~ "Duquette", 
      yearID >= 2011 ~ "Alderson", 
      TRUE ~ "Minaya" #anything that isn't the two prior conditions = Minaya
    )
    
  )
    

#all the previous commands can be chained together with piping:


twins_2 <- Teams %>% 
  filter(name == "Minnesota Twins") %>%
  filter(yearID %in% 1992:2012) %>%
  select(name,yearID, teamID, W, L, R, RA) %>%
  filter(name == "Minnesota Twins" & yearID %in% 1992:2012) %>%
  rename(RS = R) %>%
  mutate(WP = W/(W+L)) %>%
  mutate(WP_hat = 1 / (1 + (RA/RS)^2)) %>% 
  mutate(W_hat = WP_hat * (W + L))  %>% 
  mutate(
    gm = case_when(
      yearID == 2004 ~ "Duquette", 
      yearID >= 2011 ~ "Alderson", 
      TRUE ~ "Minaya" #anything that isn't the two prior conditions = Minaya
    )
    
  )

#so fast!


