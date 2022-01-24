
#Chapter 7: Iteration

#vectoeized operations

library(Lahman)

names(Teams)

#gilmpse and structure do similar things
str(Teams)
glimpse(Teams)

#for loop example: compute average for each specific column

averages <- NULL

for(i in 15:40){
  
  averages[i-14] <- mean(Teams[,i],na.rm=T) #take average of columns i
  
}

names(averages) <- names(Teams)[15:40]

#for somewhat specific cases (when having to define row or column numbers) such 
#as the code above, it is probably better to iterate without looping in R, where
#you can apply an operation to each element in a vector. vectors are key
#to the architecture of R.


a <- 'a_string'
class(a) #its character
is.vector(a) #stored as a vector

#R is good, therefore, for vectorized operations unlike general purpose languages
#like C or python. This means R has several ways to do loop-like operations
#without looping. It it suggested to avoid looping in R.


#vectorized functions: perform operation on each item in a vector, 
#and output is a vector of same length
#summary functions input each item in a  vector to produce one output (vector of 1)

#across function: run a summary operation across columns

#across numeric columns
Teams %>%
  summarise(across(where(is.numeric),mean,na.rm=T))

#across years for specific colums
Teams %>%
  summarise(across(yearID,R:SF),mean,na.rm=T)

Teams %>%
  summarise(across(c(yearID, R:SF, BPF), mean, na.rm = TRUE))


#map function
?map_dbl

library(purrr)
Teams %>%
  dplyr::select(15:40) %>% #need to specify dplyr-based select function
  map_dbl(mean,na.rm=T)

#regular 'map' returns a list. type-specific map functions will return vectors

#selects columns 15 through 40, and calculates the mean for each

#group anaheim angels team hsitory
head(Teams,1)
unique(Teams$franchID)

Teams %>%
  filter(franchID == 'ANA') %>%
  group_by(teamID,name) %>%
  summarise(began = first(yearID),end = last(yearID)) %>%
  arrange(began)

#find how long each name exited
?nchar


  Teams %>%
  filter(franchID == 'ANA') %>%
  group_by(teamID,name) %>%
  summarise(began = first(yearID),end = last(yearID)) %>%
  arrange(began) %>%
  pull(name) %>%
  map_int(nchar)


#iterate over a function

#example funtion
  top5 <- function(data, team_name) {
    
    data %>%
      dplyr::filter(name == team_name) %>%
      dplyr::select(teamID, yearID, W, L, name) %>%
      arrange(desc(W)) %>%
      head(n = 5)
  
  }
  
  
  # Teams %>%
  #   dplyr::filter(name == "Boston Red Stockings") %>%
  #   dplyr::select(teamID, yearID, W, L, name) %>%
  #   arrange(desc(W)) %>%
  #   head(n = 5)
  
  #top5(data=Teams,team_name = "Boston Red Stockings")

angels <- Teams %>% 
  filter(franchID == "ANA") %>% 
  group_by(teamID, name) %>%
  summarise(began = first(yearID), ended = last(yearID)) %>% 
  arrange(began)

angels_names <- angels %>%
  pull(name)
nchar(angels_names[1])

angels_names %>%
  map(top5, data = Teams) #vector of names is input into the 'team_name' part I guess

#turn into dataframe with map_dfr
angels_names %>% 
  map_dfr(top5, data = Teams) %>% #stop here and you can see lists are combined into a df
  group_by(teamID, name) %>% #following code gets # of years and average # of wins and then order by mean wins
  summarize(N = n(), mean_wins = mean(W)) %>%
  arrange(desc(mean_wins))

#iteration over subgroups

#predicted versus observed winning % based off of runs scores and allowed

exp_wpct <- function(x) { 
  return(1/(1 + (1/x)^1.84))
}

#calculate actual winning %
TeamRuns <- Teams %>% 
  filter(yearID >= 1954) %>%
  rename(RS = R) %>% 
  mutate(WPct = W / (W + L), run_ratio = RS/RA) %>%
  dplyr::select(yearID, teamID, lgID, WPct, run_ratio)

#plot this out with model fit as the line
ggplot(data = TeamRuns, aes(x = run_ratio, y = WPct)) +
  geom_vline(xintercept = 1, color = "darkgray", linetype = 2) +
  geom_hline(yintercept = 0.5, color = "darkgray", linetype = 2) +
  geom_point(alpha = 0.2) + 
  stat_function(fun = exp_wpct, size = 2, color = "blue") + 
  xlab("Ratio of Runs Scored to Runs Allowed") + 
  ylab("Winning Percentage")


#play around with finding the best exponential values using nls

TeamRuns %>%
  nls(
    formula = WPct ~ 1/(1 + (1/run_ratio)^k), 
    start = list(k = 2)
  ) %>%
  coef()

#k = 1.84 



#turn the nls code into a function to find k by decade:

fit_k <- function(x) {
  mod <- nls(
    formula = WPct ~ 1/(1 + (1/run_ratio)^k), 
    data = x,
    start = list(k = 2)
  )
  
  return(tibble(k = coef(mod), n = nrow(x)))
  
}

#try function over entire dataset
fit_k(TeamRuns)

#now do it by decade

TeamRuns %>% 
  mutate(decade = yearID %/% 10 * 10) %>% #integer divison
  group_by(decade) %>% 
  group_modify(~fit_k(.x))
  

# help("%/%")
# 5 %/% 2

#function to find leading hr in each year
head(Teams)

lead_hr <- function(x){
  
  x %>% 
  dplyr::select(teamID,HR) %>%
  arrange(desc(HR)) %>%
  head(1)
  
}

#look at years of interest
Teams %>% 
  dplyr::filter(yearID == 2003) %>% 
  lead_hr()

#group over all years
Teams %>% 
  group_by(yearID) %>% 
  group_modify(~lead_hr(.x),na.rm=T) %>%
  arrange(desc(HR)) #Minnesota with the record!

#look at change through time among the two leagues
hr_leaders <- Teams %>% 
  group_by(yearID, lgID) %>% 
  group_modify(~lead_hr(.x), .keep = TRUE)

hr_leaders %>% 
  filter(yearID >= 1916) %>%
  ggplot(aes(x = yearID, y = HR, color = lgID)) + 
  geom_line() + 
  geom_point() + 
  geom_smooth(se = FALSE) + 
  geom_vline(xintercept = 1973) + 
  annotate(
    "text", x = 1974, y = 25, 
    label = "AL adopts DH", hjust = "left"
  ) +
  labs(x = "Year", y = "Home runs", color = "League")




#simulation

#look at distribution of k values across each year
k_actual <- TeamRuns %>% 
  group_by(yearID) %>% 
  group_modify(~fit_k(.x))

k_actual %>%
  ungroup() %>%
  skim()

hist(k_actual$k)

ggplot(k_actual,aes(x=k)) +
  geom_density()

#do a bootstrpping procedure to get a more robust estimate of distribution of k values

n <- 10000

bstrap <- 1:n %>%
  
  map_dbl(
    ~k_actual %>%
      pull(k) %>%
      sample(replace = TRUE) %>% 
      mean()
  )

#look at distribution 
civals <- bstrap %>%
  quantile(probs = c(0.025, .975))
civals

#now look at distribution of boostrappd sample
ggplot(data = enframe(bstrap, value = "k"), aes(x = k)) + 
  geom_density() + 
  xlab("Distribution of resampled means") + 
  geom_vline(
    data = enframe(civals), aes(xintercept = value), 
    color = "red", linetype = 3
  )




