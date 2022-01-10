
#chapter 6; Tidy data


# google sheets example - doesn't work -----

library(googlesheets4)

hiv_key <- "1kWH_xdJDM4SMfT_Kzpkk-1yuxWChfurZuWYjfmv51EA"
hiv <- read_sheet(hiv_key) %>%
  rename(Country = 1) %>%
  filter(
    Country %in% c("United States", "France", "South Africa")
  ) %>%
  select(Country, `1979`, `1989`, `1999`, `2009`) %>%
  unnest(cols = c(`2009`)) %>%
  mutate(across(matches("[0-9]"), as.double))
hiv

#-----------
#tidy baby names ----

library(babynames)
 data("babynames")
head(babynames)

#look at top names in 1992
popular_names <- babynames %>% 
  group_by(sex, name, year) %>% #group aggregation by sex and names *and year)
  summarize(total_births = sum(n)) %>%  #count up # of names
  arrange(desc(total_births)) %>% #order by counts
  filter(year=='1992') #filter to the year you were born

#look at top years for Andrew
andrew <- babynames %>%
  group_by(name, year) %>% #group aggregation by sex and names *and year)
  summarize(total_births = sum(n)) %>%  #count up # of names
  filter(name=='Andrew') %>%
  arrange(desc(total_births)) %>% #order by counts

  
#reshaping -------
library(epiDisplay)
data(BP)
head(BP)

#long to wide
bp_wide <- BP %>% 
  pivot_wider(names_from = sex, values_from = c("sbp"))

#take names from 'sex' and values from sbp to create two new columns

#wide to long
bp_long <- bp_wide %>% 
  pivot_longer( -c(saltadd,birthdate,id,dbp), names_to = "sex", values_to = "sbp")

#hold constant value in '-' while splitting up the remaining into a 'sex' column were values are
#denoted in a 'sbp' column

#collapse grups of the dataframe into their own dataframes using nest:

bp_nest <- BP %>%
  group_by(sex,saltadd) %>%
  nest()

bp_nest <- bp_nest %>%
  mutate(sbp_list = map(data, pull, sbp)) #pull out spb values from this

 bp_nest %>%
  pluck("sbp_list")

 #get mean for each list
bp_nest <- bp_nest %>%
  mutate(sbp_mean = map(sbp_list, mean, na.rm = TRUE))

bp_nest <- bp_nest %>%
  unnest(cols = c(sbp_mean))

#this is akin to aggregating across groups in the aggregate function. Not sure how its better.


#follow up example

babynames %>% 
  filter(name == "Sue") %>%
  group_by(name, sex) %>% 
  summarize(total = sum(n))

#look at common names for boys and girls
babynames %>%
  filter(name %in% c("Sue", "Robin", "Leslie")) %>% #filter to key names
  group_by(name, sex) %>% #group by name and whether boy or girl
  summarize(total = sum(n)) %>% #count up all names
  pivot_wider(
    names_from = sex, 
    values_from = total
  ) #go to wide format to create a boy girl column name opopulated with the counts values

#do all baby names
baby_wide <- babynames %>%
  group_by(sex, name) %>%
  summarize(total = sum(n)) %>%
  pivot_wider(
    names_from = sex, 
    values_from = total, 
    values_fill = 0 #fill in NAs with zero
  )

head(baby_wide, 3)

#explore most even and uneven names
baby_wide_2 <- baby_wide %>% 
  filter(M > 50000, F > 50000) %>%
  mutate(ratio = pmin(M / F, F / M) ) %>% 
   arrange(desc(ratio)) #%>%
  # filter(name=='Andrew') %>%
  head(3)


#data intake ------
  
head(mtcars)
  d <- mtcars

  #save in native .rda format
saveRDS(d, file = "mtcars.rda", compress = TRUE) #compress will make file smaller

d.2 <- readRDS("mtcars.rda")

#read_csv is much faster than read.csv()

#you can have csvs exist online instead of locally for upload
mdsr_url <- "https://raw.githubusercontent.com/mdsr-book/mdsr/master/data-raw/"
houses <- mdsr_url %>%
  paste0("houses-for-sale.csv") %>%
  read_csv()
head(houses, 3)

#cool!

#you can import tables from web pages
library(rvest)
url <- "http://en.wikipedia.org/wiki/Mile_run_world_record_progression"
tables <- url %>% 
  read_html() %>% 
  html_nodes("table")

table_1 <- tables %>%
  purrr::pluck(1) %>%
  html_table()


#coding data
translations <- mdsr_url %>%
  paste0("house_codes.csv") %>%
  read_csv()
translations %>% head(5)

head(houses)

#turn numbers into meaningful groups using a code table
codes <- translations %>%
  pivot_wider(
    names_from = system_type, 
    values_from = meaning, 
    values_fill = "invalid"
  )

head(codes,3)

houses <- houses %>%
  left_join(
    codes %>% dplyr::select(code, fuel_type), 
    by = c(fuel = "code")
  ) %>%
  left_join(
    codes %>% dplyr::select(code, heat_type), 
    by = c(heat = "code")
  ) %>%
  left_join(
    codes %>% dplyr::select(code, sewer_type), 
    by = c(sewer = "code")
  )

#doesn't work unless specified by dplyr, other libraries probably messing with this.

#parse_number and parse_character convert between the two  much like as.numeric() and as.character()


#dealing with dates ----


library(lubridate)
head(ordway_birds)
data("ordway_birds")


birds <- ordway_birds %>% 
  dplyr::mutate(When = mdy_hms(Timestamp)) %>% #create timestamp
  dplyr::select(Timestamp, Year, Month, Day, When, DataEntryPerson)

birds %>% 
  glimpse()

#the timestamp create works with ggplot
birds %>% 
  ggplot(aes(x = When, y = DataEntryPerson)) + 
  geom_point(alpha = 0.1, position = "jitter")

#you can also do calculations with this timestamp format
bird_summary <- birds %>% 
  group_by(DataEntryPerson) %>% 
  summarize(
    start = first(When), 
    finish = last(When)
  ) %>%
  mutate(duration = interval(start, finish) / ddays(1))



# date times are stoped as POSIXct and POSIXlt
class(now())

#dates without time are stored as 'date' classes

class(as.Date(now()))

#reactor example

tables <- "http://en.wikipedia.org/wiki/List_of_nuclear_reactors" %>%
  read_html() %>% 
  html_nodes(css = "table")

idx <- tables %>%
  html_text() %>%
  str_detect("Fukushima Daiichi") %>%
  which()

reactors <- tables %>%
  purrr::pluck(idx) %>%
  html_table(fill = TRUE) %>%
  janitor::clean_names() 


reactors <- reactors %>%
  rename(
    reactor_type = reactor,
    reactor_model = reactor_2,
    capacity_net = net_capacity_mw #,
    #capacity_gross = capacity_in_mw_2
  ) 

reactors <- reactors %>%
  tail(-1) #gets rid of repetitve names in first row

?tail

glimpse(reactors)


reactors <- reactors %>% 
  mutate(
    plant_status = ifelse(
      str_detect(status, "Shut down"), 
      "Shut down", "Not formally shut down"
    ), 
    capacity_net = parse_number(capacity_net),
    construct_date = dmy(construction_start), 
    operation_date = dmy(commercial_operation), 
    closure_date = dmy(closure)
  )

glimpse(reactors)

head(reactors,3)

