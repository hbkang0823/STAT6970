###############################
#### Data Wraingling in R #####
###############################

library(tidyverse)
library(dplyr)
library(tidyr)

#library(devtools)
#devtools::install_github("rstudio/EDAWR")
library(EDAWR)

storms
storms$storm
storms$wind
storms$pressure
storms$date
storms$ratio <- storms$pressure / storms$wind
storms$ratio

cases
cases$country
names(cases)[-1]
unlist(cases[1:3, 2:4])

pollution
pollution$city[c(1,3,5)]
pollution$amount[c(1,3,5)]
pollution$amount[c(2,4,6)]

# 'gather' function and 'spread' function
gather(cases, "year", "n", 2:4)
spread(pollution, size, amount)

# 'separate' function and 'unite' function
storms2 <- separate(storms, date, c("year", "month", "day"), sep = "-"); storms2
storms3 <- unite(storms2, "date", year, month, day, sep = "-"); storms3
all.equal(storms, storms3)

storms3$date <- as.Date(storms3$date)
all.equal(storms, storms3)


#install.packages("nycflights13")
library(nycflights13)

select(storms, storm, pressure)
select(storms, -storm)
select(storms, wind:date)
select(storms, contains("s"))
select(storms, ends_with("e"))
#-              Select everything but
#:              Select range
#contains()     Select columns whose name contains a character string
#ends_with()    Select columns whose name ends with a string
#everything()   Select every column
#matches()      Select columns whose name matches a regular expression
#num_range()    Select columns named x1, x2, x3, x4, x5
#one_of()       Select columns whose names are in a group of names
#starts_with()  Select columns whose name starts with a character string

filter(storms, wind >= 50)
filter(storms, wind >= 50,
       storm %in% c("Alberto", "Alex", "Allison"))
?base::Logic

mutate(storms, ratio = pressure / wind)
mutate(storms, ratio = pressure / wind, inverse = ratio^-1)

#pmin(), pmax()       Element-wise min and max
#cummin(), cummax()   Cumulative min and max
#cumsum(), cumprod()  Cumulative sum and product
#between()            Are values between a and b?
#cume_dist()          Cumulative distribution of values
#cumall(), cumany()   Cumulative all and any
#cummean()            Cumulative mean
#lead(), lag()        Copy with values one position
#ntile()              Bin vector into n buckets
#dense_rank(), min_rank(), percent_rank(), row_number() Various ranking methods

pollution %>% summarise(median = median(amount), variance = var(amount))
pollution %>% summarise(mean = mean(amount), sum = sum(amount), n = n())
pollution %>% summarise(mean = mean(amount), sum = sum(amount), n_distinct = n_distinct(amount))

arrange(storms, wind)
arrange(storms, desc(wind))
arrange(storms, wind, date)

select(tb, child:elderly)
tb %>% select(child:elderly)

select(storms, storm, pressure)
storms %>% select(storm, pressure)

filter(storms, wind >= 50)
storms %>% filter(wind >= 50)

storms %>%
  filter(wind >= 50) %>%
  select(storm, pressure)

storms %>%
  mutate(ratio = pressure / wind) %>%
  select(storm, ratio)

# shortcut to type %>%: ctrl (or cmd) + shift + M

pollution %>% group_by(city)
pollution %>% group_by(city) %>%
  summarise(mean = mean(amount), sum = sum(amount), n = n())
pollution %>% group_by(city) %>% summarise(mean = mean(amount))

pollution %>% group_by(size) %>% summarise(mean = mean(amount))
#pollution %>% ungroup()


tb %>%
  group_by(country, year) %>%
  summarise(cases = sum(cases)) %>%
  summarise(cases = sum(cases))


#dplyr::bind_cols()
#dplyr::bind_rows()
#dplyr::union()
y <- tbl_df(data.frame(x1 = c("A","B","C"), x2 = 1:3))
z <- tbl_df(data.frame(x1 = c("B","C","D"), x2 = 2:4))
bind_cols(y, z)
bind_rows(y, z)
union(y, z)
intersect(y, z)
setdiff(y, z)

songs
artists
left_join(songs, artists, by = "name")
right_join(songs, artists, by = "name")

songs2
artists2
left_join(songs2, artists2, by = c("first", "last"))
right_join(songs2, artists2, by = c("first", "last"))

inner_join(songs, artists, by = "name")
semi_join(songs, artists, by = "name")
anti_join(songs, artists, by = "name")


### Dealing with strings
#install.packages("stringr")
library(stringr)
x <- c("why", "video", "cross", "extra", "deal", "authority")
str_length(x) 
str_c(x, collapse = ", ")
str_sub(x, 1, 2)

str_subset(x, "[aeiou]")
str_count(x, "[aeiou]")
str_detect(x, "[aeiou]")
str_locate(x, "[aeiou]")
str_extract(x, "[aeiou]")
str_match(x, "(.)[aeiou](.)")
str_replace(x, "[aeiou]", "?")
str_split(c("a,b", "c,d,e"), ",")


### Dealing with dates and times
#install.packages("lubridate")
library("lubridate")

ymd(20101215)
mdy("4/1/17")

bday <- dmy("14/10/1979")
month(bday)
wday(bday, label = TRUE)
year(bday) <- 2016
wday(bday, label = TRUE)

time <- ymd_hms("2010-12-13 15:30:30")
time

with_tz(time, "America/Chicago")
force_tz(time, "America/Chicago")


################################################
################ Reading Data ##################
################################################
library("readr")
readr_example()
readr_example("mtcars.csv")
mtcars <- read_csv(readr_example("mtcars.csv"))
mtcars <- read_csv(readr_example("mtcars.csv"), col_types = 
                     cols(
                       mpg = col_double(),
                       cyl = col_integer(),
                       disp = col_double(),
                       hp = col_integer(),
                       drat = col_double(),
                       vs = col_integer(),
                       wt = col_double(),
                       qsec = col_double(),
                       am = col_integer(),
                       gear = col_integer(),
                       carb = col_integer()
                     )
)


library("readxl")
readxl_example()
readxl_example("clippy.xls")
xlsx_example <- readxl_example("datasets.xlsx")
read_excel(xlsx_example)

xls_example <- readxl_example("datasets.xls")
read_excel(xls_example)

excel_sheets(xlsx_example)
excel_sheets(xls_example)

read_excel(xlsx_example, sheet = "chickwts")
read_excel(xls_example, sheet = 4)

read_excel(xlsx_example, n_max = 3)
read_excel(xlsx_example, range = "C1:E4")
read_excel(xlsx_example, range = cell_rows(1:4))
read_excel(xlsx_example, range = cell_cols("B:D"))
read_excel(xlsx_example, range = "mtcars!B1:D5")

read_excel(xlsx_example, na = "setosa")



library("haven")

# SAS
read_sas("mtcars.sas7bdat")
write_sas(mtcars, "mtcars.sas7bdat")

# SPSS
read_sav("mtcars.sav")
write_sav(mtcars, "mtcars.sav")

# Stata
read_dta("mtcars.dta")
write_dta(mtcars, "mtcars.dta")


################################################
################ Web Scraping ##################
################################################
library(rvest)
#read_html(url) : scrape HTML content from a given URL
#html_nodes(): identifies HTML wrappers.
#html_nodes(".class"): calls node based on CSS class
#html_nodes("#id"): calls node based on <div> id
#html_attrs(): identifies attributes (useful for debugging)
#html_table(): turns HTML tables into data frames
#html_text(): strips the HTML tags and extracts only the text


# Example: html_nodes
ateam <- read_html("http://www.boxofficemojo.com/movies/?id=ateam.htm")
html_nodes(ateam, "center")
html_nodes(ateam, "center font")
html_nodes(ateam, "center font b")

ateam %>% html_nodes("center") %>% html_nodes("td")
ateam %>% html_nodes("center") %>% html_nodes("font")

ateam %>% html_nodes("center") %>% html_nodes("td") %>% 
  html_text() %>% str_split(., ":") %>% unlist() %>% matrix(.,ncol=2,byrow=T) %>% as.data.frame()



# Example: Lego Movie
lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")

rating <- lego_movie %>% 
  html_nodes("strong span") %>%
  html_text() %>%
  as.numeric()
rating

cast <- lego_movie %>%
  html_nodes("#titleCast .primary_photo img") %>%
  html_attr("alt")
cast

lego_movie %>% 
  html_nodes("#titleCast a img") %>% 
  html_attr("alt")

poster <- lego_movie %>%
  html_nodes(".poster img") %>%
  html_attr("src")
poster


# Example: html_table
births <- read_html("https://www.ssa.gov/oact/babynames/numberUSbirths.html")
tmp <- html_table(html_nodes(births, "table"))[[1]]
tmp2 <- apply(tmp, c(1,2), str_replace_all, ",","")
tmp3 <- apply(tmp2, c(1,2), as.numeric)



# EXAMPLE: 100 popular films released in 2016.
# following https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/
url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'
webpage <- read_html(url)

rank_data <- html_nodes(webpage,'.text-primary')
rank_data <- html_text(rank_data_html)
head(rank_data)
rank_data<-as.numeric(rank_data)
head(rank_data)

rank_data <- html_nodes(webpage,'.text-primary') %>%
  html_text() %>%
  as.numeric()
head(rank_data)

title_data <- html_nodes(webpage,'.lister-item-header a') %>%
  html_text()
head(title_data)

description_data <- html_nodes(webpage,'.ratings-bar+ .text-muted') %>%
  html_text() %>%
  str_remove_all(.,"\n") %>%
  str_trim()
head(description_data)

runtime_data <- html_nodes(webpage,'.text-muted .runtime') %>%
  html_text() %>%
  str_remove_all(" min") %>%
  as.numeric()
head(runtime_data)

genre_data <- html_nodes(webpage,'.genre') %>%
  html_text() %>%
  str_remove_all("\n") %>%
  str_trim() %>%
  sapply(., function(x){str_split(x,", ")[[1]][1]}) %>%
  #taking only the first genre of each movie
  as.factor()
  

#Using CSS selectors to scrape the IMDB rating section
rating_data <- html_nodes(webpage,'.ratings-imdb-rating strong') %>%
  html_text() %>%
  as.numeric()
head(rating_data)

votes_data <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)') %>%
  html_text() %>%
  str_replace_all(., ",","") %>%
  as.numeric()
head(votes_data)

#Using CSS selectors to scrape the directors section
directors_data <- html_nodes(webpage,'.text-muted+ p a:nth-child(1)') %>%
  html_text() %>%
  as.factor()
head(directors_data)

actors_data <- html_nodes(webpage,'.lister-item-content .ghost+ a') %>%
  html_text() %>%
  as.factor()
head(actors_data)

metascore_data <- html_nodes(webpage,'.metascore') %>%
  html_text() %>%
  str_trim() %>%
  as.numeric()
length(metascore_data) # not 100

#put NA's
metascore_data2 <- rep(NA,100)
#metascore_data2[-c(29,58,73,96)] <- metascore_data NEED TO BE CHANGED
metascore_data <- as.numeric(metascore_data2)
summary(metascore_data)


gross_data <- html_nodes(webpage,'.ghost~ .text-muted+ span') %>%
  html_text() %>%
  str_replace(.,"M","") %>%
head(gross_data)
gross_data<-substring(gross_data,2,6)
length(gross_data)

#put NA's
gross_data2 <- rep(NA, 100)
gross_data2[-c(29,45,57,62,73,93,98)] <- gross_data
gross_data <- as.numeric(gross_data2)
length(gross_data)


movies_df<-data.frame(Rank = rank_data, Title = title_data,
                      Description = description_data, Runtime = runtime_data,
                      Genre = genre_data, Rating = rating_data,
                      Metascore = metascore_data, Votes = votes_data,
                      Gross_Earning_in_Mil = gross_data,
                      Director = directors_data, Actor = actors_data)

library(ggplot2)
qplot(data = movies_df,Runtime,fill = Genre,bins = 30)
ggplot(movies_df,aes(x=Runtime,y=Rating))+
  geom_point(aes(size=Votes,col=Genre))
ggplot(movies_df,aes(x=Runtime,y=Gross_Earning_in_Mil))+
  geom_point(aes(size=Rating,col=Genre))




