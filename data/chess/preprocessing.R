# if running in Rstudio, remember to change working directory
if(Sys.getenv("RSTUDIO")==1) setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# import libraries with pacman
if(!require(pacman)) install.packages("pacman")
pacman::p_load(magrittr,tidyverse,lubridate,glue,rvest)

# download and parse latest FIDE ratings
for(FORMAT in c("standard","rapid","blitz")){
  temp = tempfile()
  download.file(glue("http://ratings.fide.com/download/{FORMAT}_rating_list.zip"),temp,quiet=T)
  f = readLines(unz(temp,glue("{FORMAT}_rating_list.txt")))
  writeLines(f,temp)
  Names = f[[1]] %>%
    sub("ID Number","ID_Number",.) %>%
    sub("B-day","B_day",.) %>%
    strsplit("(?<= )(?=[^ ])",perl=T) %>%
    unlist
  df = temp %>%
    read_fwf(fwf_widths(widths=str_length(Names)),skip=1) %>%
    setNames(trimws(Names)) %>%
    mutate(B_day = na_if(as.numeric(B_day),0))
  write_csv(df,gzfile(glue("data/{FORMAT}.csv.gz"),compression=9))
  unlink(temp)
  rm(FORMAT,temp,f,Names,df)
}

# download FIDE country codes if file doesn't exist
if(!file.exists("data/country_codes.csv")){
  countries = read_html("https://www.olimpbase.org/help/help41.html") %>%
    html_node("table") %>%
    html_table(fill=T) %>%
    select(2:3) %>%
    setNames(c("Fed","Country")) %>%
    mutate(Country = sub(" of America","",Country)) %>%
    filter(row_number()>1) %>%
    head(-4) %>%
    mutate(Country = sub("\\(.*|\\d+","",Country))
  write_csv(countries,"data/country_codes.csv")
}

countries = read_csv("data/country_codes.csv")



# read in standard ratings and rename/convert columns as appropriate
# removing junior (B_day >= 2000) and historic (< 1900) players
ratings.standard = read_csv("data/standard.csv.gz") %>% 
  rename(Rating = FEB21) %>% 
  filter(between(B_day,1900,1999)) %>% 
  left_join(countries,by="Fed") %>% 
  filter(!is.na(Country))

# make density plot
ratings.standard %>% 
  ggplot(aes(x=Rating,color=Sex)) + 
  geom_density(adjust=.1)

# which countries have most players? (looking only at top 30)
ratings.standard %>% 
  mutate(Country=fct_infreq(fct_lump_n(Country,30))) %>% 
  filter(Country != "Other") %>%
  ggplot(aes(x=Country)) + geom_bar(stat="count") + 
  theme(axis.text.x=element_text(angle=45,hjust=1))

# plot distributions of top countries
ratings.standard %>% 
  mutate(Country=fct_relevel(fct_infreq(fct_lump_n(Country,30)),"Other",after=Inf)) %>% 
  # filter(Country != "Other") %>% 
  ggplot(aes(x=Rating,color=Sex,fill=Sex)) + facet_wrap(vars(Country)) + 
  geom_density(adjust=.5,alpha=.6) + 
  theme(axis.text.x=element_text(angle=45,hjust=1))

# define permutation function
permutedMaxSexDiff = function(df,n){
  diffs = rep(NA,n)
  for(i in 1:n){
    num.draw = sum(df$Sex == "F")
    permuted = sample(df$Rating)
    max.F = max(permuted[1:num.draw])
    max.M = max(permuted[(num.draw+1):length(permuted)])
    diffs[i] = max.M-max.F
  }
  return(diffs)
}

# filter to look at US ratings only
permutedRatingsUS = ratings.standard %>% 
  filter(Country == "United States") %>% 
  arrange(desc(Rating)) %>% 
  permutedMaxSexDiff(10000)

permutedRatingsUS %>% 
  enframe(value="Difference") %>% 
  ggplot(aes(x=Difference)) + geom_density(adjust=3)

ratings.standard %>% 
  group_by(Sex) %>% 
  summarise(max = max(Rating),.groups="drop") %>% 
  pull(max) %>% 
  diff



# generate csv for discussion 6
read_csv("data/standard.csv.gz") %>% 
  rename(Rating = FEB21) %>% 
  left_join(countries,by="Fed") %>% 
  filter(!is.na(Country) & !is.na(Rating)) %>% 
  # filter(Country == "United States") %>% 
  arrange(desc(Rating)) %>% 
  select(ID_Number,Name,Sex,Country,B_day,Rating) %>% 
  write_csv(gzfile("../../docs/discussion/06/chess.csv.gz",compression=9))
# filter(between(B_day,1900,1999)) %>% 




# =============================================================


library(tidyverse)
library(magrittr)
countries = read_csv("data/country_codes.csv")

# checking for other countries
read_csv("data/standard.csv.gz") %>% 
  rename(Rating = FEB21) %>% 
  left_join(countries,by="Fed") %>% 
  filter(!is.na(Country) & !is.na(Rating)) %>% 
  arrange(desc(Rating)) %>% 
  select(ID_Number,Name,Sex,Country,B_day,Rating) -> 
  ratings.standard

ratings.standard %>% 
  mutate(Country=fct_infreq(fct_lump_n(Country,30))) %>% 
  filter(Country != "Other") %>% 
  filter(B_day < 2000) %>% 
  group_by(Country) %>% 
  summarise(n=n()) %>% 
  mutate(Country = as.character(Country)) %>% 
  pull(Country) -> 
  top30countries

permutedMaxSexDiff = function(df,cntry,n){
  diffs = rep(NA,n)
  which.people = (df$Country==cntry)
  n.F = sum(df$Sex[which.people] == "F")
  which.ratings = df$Rating[which.people]
  N = length(which.ratings)
  for(i in 1:n){
    permuted = sample(which.ratings)
    max.F = max(permuted[1:n.F])
    max.M = max(permuted[(n.F+1):N])
    diffs[i] = max.M-max.F
  }
  return(diffs)
}


# get actual diffs
ratings.standard %>% 
  mutate(Country=fct_infreq(fct_lump_n(Country,30))) %>% 
  filter(Country != "Other") %>% 
  filter(B_day < 2000) %>% 
  group_by(Country,Sex) %>% 
  summarise(max=max(Rating)) %>% 
  spread(Sex,max) %>% 
  mutate(diff.real=M-F,M=NULL,F=NULL) -> 
  diffs.real


# get all permuted diffs
diffs = sapply(top30countries,.%>%permutedMaxSexDiff(ratings.standard,.,10000),
               simplify = FALSE, USE.NAMES = TRUE)
diffs %<>% 
  as.data.frame %>% 
  setNames(.,gsub("."," ",names(.),fixed=T))

diffs %>% 
  gather("Country","Difference") %>% 
  full_join(diffs.real,by="Country") %>% 
  mutate(Country = fct_infreq(as_factor(Country))) -> 
  diffs.long

ggplot(diffs.long,aes(x=Difference)) + 
  facet_wrap(vars(Country),scales = "free") + 
  geom_density(adjust=2,fill="blue",color=NA,alpha=0.5) + 
  geom_vline(aes(xintercept=diff.real))

ggsave("gaps.png",width=12,height=8,dpi=600)

ratings.standard %>% 
  filter(Country %in% c("United States","India")) %>% 
  ggplot(aes(x=B_day,color=Sex)) + facet_wrap(vars(Country)) + 
  geom_line(stat="count")

