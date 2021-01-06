# if running in Rstudio, remember to change working directory
if(Sys.getenv("RSTUDIO")==1) setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# import libraries with pacman
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,ggplot2,lubridate,magrittr)

# useful function
'%notin%' = Negate("%in%")

# if no data exists or is older than available, throw exception and download files
# finally, load files
tryCatch({
  
  getDateFromCsv = function(filename){
    filename %>% 
      read.csv(check.names=F,nrows=1) %>%   # read csv
      names %>%                             # get names
      tail(n=1) %>%                         # get last column (most recent)
      mdy                                   # convert to date
  }
  
  if(!dir.exists("raw")) dir.create("raw")
  newDate = getDateFromCsv("https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv")
  oldDate = getDateFromCsv("./raw/cases.csv")
  
  if(oldDate < newDate) stop("Downloading latest data.")
  
}, error = function(e){
  download.file(url="https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv",
                destfile="./raw/cases.csv")
  download.file(url="https://static.usafacts.org/public/data/covid-19/covid_deaths_usafacts.csv",
                destfile="./raw/deaths.csv")
  download.file(url="https://static.usafacts.org/public/data/covid-19/covid_county_population_usafacts.csv",
                destfile="./raw/counties.csv")
}, finally={
  cases    = read.csv("./raw/cases.csv",check.names=F)
  deaths   = read.csv("./raw/deaths.csv",check.names=F)
  counties = read.csv("./raw/counties.csv",check.names=F)
  suppressWarnings(rm(newDate,oldDate,getDateFromCsv))
})

# rename county/state info columns to be more consistent
renameCountyState = function(df){
  
  # find column in a robust way (also save to global environment with <<- to be used for counties data later)
  whichColumnMatches <<- function(df,regex,ignore.case=T,allow.multiple=F){
    col = grep(regex,names(df),ignore.case=ignore.case)
    if(length(col)!=1 & !allow.multiple) stop("More/fewer than 1 column matched. Format of data has changed!")
    col
  }
  
  # rename to be consistent
  names(df)[whichColumnMatches(df,".*county.*fips.*")] = "countyFIPS"
  names(df)[whichColumnMatches(df,".*county.*name.*")] = "countyName"
  
  # handle state names and FIPS columns in a special way to make sure they're identified correctly
  names(df)[stateFIPScol <- whichColumnMatches(df,".*state.*fips.*")]  = "stateFIPS"
  names(df)[setdiff(whichColumnMatches(df,".*state.*",allow.multiple=T),stateFIPScol)] = "stateName"
  
  # reorder columns in a logical way
  df %>% relocate(stateName,stateFIPS,countyName,countyFIPS,.before=1)
}

# remove rows with FIPS=0 (as far as I can tell these are useless artifacts)
removeZeroFips = function(df){
  df %>% filter(countyFIPS!=0 & stateFIPS!=0)
}

# apply function to cases and deaths (these lines should be idempotent)
cases  %<>% renameCountyState %>% removeZeroFips
deaths %<>% renameCountyState %>% removeZeroFips

# handle counties data frame differently due to uniqueness (it seems to be missing stateFIPS)
# the code is written in this funny way to also maintain idempotence
names(counties)[whichColumnMatches(counties,".*county.*fips.*")] = "countyFIPS"
counties %<>% mutate(stateFIPS=9999) %>% removeZeroFips %>% select(-stateFIPS)

# double check countyFIPS all match
setdiff(Reduce(base::union,list(cases$countyFIPS,deaths$countyFIPS,counties$countyFIPS)),
        Reduce(base::intersect,list(cases$countyFIPS,deaths$countyFIPS,counties$countyFIPS)))

# NYC "unallocated" cases/deaths received placeholder countyFIPS code of 1
# these have non-trivial counts, so keep them for now?
cases %>% 
  filter(countyFIPS==1) %>% 
  gather(date,count,5:last_col()) %>% 
  filter(count>0)

deaths %>% 
  filter(countyFIPS==1) %>% 
  gather(date,count,5:last_col()) %>% 
  filter(count>0)

# join county population data into cases and deaths before pivotting should be easiest
if("population" %notin% names(cases)){
  cases %<>% 
    full_join(counties %>% select(countyFIPS,population)
              ,by="countyFIPS") %>% 
    relocate(population,.after=countyFIPS) %>% 
    arrange(countyFIPS)
}
if("population" %notin% names(deaths)){
  deaths %<>% 
    full_join(counties %>% select(countyFIPS,population)
              ,by="countyFIPS") %>% 
    relocate(population,.after=countyFIPS) %>% 
    arrange(countyFIPS)
}

# gather to long format
cases_long  = cases  %>% gather(date,cases,6:last_col())
deaths_long = deaths %>% gather(date,deaths,6:last_col())

# combine all
covid19 = full_join(cases_long %>% select(countyFIPS,date,cases),
                    deaths_long, 
                    by=c("countyFIPS","date")) %>% 
  mutate(date = mdy(date),
         countyFIPS = as.character(countyFIPS),
         stateFIPS  = as.character(stateFIPS)) %>% 
  relocate(date,stateName,stateFIPS,countyName,countyFIPS,population,.before=1)

# check result
str(covid19)
head(covid19,10)
tail(covid19,10)

# save result
write.csv(covid19,file=gzfile("covid19.csv.gz",compression=9))
