# if running in Rstudio, remember to change working directory
if(Sys.getenv("RSTUDIO")==1) setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# import libraries with pacman
if(!require(pacman)) install.packages("pacman")
pacman::p_load(magrittr,tidyverse,lubridate,rjson,tidyjson)

# define function to read dates
readDate = function(jsonFile){
  fromJSON(file=jsonFile) %>% 
    as_tbl_json %>% 
    spread_all %>% 
    select(-document.id) %>% 
    mutate(time = time %>% 
             gsub("(.*):(..)$","\\1\\2",.) %>% 
             strptime("%Y-%m-%dT%H:%M:%S%z",tz="UTC"))
}

# define function to read users
readUsers = function(jsonFile="https://raw.githubusercontent.com/alexlitel/congresstweets-automator/master/data/historical-users-filtered.json"){
  fromJSON(file=jsonFile) %>% 
    as_tbl_json %>% 
    spread_all %>% 
    select(-document.id)
}

# test
D = readDate("raw/2020-12-12.json.xz")
head(D)

U = readUsers()
head(U)
