# reference: https://www.bls.gov/lau/

# if running in Rstudio, remember to change working directory
if(Sys.getenv("RSTUDIO")==1) setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# useful function
'%notin%' = Negate("%in%")

# # install blscrape if not installed
# if("blscrapeR" %notin% rownames(installed.packages())) install.packages("blscrapeR")

# import libraries with pacman
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,ggplot2)

# create raw data directory if none exists
if(!dir.exists("raw")) dir.create("raw")

# download raw text files (years 1990-2019 available)
for(i in 1990:2019){
  j = substr(toString(i),3,4)
  download.file(sprintf("https://www.bls.gov/lau/laucnty%s.txt",j),
                destfile=sprintf("raw/%s.txt",i),quiet=T)
}

readLAUSfile = function(FILE){
  
  # read in file
  df = readLines(FILE)
  
  # start when we see numbers separated by spaces, and stop before we see "SOURCE:", ignoring blank lines
  try({ df = setdiff(df[(grep('\\d\\s+\\d',df)[1]):(grep('SOURCE:',df)[1]-1)],'') }, silent=T)
  
  # check just in case there's a double space due to entry error (this shouldn't exist)
  if( length(grep('\\S\\s{2}\\S',df,value=T)) > 0 ) stop("Double space found somewhere, check data frame!")
  
  # add state name 'DC' to row for District of Columbia
  df = sub('(.*)District of Columbia(.*)','\\1District of Columbia, DC\\2',df,ignore.case=T)
  
  # now check if any lines don't have state name after county name (also indicates inconsistencies)
  if (length(grep('[A-z], [A-z]{2}',df,invert=T)) > 0 ) stop("Missing state info somewhere, check data frame!")
  
  # replace multiple spaces or ", " before state name with ; to use as delimiter,
  # replace commas in numbers, and convert to data frame
  df = gsub('(\\d),(\\d)','\\1\\2',gsub('\\s{3,}|, ',';',df))
  df = read.delim(text=df,sep=';',header=F,na.strings=c("N.A."),colClasses=c(rep("character",5),rep("numeric",5)),
                  col.names=c("LAUScode","stateFIPS","countyFIPS","countyName","stateName",
                              "year","laborForce","employed","unemployed","unemployment_rate"))
  
  # # blscrape doesn't have all county fips, which is why we need to do our own state name extraction above
  # left_join(df,blscrapeR::county_fips,by=c("fips_state","fips_county")) %>% filter(!complete.cases(.))
  
  df
}

# run function on all files
unemployment = list.files("raw",full.names=T) %>% lapply(readLAUSfile) %>% do.call(rbind,.)

# show missing values
unemployment[!complete.cases(unemployment),]

# save unemployment data
write.csv(unemployment,file="unemployment.csv",row.names=F)
