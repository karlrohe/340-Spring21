# if running in Rstudio, remember to change working directory
if(Sys.getenv("RSTUDIO")==1) setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# import libraries with pacman
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,ggplot2,lubridate,magrittr)

# define function to download all files
getDates = function(dir="raw",start=Sys.Date(),stop=NULL,tries=20L,stopIfExists=T){
  
  # create raw data directory if none exists
  if(!dir.exists(dir)) dir.create(dir)
  
  # define function to get a single date
  getDate = function(date,dir=dir){
    tryCatch({
      page = readLines(sprintf("https://alexlitel.github.io/congresstweets/data/%s.json",date),warn=F)
    },error=function(e){
      stop(sprintf("    Failed to fetch date!",date))
    })
    if(!is.character(page) || nchar(page)<3){
      stop(sprintf("    Empty date!",date))
    }else{
      f = xzfile(sprintf("%s/%s.json.xz",dir,date),compression=9)
      write(page,file=f)
      close(f)
    }
  }
  
  if(is.null(stop) || is.na(stop)){
    nDates = Inf
  } else if(is.numeric(stop) && stop>0){
    nDates = stop
  } else tryCatch({
    nDates = as.numeric(start-as.Date(stop))+1
  },error = function(e){
    stop("Invalid start/stop!")
  })
  
  # now we loop and get the entire history
  while(tries>0 && nDates>0){
    message(sprintf("Trying %s",start))
    if(file.exists(sprintf("%s/%s.json.xz",dir,start))){
      message("    already exists, skipping")
    }else{
      message("    downloading")
      tryCatch({
        getDate(start,dir)
        message("    download complete")
      },error=function(e){
        tries <<- tries-1
        message(sprintf("    download failed; #tries left: %d",tries))
      })
    }
    start  = start-1
    nDates = nDates-1
  }
}

# run function with default values:
# dir          = "raw"       # output directory
# start        = Sys.Date()  # start date
# stop         = NULL        # end (can be NULL/NA, # of days, or date string)
# tries        = 20L         # no. dates fails before abort (useful if no end or some dates missing)
# stopIfExists = True        # skip if file already exists
getDates()



