# reference: https://www.bls.gov/lau/

# if running in Rstudio, remember to change working directory
if(Sys.getenv("RSTUDIO")==1) setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# import libraries with pacman
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,ggplot2,blscrapeR,magrittr,gganimate,gifski)
pacman::p_load_gh("thomasp85/transformr")
pacman::p_load_gh("ropensci/plotly")

# read in csv, making sure to keep fips codes as characters
unemployment = read.csv("unemployment.csv",colClasses=c(rep("character",5),rep("numeric",5)))

# create id column for matching values to regions in geom_map
unemployment %<>% mutate(id = as.character(paste0(stateFIPS,countyFIPS)),
                         year = as.integer(year))

# # static plot
# unemployment %>%
#   filter(year==2019) %>%
#   ggplot(aes(map_id=id)) + theme_void() + coord_fixed() + 
#   geom_map(aes(fill=unemployment_rate),map=county_map_data,color="black",size=.1) + 
#   expand_limits(x=county_map_data$long,y=county_map_data$lat) + 
#   geom_map(data=state_map_data,aes(group=group),map=state_map_data,fill=NA,color="black",size=.2) + 
#   scale_fill_continuous(type="viridis",trans='log10',
#                         breaks=B<-c(.7,1,1.5,2,2.5,3,4,5,6,8,10,12,14,16,18,20),labels=B,
#                         limits=c(0.7,20)) + 
#   guides(fill = guide_colourbar(barwidth=1,barheight=20,title="Rate (%)",
#                                 title.theme=element_text(margin=unit(c(0,0,12,0),"pt")))) +
#   ggtitle("US unemployment by county (2019)") + theme(plot.title=element_text(hjust=.5,size=16))

# animated plot (may require latest developmental version of ggplot2/gganimate from github)
p = ggplot(unemployment, aes(map_id=id)) + theme_void() + coord_fixed() + 
  geom_map(aes(fill=unemployment_rate),map=county_map_data,color="black",size=.1) + 
  expand_limits(x=county_map_data$long,y=county_map_data$lat) + 
  geom_map(data=state_map_data,aes(group=group),map=state_map_data,fill=NA,color="black",size=.5) + 
  scale_fill_continuous(type="viridis",trans='log10',
                        breaks=B<-c(.7,1,1.5,2,2.5,3,4,5,6,8,10,12,14,16,18,20),labels=B,
                        limits=c(0.7,20)) + 
  guides(fill = guide_colourbar(barwidth=1,barheight=20,title="Rate (%)",
                                title.theme=element_text(margin=unit(c(0,0,12,0),"pt")))) + 
  ggtitle("US unemployment by county ({current_frame})") + 
  theme(plot.title=element_text(hjust=.5,size=16)) + 
  transition_manual(year)

animate(p,nframes=length(unique(unemployment$year))+6,fps=2,
        width=9,height=6,units='in',res=150,start_pause=1,end_pause=5)
anim_save("unemployment.gif")
