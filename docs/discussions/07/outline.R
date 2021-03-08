#' ---
#' title: "Discussion 7 draft"
#' author: "Bi Cheng Wu"
#' documentclass: article
#' classoption: letterpaper
#' output:
#'  html_document:
#'   highlight: tango
#'   fig_caption: false
#' ---

#' ## Preprocessing

# library(glue)
# 
# dates = seq(as.Date("2000-01-01"),as.Date("2022-01-01"),by="year")
# for(d in seq_along(dates)){
#   if(d>1){
#     start = dates[d-1]
#     stop  = dates[d]-1
#     browseURL(glue(
#       "https://earthquake.usgs.gov/earthquakes/map/?",
#       "extent=-89.59492,-382.5&extent=89.58992,742.5&",
#       "range=search&timeZone=utc&search=%7B%22name%22:",
#       "%22Search%20Results%22,%22params%22:%7B%22",
#       "starttime%22:%22",toString(start),"%2000:00:00%22,%22",
#       "endtime%22:%22",toString(stop),"%2023:59:59%22,%22",
#       "minmagnitude%22:4,%22orderby%22:%22time%22%7D%7D"))
#   }
# }
# 
# pacman::p_load(plyr,tidyverse,magrittr,lubridate)
# 
# options(digits.secs=3)
# 
# read.csv("raw.csv.gz") %>%
#   mutate_at(c("time","updated"),
#             . %>% {as.POSIXct(strptime(.,"%Y-%m-%dT%H:%M:%OSZ",tz="UTC"))}) %>%
#   mutate_at(c("latitude","longitude","depth","mag","nst","gap","dmin",
#               "rms","horizontalError","depthError","magError","magNst"),as.numeric) %>%
#   filter(magSource == "us" & magError<.5 & between(year(time),2017,2020)) %>%
#   relocate(id,1) %>%
#   mutate(year = year(time),
#          mag.binned = mag %>%
#            cut_width(center=.5,width=.5,closed="l") %>%
#            fct_relabel(function(..) sapply(regmatches(..,gregexpr("[0-9.]+",..)),
#                                            . %>% as.numeric %>% mean %>% as.character)) %>%
#            as.character %>% 
#            as.numeric) %>% 
#   relocate(mag.binned,.after=mag) %>% 
#   filter(between(mag.binned,4.5,7)) ->
#   quakes.us
# 
# write.csv(quakes.us,gzfile("quakes.us.csv.gz",compression=9),row.names=F)

#' ## Visualization

pacman::p_load(plyr,tidyverse,magrittr,lubridate)

quakes.us = 
  read.csv("quakes.us.csv.gz") %>% 
  mutate_at(c("time","updated"),
            function(.) as.POSIXct(strptime(.,"%Y-%m-%d %H:%M:%OS",tz="UTC")))

quakes.us.count = 
  quakes.us %>% 
  count(year,mag.binned) %>% 
  arrange(year,desc(mag.binned)) %>% 
  group_by(year) %>% 
  mutate(N = cumsum(n)) %>% 
  arrange(year,mag.binned)

quakes.us.count %>% 
  ggplot(aes(x=mag.binned,y=N,group=factor(year),color=factor(year),linetype=factor(year))) +
  geom_point() + geom_line() + scale_y_log10() + 
  labs(title="Annual cumulative count of earthquakes v. magnitude in the US",
       x="Magnitude (rounded to nearest half-integer)", y="Cumulative count")


#' ## Estimation
#' 
#' Letting $N$ be cumulative count and $M$ be magnitude, the 
#' [Gutenberg-Richter law](https://en.wikipedia.org/wiki/Gutenberg%E2%80%93Richter_law)
#' tells us that $$N=10^{a-bM}$$ or in other words $$\log_{10}(N)=a-bM$$
#' Fit a linear model provides to estimate a and b, then 
#' show the fit with `summary` and plot the results

lm.quakes.us = lm(log10(N) ~ mag.binned, data=quakes.us.count)
summary(lm.quakes.us)

#' #### Plot automagically

ggplot(quakes.us.count,aes(x=mag.binned,y=N)) + scale_y_log10() + 
  geom_point(aes(color=factor(year))) + 
  geom_smooth(method='lm',se=F,size=.5) + 
  labs(title="Linear regression of annual cumulative count v. magnitude",
       x="Magnitude", y="Cumulative count")

#' #### Plot manually

ggplot(quakes.us.count,aes(x=mag.binned,y=log10(N),color=factor(year))) + 
  geom_point() + geom_abline(slope=coef(lm.quakes.us)[2],intercept=coef(lm.quakes.us)[1]) + 
  ylab(expression(log[10](Cumulative~count))) + 
  labs(title="Linear regression of annual cumulative count v. magnitude", x="Cumulative count")

#' ## Prediction
#' Using your estimates, estimate the average total number of earthquakes
#' in the US of ANY magnitude using $$N_{\text Total}=10^a$$

a = coef(lm.quakes.us)[1]
10^a

#' According to this model, how many earthquakes of magnitude 7 or 
#' greater do you expect to see on average per year in the US?

b = coef(lm.quakes.us)[2]
10^(a+b*7)

#' According to this model, you would expect to see an earthquake with magnitude
#' between 9 and 10 on average every how many years?

1/(10^(a+b*9)-10^(a+b*10))

confint(lm.quakes.us,level=.95)

      