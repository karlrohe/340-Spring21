#' ---
#' title: "Exam review Q4"
#' author: "Bi Cheng Wu"
#' documentclass: article
#' classoption: letterpaper
#' output:
#'  html_document:
#'   highlight: tango
#'   fig_caption: false
#' ---
#' 
#' [Link to source](exam_review_4.R). Note this source file is knitted using the
#' [spin feature](https://bookdown.org/yihui/rmarkdown-cookbook/spin.html) of `knitr`.
#' 
#' ## Background
#' 
#' After class, Karl invited me to tackle question 4 on the exam review. See recording
#' [**here**](https://us-lti.bbcollab.com/recording/bb5cd70603a84654b5faa27a266fbbf2).
#' This is the slightly more polished version than what I coded up live, with some
#' fixes to the MC in MC part so you get a proper coverage estimate.
#' 
#' ## Naive interval
#' 
#' First, we estimate the coverage of the naive interval.
#' Define the naive confidence interval function according to
#' [wiki](https://en.wikipedia.org/wiki/Mark_and_recapture#Confidence_interval)[^1]:
#' 
#' [^1]: See [Sadinle (2009)](https://doi.org/10.1080/03610910903168595) for details.

# naive confidence interval
confint.recapture.exact = function(num.1st,num.2nd,num.2nd.tag,level = 0.95){
  k = num.2nd.tag
  K = num.2nd
  n = num.1st
  a2 = (1-level)/2
  k5 = k+0.5
  nk5 = n-k+0.5
  Kk5 = K-k+0.5
  sig = sqrt(1/k5+1/Kk5+1/nk5+k5/(nk5*Kk5))
  int = K+nk5-1+Kk5*nk5/k5*exp(c(1,-1)*qnorm(a2)*sig)
  names(int) = stats:::format.perc(c(a2,1-a2),3)
  return(int)
}

#' Now, there are two ways we can simulate this. We can either assume
#' number of tagged fish roughly follows binomial distribution with
#' `size=502` and `p=501/N` where `N` is our estimate of the population size
#' (or equivalently `p=8/502`, since this is used to estimate `N`), _**OR**_ we can
#' sample two groups of fish of sizes `501` and `502` and count how many overlap.
#' 
#' We implement both below and compare their coverage.

# compute estimate of N
N_est = 501*502/8

# method 1: use rbinom
sim.tag = function(){
  tagged = rbinom(1,size=502,p=501/N_est)
  return(tagged)
}

# method 2: use sample
sim.tag2 = function(){
  june_fish = sample.int(round(N_est),size=501)
  july_fish = sample.int(round(N_est),size=502)
  tagged = length(base::intersect(june_fish,july_fish))
  return(tagged)
}

# for each rep, compute naive interval for N and
# check if it contains our original N_est
check.naive.covered = function(num_tag){
  int = confint.recapture.exact(501,502,num_tag)
  if(N_est >= int[1] && N_est <= int[2]){
    return(TRUE)
  } else return(FALSE)
}

#' Now, we run both methods and compare their coverage.

r = 10000
res = rep(NA,r)
for(i in 1:r){
  res[i] = check.naive.covered(sim.tag())
}
mean(res)

res2 = rep(NA,r)
for(i in 1:r){
  res2[i] = check.naive.covered(sim.tag2())
}
mean(res2)

#' Their coverage seem to be pretty similar. Just for fun, let's directly plot
#' the output of the two simulation methods for comparison.

#+ message=F,warning=F,cache=T
library(tidyverse)
r = 100000
data.frame(method=gl(2,r,labels=c("binom","sample")),
           tagged=c(replicate(r,sim.tag()),replicate(r,sim.tag2()))) %>%
  ggplot(aes(x=tagged)) + facet_wrap(vars(method),ncol=1) + geom_bar()

#' The two methods give nearly identical results, so we don't need to
#' worry about which method we are using.
#' 
#' <br/>
#' 
#' ## MC interval

#' Next, we can estimate the coverage of the MC confidence intervals.
#' Since we are using MC to both construct and test the interval, this is
#' a little bit more complicated (you can see me struggle a little at
#' the end of the recording).
#' 
#' I did eventually find and fix the bug (I was using the same `N_est` for
#' each replicate of the experiment, instead of using a randomly generated
#' estimate of `N`).
#' 
#' Since both sampling methods (`rbinom` used in `sim.tag` and
#' `sample` using in `sim.tag2`) have been shown to be nearly identical,
#' we will use the (probably) more efficient `rbinom` instead.
#' 
#' First we define a function that computes a SINGLE confidence interval
#' using `r` MC replicates and checks if the "true" value of N
#' is in the computed interval.

# define function for checking coverage of a single MC interval
single.MC.covered = function(r,level=.95,num1=501,num2=502,tagged=8){
  
  # convert confidence level to half-area on either side
  # e.g. if level is 0.95, a2 is 0.025
  a2 = (1-level)/2
  
  # get "true" value of N
  N.est = num1*num2/tagged
  
  # define function to simulate number of tagged fish using rbinom,
  # then using that to compute a new simulated estimate of N
  sim.N = function(N) num1*num2/rbinom(1,size=num1,p=num1/N)
  
  # run sim.N to get 1 experiment where we assume the true value of N is
  # N.est, and we go out in July and catch 502 fish, and use our tagged count
  # to produce a single estimate of what we THINK N is
  # (note in this experiment, we don't know the real N is N.est, we just know
  # we THINK N is roughly N.sim)
  N.sim = sim.N(N.est)
  
  # using our N.sim, we RERUN sim.N r times to get a MC confidence interval
  # around out N.sim, using a2 and 1-a2 as the lower and upper quantiles
  ci = quantile(replicate(r,sim.N(N.sim)),c(a2,1-a2))
  
  # return TRUE if CI contains the true value N.est, or FALSE otherwise
  return(N.est>=ci[1] && N.est<=ci[2])
}

#' Hopefully that wasn't too confusing. Let's check the coverage of this
#' MC interval. For fun, let's again do a low rep and high rep and compare them.
#' Note the difference below between `r` and `n`.
#' `r` is the **number of reps used to construct** the MC interval, 
#' whereas `n` is the **number of times we test these intervals**.
#' For low/high reps, we let `r=100` vs `r=1000` to see if using more reps gives
#' better coverage. To estimate this coverage, we use the same `n=1000` for both.

#+ cache=T
# get coverage of low r
mean(
  replicate(n=1000,single.MC.covered(r=100))
)

# get coverage of high r
mean(
  replicate(n=1000,single.MC.covered(r=1000))
)

#' It seems like the high `r` intervals are more reliable. If we are **REALLY**
#' crazy, we can go _**ONE LEVEL DEEPER**_ and repeat the above chunk many times
#' to see how variable the coverage of each interval is. This is getting super meta,
#' since we're using MC to determine the variability of the MC-estimated coverage
#' of an MC-computed interval. This is starting to reach the computational limits of
#' my single machine, so I'm just setting `n=200` in the coverage computation and
#' only use 200 coverage computations to find the variability, but it
#' should be enough to give us an idea of the variability of each coverage.
#' 
#' I'm also going to throw in the naive interval for comparison, so we can see
#' whether or not it's on par with the MC intervals.

#+ cache=T
df.var = data.frame(
  method = gl(3,200,labels=c("low r MC","high r MC",'naive')),
  coverage = c(
    # get low-r coverages
    replicate(
      200,
      mean(
        replicate(n=200,single.MC.covered(r=100))
      )
    ), 
    # get high-r coverages
    replicate(
      200,
      mean(
        replicate(n=200,single.MC.covered(r=1000))
      )
    ),
    replicate(
      200,
      mean(
        replicate(n=200,check.naive.covered(sim.tag()))
      )
    )
  )
)
ggplot(df.var,aes(x=coverage)) + facet_wrap(vars(method),ncol=1) + geom_bar() +
  scale_x_continuous(expand=c(.02,0),
                     breaks=seq(min(df.var$coverage),max(df.var$coverage),by=.01))

#' As you can see, the low-r 95% MC confidence interval sometimes achieves
#' the desired level of 95% coverage, but it often falls short. However,
#' a high-r 95% MC confidence interval much more often achieves the desired
#' 95% coverage.
#' 
#' Surprisingly, the naive interval is able to achieve even better coverage
#' than both the MC methods. We can quickly compute what proportion of the resulting
#' coverages are at least 95%.

#+ cache=T
df.var %>% 
  group_by(method) %>% 
  summarize(">= 0.95" = mean(coverage >= 0.95), .groups="drop")

#' Our results tell us that roughly speaking, for this problem,
#' low-r MC intervals give us about 19% chance of having >= 95% coverage,
#' high-r MC intervals give us about 58% chance of having >= 95% coverage, and
#' naive intervals give us about 91% chance of having >= 95% coverage.
