# Math23C Final Project
# Jenny Gu and Yijiang Zhao
# https://youtu.be/FjRerBQA5HA
# LINK TO YOUTUBE VIDEO <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<!!!!!!!!!!!!!!!!!!

# install.packages("formattable")
# install.packages("ggplot2")
# install.packages("data.table")
# install.packages("dplyer")
# install.packages("lubridate")
# install.packages("RColorBrewer")
# install.packages("maps")
# install.packages("e1071")

# opening libraries
library(formattable)
library(data.table)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(maps)
library(e1071)
# source https://ggplot2.tidyverse.org/reference/
# source http://www.sthda.com/english/wiki/ggplot2-essentials 
# ^^^ for all / most of our ggplot questions
library(ggplot2)

# datasets >>> HC is for the compiled one of cities which is merged with a dataset of populations of
# said cities, HCO is the original dataset of each hatecrime incident reported to the FBI
HC <- read.csv("CrimeData.csv"); head(HC)
HCO <- read.csv("hate_crime.csv"); head(HCO)

# --------------------------------- TABLE OF CONTENTS --------------------------------- 
# Contingency Table of race by race (contingency table, analysis, software engineering (abstraction, functions, etc.), ggplot2 (throughout))
# Barplot of Counts over Time (barplot)
# Graphical Display over Time (line graph--unique display)
# Further Analysis of 9/11 (permutation test)
# Distribution of Hate Crimes for Cities (histogram, overlayed density graph, novel statistic - skewness)
# Map Plots (unique display, chi-squared test, compared chi-squared and permutation)
# Covariance (covariance / correlation)
# Permutation Tests (comparison of classical and simulation methods)
# Linear Regressions (linear regressions)

# --------------------------------- CONTINGENCY TABLE ---------------------------------
# contingency table regarding races and such
# the vector of the categories which quantify as racially-based hate crimes

# software engineering (abstraction, cuz we use it later as well)
toMatch <- c("Anti-American Indian or Alaska Native", "Anti-Arab", "Anti-Asian", "Anti-Black or African American", "Anti-Hispanic or Latino", 
             "Anti-Multiple Races, Group", "Anti-Native Hawaiian or Other Pacific Islander", "Anti-Other Race/Ethnicity/Ancestry", "Anti-White")

# combined string into correct formatting for grepl
filter <- paste("^",paste(toMatch,collapse="$|^"),"$",sep=""); filter
# filter HCO to find the indexes and a vector of incidents which were driven by race
ind <- which((grepl(filter, HCO$BIAS_DESC))==TRUE); ind
offender <- HCO$OFFENDER_RACE[ind]; offender
victim <- HCO$BIAS_DESC[ind]; victim
# finds all categories for BIAS_DESC so that we can filter out the non-race ones
categories <- unique(HCO$BIAS_DESC); categories
tot <- unlist(categories, recursive = TRUE, use.names = TRUE); tot
nonrace <-   setdiff(tot, toMatch); nonrace
# creates a contingency table excluding the nonrace columns (we did not do ones which were both
# race and another variable as we do not which one drove the thing more)
tbl <- table(offender, victim, exclude = nonrace); tbl
colnames <- toMatch; colnames
rownames <- c("American Indian or Alaska Native", "Asian", "Black or African American", "Multiple", "Native Hawaiian or Other Pacific Islander", "Unknown", "White"); rownames
length(rownames)

freqs <- data.frame(Native=c(79,15,192,31,1,620,714), row.names=rownames); freqs
# software engineering
convert <- function (dfvar, tblvar) {
  for (col in 2:length(colnames)) {
    temp <- c()
    for (row in 2:(length(rownames)+1)) {
      temp <- c(temp, tblvar[row, col])
    }
    dfvar <- cbind(dfvar, temp)
  }
  colnames(dfvar) <- colnames
  conttable <- data.table(dfvar, keep.rownames=T)
  return(conttable)
}
formattable(convert (freqs, tbl))

Expected <- outer(rowSums(tbl), colSums(tbl))/sum(tbl); Expected
col1 <- c(Expected[2,1])
for (i in 3 : (length(rownames) + 1)) {
  col1 <- c(col1, Expected[i,1])
}
col1
exps <- data.frame(Native=col1, row.names=rownames); exps
formattable(convert(exps, Expected))
chisq.test(offender, victim) 

# --------------------------------- BARPLOT OVER TIME --------------------------------- 
# histogram of total incidents of hate crime over time by year
# total number of crimes per year
year <- vector()
totCrimes <- vector()
for (i in 2000:2017) {
  year <- c(year, i)
  totCrimes <- c(totCrimes, sum(HC[which(HC$year == i),]$tot))
}

ggplot(data=data.frame(year, totCrimes), aes(x=year,y=totCrimes)) + geom_bar(stat="identity") + 
  labs(title="Total Incidents of Hate Crime over Time", x="Year", y = "Total Hate Crimes Count") +
  theme(plot.title = element_text(hjust = 0.5))
# noticed in the graph that 2001 had higher levels of hate crime than other years

# --------------------------------- GRAPHICAL DISPLAY OVER TIME ---------------------------------
# graphical display of total incidents of hate crime across time
dates <- table(HCO$INCIDENT_DATE); dates
dt <- setNames(data.frame(dates),c("Date","Count")); dt

# creating vector of type datetime
d <- as.Date(dt$Date[1], "%m/%d/%Y"); d
newd <- c(d); newd
# converting all date strings into date time
for (i in 2:length(dt$Date)) {
  d <- as.Date(dt$Date[i], "%m/%d/%Y")
  newd <- c(newd, d)
}; newd

data <- data.frame(date=newd, count=dt$Count); data
# source: https://ro-che.info/articles/2017-02-22-group_by_month_r
data <- data %>% group_by(month=floor_date(date, "month")) %>%
  summarize(count=sum(count)); data

# month of 9/11 and Trump's inauguration
terr <- as.Date("9/1/2001", "%m/%d/%Y"); terr
trump <- as.Date("11/1/2016", "%m/%d/%Y"); trump

# Plotting it over time
range <- c(data$month[1], data$month[length(data$month)]); range
ggplot(data=data, aes(x=data$month, y=data$count, group=1)) + geom_line() + 
  scale_x_date(date_breaks = "years" , date_labels = "%Y") + labs(title="National Incidents of Hate Crime over Time", x="Date (year)", y = "National Hate Crimes Count") +
  theme(plot.title = element_text(hjust = 0.5)) + geom_vline(aes(xintercept=terr), color="blue", linetype="dashed", size=0.5) + geom_vline(aes(xintercept=trump), color="red", linetype="dashed", size=0.5)

# --------------------------------- Further Analysis 9/11 ---------------------------------
# permutation test for the year 2001 to see if it had significantly higher levels of hatecrime
# observed difference between hate crimes in 2001 to average number of hate crimes in other years
obsDiff2001 <- totCrimes[2] - (totCrimes[1] + sum(totCrimes[3:18])) / 17; obsDiff2001

N <- 10000
diffs <- numeric(N)
for (i in 1:N) {
  yr <- sample(year)
  diffs[i] <-sum(HC[which(yr == 2001),]$tot) - sum(HC[which(yr != 2001),]$tot / 17)
}
# mean of differences is now about 0 
mean2001 <- mean(diffs); mean2001

# pvalue to see if the observed difference is statistically significant
# pvalue is about 10^(-4). Therefore, there was a statistically significant increase in hate crimes for the year 2001
pvalue2001 <- (sum(abs(diffs) >= abs(obsDiff2001))+1)/(N+1); pvalue2001


# graphs of each type of hate crime over time

# spike in anti-Islam hate crimes in 2001
IsCrimes <- vector()
for (i in 2000:2017) {
  IsCrimes <- c(IsCrimes, sum(HC[which(HC$year == i),]$is))
}
antiislam2001 <- ggplot(data=data.frame(year, IsCrimes), aes(x=year,y=IsCrimes)) + geom_bar(stat="identity",fill="#244B70") + labs(title = "Anti-Islam Hate Crimes Per Year", x="Year", y="Total Number") +
  theme(plot.title = element_text(hjust = 0.5)); antiislam2001

# anti-Black or African American hate crimes
AfCrimes <- vector()
for (i in 2000:2017) {
  AfCrimes <- c(AfCrimes, sum(HC[which(HC$year == i),]$afr))
}
antiblack2001 <- ggplot(data=data.frame(year, AfCrimes), aes(x=year,y=AfCrimes)) + geom_bar(stat="identity",fill="#244B70") + labs(title = "Anti-Black or African American Hate Crimes Per Year", x="Year", y="Total Number")+
  theme(plot.title = element_text(hjust = 0.5)); antiblack2001

# anti-White hate crimes
WhCrimes <- vector()
for (i in 2000:2017) {
  WhCrimes <- c(WhCrimes, sum(HC[which(HC$year == i),]$whi))
}
antiwhite2001 <- ggplot(data=data.frame(year, WhCrimes), aes(x=year,y=WhCrimes)) + geom_bar(stat="identity",fill="#244B70") + labs(title = "Anti-White Hate Crimes Per Year", x="Year", y="Total Number")+
  theme(plot.title = element_text(hjust = 0.5)); antiwhite2001


# anti-Arab hate crimes
# spikes up in 2001 and has been increasing since 2014
ArCrimes <- vector()
for (i in 2000:2017) {
  ArCrimes <- c(ArCrimes, sum(HC[which(HC$year == i),]$ar))
}
antiarab2001 <- ggplot(data=data.frame(year, ArCrimes), aes(x=year,y=ArCrimes)) + geom_bar(stat="identity",fill="#244B70") + labs(title = "Anti-Arab Hate Crimes Per Year", x="Year", y="Total Number") + theme(plot.title = element_text(hjust = 0.5)); antiarab2001


# --------------------------------- DISTRIBUTION OF HATE CRIMES FOR CITIES ---------------------------------
# selected on cities with a population from 250,000 through 499,999
# not normal distribution
# count of hate crimes for all cities in 2017
cities <- (HC[which(HC$desc == "Cities from 250,000 thru 499,999"),]); cities
citydf <- data.frame(city=cities$agency, count=cities$tot); citydf


# number of hate crimes does not fit normal distribution
ggplot(citydf, aes(x=citydf$count)) + labs(title="Frequency of Hate Crimes in Cities 250,000 - 499,999", x="Number of Hate Crimes", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_histogram(aes(y = ..density..), binwidth=1) + 
  stat_function(
    fun = dnorm, 
    args = with(citydf, c(mean = mean(citydf$count), sd = sd(citydf$count))),
    color = "red"
  )

hist(HC[which(HC$desc == "Cities from 250,000 thru 499,999"),]$tot, breaks=130, freq=FALSE, xlab = "Number of Hate Crimes", main = "Frequency of Hate Crimes in Cities 250,000-499,999")
# source https://www.rdocumentation.org/packages/Rlab/versions/2.15.1/topics/Gamma
# source https://www.rdocumentation.org/packages/stats/versions/3.6.0/topics/Exponential
# overlay exponential, chi-square, and gamma distributions
curve(dexp (x, rate=.15), add = TRUE, col = "blue", lwd = 2)
curve(dchisq(x, df=5), add=TRUE, col="green")
curve(dgamma(x, shape=5.6), add=TRUE, col="red")

# number of histograms clearly does not fit a normal distribution
ggplot(citydf, aes(x = citydf$count)) + 
  geom_histogram(aes(y = ..density..), binwidth=1) + 
  stat_function(
    fun = dgamma, 
    args = with(citydf, c(shape=16))
  ) 

# sampling gives normal distribution
sample(HC$tot, 10)
N <- 10000; avgHC <- numeric(N)
for (i in 1:N){
  avgHC[i] <- mean(sample(HC[which(HC$desc == "Cities from 250,000 thru 499,999"),]$tot, 10))
}

# red is normal, blue is gamma
# overlaying gamma and normal curves on top of histogram of sampling distribution
# the sampling means is skewed to the right
avgHCdf <- data.frame(avgHC); avgHCdf
ggplot(avgHCdf, aes(x = avgHCdf$avgHC)) + 
  geom_histogram(aes(y = ..density..), binwidth=0.5) + 
  stat_function(
    fun = dnorm, 
    args = with(avgHCdf, c(mean = mean(avgHCdf$avgHC), sd = sd(avgHCdf$avgHC))),
    color = "red"
  ) + 
  stat_function(
    fun = dgamma, 
    args = with(avgHCdf, c(shape=10, rate = 0.83)),
    color = "blue"
  ) + labs(title="Sampling Hate Crimes Frequencies for Cities 250,000-499,999", x="Number of Hate Crimes", y = "Density") + theme(plot.title = element_text(hjust = 0.5))
# source http://www.r-tutor.com/elementary-statistics/numerical-measures/skewness
# source https://www.rdocumentation.org/packages/e1071/versions/1.7-1/topics/skewness
# skewness is about 0.6 - skewed to the right
skewness(avgHC, type=1)

# --------------------------------- MAP PLOTS ---------------------------------
# map plot
# population data of cities in 2017
P2017 <- read.csv("pop2017.csv", stringsAsFactors = FALSE); head(P2017)
# only looking at hate crimes in 2017
x <- data.frame(HC[which(HC$year == 2017),])
y <- data.frame(P2017)
# source https://www.quora.com/How-do-you-merge-two-data-frames-based-on-multiple-columns-in-R
# source https://www.statmethods.net/management/merging.html

# merge the population data with the city hate crime data
# leave only the results where the merge was successful
merged <- merge(x, y, by.x=c("states","agency"), by.y = c("STNAME", "NAME"), all = FALSE, no.dups = FALSE)

statelist <- vector()
staterate <- vector()

# for each state, calculate the number of hate crimes per person
for (i in unique(merged[which(merged$year == 2017),]$states)) {
  statelist <- c(statelist, i)
  # total hate crimes in state
  tot <- 0
  # population of state 
  p <- 0
  for (j in unique(merged[which(merged$states == i),]$agency)) {
    if (is.na(merged[which(merged$states == i & merged$agency == j)[1],]$POPESTIMATE2017)) {
      tot <- tot
    }
    else {
      p <- p + merged[which(merged$states == i & merged$agency == j)[1],]$POPESTIMATE2017
      tot <- tot + sum(merged[which(merged$states == i & merged$agency == j),]$tot)
    }
  }
  staterate <- c(staterate, tot/p*100)
}
# source https://ggplot2.tidyverse.org/reference/geom_map.html
hatecrimes <- data.frame(HC =  staterate, state = tolower(statelist))
# plot rate of hate crimes on U.S. map
State_Rate <- staterate
if (require(maps)) {
  states_map <- map_data("state")
  ggplot (hatecrimes, aes(map_id = state)) + 
    geom_map (aes(fill = State_Rate), map = states_map) + 
    expand_limits(x = states_map$long, y= states_map$lat) + 
    labs(title="Rates of Hate Crimes per State (per 100)", x="", y= "") + theme(plot.title = element_text(hjust = 0.5))
}


# Anti-American Indian hate crime in Kentucky
statelist <- vector()
staterate <- vector()

# for each state, calculate the number of hate crimes per person
for (i in unique(merged[which(merged$year == 2017),]$states)) {
  statelist <- c(statelist, i)
  # total hate crimes in state
  tot <- 0
  # population of state 
  p <- 0
  for (j in unique(merged[which(merged$states == i),]$agency)) {
    if (is.na(merged[which(merged$states == i & merged$agency == j)[1],]$POPESTIMATE2017)) {
      tot <- tot
    }
    else {
      p <- p + merged[which(merged$states == i & merged$agency == j)[1],]$POPESTIMATE2017
      tot <- tot + sum(merged[which(merged$states == i & merged$agency == j),]$ai)
    }
  }
  staterate <- c(staterate, tot/p*100)
}
# source https://ggplot2.tidyverse.org/reference/geom_map.html
hatecrimes <- data.frame(HC =  staterate, state = tolower(statelist))
# plot rate of hate crimes on U.S. map
if (require(maps)) {
  states_map <- map_data("state")
  ggplot (hatecrimes, aes(map_id = state)) + 
    geom_map (aes(fill = staterate), map = states_map) + 
    expand_limits(x = states_map$long, y= states_map$lat) + 
    labs(title="Rate of Anti-American Indian and Alaska Native Hate Crimes", x="", y= "") + theme(plot.title = element_text(hjust = 0.5))
}

# --------------------------------- COVARIANCE ---------------------------------
# covariance / correlation
ccities <- HC[which(HC$desc == "Cities from 250,000 thru 499,999"),]
afrhc <- ccities$afr
whitehc <- ccities$whi
# covariance
cov(afrhc, whitehc)
# positive correlation (0.616) between the number of anti-White hate crimes and the number of anti-Black or African American hate crimes
cor(afrhc, whitehc)

A <- cbind(ccities$afr, 
           ccities$whi, 
           ccities$asi, 
           ccities$hl, 
           ccities$ar, 
           ccities$ai)

# covariance and correlation matrices of the various factors
# the strongest correlation exists between the number of anti-White hate crimes and the number of anti-Black or African American hate crimes
S <- var(A); S
S1 <- cor(A); S1

mean(ccities$afr)

# --------------------------------- PERMUTATION TESTS --------------------------------- 
# non-MSA vs MSA counties
# choose a subset of the data that only includes MSA or Non-MSA counties of a certain size 
# source https://en.wikipedia.org/wiki/Metropolitan_statistical_area
# MSA = Metropolitan Statistical Area - high population densities
subgroup <- HC[which((HC$desc == "MSA counties from 25,000 thru 99,999" | HC$desc == "Non-MSA counties from 25,000 thru 99,999") & HC$year == 2017),]; head(subgroup)
# mean total hate crimes for MSA counties
msa <- mean(subgroup[which(subgroup$desc == "MSA counties from 25,000 thru 99,999"),]$tot)
# mean total hate crimes for non-MSA counties
nmsa <- mean(subgroup[which(subgroup$desc == "Non-MSA counties from 25,000 thru 99,999"),]$tot)
# observed difference
obs <- msa - nmsa; obs

# test that the proportion of non-MSA and MSA counties in the different regions are similar
# the geographical distribution of non-MSA and MSA counties are comparable
length(which(subgroup[which(subgroup$desc == "MSA counties from 25,000 thru 99,999"),]$region == "South"))/length(subgroup[which(subgroup$desc == "MSA counties from 25,000 thru 99,999"),]$region)
length(which(subgroup[which(subgroup$desc == "Non-MSA counties from 25,000 thru 99,999"),]$region == "South"))/length(subgroup[which(subgroup$desc == "Non-MSA counties from 25,000 thru 99,999"),]$region)
length(which(subgroup[which(subgroup$desc == "MSA counties from 25,000 thru 99,999"),]$region == "Northeast"))/length(subgroup[which(subgroup$desc == "MSA counties from 25,000 thru 99,999"),]$region)
length(which(subgroup[which(subgroup$desc == "Non-MSA counties from 25,000 thru 99,999"),]$region == "Northeast"))/length(subgroup[which(subgroup$desc == "Non-MSA counties from 25,000 thru 99,999"),]$region)
length(which(subgroup[which(subgroup$desc == "MSA counties from 25,000 thru 99,999"),]$region == "Midwest"))/length(subgroup[which(subgroup$desc == "MSA counties from 25,000 thru 99,999"),]$region)
length(which(subgroup[which(subgroup$desc == "Non-MSA counties from 25,000 thru 99,999"),]$region == "Midwest"))/length(subgroup[which(subgroup$desc == "Non-MSA counties from 25,000 thru 99,999"),]$region)
length(which(subgroup[which(subgroup$desc == "MSA counties from 25,000 thru 99,999"),]$region == "West"))/length(subgroup[which(subgroup$desc == "MSA counties from 25,000 thru 99,999"),]$region)
length(which(subgroup[which(subgroup$desc == "Non-MSA counties from 25,000 thru 99,999"),]$region == "West"))/length(subgroup[which(subgroup$desc == "Non-MSA counties from 25,000 thru 99,999"),]$region)

# permuatation test 
diff <- numeric(N)
for (i in 1:N) {
  p <- sample(subgroup$desc)
  diff[i] <- mean(subgroup[which(p == "MSA counties from 25,000 thru 99,999"),]$tot) - mean(subgroup[which(p == "Non-MSA counties from 25,000 thru 99,999"),]$tot)
}
# difference is about 0
mean(diff)
# p value is about .04 - statistically significant difference between MSA and non-MSA counties
pvalue <- (sum((diff) >= abs(obs))+1)/(N+1); pvalue

# chi square
total <- sum(subgroup$tot); total
observed <- c(sum(subgroup[(which(subgroup$desc == "MSA counties from 25,000 thru 99,999")),]$tot), sum(subgroup[(which(subgroup$desc=="Non-MSA counties from 25,000 thru 99,999")),]$tot))
# expected values (if non-MSA and MSA counties have same hate crime rates) for the counties are in proportion to the fractino of non-MSA and MSA counties
expected <- c(total*length(which(subgroup$desc == "MSA counties from 25,000 thru 99,999"))/length(subgroup$tot), total*length(which(subgroup$desc == "Non-MSA counties from 25,000 thru 99,999"))/length(subgroup$tot))

# The chi-squared method gave us a lower p-value than the simulation method
# chi square software engineering
ChiSq <-function(Obs,Exp){
  sum((Obs-Exp)^2/Exp)
}
observed; expected
msachi <- ChiSq (observed, expected); msachi
# pvalue is less than 0.05 -> statistically significant
pchisq(msachi, df = 1, lower.tail = FALSE)
# chi square distribution
# curve(dchisq(x, df =1), xlim = c(0,8))
# abline(v=chi, col="red")

# chisq function
# graphing the chi-squared
ggplot(data.frame(x), aes(x=x)) + labs(title="Chi-Squared Distribution (df = 1)", x="chi-squared statistic", y = "p-value") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  stat_function(
    fun = c5
  ) + geom_vline(aes(xintercept=msachi), color="red", linetype="dashed", size=0.5)

# South vs other regions
subgroup <- HC[which(HC$year == 2017 & HC$desc == "Cities from 250,000 thru 499,999"),]
# the South actually had lower mean hate crime rates for cities from 250,000 thru 499,999
obsd <- mean(subgroup[which(subgroup$region == "South"),]$tot) - mean(subgroup[which(subgroup$region != "South"),]$tot); obsd
d <- numeric(N)
for (i in 1:N) {
  p <- sample(subgroup$region)
  d[i] <- mean(subgroup[which(p == "South"),]$tot) - mean(subgroup[which(p != "South"),]$tot)
}
mean(diff)
# pvalue is greater than 0.05
# no statistically significant difference between the South and overall U.S. in cities from 250,000 thru 499,999
pvalue <- (sum(abs(d) >= abs(obsd))+1)/(N+1); pvalue


# South vs. Other for smaller city size
subgroup <- HC[which(HC$year == 2017 & HC$desc == "Cities from 10,000 thru 24,999"),]; subgroup
obsd <- mean(subgroup[which(subgroup$region == "South"),]$tot) - mean(subgroup[which(subgroup$region != "South"),]$tot); obsd
d <- numeric(N)
for (i in 1:N) {
  p <- sample(subgroup$region)
  d[i] <- mean(subgroup[which(p == "South"),]$tot) - mean(subgroup[which(p != "South"),]$tot)
}
mean(diff)
# pvalue (about 0.1) is still greater than 0.05, but is much lower than for the larger city
pvalue <- (sum((d) >= (obsd))+1)/(N+1); pvalue

# analyze hate crimes by specific region
# Compare the South and Northeast
subgroup <- HC[which((HC$region == "South" | HC$region == "Northeast") & HC$year == 2017 & HC$desc == "Cities from 250,000 thru 499,999"),]; head(subgroup)
# observed difference
obsd <- mean(subgroup[which(subgroup$region == "South"),]$tot) - mean(subgroup[which(subgroup$region == "Northeast"),]$tot); obsd
d <- numeric(N)
# permutation test
for (i in 1:N) {
  p <- sample(subgroup$region)
  d[i] <- mean(subgroup[which(p == "South"),]$tot) - mean(subgroup[which(p == "Northeast"),]$tot)
}
mean(d)
# pvalue is 0.5, not statistically significant
pvalue <- (sum(d >= abs(obsd))+1)/(N+1); pvalue

# Midwest and Other Regions
subgroup <- HC[which(HC$year == 2017 & HC$desc == "Cities from 250,000 thru 499,999"),]
# observed difference
obsd <- mean(subgroup[which(subgroup$region == "Midwest"),]$tot) - mean(subgroup[which(subgroup$region != "Midwest"),]$tot); obsd
d <- numeric(N)
for (i in 1:N) {
  p <- sample(subgroup$region)
  d[i] <- mean(subgroup[which(p == "Midwest"),]$tot) - mean(subgroup[which(p != "Midwest"),]$tot)
}
mean(d)
# pvalue is about 0.008, statistically significant difference between the number of hate crimes in the midwest and elsewhere in cities
# of population 250,000 through 499,999
pvalue <- (sum(d >= abs(obsd))+1)/(N+1); pvalue

# midwest vs. other regions chi square test
total <- sum(subgroup$tot); total
observed <- c(sum(subgroup[which(subgroup$region == "Midwest"),]$tot)/length(which(subgroup$region=="Midwest")), sum(subgroup[which(subgroup$region != "Midwest"),]$tot)/length(which(subgroup$region != "Midwest")))
# expected values (if hate crimes evenly distributed) are in proportion to the fraction of cities in the Midwest and the fraction of cities in other regions
expected <- c(total/length(subgroup$region), total/length(subgroup$region))
observed; expected
chi <- ChiSq(observed, expected); chi
# p value is about 0.01 - statistically significant
pchisq(chi, df=1, lower.tail=FALSE)
# chi square distribution for one degree of freedom
# x axis - chi square value, y axis - pvalue
# curve(dchisq(x, df =1), xlim = c(0,8))
# abline(v=chi, col="red")

# range
x <- c(0,8)
# chisquared function with one degree of freedom
c5 <- function (x) {dchisq(x, df = 1)}
# graphing the chi-squared
ggplot(data.frame(x), aes(x=x)) + labs(title="Chi-Squared Distribution (df = 1)", x="chi-squared statistic", y = "p-value") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  stat_function(
    fun = c5
  ) + geom_vline(aes(xintercept=chi), color="red", linetype="dashed", size=0.5)


# in the map of anti-American Indian hate crimes by states, Kentucky seemed to have much higher rates than the other states
# Kentucky vs other states
subgroup <- HC[which(HC$year == 2017 & HC$desc == "Cities from 250,000 thru 499,999"),]; head(subgroup)
# observed difference
obsd <- mean(subgroup[which(subgroup$state == "Kentucky"),]$tot) - mean(subgroup[which(subgroup$state != "Kentucky"),]$ai); obsd
d <- numeric(N)
# permutation test
for (i in 1:N) {
  p <- sample(subgroup$state)
  d[i] <- mean(subgroup[which(p == "Kentucky"),]$ai) - mean(subgroup[which(p != "Kentucky"),]$ai)
}
mean(d)
# pvalue is about 10 ^ (-4). Kentucky has statistically more hate crimes against American Indians than other places
pvalue <- (sum(d >= abs(obsd))+1)/(N+1); pvalue


# --------------------------------- LINEAR REGRESSIONS --------------------------------- 
year <- vector()
# anti-African American or Black hate crimes by year
AfCrimes <- vector()
for (i in 2000:2017) {
  year <- c(year, i)
  AfCrimes <- c(AfCrimes, sum(HC[which(HC$year == i),]$afr))
}
# linear regression   
af.lm <- lm(AfCrimes~year); af.lm
# intercept
af.a <- coefficients(af.lm)[1]; af.a
# slope
af.b <- coefficients(af.lm)[2]; af.b
# y = -70.96 x + 144878.88
ggplot(data=data.frame(year, AfCrimes), aes(x=year,y=AfCrimes)) + geom_point(stat="identity") + 
  geom_abline(aes(slope = af.b, intercept = af.a)) + labs(title = "Anti-Black or African American Hate Crimes Over Time", x = "Year", y = "Hate Crimes")

# anti-White hate crimes by year
WhCrimes <- vector()
for (i in 2000:2017) {
  WhCrimes <- c(WhCrimes, sum(HC[which(HC$year == i),]$whi))
}
# linear regression
wh.lm <- lm(WhCrimes~year); wh.lm
# intercept
wh.a <- coefficients(wh.lm)[1]; wh.a
# slope
wh.b <- coefficients(wh.lm)[2]; wh.b
# y = -12.9 x + 26633.3
ggplot(data=data.frame(year, WhCrimes), aes(x=year,y=WhCrimes)) + 
  geom_point(stat="identity") + geom_abline(aes(slope = wh.b, intercept = wh.a)) + labs(title = "Anti-White Hate Crimes Over Time", x = "Year", y = "Hate Crimes")

# anti-White hate crimes are decreasing at a slower overall rate than anti-Black hate crimes, but there were much more anti-Black hate crimes to begin with
# Assuming that hate crime rates continue the existing trend, calculate where anti-White and anti-Black hate crimes are predicted to equal in the future
# anti-White hate crime equation = anti-Black hate crime equation
# 26633.3 - 12.9 x = 144878.88 - 70.96 x 
# 118245.58 = 58.06 x 
# x = 2036 
# predicted to be equal in the year 2036
# however, hate crimes have been rising in the past few years, so unclear whether this overall trend will continue
