# 2020 US. Coronavirus daily updated data: R script


## Daily updated data 

Avaible on [www.tdeguilhem.com](https://www.tdeguilhem.com/uploads/4/8/6/5/48652267/us_states_covid19_daily.csv)


## Needed packages

```{r}

install.packages("hrbrthemes")
install.packages("plotly")

library(tidyverse)
library(ggplot2)
library(tibble)
library(tidyr)
library(dplyr)
library(lubridate)
library(hrbrthemes)
library(plotly)

require(ggplot2)  
require(gridExtra) 
```


## Data cleaning and subsets (States)

```{r}



COVID19REGIONUS=read.csv("https://www.tdeguilhem.com/uploads/4/8/6/5/48652267/us_states_covid19_daily.csv",
                 header=TRUE,sep=",")

print(COVID19REGIONUS[1:10,])



COVID19REGIONUS$positive[is.na(COVID19REGIONUS$positive)] = 0

COVID19REGIONUS$negative[is.na(COVID19REGIONUS$negative)] = 0

COVID19REGIONUS$pending[is.na(COVID19REGIONUS$pending)] = 0

COVID19REGIONUS$hospitalized[is.na(COVID19REGIONUS$hospitalized)] = 0

COVID19REGIONUS$death[is.na(COVID19REGIONUS$death)] = 0

COVID19REGIONUS$total[is.na(COVID19REGIONUS$total)] = 0


COVID19REGIONUS <- COVID19REGIONUS[order(COVID19REGIONUS$date),]

COVID19REGIONUS$date<-as.Date(as.character(COVID19REGIONUS$date),format="%Y%m%d")

COVID19REGIONUS$Rate_d = (COVID19REGIONUS$death/COVID19REGIONUS$positive) * 100

COVID19REGIONUS$Rate_hosp = (COVID19REGIONUS$hospitalized/COVID19REGIONUS$positive) * 100


COVID19REGIONUS$Rate_death_hosp = (COVID19REGIONUS$death/COVID19REGIONUS$hospitalized) * 100

COVID19REGIONUS$Rate_death_hosp[is.infinite(COVID19REGIONUS$Rate_death_hosp) | is.nan(COVID19REGIONUS$Rate_death_hosp) ] <- NA

COVID19REGIONUS$Rate_contaminated = (COVID19REGIONUS$positive/(COVID19REGIONUS$negative+COVID19REGIONUS$positive)) * 100

COVID19REGIONUS$LOG_confirmed = log10(COVID19REGIONUS$positive)

COVID19REGIONUS$LOG_death = log10(COVID19REGIONUS$death)







COVID19REGIONUS$confirmed_pop_CA <- ifelse(COVID19REGIONUS$state == "CA", (COVID19REGIONUS$positive/39512000)*100000, 0)
COVID19REGIONUS$confirmed_pop_NY <- ifelse(COVID19REGIONUS$state == "NY", (COVID19REGIONUS$positive/19453000)*100000, 0)
COVID19REGIONUS$confirmed_pop_TX <- ifelse(COVID19REGIONUS$state == "TX", (COVID19REGIONUS$positive/28995000)*100000, 0)
COVID19REGIONUS$confirmed_pop_FL <- ifelse(COVID19REGIONUS$state == "FL", (COVID19REGIONUS$positive/21477000)*100000, 0)
COVID19REGIONUS$confirmed_pop_IL <- ifelse(COVID19REGIONUS$state == "IL", (COVID19REGIONUS$positive/12671000)*100000, 0)
COVID19REGIONUS$confirmed_pop_OR <- ifelse(COVID19REGIONUS$state == "OR", (COVID19REGIONUS$positive/4217000)*100000, 0)
COVID19REGIONUS$confirmed_pop_OK <- ifelse(COVID19REGIONUS$state == "OK", (COVID19REGIONUS$positive/3957000)*100000, 0)
COVID19REGIONUS$confirmed_pop_CO <- ifelse(COVID19REGIONUS$state == "CO", (COVID19REGIONUS$positive/5759000)*100000, 0)
COVID19REGIONUS$confirmed_pop_WA <- ifelse(COVID19REGIONUS$state == "WA", (COVID19REGIONUS$positive/7615000)*100000, 0)
COVID19REGIONUS$confirmed_pop_PA <- ifelse(COVID19REGIONUS$state == "PA", (COVID19REGIONUS$positive/12802000)*100000, 0)
COVID19REGIONUS$confirmed_pop_MN <- ifelse(COVID19REGIONUS$state == "MN", (COVID19REGIONUS$positive/6950000)*100000, 0)
COVID19REGIONUS$confirmed_pop_GA <- ifelse(COVID19REGIONUS$state == "GA", (COVID19REGIONUS$positive/10617000)*100000, 0)

COVID19REGIONUS$confirmed_pop = COVID19REGIONUS$confirmed_pop_GA + COVID19REGIONUS$confirmed_pop_MN 
+ COVID19REGIONUS$confirmed_pop_PA + COVID19REGIONUS$confirmed_pop_WA
+ COVID19REGIONUS$confirmed_pop_CO + COVID19REGIONUS$confirmed_pop_OK
+ COVID19REGIONUS$confirmed_pop_OR + COVID19REGIONUS$confirmed_pop_IL
+ COVID19REGIONUS$confirmed_pop_FL + COVID19REGIONUS$confirmed_pop_TX
+ COVID19REGIONUS$confirmed_pop_NY + COVID19REGIONUS$confirmed_pop_CA

COVID19REGIONUS<-select(COVID19REGIONUS, -confirmed_pop_GA, -confirmed_pop_MN, -confirmed_pop_PA, -confirmed_pop_WA, -confirmed_pop_CO , -confirmed_pop_OK, -confirmed_pop_OR , -confirmed_pop_IL, -confirmed_pop_FL , -confirmed_pop_TX, -confirmed_pop_NY , -confirmed_pop_CA)







COVID19REGIONUS$confirmed_pop_CA <- ifelse(COVID19REGIONUS$state == "CA", (COVID19REGIONUS$death/39512000)*100000, 0)
COVID19REGIONUS$confirmed_pop_NY <- ifelse(COVID19REGIONUS$state == "NY", (COVID19REGIONUS$death/19453000)*100000, 0)
COVID19REGIONUS$confirmed_pop_TX <- ifelse(COVID19REGIONUS$state == "TX", (COVID19REGIONUS$death/28995000)*100000, 0)
COVID19REGIONUS$confirmed_pop_FL <- ifelse(COVID19REGIONUS$state == "FL", (COVID19REGIONUS$death/21477000)*100000, 0)
COVID19REGIONUS$confirmed_pop_IL <- ifelse(COVID19REGIONUS$state == "IL", (COVID19REGIONUS$death/12671000)*100000, 0)
COVID19REGIONUS$confirmed_pop_OR <- ifelse(COVID19REGIONUS$state == "OR", (COVID19REGIONUS$death/4217000)*100000, 0)
COVID19REGIONUS$confirmed_pop_OK <- ifelse(COVID19REGIONUS$state == "OK", (COVID19REGIONUS$death/3957000)*100000, 0)
COVID19REGIONUS$confirmed_pop_CO <- ifelse(COVID19REGIONUS$state == "CO", (COVID19REGIONUS$death/5759000)*100000, 0)
COVID19REGIONUS$confirmed_pop_WA <- ifelse(COVID19REGIONUS$state == "WA", (COVID19REGIONUS$death/7615000)*100000, 0)
COVID19REGIONUS$confirmed_pop_PA <- ifelse(COVID19REGIONUS$state == "PA", (COVID19REGIONUS$death/12802000)*100000, 0)
COVID19REGIONUS$confirmed_pop_MN <- ifelse(COVID19REGIONUS$state == "MN", (COVID19REGIONUS$death/6950000)*100000, 0)
COVID19REGIONUS$confirmed_pop_GA <- ifelse(COVID19REGIONUS$state == "GA", (COVID19REGIONUS$death/10617000)*100000, 0)

COVID19REGIONUS$death_pop = COVID19REGIONUS$confirmed_pop_GA + COVID19REGIONUS$confirmed_pop_MN 
+ COVID19REGIONUS$confirmed_pop_PA + COVID19REGIONUS$confirmed_pop_WA
+ COVID19REGIONUS$confirmed_pop_CO + COVID19REGIONUS$confirmed_pop_OK
+ COVID19REGIONUS$confirmed_pop_OR + COVID19REGIONUS$confirmed_pop_IL
+ COVID19REGIONUS$confirmed_pop_FL + COVID19REGIONUS$confirmed_pop_TX
+ COVID19REGIONUS$confirmed_pop_NY + COVID19REGIONUS$confirmed_pop_CA


COVID19REGIONUS<-select(COVID19REGIONUS, -confirmed_pop_GA, -confirmed_pop_MN, 
                  -confirmed_pop_PA, -confirmed_pop_WA
                  , -confirmed_pop_CO , -confirmed_pop_OK
                  , -confirmed_pop_OR , -confirmed_pop_IL
                  , -confirmed_pop_FL , -confirmed_pop_TX
                  , -confirmed_pop_NY , -confirmed_pop_CA)





COVID19REGIONUS$confirmed_pop_CA <- ifelse(COVID19REGIONUS$state == "CA", (COVID19REGIONUS$hospitalized/39512000)*100000, 0)
COVID19REGIONUS$confirmed_pop_NY <- ifelse(COVID19REGIONUS$state == "NY", (COVID19REGIONUS$hospitalized/19453000)*100000, 0)
COVID19REGIONUS$confirmed_pop_TX <- ifelse(COVID19REGIONUS$state == "TX", (COVID19REGIONUS$hospitalized/28995000)*100000, 0)
COVID19REGIONUS$confirmed_pop_FL <- ifelse(COVID19REGIONUS$state == "FL", (COVID19REGIONUS$hospitalized/21477000)*100000, 0)
COVID19REGIONUS$confirmed_pop_IL <- ifelse(COVID19REGIONUS$state == "IL", (COVID19REGIONUS$hospitalized/12671000)*100000, 0)
COVID19REGIONUS$confirmed_pop_OR <- ifelse(COVID19REGIONUS$state == "OR", (COVID19REGIONUS$hospitalized/4217000)*100000, 0)
COVID19REGIONUS$confirmed_pop_OK <- ifelse(COVID19REGIONUS$state == "OK", (COVID19REGIONUS$hospitalized/3957000)*100000, 0)
COVID19REGIONUS$confirmed_pop_CO <- ifelse(COVID19REGIONUS$state == "CO", (COVID19REGIONUS$hospitalized/5759000)*100000, 0)
COVID19REGIONUS$confirmed_pop_WA <- ifelse(COVID19REGIONUS$state == "WA", (COVID19REGIONUS$hospitalized/7615000)*100000, 0)
COVID19REGIONUS$confirmed_pop_PA <- ifelse(COVID19REGIONUS$state == "PA", (COVID19REGIONUS$hospitalized/12802000)*100000, 0)
COVID19REGIONUS$confirmed_pop_MN <- ifelse(COVID19REGIONUS$state == "MN", (COVID19REGIONUS$hospitalized/6950000)*100000, 0)
COVID19REGIONUS$confirmed_pop_GA <- ifelse(COVID19REGIONUS$state == "GA", (COVID19REGIONUS$hospitalized/10617000)*100000, 0)

COVID19REGIONUS$hosp_pop = COVID19REGIONUS$confirmed_pop_GA + COVID19REGIONUS$confirmed_pop_MN 
+ COVID19REGIONUS$confirmed_pop_PA + COVID19REGIONUS$confirmed_pop_WA
+ COVID19REGIONUS$confirmed_pop_CO + COVID19REGIONUS$confirmed_pop_OK
+ COVID19REGIONUS$confirmed_pop_OR + COVID19REGIONUS$confirmed_pop_IL
+ COVID19REGIONUS$confirmed_pop_FL + COVID19REGIONUS$confirmed_pop_TX
+ COVID19REGIONUS$confirmed_pop_NY + COVID19REGIONUS$confirmed_pop_CA


COVID19REGIONUS<-select(COVID19REGIONUS, -confirmed_pop_GA, -confirmed_pop_MN, 
                  -confirmed_pop_PA, -confirmed_pop_WA
                  , -confirmed_pop_CO , -confirmed_pop_OK
                  , -confirmed_pop_OR , -confirmed_pop_IL
                  , -confirmed_pop_FL , -confirmed_pop_TX
                  , -confirmed_pop_NY , -confirmed_pop_CA)










COVID19_USCOMPARE = subset(COVID19REGIONUS, COVID19REGIONUS$state=="CA" |
                             COVID19REGIONUS$state=="NY" | 
                             COVID19REGIONUS$state=="FL" | 
                             COVID19REGIONUS$state=="OR" | 
                             COVID19REGIONUS$state=="OK" | 
                             COVID19REGIONUS$state=="CO" | 
                             COVID19REGIONUS$state=="MN" | 
                             COVID19REGIONUS$state=="WA" | 
                             COVID19REGIONUS$state=="IL" | 
                             COVID19REGIONUS$state=="TX" |
                             COVID19REGIONUS$state=="TX" |
                               COVID19REGIONUS$state=="GA" |
                             COVID19REGIONUS$state=="PA")

COVID19_USCOMPARE_R = subset(COVID19_USCOMPARE, COVID19_USCOMPARE$date > "2020-03-14")



```




## Visualizations


### *Log confirmed cases in most populated states in US (COVID-19)*

```{r}

gather(COVID19_USCOMPARE,
       value = "LOG_confirmed",
       key = "state")
conf_US <- ggplot(COVID19_USCOMPARE,
                  aes(x=date,
                      y=LOG_confirmed,
                      color=state)) +
  geom_line()+
  geom_point(size=1)+
  theme(legend.position="top")
conf_US + labs(y= "LOG Confirmed cases (daily updated", color = "US. State")


```


### *Confirmed cases for 100.000 in most populated states in US (COVID-19)*

```{r}

gather(COVID19_USCOMPARE_R,
       value = "confirmed_pop",
       key = "state")
conf_USDC <- ggplot(COVID19_USCOMPARE_R,
                   aes(x=date,
                       y=confirmed_pop,
                       color=state)) +
  geom_line()+
  geom_point(size=1)+
  theme(legend.position="top")
conf_USDC + labs(x = "Date", y= "Confirmed/100.000 (daily updated)", color = "US. State")

```



### *Deaths and LOG deaths in most populated states in US (COVID-19)*

```{r}

gather(COVID19_USCOMPARE_R,
       value = "death",
       key = "state")
conf_USD <- ggplot(COVID19_USCOMPARE_R,
                   aes(x=date,
                       y=death,
                       color=state)) +
  geom_line()+
  geom_point(size=1)+
  theme(legend.position="top")
conf_USD + labs(x = "Date", y= "Deaths (daily updated)", color = "US. State")


gather(COVID19_USCOMPARE,
       value = "LOG_death",
       key = "state")
conf_US <- ggplot(COVID19_USCOMPARE,
                  aes(x=date,
                      y=LOG_death,
                      color=state)) +
  geom_line()+
  geom_point(size=1)+
  theme(legend.position="top")
conf_US + labs(y= "LOG Deaths (daily updated", color = "US. State")


```



### *Death rate in most populated states in US (COVID-19)*

```{r}

gather(COVID19_USCOMPARE_R,
       value = "Rate_d",
       key = "state")
conf_USDR <- ggplot(COVID19_USCOMPARE_R,
                     aes(x=date,
                         y=Rate_d,
                         color=state)) +
  geom_line()+
  geom_point(size=1)+
  theme(legend.position="top")
conf_USDR + labs(x = "Date", y= "Death rate (daily updated)", color = "US. State")


```


### *Hospitalization rate and Death/Hospitalization rate in most populated states in US (COVID-19)*

```{r}

gather(COVID19_USCOMPARE_R,
       value = "Rate_hosp",
       key = "state")
conf_USD <- ggplot(COVID19_USCOMPARE_R,
                   aes(x=date,
                       y=Rate_hosp,
                       color=state)) +
  geom_line()+
  geom_point(size=1)+
  theme(legend.position="top")
conf_USD + labs(x = "Date", y= "Hospitalization rate (daily updated)", color = "US. State")


gather(COVID19_USCOMPARE_R,
       value = "Rate_death_hosp",
       key = "state")
conf_USD <- ggplot(COVID19_USCOMPARE_R,
                   aes(x=date,
                       y=Rate_death_hosp,
                       color=state)) +
  geom_line()+
  geom_point(size=1)+
  theme(legend.position="top")
conf_USD + labs(x = "Date", y= "Death/Hospitalization rate (daily updated)", color = "US. State")

```

