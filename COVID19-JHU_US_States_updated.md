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


## Data cleaning and subsets (countries)

```{r}



COVID19REGIONUS=read.csv("https://www.tdeguilhem.com/uploads/4/8/6/5/48652267/us_states_covid19_daily.csv",
                 header=TRUE,sep=",")

print(COVID19REGIONUS[1:10,])


COVID19REGIONUS <- COVID19REGIONUS[order(COVID19REGIONUS$date),]


COVID19REGIONUS$positive[is.na(COVID19REGIONUS$positive)] = 0

COVID19REGIONUS$negative[is.na(COVID19REGIONUS$negative)] = 0

COVID19REGIONUS$pending[is.na(COVID19REGIONUS$pending)] = 0

COVID19REGIONUS$hospitalized[is.na(COVID19REGIONUS$hospitalized)] = 0

COVID19REGIONUS$death[is.na(COVID19REGIONUS$death)] = 0

COVID19REGIONUS$total[is.na(COVID19REGIONUS$total)] = 0



COVID19REGIONUS$Rate_d = (COVID19REGIONUS$death/COVID19REGIONUS$positive) * 100

COVID19REGIONUS$Rate_hosp = (COVID19REGIONUS$hospitalized/COVID19REGIONUS$positive) * 100


COVID19REGIONUS$Rate_death_hosp = (COVID19REGIONUS$death/COVID19REGIONUS$hospitalized) * 100

COVID19REGIONUS$Rate_death_hosp[is.infinite(COVID19REGIONUS$Rate_death_hosp) | is.nan(COVID19REGIONUS$Rate_death_hosp) ] <- NA

COVID19REGIONUS$Rate_contaminated = (COVID19REGIONUS$positive/(COVID19REGIONUS$negative+COVID19REGIONUS$positive)) * 100

COVID19REGIONUS$LOG_confirmed = log10(COVID19REGIONUS$positive)

COVID19REGIONUS$confirmed_pop <- ifelse(COVID19REGIONUS$state == "CA", COVID19REGIONUS$positive/39512, 0)
COVID19REGIONUS$confirmed_pop <- ifelse(COVID19REGIONUS$state == "NY", COVID19REGIONUS$positive/19453, 0)
COVID19REGIONUS$confirmed_pop <- ifelse(COVID19REGIONUS$state == "TX", COVID19REGIONUS$positive/28995, 0)
COVID19REGIONUS$confirmed_pop <- ifelse(COVID19REGIONUS$state == "FL", COVID19REGIONUS$positive/21477, 0)
COVID19REGIONUS$confirmed_pop <- ifelse(COVID19REGIONUS$state == "IL", COVID19REGIONUS$positive/12671, 0)
COVID19REGIONUS$confirmed_pop <- ifelse(COVID19REGIONUS$state == "OR", COVID19REGIONUS$positive/4217, 0)
COVID19REGIONUS$confirmed_pop <- ifelse(COVID19REGIONUS$state == "OK", COVID19REGIONUS$positive/3957, 0)
COVID19REGIONUS$confirmed_pop <- ifelse(COVID19REGIONUS$state == "CO", COVID19REGIONUS$positive/5759, 0)
COVID19REGIONUS$confirmed_pop <- ifelse(COVID19REGIONUS$state == "WA", COVID19REGIONUS$positive/7615, 0)
COVID19REGIONUS$confirmed_pop <- ifelse(COVID19REGIONUS$state == "PA", COVID19REGIONUS$positive/12802, 0)
COVID19REGIONUS$confirmed_pop <- ifelse(COVID19REGIONUS$state == "MN", COVID19REGIONUS$positive/6950, 0)
COVID19REGIONUS$confirmed_pop <- ifelse(COVID19REGIONUS$state == "GA", COVID19REGIONUS$positive/10617, 0)




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
COVID19_USCOMPARE_R = subset(COVID19_USCOMPARE, COVID19_USCOMPARE$date > 20200314)


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
conf_US + labs(x = "Date", y= "LOG Confirmed cases 03/26/2020", color = "US. State")

```


### *Confirmed cases for 100.000 in most populated states in US (COVID-19)*

```{r}

gather(COVID19_USCOMPARE_R,
       value = "confirmed_pop",
       key = "state")
conf_USD <- ggplot(COVID19_USCOMPARE_R,
                   aes(x=date,
                       y=confirmed_pop,
                       color=state)) +
  geom_line()+
  geom_point(size=1)+
  theme(legend.position="top")
conf_USD + labs(x = "Date", y= "Confirmed/100.000 03/26/2020", color = "US. State")

```



### *Deaths in most populated states in US (COVID-19)*

```{r}

gather(COVID19_USCOMPARE_R,
       value = "death",
       key = "state")
conf_USD <- ggplot(COVID19_USCOMPARE_R,
                   aes(x=date,
                       y=Rate_death,
                       color=state)) +
  geom_line()+
  geom_point(size=1)+
  theme(legend.position="top")
conf_USD + labs(x = "Date", y= "Deaths 03/26/2020", color = "US. State")

```



### *Death rate in most populated states in US (COVID-19)*

```{r}
gather(COVID19_USCOMPARE_R,
       value = "Rate_d",
       key = "state")
conf_USD <- ggplot(COVID19_USCOMPARE_R,
                     aes(x=date,
                         y=Rate_d,
                         color=state)) +
  geom_line()+
  geom_point(size=1)+
  theme(legend.position="top")
conf_USD + labs(x = "Date", y= "Death rate 03/26/2020", color = "US. State")

```


### *Death/Hospitalization rate in most populated states in US (COVID-19)*

```{r}

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
conf_USD + labs(x = "Date", y= "Death/Hospitalization rate 03/26/2020", color = "US. State")

```

