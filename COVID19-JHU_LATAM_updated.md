# 2020 Latin America Coronavirus daily updated data: R script


## Daily updated data

Avaible on [www.tdeguilhem.com](https://48652267-876637319111280815.preview.editmysite.com/uploads/4/8/6/5/48652267/covid_19_clean_complete.csv)


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


COVID19=read.csv("https://www.tdeguilhem.com/uploads/4/8/6/5/48652267/covid_19_clean_complete.csv",
                     header=TRUE,sep=",")

print(COVID19[1:10,])

class(COVID19$Date)

COVID19$Date_ok=as.Date(COVID19$Date, format = "%Y-%m-%d")

class(COVID19$Date_ok)

COVID19[!is.na(COVID19$Date_ok), ]

COVID19$Rate_d = (COVID19$Deaths/COVID19$Confirmed) * 100

COVID19$Rate_r = (COVID19$Recovered/COVID19$Confirmed) * 100

COVID19$Rate_d[is.na(COVID19$Rate_d)] = 0

COVID19$Rate_r[is.na(COVID19$Rate_r)] = 0


COVID19$LOG_confirmed = log10(COVID19$Confirmed)
COVID19$LOG_confirmed[is.na(COVID19$LOG_confirmed)] = 0
COVID19$LOG_confirmed[is.infinite(COVID19$LOG_confirmed)] = 0



COVID19$LOG_Death = log10(COVID19$Deaths)
COVID19$LOG_Death[is.na(COVID19$LOG_Death)] = 0
COVID19$LOG_Death[is.infinite(COVID19$LOG_Death)] = 0





COVID19$confirmed_pop_SPAIN <- ifelse(COVID19$Country.Region == "Spain", (COVID19$Confirmed/46935000)*100000,  0 )

COVID19$confirmed_pop_ITALY <- ifelse(COVID19$Country.Region == "Italy", (COVID19$Confirmed/60360000)*100000, 0)
COVID19$confirmed_pop_GERMAN <- ifelse(COVID19$Country.Region == "Germany", (COVID19$Confirmed/83019000)*100000, 0)
COVID19$confirmed_pop_US <- ifelse(COVID19$Country.Region == "US", (COVID19$Confirmed/327096265)*100000, 0)

COVID19$confirmed_pop = COVID19$confirmed_pop_SPAIN + COVID19$confirmed_pop_ITALY + COVID19$confirmed_pop_GERMAN + COVID19$confirmed_pop_US 

COVID19<-select(COVID19, -confirmed_pop_SPAIN, -confirmed_pop_ITALY, -confirmed_pop_GERMAN, -confirmed_pop_US)




COVID19$death_pop_SPAIN <- ifelse(COVID19$Country.Region == "Spain", (COVID19$Deaths/46935000)*100000,  0 )

COVID19$death_pop_ITALY <- ifelse(COVID19$Country.Region == "Italy", (COVID19$Deaths/60360000)*100000, 0)
COVID19$death_pop_GERMAN <- ifelse(COVID19$Country.Region == "Germany", (COVID19$Deaths/83019000)*100000, 0)
COVID19$death_pop_US <- ifelse(COVID19$Country.Region == "US", (COVID19$Deaths/327096265)*100000, 0)

COVID19$death_pop = COVID19$death_pop_SPAIN + COVID19$death_pop_ITALY + COVID19$death_pop_GERMAN + COVID19$death_pop_US


COVID19<-select(COVID19, -death_pop_SPAIN, -death_pop_ITALY, -death_pop_GERMAN, -death_pop_US)






COVID19_LATAM = subset(COVID19, COVID19$Country.Region == "Colombia" | COVID19$Country.Region == "Peru" | COVID19$Country.Region == "Mexico" | COVID19$Country.Region == "Brazil" | COVID19$Country.Region == "Chile" | COVID19$Country.Region == "Argentina" | COVID19$Country.Region == "Ecuador" | COVID19$Country.Region == "Bolivia" | COVID19$Country.Region == "Nicaragua" | COVID19$Country.Region == "Honduras" | COVID19$Country.Region == "Costa Rica" | COVID19$Country.Region == "Panama" | COVID19$Country.Region == "Paraguay")

COVID19_LATAM_E = subset(COVID19, COVID19$Country.Region == "Colombia" | COVID19$Country.Region == "Peru" | COVID19$Country.Region == "Mexico" | COVID19$Country.Region == "Brazil" | COVID19$Country.Region == "Chile" | COVID19$Country.Region == "Argentina" | COVID19$Country.Region == "Ecuador" | COVID19$Country.Region == "Mexico")
COVID19_LATAM_WK = subset(COVID19_LATAM_E, COVID19_LATAM_E$Date_ok > "2020-03-09")



COVID19_COMPARE = subset(COVID19, COVID19$Country.Region == "Italy" | COVID19$Country.Region == "Spain" | COVID19$Country.Region == "Germany" | COVID19$Country.Region == "US")
COVID19_COMPARE = subset(COVID19_COMPARE, COVID19_COMPARE$Date_ok > "2020-03-04")




```

## Visualizations

### *Confirmed cases in Latin America and confirmed cases (LOG and per 100,000 people) in Europe (Italy, Spain and Germany) and US*

```{r}


gather(COVID19_LATAM_WK,
                       value = "Confirmed",
                       key = "Country.Region")
conf_LATAM <- ggplot(COVID19_LATAM_WK,
       aes(x=Date_ok,
           y=Confirmed,
           color=Country.Region)) +
  geom_line()+
geom_point(size=1)+
  theme(legend.position="top")
conf_LATAM + labs(x = "Date", y= "Confirmed cases (daily updated)", color = "Country")



gather(COVID19_COMPARE,
       value = "Confirmed",
       key = "Country.Region")
COMPARE_Con <-ggplot(COVID19_COMPARE,
                     aes(x=Date_ok,
                         y=Confirmed,
                         color=Country.Region)) +
  geom_line()+
  geom_point(size=1)+
  theme(legend.position="top")
COMPARE_Con+ labs(x = "Date", y= "Confirmed cases (daily updated)", color = "Country")



gather(COVID19_COMPARE,
       value = "Confirmed",
       key = "Country.Region")
COMPARE_LOGCon <-ggplot(COVID19_COMPARE,
                     aes(x=Date_ok,
                         y=LOG_confirmed,
                         color=Country.Region)) +
  geom_line()+
  geom_point(size=1)+
  theme(legend.position="top")
COMPARE_LOGCon+ labs(x = "Date", y= "Log Confirmed cases 03/24/2020", color = "Country")




gather(COVID19_COMPARE,
       value = "confirmed_pop",
       key = "Country.Region")
CONF_POP <- ggplot(COVID19_COMPARE,
                   aes(x=Date_ok,
                       y=confirmed_pop,
                       color=Country.Region)) +
  geom_line()+
  geom_point(size=1)+
  theme(legend.position="top")
CONF_POP + labs(x = "Date", y= "Cases per 100,000 people (daily updated)", color = "Country")

```

### *Interactive plots*

```{r}

ggplotly(conf_LATAM)

ggplotly(COMPARE_Con)

ggplotly(COMPARE_LOGCon)

ggplotly(CONF_POP)

```

### *Deaths and death rate in Latin America, and Deaths (LOG and per 100,000 people) and death rate in Europe (Italy, Spain and Germany) and US*

```{r}
 
 gather(COVID19_LATAM_WK,
                           value = "Deaths",
                           key = "Country.Region")
death_LATAM <-ggplot(COVID19_LATAM_WK,
       aes(x=Date_ok,
           y=Deaths,
           color=Country.Region)) +
  geom_line()+
geom_point(size=1)+
  theme(legend.position="top")
death_LATAM + labs(x = "day", y= "Deaths (daily updated)", color = "Country")



gather(COVID19_LATAM_WK,
       value = "Rate_d",
       key = "Country.Region")
LATAM_DR <-ggplot(COVID19_LATAM_WK,
                     aes(x=Date_ok,
                         y=Rate_d,
                         color=Country.Region)) +
  geom_line()+
  geom_point(size=1)+
  theme(legend.position="top")
LATAM_DR+ labs(x = "day", y= "Death rate (daily updated)", color = "Country")



gather(COVID19_COMPARE,
       value = "Deaths",
       key = "Country.Region")
COMPARE_Dea <-ggplot(COVID19_COMPARE,
                   aes(x=Date_ok,
                       y=Deaths,
                       color=Country.Region)) +
  geom_line()+
  geom_point(size=1)+
  theme(legend.position="top")
COMPARE_Dea+ labs(x = "day", y= "Deaths (daily updated)", color = "Country")



gather(COVID19_COMPARE,
       value = "Rate_d",
       key = "Country.Region")
COMPARE_D <-ggplot(COVID19_COMPARE,
                     aes(x=Date_ok,
                         y=Rate_d,
                         color=Country.Region)) +
  geom_line()+
  geom_point(size=1)+
  theme(legend.position="top")
COMPARE_D+ labs(x = "day", y= "Death rate (daily updated)", color = "Country")


gather(COVID19_COMPARE,
       value = "death_pop",
       key = "Country.Region")
CONF_POP <- ggplot(COVID19_COMPARE,
                   aes(x=Date_ok,
                       y=death_pop,
                       color=Country.Region)) +
  geom_line()+
  geom_point(size=1)+
  theme(legend.position="top")
CONF_POP + labs(x = "Date", y= "Deaths per 100,000 people (daily updated)", color = "Country")


```

### *Interactive plots*

```{r}

ggplotly(death_LATAM)

ggplotly(LATAM_DR)

ggplotly(COMPARE_Dea)

ggplotly(COMPARE_D)

ggplotly(CONF_POP)

```
