# Latin America Coronavirus project


## R Code


### Data updated each day

Avaible on [www.tdeguilhem.com](https://48652267-876637319111280815.preview.editmysite.com/uploads/4/8/6/5/48652267/covid_19_clean_complete.csv)


### Needed packages

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


### Data cleaning and subsets (coutries)

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




COVID19_LATAM = subset(COVID19, COVID19$Country.Region == "Colombia" | COVID19$Country.Region == "Peru" | COVID19$Country.Region == "Mexico" | COVID19$Country.Region == "Brazil" | COVID19$Country.Region == "Chile" | COVID19$Country.Region == "Argentina" | COVID19$Country.Region == "Ecuador" | COVID19$Country.Region == "Bolivia" | COVID19$Country.Region == "Nicaragua" | COVID19$Country.Region == "Honduras" | COVID19$Country.Region == "Costa Rica" | COVID19$Country.Region == "Panama" | COVID19$Country.Region == "Paraguay")

COVID19_LATAM_E = subset(COVID19, COVID19$Country.Region == "Colombia" | COVID19$Country.Region == "Peru" | COVID19$Country.Region == "Mexico" | COVID19$Country.Region == "Brazil" | COVID19$Country.Region == "Chile" | COVID19$Country.Region == "Argentina" | COVID19$Country.Region == "Ecuador" | COVID19$Country.Region == "Mexico")
COVID19_LATAM_WK = subset(COVID19_LATAM_E, COVID19_LATAM_E$Date_ok > "2020-03-09")



COVID19_COMPARE = subset(COVID19, COVID19$Country.Region == "Italy" | COVID19$Country.Region == "Spain" | COVID19$Country.Region == "Germany")
COVID19_COMPARE = subset(COVID19_COMPARE, COVID19_COMPARE$Date_ok > "2020-03-04")
```

### *Confirmed cases in Latin America (COVID-19) and Eurpoe (Italy, Spain and Germany)*

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
conf_LATAM + labs(x = "day", y= "Confirmed cases 03/24/2020", color = "Country")



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
COMPARE_Con+ labs(x = "day", y= "Confirmed cases 03/24/2020", color = "Country")



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
COMPARE_LOGCon+ labs(x = "day", y= "Log Confirmed cases 03/24/2020", color = "Country")

```

### *Interactive plot*

```{r}

ggplotly(conf_LATAM)

ggplotly(COMPARE_Con)

ggplotly(COMPARE_LOGCon)

```

### *Deaths and death rate in Latin America (COVID-19) and Europe (Italy, Spain and Germany)*

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
death_LATAM + labs(x = "day", y= "Deaths 03/24/2020", color = "Country")



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
LATAM_DR+ labs(x = "day", y= "Death rate 03/24/2020", color = "Country")



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
COMPARE_Dea+ labs(x = "day", y= "Deaths 03/24/2020", color = "Country")



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
COMPARE_D+ labs(x = "day", y= "Death rate 03/24/2020", color = "Country")

```

### *Interactive plot*

```{r}

ggplotly(death_LATAM)

ggplotly(LATAM_DR)

ggplotly(COMPARE_Dea)

ggplotly(COMPARE_D)

```
