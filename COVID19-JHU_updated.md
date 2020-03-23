# Latin America Coronavirus project

## Code R

```{r}

install.packages("Rcpp")
library(Rcpp)
% COVID19 %
  
library(tidyverse)
library(ggplot2)
library(tibble)
library(tidyr)
library(dplyr)
library(lubridate)
library(hrbrthemes)


require(ggplot2)  
require(gridExtra) 

COVID19=read.csv("https://48652267-876637319111280815.preview.editmysite.com/uploads/4/8/6/5/48652267/covid_19_clean_complete_03232020.csv",
                     header=TRUE,sep=",")

print(COVID19[1:10,])

class(COVID19$Date)

COVID19$Date_ok=as.Date(COVID19$Date, format = "%m/%d/%Y")

class(COVID19$Date_ok)

COVID19[!is.na(COVID19$Date_ok), ]



COVID19_EUROPE = subset(COVID19, COVID19$Country.Region == "France" | COVID19$Country.Region == "Spain" | COVID19$Country.Region == "Italy" | COVID19$Country.Region == "Germany" | COVID19$Country.Region == "United Kingdom" | COVID19$Country.Region == "Ireland")

COVID19_LATAM = subset(COVID19, COVID19$Country.Region == "Colombia" | COVID19$Country.Region == "Peru" | COVID19$Country.Region == "Mexico" | COVID19$Country.Region == "Brazil" | COVID19$Country.Region == "Chile" | COVID19$Country.Region == "Argentina" | COVID19$Country.Region == "Ecuador" | COVID19$Country.Region == "Bolivia" | COVID19$Country.Region == "Nicaragua" | COVID19$Country.Region == "Honduras" | COVID19$Country.Region == "Costa Rica" | COVID19$Country.Region == "Panama" | COVID19$Country.Region == "Paraguay")


COVID19_LATAM_E = subset(COVID19, COVID19$Country.Region == "Colombia" | COVID19$Country.Region == "Peru" | COVID19$Country.Region == "Mexico" | COVID19$Country.Region == "Brazil" | COVID19$Country.Region == "Chile" | COVID19$Country.Region == "Argentina" | COVID19$Country.Region == "Ecuador" | COVID19$Country.Region == "Mexico")

COVID19_LATAM_WK = subset(COVID19_LATAM_E, COVID19_LATAM_E$Date_ok > "0020-03-01")

COVID19_SPAIN = subset(COVID19, COVID19$Country.Region == "Spain")

COVID19_EU2 = subset(COVID19, COVID19$Country.Region == "Spain" | COVID19$Country.Region == "Italy")

COVID19_FR = subset(COVID19, COVID19$Country.Region == "France")



install.packages("plotly")

library(plotly)



% LATAM %
  
  % Confirmed cases %
```
```{r, echo=FALSE}

gather(COVID19_LATAM_WK,
                       value = "Confirmed",
                       key = "Country.Region",
                       Colombia, Brazil, Chile)
conf_LATAM <- ggplot(COVID19_LATAM_WK,
       aes(x=Date_ok,
           y=Confirmed,
           color=Country.Region)) +
  geom_line()+
geom_point(size=1)+
  theme(legend.position="top")
conf_LATAM + labs(x = "day", y= "Confirmed cases 03/22/2020", color = "Country")
ggplotly(conf_LATAM)



% Deaths %

```

```{r, echo=FALSE}

 gather(COVID19_LATAM_WK,
                           value = "Deaths",
                           key = "Country.Region",
                           Colombia, Brazil, Chile)
death_LATAM <-ggplot(COVID19_LATAM_WK,
       aes(x=Date_ok,
           y=Deaths,
           color=Country.Region)) +
  geom_line()+
geom_point(size=1)+
  theme(legend.position="top")
death_LATAM + labs(x = "day", y= "Deaths 03/22/2020", color = "Country")

ggplotly(death_LATAM)
```
