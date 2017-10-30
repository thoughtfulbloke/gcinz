--- 
title: "A New Zealand view of the Good Country Index"
author: "David Hood"
date: "2017-10-30"
site: bookdown::bookdown_site
documentclass: book
description: "A NZer on GCI"
---

# Introduction




In response to a tweet from [helenclarknz](https://twitter.com/HelenClarkNZ/status/919274220890284033) I thought I would take a closer look, from a New Zealand perspective, at the Good Country Index:

https://goodcountry.org/index/results

Which has a description of the original data here

https://goodcountry.org/index/source-data

As the Good Country Index is licenced as Creative Commmons Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0) the products of this analysis should be considered similar. The code for generating my analysis I am releasing under an MIT licence, so you are welcome to use and adapt it with attribution. The data forming the index was downloaded from the site on the 23rd of October 2017

I am also drawing on World Bank GDP per capita data

https://data.worldbank.org/indicator/NY.GDP.PCAP.CD

and World Bank GDP (current US$)

https://data.worldbank.org/indicator/NY.GDP.MKTP.CD

which is open licenced, via the direct csv download link from the above pages

Wikipedia's list of oldest Universities by County

https://en.wikipedia.org/wiki/List_of_oldest_universities_in_continuous_operation

which is licenced with Creative Commons Attribution-ShareAlike 3.0

Finally, I am drawing directly on one of the sources the Good Country Index is based on, The World Bank: Distance to Frontier Score: Doing Business Indicators,

http://www.doingbusiness.org/data/distance-to-frontier

via the downloadable excel file of data

http://www.doingbusiness.org/data/~/media/WBG/DoingBusiness/Documents/Miscellaneous/DB17-Distance-to-Frontier-historical-dataset.xlsx

(note: directly downloading the file gives a different format than clicking the button on the webpage, for this I have used the former)

**Data gathering code**


```r
library(readxl)
library(jsonlite)
library(dplyr)
library(tidyr)
library(countrycode)
library(rvest)
library(feather)

GCI_country_info <- function(x, gciver){
  gooddf <- data.frame(id = x$countries$id,
                     name = x$countries$name,
                     isoCode = x$countries$isoCode,
                     stringsAsFactors = FALSE)
  gooddf$version <- gciver
  return(gooddf)
}

v1.1_json <- fromJSON("https://goodcountry.org/indexData.json?version=1.1")
v1.0_json <- fromJSON("https://goodcountry.org/indexData.json?version=1.0")
gci1.1 <- GCI_country_info(v1.1_json, 1.1)
gci1.0 <- GCI_country_info(v1.0_json, 1.0)

indicate_desc <- function(x, y, inlist){
  section <- c("4","12","13","14","15","16","17")
  group <- c("Science & Technology", "Culture", "International Peace & Security",
             "World Order", "Planet & Climate", "Prosperity & Equality", "Health & Wellbeing")
  chap <- group[section == x]
    title = inlist[["indicators"]][["children"]][[y]][["title"]]
    title <- title[!is.na(title)]
    code = inlist[["indicators"]][["children"]][[y]][["code"]]
    code <- code[!is.na(code)]
    description = inlist[["indicators"]][["children"]][[y]][["description"]]
    description <- description[!is.na(description)]
    reference = inlist[["indicators"]][["children"]][[y]][["reference"]]
    reference <- reference[!is.na(reference)]
    treatment = inlist[["indicators"]][["children"]][[y]][["treatment"]]
    treatment <- treatment[!is.na(treatment)]
 z <- data.frame(section=chap, title, code, description, reference,
                 treatment, stringsAsFactors = FALSE)
  return(z)
}

arrangement = "
group,indicator
4,5
4,6
4,7
4,8
4,9
12,10
12,11
12,18
12,19
12,20
13,21
13,22
13,23
13,24
13,25
14,26
14,27
14,28
14,29
14,30
15,31
15,32
15,33
15,34
15,35
16,36
16,37
16,38
16,39
16,40
17,41
17,42
17,43
17,44
17,45"

json_locations <- read.csv(text=arrangement, colClasses="character")

gci1.1_indicators_meta <- bind_rows(apply(json_locations,1,
                           function(x){indicate_desc(x=x[1], y= x[2],
                                                inlist=v1.1_json)}))
gci1.0_indicators_meta <- bind_rows(apply(json_locations,1,
                           function(x){indicate_desc(x=x[1], y= x[2],
                                                inlist=v1.0_json)}))

gci_indicator_info <- function(x,y, inlist){
    z <- data.frame(ranks = 
  as.numeric(v1.1_json[["countries"]][["indicators"]][[x]][["children"]][[y]][["rank"]]),
                    stringsAsFactors = FALSE)
    codes <- v1.1_json[["countries"]][["indicators"]][[x]][["children"]][[y]][["code"]]
    codes <- codes[min(which(!is.na(codes)))]
    names(z) <- codes
    return(z)
}

gci1.1_indicators <- bind_cols(apply(json_locations,1,
                           function(x){gci_indicator_info(x=x[1], y= x[2],
                                                inlist=v1.1_json)}))

gci1.0_indicators <- bind_cols(apply(json_locations,1,
                           function(x){gci_indicator_info(x=x[1], y= x[2],
                                                inlist=v1.0_json)}))

rm(v1.0_json, v1.1_json, json_locations)

# upon inspection Côte d'Ivoire is present twice, the second time as a phantom entry, 
# so I am removing the second entry(number 42) which has no associated data
gci1.0 <- gci1.0[-42,]
gci1.1 <- gci1.1[-42,]
gci1.0_indicators <- gci1.0_indicators[-42,]
gci1.1_indicators <- gci1.1_indicators[-42,]

## rather than calculate out the aggregate scores (given tiebreaking etc) I have just
## copied the aggregates from the website
goodc <- readLines("raw_data/goodcountryindex.txt")
gci1.1_aggregate <- data.frame(Country.Name = goodc[1:163 * 9 - 6],
                 a_Overall = goodc[1:163 * 9 - 7],
                 b_ScienceTechnology = goodc[1:163 * 9 - 5],
                 c_Culture = goodc[1:163 * 9 - 4],
                 d_InternationalPeaceSecurity = goodc[1:163 * 9 - 3],
                 e_WorldOrder = goodc[1:163 * 9 - 2],
                 f_PlanetClimate = goodc[1:163 * 9 - 1],
                 g_ProsperityEquality = goodc[1:163 * 9],
                 h_HealthWellbeing = goodc[1:163 * 9 + 1],
                 stringsAsFactors = FALSE)

# combine the aggregate and the individual scores
# this implicitely clears out entries without data for the 1.1 aggregate merge
gci1.0 <- cbind(gci1.0, gci1.0_indicators)
gci1.1 <- cbind(gci1.1, gci1.1_indicators)

gci1.1 <- merge(gci1.1_aggregate, gci1.1, by.x="Country.Name", by.y="name")
# make aggregates numerics
gci1.1$a_Overall <-  as.numeric(gsub("[ abcdefghijklmnopqrstuvwxyzz]+.*",
                                     "",gci1.1$a_Overall))
gci1.1$b_ScienceTechnology <-  as.numeric(gsub("[ abcdefghijklmnopqrstuvwxyzz]+.*",
                                               "",gci1.1$b_ScienceTechnology))
gci1.1$c_Culture <-  as.numeric(gsub("[ abcdefghijklmnopqrstuvwxyzz]+.*",
                                     "",gci1.1$c_Culture))
gci1.1$d_InternationalPeaceSecurity <-  as.numeric(gsub("[ abcdefghijklmnopqrstuvwxyzz]+.*","",gci1.1$d_InternationalPeaceSecurity))
gci1.1$e_WorldOrder <-  as.numeric(gsub("[ abcdefghijklmnopqrstuvwxyzz]+.*",
                                        "",gci1.1$e_WorldOrder))
gci1.1$f_PlanetClimate <-  as.numeric(gsub("[ abcdefghijklmnopqrstuvwxyzz]+.*",
                                           "",gci1.1$f_PlanetClimate))
gci1.1$g_ProsperityEquality <-  as.numeric(gsub("[ abcdefghijklmnopqrstuvwxyzz]+.*",
                                                "",gci1.1$g_ProsperityEquality))
gci1.1$h_HealthWellbeing <-  as.numeric(gsub("[ abcdefghijklmnopqrstuvwxyzz]+.*",
                                             "",gci1.1$h_HealthWellbeing))

rm(gci1.0_indicators, gci1.1_aggregate, gci1.1_indicators)
# world bank GDP per capita in 2011
GDPcap <- read.csv("raw_data/API_NY_GDP_PCAP/API_NY.GDP.PCAP.CD_DS2_en_csv_v2.csv",
                   skip=4, stringsAsFactors = FALSE)
GDPcap11 <- GDPcap[,c("Country.Name", "Country.Code", "X2011")]
rm(GDPcap)

# world bank GDP  in 2011
GDP <- read.csv("raw_data/API_NY_GDP_MKTP/API_NY.GDP.MKTP.CD_DS2_en_csv_v2.csv",
                   skip=4, stringsAsFactors = FALSE)
GDP11 <- GDP[,c("Country.Name", "Country.Code", "X2011")]
rm(GDP)

# wikipedia page
wp <- "https://en.wikipedia.org/wiki/List_of_oldest_universities_in_continuous_operation"
html = read_html(wp)
tbls <- html_table(html_nodes(html, "table"), fill=TRUE)
u1 <- data.frame(yr=tbls[[1]][["Year"]], whr=tbls[[1]][[4]], stringsAsFactors = FALSE)
u2 <- data.frame(yr=tbls[[2]][["Year"]], whr=tbls[[2]][[1]], stringsAsFactors = FALSE)
u3 <- data.frame(yr=tbls[[3]][["Year"]], whr=tbls[[3]][[1]], stringsAsFactors = FALSE)
u4 <- data.frame(yr=tbls[[4]][["Year"]], whr=tbls[[4]][[1]], stringsAsFactors = FALSE)
u5 <- data.frame(yr=tbls[[5]][["Year"]], whr=tbls[[5]][[1]], stringsAsFactors = FALSE)
u6 <- data.frame(yr=tbls[[6]][["Year"]], whr=tbls[[6]][[1]], stringsAsFactors = FALSE)
u7 <- data.frame(yr=tbls[[7]][["Year"]], whr=tbls[[7]][[1]], stringsAsFactors = FALSE)
u1$whr <- gsub(".*, ", "", u1$whr)
u1$yr[10] <- "1293"
u1 <- u1 %>% separate(col=yr, into=c("yr","mr"), sep=4) %>% select(-mr)
u2 <- u2 %>% separate(col=whr, into=c("whr","nr"), sep="\n\\(", fill="right") %>% 
  separate(col=yr, into=c("yr","mr"), sep=4) %>% select(-mr, -nr)
u3 <- u3 %>% separate(col=yr, into=c("yr","mr"), sep=4) %>% select(-mr)
u4 <- u4 %>%
  separate(col=whr, into=c("whr","nr"), sep="\n\\(", fill="right", extra="merge") %>% 
  separate(col=yr, into=c("yr","mr"), sep=4) %>% select(-mr, -nr)
u5a <- data.frame(whr=c("Anguilla",
                        "Antigua and Barbuda",
                        "Bahamas",
                        "Barbados",
                        "Belize",
                        "British Virgin Islands",
                        "Cayman Islands",
                        "Dominica",
                        "Grenada",
                        "Jamaica",
                        "Montserrat",
                        "St. Kitts and Nevis",
                        "St. Lucia",
                        "St. Vincent and the Grenadines",
                        "Trinidad and Tobago",
                        "Turks and Caicos"), stringsAsFactors = FALSE)
u5a$yr <- "1948"
u5 <- u5[-25,]
u6 <- u6 %>%
  separate(col=whr, into=c("whr","nr"), sep="\n\\(", fill="right", extra="merge") %>% 
  separate(col=yr, into=c("yr","mr"), sep=4) %>% select(-mr, -nr)
u7a <- data.frame(whr=c("Cook Islands",
                           "Fiji",
                           "Kiribati",
                           "Marshall Islands",
                           "Nauru",
                           "Niue",
                           "Samoa",
                           "Solomon Islands",
                           "Tokelau",
                           "Tonga",
                           "Tuvalu",
                           "Vanuatu"), stringsAsFactors = FALSE)
u7a$yr <- "1968"
u7 <- u7[-11,]
u7 <- u7 %>%
  separate(col=whr, into=c("whr","nr"), sep="\n\\(", fill="right", extra="merge") %>% 
  separate(col=yr, into=c("yr","mr"), sep=4) %>% select(-mr, -nr)
unis <- bind_rows(u1,u2,u3,u4,u5,u5a,u6,u7,u7a)
rm(u1,u2,u3,u4,u5,u5a,u6,u7,u7a,html,tbls)
unis <- unis %>% mutate(yr = as.numeric(yr)) %>% filter(!is.na(yr)) %>%
  group_by(whr) %>% summarise(yr = min(yr))

# wikipedia's country names not standard, so I am adding the ISO codes
unis$isocode <- countrycode(unis$whr, origin="country.name", destination="iso3c")

# Doing business spreadsheet
# the wp1 wp2 paste0 is just to fit code with margins
wp1 <- "http://www.doingbusiness.org/data/~/media/WBG/DoingBusiness/Documents/"
wp2 <- "Miscellaneous/DB17-Distance-to-Frontier-historical-dataset.xlsx"
download.file(paste0(wp1, wp2), destfile="raw_data/DTF.xlsx")
dtf <- read_excel("raw_data/DTF.xlsx", sheet="DB05-17 for Excel") %>%
  select("cod", "economy", "region", "incomegroup", "dbyear","DTFtradedef_db1415",
         "tradingexportdocsdtf", "tradingexporttimedtf", "tradingimportdocsdtf",
         "tradingimporttimedtf", "tradingexportcost_defdtf", "tradingimportcost_defdtf",
         "tradeXcostborderdtf", "tradeMcostborderdtf", "tradeXtimeborderdtf",
         "tradeMtimeborderdtf", "tradeMcostdocsdtf", "tradeMtimedocsdtf",
         "tradeXcostdocsdtf", "tradeXtimedocsdtf") %>%
  mutate(dbyear = as.numeric(dbyear)) 

#GCI1.1 and GCI1.0 are both missing a few ISO codes
gci1.1$isoCode[is.na(gci1.1$isoCode)] <- 
  countrycode(gci1.1$Country.Name[is.na(gci1.1$isoCode)], origin="country.name",
              destination="iso3c")
gci1.0$isoCode[is.na(gci1.0$isoCode)] <- 
  countrycode(gci1.0$name[is.na(gci1.0$isoCode)], origin="country.name",
              destination="iso3c")

## now save everything in feather format for the other chapters
write_feather(dtf, "processed_data/dtf.feather")
write_feather(gci1.0, "processed_data/gci10.feather")
write_feather(gci1.0_indicators_meta, "processed_data/gci10meta.feather")
write_feather(gci1.1, "processed_data/gci11.feather")
write_feather(gci1.1_indicators_meta, "processed_data/gci11meta.feather")
write_feather(GDP11, "processed_data/gdp11.feather")
write_feather(GDPcap11, "processed_data/gdpcap11.feather")
write_feather(unis, "processed_data/unis.feather")
```

