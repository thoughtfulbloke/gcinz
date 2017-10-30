




# World Order - Aggregate Score

**Neighbouring Countries**:




(worse), Chile, Costa Rica, Spain, Thailand, Slovenia, New Zealand, Belgium, Ireland, Italy, France, United Kingdom, (better)



![](c040_Order_files/figure-latex/unnamed-chunk-6-1.pdf)<!-- --> 

**My Interpretation and Opinions**:

The World Order aggregate category is sensitive to GDP per capita, as no poor country is among the best ranked countries.

New Zealand's place is a bit under where it could be given the trend of the data, though relatively high overall.



```r
library(feather)
library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)
library(viridis)
gci11 <- read_feather("processed_data/gci11.feather")
metagci11 <- read_feather("processed_data/gci11meta.feather")
gdpcap11 <- read_feather("processed_data/gdpcap11.feather")
```


```r
var_code <- "e_WorldOrder"
var_prettynom <- "World Order"
var_sides <- 5
var_cciso <- "NZL"
var_highlight <- "New Zealand"
```



```r
nznear <- gci11[,c("Country.Name", "isoCode", var_code)]
names(nznear) <- c("Country", "isoCode", "Rank")
nznear <- nznear %>% filter(Rank != 0) %>% arrange(desc(Rank))
nzloc <- which(nznear$isoCode == var_cciso)
above <- ifelse(nzloc - var_sides < 1, 1, nzloc - var_sides )
below <- ifelse(nzloc + var_sides > nrow(nznear), nrow(nznear), nzloc + var_sides )
neighbours <- paste(c("(worse)", nznear$Country[above:below], "(better)"),collapse=", ")
```



```r
  gci <- gci11[,c("isoCode",var_code)]
  names(gci) <- c("isoCode","gci_rank")
  axtitle <- paste("GDP per capita and\n", trimws(var_prettynom), "Good Country Rank")
  gci %>%
    inner_join(gdpcap11, by=c("isoCode" = "Country.Code")) %>%
    filter(!is.na(X2011) & gci_rank != 0) %>% 
    mutate(country = ifelse(isoCode == var_cciso, var_highlight, "other")) %>%
    ggplot(aes(x=X2011, y=gci_rank, colour=country, shape=country)) + geom_point() + xlab("GDP per capita 2011") +
    ylab("Good Country Rank") + theme_minimal() + scale_color_viridis(discrete=TRUE, begin=0, end=0.9) +
    ggtitle(axtitle)
```
