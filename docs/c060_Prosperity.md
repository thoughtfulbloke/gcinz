




# Prosperity & Equality - Aggregate Score

**Neighbouring Countries**:




(worse), Barbados, Germany, Philippines, Cyprus, Montenegro, New Zealand, Senegal, Canada, Ireland, Lebanon, Trinidad and Tobago, (better)



![](c060_Prosperity_files/figure-latex/unnamed-chunk-6-1.pdf)<!-- --> 

**My Interpretation and Opinions**:

The Prosperity and Equality aggregate is mildly related to GDP per capita, as wealthier countries are concentrated in the best half of ranks. Given the category, I was expecting the relationship to be stronger.



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
var_code <- "g_ProsperityEquality"
var_prettynom <- "Prosperity & Equality"
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
