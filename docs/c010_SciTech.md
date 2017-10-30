




# Science & Technology - Aggregate Score

**Neighbouring Countries**:




(worse), Germany, Switzerland, Hungary, Sweden, Finland, New Zealand, Cyprus, Denmark, Czech Republic, Austria, United Kingdom, (better)



![](c010_SciTech_files/figure-latex/unnamed-chunk-6-1.pdf)<!-- --> 

**My Interpretation and Opinions**:

This graph is highly consistent with the role of wealth in the overall ranking- Economy is playing a less clear role (as measures by the spread of the data) as it has been used less often, but there is still a distinct relationship where no country of low wealth is at the upper end of the the Science and Technology index.

In rankings, New Zealand is in a generally good place, better than Germany or Sweden and worse than Denmark or the United Kingdom. Even if we consider the GCI in relation to GDP per capita, we are still doing very well for countries in our wealth range.

As a comment on the sources that make up the measure, I think it might be interesting to consider if number of science graduates as a proportion of the population is a more current measure of a country's contribution to science than some of the measures in this index.



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
var_code <- "b_ScienceTechnology"
var_prettynom <- "Science & Technology"
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
