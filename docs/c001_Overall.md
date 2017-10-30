




# Overall - Total Score

**Neighbouring Countries**:




(worse), Luxembourg, Italy, Cyprus, Belgium, Norway, New Zealand, Ireland, Canada, Austria, France, Finland, (better)

We can call New Zealand a "pretty good" country within the Good Country Index- Better than Italy or Norway and worse than Canada or the France, if we pick some countries that we normally think in terms of. Let's understand the detail, though.

The Good Country Index uses eight ranked criteria (each made from a bundle of indicators), that are averaged together to produce the overall ranking. However, in reading the source details I noticed that the individual sources were “in relation to the size of the economy”. As the individual parts are in relation to the economy, that means that, when aggregated, the individual sources each contribute a small amount and the economy (common throughout) plays a greater roll as the aggregation takes place. If five of the individual scores of a section are in relation to the size of the economy, then in calculating the section aggregate the size of the economy is used five times more often than any one of the individual factors.

To explore this idea I am using GDP per capita on the grounds that if related to the size of the economy is minimising biases relating to country size and wealth, it may be introducing biases related to the welath of inhabitants.

Graphing the relationship between the Good Country rank and the GDP per capita, the graph looks like:



![](c001_Overall_files/figure-latex/unnamed-chunk-6-1.pdf)<!-- --> 

**My Interpretation and Opinions**:

The lack of entries in the lower right of the graph indicated that countries with a GDP per person of less than 30000 PPP dollars cannot be in the top 20 countries. Overall, given the wealth of inhabitants, New Zealand seems to be near to as highest ranked as it can be, without engaging in extraordinary actions. For example Moldova has a rank much lower (better) than many all other countries in its wealth band largely due to its peacekeeping efforts.

The group of countries in ranks 75 to 110 with higher GDP per capita are dominated by Middle Eastern Oil producing countries, and should perhaps be read as being displaced left from similar countries without the same resource extraction. If this group is excluded, then the graph forms a very strong overall pattern of maximum rank being linked to wealth.

This is not to say that wealth is the primary cause- only that wealth of inhabitants is a strong indicator, as there may be factors linked to wealth contributing to the outcome.




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
var_code <- "a_Overall"
var_prettynom <- "Overall"
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
