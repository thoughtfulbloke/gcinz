section_graph1 <- function(xaxiscol, axisname, gcidata, gdpdata){
  gci <- gcidata[,c("isoCode",xaxiscol)]
  names(gci) <- c("isoCode","gci_rank")
  axtitle <- paste("GDP per capita and", trimws(axisname), "Good Country Rank")
  gci %>%
    inner_join(gdpcap11, by=c("isoCode" = "Country.Code")) %>%
    filter(!is.na(X2011) & gci_rank != 0) %>% 
    mutate(country = ifelse(isoCode == "NZL", "New Zealand", "other")) %>%
    ggplot(aes(x=X2011, y=gci_rank, colour=country, shape=country)) + geom_point() + xlab("GDP per capita 2011") +
    ylab("Good Country Rank") + theme_minimal() + scale_color_viridis(discrete=TRUE, begin=0, end=0.9) +
    ggtitle(axtitle)
}

section_rank <- function(rankcolumn, ccode, gcidata, rankrange){
  nznear <- gci11[,c("Country.Name", "isoCode", rankcolumn)]
  names(nznear) <- c("Country", "isoCode", "Rank")
  nznear <- nznear %>% filter(Rank != 0) %>% arrange(desc(Rank))
  nzloc <- which(nznear$isoCode == "NZL")
  above <- ifelse(nzloc - rankrange < 1, 1, nzloc - rankrange )
  below <- ifelse(nzloc + rankrange > nrow(nznear), nrow(nznear), nzloc + rankrange )
  neighbours <- paste(c("(worse)", nznear$Country[above:below], "(better)"),collapse=", ")
  return(neighbours)
}