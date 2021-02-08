
install.packages("devtools")
devtools::install_github('PMassicotte/gtrendsR', ref = 'new-api')


library(gtrendsR)

USelections2016 <- gtrends(c("Donald Trump", "Hillary Clinton"), geo = c("US"), time = "2016-01-01 2016-12-31")
plot(USelections2016)
