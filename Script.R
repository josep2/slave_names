library(babynames)
library(dplyr)
library(data.table)
library(reshape2)
library(DT)
library(streamgraph)

###### JavaScript Configurations###

colDefs2 <- list(list(targets = 1, render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }")))

bar_string <- "type: 'bar', barColor: '#FF9966', negBarColor: 'purple', highlightColor: 'black'"
cb_bar = JS(paste0("function (oSettings, json) { $('.spark:not(:has(canvas))').sparkline('html', { ", 
                   bar_string, " }); }"), collapse = "")



data_set<- read.csv("names_data.csv")
data_set<- select(data_set, slave_name) %>% distinct(slave_name)
data("babynames")

joined = data_set %>% inner_join(babynames, by = c("slave_name" = "name")) %>% group_by(slave_name, year) %>% summarise(n = sum(n))

joined %>%
  group_by(year, name) %>%
  streamgraph("name", "n", "year")

ranges = range(joined$n)

completeData<- joined %>% group_by(slave_name) %>% summarise(count = paste(n, collapse = ","))

maxYear = joined %>% group_by(slave_name) %>% slice(which.max(year)) %>% select(year) %>% rename(maxYear = year)
minYear = joined %>% group_by(slave_name) %>% slice(which.min(year)) %>% select(year) %>% rename(minYear = year)
maxVal = joined %>% group_by(slave_name) %>% slice(which.max(n)) %>% select(n) %>% rename(maxCount = n)
minVal = joined %>% group_by(slave_name) %>% slice(which.min(n)) %>% select(n) %>% rename(minCount = n)

completeData=completeData%>% right_join(maxVal, by = "slave_name") %>% right_join(minYear, by = "slave_name") %>% right_join(maxYear, by = "slave_name")


options(DT.options = list(pageLength = 25))
d1 <- datatable(data.table(completeData), rownames = FALSE, options = list(columnDefs = colDefs2, 
                                                                     fnDrawCallback = cb_bar),
                colnames = c('Slave Name', 'Popularity (U.S.) Through 2014', 'Max Year #', 'First Year Appeared', 'Last Year Appeared')) 
d1$dependencies <- append(d1$dependencies, htmlwidgets:::getDependency("sparkline"))

d1




