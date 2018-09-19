# https://cloud.google.com/bigquery/public-data/nyc-tlc-trips
# https://bigquery.cloud.google.com/table/nyc-tlc:yellow.trips?tab=schema

rm(list = ls(all = TRUE))

##library(bigrquery)
library(tidyverse)
library(plotly)


# project <- "testds-215409"
# sql <- "SELECT
# substr(cast(pickup_datetime as String), 1, 7) as date
# ,payment_type as type
# ,sum(total_amount) as amount
# 
# FROM `nyc-tlc.yellow.trips`
# group by 1, 2"

#df <- query_exec(sql, project = project,
#                 use_legacy_sql = FALSE)
#plot_ly(df, x = ~date, y = ~amount,
#        color = ~type) %>% add_lines()

#saveRDS(df, "df.rds")

#setwd("/Users/usr/Desktop/")
setwd("~/GitHub/sba-1")

t <- readRDS("tbl_df_nyc.rds")

summary(t)

## remove dispute transactions
t$type <- toupper(t$type)
d <- filter(t, type != "DIS")
d <- filter(d, amount > 0)

d <- mutate(d , ptype = case_when(
  type == "CRD" | type == "CRE" ~  "CREDIT CARD",
  type == "CAS" | type == "CSH" ~  "CASH",
  type == "NA " | type == "UNK" ~  "UNKNOWN",
  type == "NOC" | type == "NO " ~  "NO CHARGE",
  TRUE ~ "OTHER"
))

d <- filter(d , ptype == "CREDIT CARD" | ptype == "CASH")

## remove raw type row
d <- d[, c(1, 3, 4)]

summary(d)

by_date_type <- group_by(d, date, ptype)
by_date_type$amount <- by_date_type$amount

library(lubridate)

by_date_type <- mutate(by_date_type, dtm = lubridate::parse_date_time(date, "y-m"))
by_date_type <- mutate(by_date_type, year = lubridate::year(dtm))

by_date_type_year <- group_by(by_date_type, year, ptype)
                       
s <- summarise(by_date_type_year, sum(amount))
s$Payment <- as.factor(s$ptype)
s$year <- as.factor(s$year)
s <- mutate(s, Amount = `sum(amount)`/1000000)

##g <- ggplot(by_date_type, aes(x = date, y = amount, col = ptype))
##g + geom_point() + theme(axis.text.x = element_text(angle = 75, hjust = 1))
##g + geom_point() + theme(axis.text.x = element_text(angle = 75, hjust = 1))

library(ggthemes)

g <- ggplot(s, aes(x = year, y = Amount))
g <- g + geom_bar(aes(fill = Payment), stat = "identity")
g <- g + theme_economist()
g <- g + labs(x = "Year", y = "Total amount (in millions USD)") + ggtitle("NYC Yellow Taxi Payments over time")
g



