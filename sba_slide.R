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

setwd("/Users/usr/Desktop/")

t <- readRDS("/Users/usr/Desktop/tbl_df_nyc.rds")

summary(t)

## remove dispute transactions
t$type <- toupper(t$type)
d <- filter(t, type != "DIS")
d <- filter(d, amount > 0)

d <- mutate(d , ptype = case_when(
  type == "CRD" | type == "CRE" ~  "CREDIT",
  type == "CAS" | type == "CSH" ~  "CASH",
  type == "NA " | type == "UNK" ~  "UNKNOWN",
  type == "NOC" | type == "NO " ~  "NO CHARGE",
  TRUE ~ "OTHER"
))

d <- filter(d , ptype == "CREDIT" | ptype == "CASH")

## remove raw type row
d <- d[, c(1, 3, 4)]

summary(d)

by_date_type <- group_by(d, date, ptype)
by_date_type$amount <- by_date_type$amount / 1000

g <- ggplot(by_date_type, aes(x = date, y = amount, col = ptype))
g + geom_point() + theme(axis.text.x = element_text(angle = 75, hjust = 1))

