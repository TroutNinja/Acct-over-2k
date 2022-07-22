Customers over Certain Dollar Amount
================

#### Clear Environment

``` r
rm(list = ls())
```

#### Load Packages

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.0.2

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.7     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1

    ## Warning: package 'tidyr' was built under R version 4.0.5

    ## Warning: package 'readr' was built under R version 4.0.5

    ## Warning: package 'forcats' was built under R version 4.0.2

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
library(writexl)
library(readxl)
```

    ## Warning: package 'readxl' was built under R version 4.0.5

``` r
library(stringr)
library(ggplot2)
library(dplyr)
library(lubridate)
```

    ## Warning: package 'lubridate' was built under R version 4.0.2

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

#### Load Dataset

``` r
pi <- read_xlsx("/Users/matt/Desktop/Puffin/Projects/_Puffin_Data/220526_Posted_Sales_Invoice.xlsx")
head(pi)
```

    ## # A tibble: 6 × 21
    ##   No.       `Document Date`     `Posting Date`      `Order No.` `External Docu…`
    ##   <chr>     <dttm>              <dttm>              <chr>       <chr>           
    ## 1 SI-109751 2022-05-26 00:00:00 2022-05-26 00:00:00 SO-107882   <NA>            
    ## 2 SI-109750 2022-05-26 00:00:00 2022-05-26 00:00:00 SO-107878   TRAVISMANSON    
    ## 3 SI-109749 2022-05-26 00:00:00 2022-05-26 00:00:00 SO-107865   202-12696       
    ## 4 SI-109748 2022-05-26 00:00:00 2022-05-26 00:00:00 SO-107858   4000-3015544    
    ## 5 SI-109747 2022-05-26 00:00:00 2022-05-26 00:00:00 SO-107809   107809          
    ## 6 SI-109746 2022-05-26 00:00:00 2022-05-26 00:00:00 SO-107875   JOEYANTONELLI   
    ## # … with 16 more variables: `Customer No.` <chr>, `Customer Name` <chr>,
    ## #   `Payment Terms Code` <chr>, `Currency Code` <chr>, `Due Date` <dttm>,
    ## #   `Shipment Date` <dttm>, Amount <dbl>, `Amount Including Tax` <dbl>,
    ## #   `Remaining Amount` <dbl>, `Location Code` <chr>, `No. Printed` <dbl>,
    ## #   Closed <dbl>, Canceled <dbl>, Corrective <dbl>,
    ## #   `Sales Rep Group Code` <chr>, `Salesperson Code` <chr>

#### Create new dataframe with filters

``` r
df <- pi %>%
  filter(`Document Date` > "2021-01-01") %>% 
  filter(`Customer Name` != 'Shopify') %>% 
  filter(`Customer Name` != 'Shopify Orders') %>% 
  filter(Canceled == 0)
```

#### Group DataFrame by SO & Customer

-   total the amount per SO

``` r
df_grouped <- df %>%
  group_by(`Order No.`, `Customer Name`) %>%
  summarise(Total_Amt = sum(Amount)) %>%
  ungroup()
```

    ## `summarise()` has grouped output by 'Order No.'. You can override using the
    ## `.groups` argument.

#### Find Customers with Avg Orders over $2k

``` r
customers_avg_over_2k <- df_grouped %>% 
  group_by(`Customer Name`) %>% 
  summarise(Avg_Order = mean(Total_Amt),
            Num_Orders = n()) %>% 
  filter(Avg_Order >= 2000) %>% 
  ungroup()
```

#### Subset these customers from the original dataframe

``` r
customers_2k <- subset(df, df$`Customer Name` %in% customers_avg_over_2k$`Customer Name`)
```

#### Determine Number of Orders Customers with $2k average have placed and compute the average per month

``` r
customers_2k_count <- customers_2k %>% 
  group_by(`External Document No.`, `Customer Name`) %>% 
  summarise(c = n()) %>% 
  ungroup()
```

    ## `summarise()` has grouped output by 'External Document No.'. You can override
    ## using the `.groups` argument.

``` r
customers_2k_count_avg <- customers_2k_count %>% count(`Customer Name`) %>% 
  mutate(average_per_month = n / 12) %>% 
  arrange(desc(average_per_month))
```

#### Find number of orders per month that these customers place

``` r
pi_over_2k <- customers_2k %>% 
  filter(Amount >= 2000, Canceled == 0)

orders_over_2k_per_month <- pi_over_2k %>%
  group_by(month = floor_date(`Posting Date`, "month")) %>%
  summarise(num_orders = n())

print(paste("2k Customers average orders per month = ", mean(orders_over_2k_per_month$num_orders)))
```

    ## [1] "2k Customers average orders per month =  23.2941176470588"

\####Write to Excel File

``` r
write_xlsx(customers_2k_count_avg, "Customers_Avg_Over_2k.xlsx")
```
