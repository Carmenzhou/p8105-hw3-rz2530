hw3
================
Ruwen Zhou
10/8/2020

### Problem 1

``` r
data("instacart")
```

This dataset contains 1384617 rows and 15 columns.

Observations are the level of items in orders by user. There are user /
order variables – user ID, order ID, order day, and order hour. There
are also item variables – name, aisle, department, and some numeric
codes.

How many aisles, and which are most items from?

``` r
instacart %>% 
    count(aisle) %>% 
    arrange(desc(n))
```

    ## # A tibble: 134 x 2
    ##    aisle                              n
    ##    <chr>                          <int>
    ##  1 fresh vegetables              150609
    ##  2 fresh fruits                  150473
    ##  3 packaged vegetables fruits     78493
    ##  4 yogurt                         55240
    ##  5 packaged cheese                41699
    ##  6 water seltzer sparkling water  36617
    ##  7 milk                           32644
    ##  8 chips pretzels                 31269
    ##  9 soy lactosefree                26240
    ## 10 bread                          23635
    ## # … with 124 more rows

doc plot

``` r
instacart %>% 
    count(aisle) %>% 
    filter(n > 10000) %>% 
    mutate(
        aisle = factor(aisle),
        aisle = fct_reorder(aisle, n)
    ) %>% 
    ggplot(aes(x = aisle, y = n)) + 
    geom_point() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
```

<img src="hw3_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

plot table

``` r
instacart %>% 
    filter(aisle %in% c("baking ingredients", "dog food care", "packaged vegetables fruits")) %>% 
    group_by(aisle) %>% 
    count(product_name) %>% 
    mutate(rank = min_rank(desc(n))) %>% 
    filter(rank < 4) %>% 
    arrange(aisle, rank) %>% 
    knitr::kable()
```

| aisle                      | product\_name                                 |    n | rank |
| :------------------------- | :-------------------------------------------- | ---: | ---: |
| baking ingredients         | Light Brown Sugar                             |  499 |    1 |
| baking ingredients         | Pure Baking Soda                              |  387 |    2 |
| baking ingredients         | Cane Sugar                                    |  336 |    3 |
| dog food care              | Snack Sticks Chicken & Rice Recipe Dog Treats |   30 |    1 |
| dog food care              | Organix Chicken & Brown Rice Recipe           |   28 |    2 |
| dog food care              | Small Dog Biscuits                            |   26 |    3 |
| packaged vegetables fruits | Organic Baby Spinach                          | 9784 |    1 |
| packaged vegetables fruits | Organic Raspberries                           | 5546 |    2 |
| packaged vegetables fruits | Organic Blueberries                           | 4966 |    3 |

apple vs ice cream

``` r
instacart %>% 
    filter(product_name %in% c("Pink Lady Apples", "Coffee Ice Cream")) %>% 
    group_by(product_name, order_dow) %>% 
    summarize(mean_hour = mean(order_hour_of_day)) %>% 
    pivot_wider(
        names_from = order_dow,
        values_from = mean_hour
    )
```

    ## `summarise()` regrouping output by 'product_name' (override with `.groups` argument)

    ## # A tibble: 2 x 8
    ## # Groups:   product_name [2]
    ##   product_name       `0`   `1`   `2`   `3`   `4`   `5`   `6`
    ##   <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 Coffee Ice Cream  13.8  14.3  15.4  15.3  15.2  12.3  13.8
    ## 2 Pink Lady Apples  13.4  11.4  11.7  14.2  11.6  12.8  11.9

# Problem 2

\#\#Load and clean data

``` r
accel = 
  read_csv("./data/accel_data.csv") %>% 
  janitor::clean_names() %>%
  pivot_longer(
    activity_1:activity_1440,
    names_to = "minute",
    names_prefix = "activity_",
    values_to = "number_of_activity") %>% 
  drop_na("number_of_activity") %>% 
  mutate(
    minute = as.numeric(minute),
    number_of_activity = as.numeric(number_of_activity),
    week = as.character(week),
    day = as.factor(day),
    day_id = as.factor(day_id) 
  ) %>% 
  mutate(weekend = day %in% c("Sunday","Saturday"),
         weekday = day %in% c("Monday","Tuesday", "Wednesday","Thursday","Friday"),
         weekend_vs_weekday=
           case_when(weekend ~"weekend",
                     weekday ~ "weekday")) %>% 
  mutate(weekend_vs_weekday = as.factor(weekend_vs_weekday),
         day = forcats::fct_relevel(day,c("Monday","Tuesday","Wednesday","Thursday",
                                     "Friday", "Saturday","Sunday"))) %>% 
  group_by(week,day) %>% 
  arrange(day,.by_group = T) %>% 
  relocate(day_id, week, weekend_vs_weekday)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   day = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
accel
```

    ## # A tibble: 50,400 x 8
    ## # Groups:   week, day [35]
    ##    day_id week  weekend_vs_weekd… day    minute number_of_activ… weekend weekday
    ##    <fct>  <chr> <fct>             <fct>   <dbl>            <dbl> <lgl>   <lgl>  
    ##  1 2      1     weekday           Monday      1                1 FALSE   TRUE   
    ##  2 2      1     weekday           Monday      2                1 FALSE   TRUE   
    ##  3 2      1     weekday           Monday      3                1 FALSE   TRUE   
    ##  4 2      1     weekday           Monday      4                1 FALSE   TRUE   
    ##  5 2      1     weekday           Monday      5                1 FALSE   TRUE   
    ##  6 2      1     weekday           Monday      6                1 FALSE   TRUE   
    ##  7 2      1     weekday           Monday      7                1 FALSE   TRUE   
    ##  8 2      1     weekday           Monday      8                1 FALSE   TRUE   
    ##  9 2      1     weekday           Monday      9                1 FALSE   TRUE   
    ## 10 2      1     weekday           Monday     10                1 FALSE   TRUE   
    ## # … with 50,390 more rows

First, we read data from “accel\_data.csv” and clean it. Since the data
in the excel is in a format that hard to read, I used pivot\_longer and
count the number of activities of different time length. And then drop
null values and mutate the format of some variables for calculation.
Then we create weekend\_vs\_weekday column. The last step is to group,
relocate the data and make it more readable. Finally, this dataset has
50400 rows and 8 columns, with variables day\_id, week,
weekend\_vs\_weekday, day, minute, number\_of\_activity, weekend,
weekday.

## Traditinal analysis

``` r
accel %>% 
  summarize(total_activity = sum(number_of_activity)) %>% 
  pivot_wider(names_from = week,
              values_from = total_activity,
              names_prefix = "week") %>% 
  knitr::kable()
```

    ## `summarise()` regrouping output by 'week' (override with `.groups` argument)

| day       |     week1 |  week2 |  week3 |  week4 |  week5 |
| :-------- | --------: | -----: | -----: | -----: | -----: |
| Monday    |  78828.07 | 295431 | 685910 | 409450 | 389080 |
| Tuesday   | 307094.24 | 423245 | 381507 | 319568 | 367824 |
| Wednesday | 340115.01 | 440962 | 468869 | 434460 | 445366 |
| Thursday  | 355923.64 | 474048 | 371230 | 340291 | 549658 |
| Friday    | 480542.62 | 568839 | 467420 | 154049 | 620860 |
| Saturday  | 376254.00 | 607175 | 382928 |   1440 |   1440 |
| Sunday    | 631105.00 | 422018 | 467052 | 260617 | 138421 |
