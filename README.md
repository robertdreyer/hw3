HW3 Stat 433
================
Robert Dreyer
2022-10-17

[robertdreyer github](https://github.com/robertdreyer/hw3)

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
#install.packages("nycflights13")
```

``` r
library(nycflights13)
library(ggplot2)
```

``` r
flights <- flights
airlines <- airlines
airports <-airports
planes <- planes
weather <- weather
```

Q1:Compute the average delay by destination, then join on the airports
data frame so you can show the spatial distribution of delays. Here’s an
easy way to draw a map of the United States:

``` r
flights2<-flights%>% 
    group_by(dest) %>%
    summarise(avg_delay = mean(arr_delay)) %>%
    inner_join(airports, by = c(dest = "faa"))

ggplot(flights2, aes(lon, lat, color = avg_delay)) +
    borders("state") +
    geom_point() +
    coord_quickmap()
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Q2: Add the location of the origin and destination (i.e. the lat and
lon) to flights.

``` r
airports %>%
    select(faa,lat,lon) %>%
    inner_join(flights, by = c(faa = "dest"))
```

    ## # A tibble: 329,174 x 21
    ##    faa     lat   lon  year month   day dep_time sched_dep_time dep_delay
    ##    <chr> <dbl> <dbl> <int> <int> <int>    <int>          <int>     <dbl>
    ##  1 ABQ    35.0 -107.  2013    10     1     1955           2001        -6
    ##  2 ABQ    35.0 -107.  2013    10     2     2010           2001         9
    ##  3 ABQ    35.0 -107.  2013    10     3     1955           2001        -6
    ##  4 ABQ    35.0 -107.  2013    10     4     2017           2001        16
    ##  5 ABQ    35.0 -107.  2013    10     5     1959           1959         0
    ##  6 ABQ    35.0 -107.  2013    10     6     1959           2001        -2
    ##  7 ABQ    35.0 -107.  2013    10     7     2002           2001         1
    ##  8 ABQ    35.0 -107.  2013    10     8     1957           2001        -4
    ##  9 ABQ    35.0 -107.  2013    10     9     1957           2001        -4
    ## 10 ABQ    35.0 -107.  2013    10    10     2011           2001        10
    ## # … with 329,164 more rows, and 12 more variables: arr_time <int>,
    ## #   sched_arr_time <int>, arr_delay <dbl>, carrier <chr>, flight <int>,
    ## #   tailnum <chr>, origin <chr>, air_time <dbl>, distance <dbl>, hour <dbl>,
    ## #   minute <dbl>, time_hour <dttm>

Q3: Is there a relationship between the age of a plane and its delays?

``` r
flights %>%
    group_by(tailnum) %>%
    summarise(delay = mean(arr_delay, na.rm = T)) %>%
    inner_join(planes, by = c(tailnum = "tailnum")) %>%
    mutate('age' = (2022 - year)) %>%
    ggplot(mapping = aes(x = age, y = delay)) +
    geom_point() 
```

    ## Warning: Removed 76 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

There is no definite relationship between the age of a plane and it’s
delay. Planes older than 40 years have similar delays to ages 10-30.

Q4: What weather conditions make it more likely to see a delay?

``` r
weather_flights <-
    flights %>%
    inner_join(weather, by = c(
        "origin" = "origin",
        "year" = "year",
        "month" = "month",
        "day" = "day",
        "hour" = "hour"
    ))

weather_flights %>%
  ggplot() +
    geom_point(mapping = aes(x = precip, y = dep_delay)) 
```

    ## Warning: Removed 8227 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
weather_flights %>%
  ggplot() +
    geom_point(mapping = aes(x = wind_speed, y = dep_delay)) 
```

    ## Warning: Removed 8305 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
weather_flights %>%
  ggplot() +
  geom_point(mapping = aes(x = humid, y = dep_delay)) 
```

    ## Warning: Removed 8244 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

``` r
weather_flights %>%
  ggplot() +
  geom_point(mapping = aes(x = dewp, y = dep_delay))
```

    ## Warning: Removed 8244 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-7-4.png)<!-- -->

``` r
weather_flights %>%
  ggplot() +
  geom_point(mapping = aes(x = temp, y = dep_delay))
```

    ## Warning: Removed 8244 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-7-5.png)<!-- --> Humidity,
Temperature, and Dewp are the most likely to cause to delays based on
the density of their plots. Dewp shows a positive correlation with
departure delays, with delay times increasing as dewp increases.

Q5: What happened on June 13 2013? Display the spatial pattern of
delays, and then use Google to cross-reference with the weather.

``` r
june13 <- flights %>% 
    filter(month == 6, day == 13, year == 2013) %>%
    group_by(hour) %>%
    inner_join(weather, by = c(
        "origin" = "origin",
        "year" = "year",
        "month" = "month",
        "day" = "day",
        "hour" = "hour"
    )) %>%
    summarise(avg_delay = mean(dep_delay, na.rm = TRUE),
              precip = mean(precip, na.rm = TRUE),
              dewpoint = mean(dewp, na.rm = TRUE),
              humidity = mean(humid, na.rm = TRUE),
              delay_counts = n())
june13
```

    ## # A tibble: 19 x 6
    ##     hour avg_delay  precip dewpoint humidity delay_counts
    ##  * <dbl>     <dbl>   <dbl>    <dbl>    <dbl>        <int>
    ##  1     5     -2.83 0           56.5     70.3            6
    ##  2     6      3.54 0           56.5     68.1           84
    ##  3     7      2.81 0.00406     57.2     70.2           69
    ##  4     8      7.91 0.02        57.5     73.4           73
    ##  5     9     23.2  0.0110      57.3     73.1           58
    ##  6    10     27.8  0.074       58.8     80.5           50
    ##  7    11     35.6  0.240       56.8     79.8           47
    ##  8    12     61.3  0           57.0     76.3           53
    ##  9    13     51.8  0           56.8     77.3           54
    ## 10    14     61.5  0           57.7     79.9           67
    ## 11    15     59.2  0           58.5     81.2           72
    ## 12    16     52    0           59.5     79.0           59
    ## 13    17     63.8  0           60.3     82.9           76
    ## 14    18     70    0           60.0     83.8           60
    ## 15    19     92.9  0.123       61.4     92.7           64
    ## 16    20     87.9  0.295       60.5     93.8           54
    ## 17    21     83.3  0.0816      58.7     94.6           31
    ## 18    22    103.   0.127       55.9     89.4            9
    ## 19    23     25.3  0.13        55.0     93.1            3

``` r
june13 %>%
    group_by(hour) %>%
    ggplot(mapping = aes(x = hour)) +
    geom_line(aes(y = avg_delay, color = "Avg Delay Time"))
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Starting in the afternoon, average delay times by hour began to increase
as humidity started increased into the evening This indicates that there
could have been a storm.
