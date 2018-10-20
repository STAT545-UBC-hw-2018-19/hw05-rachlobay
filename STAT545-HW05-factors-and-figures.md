STAT545-HW05-factor-and-figure-management
================
Rachel Lobay
2018-10-13

Table of Contents:

-   [Load required packages](#load-required-packages)
-   [The gapminder data set](#the-gapminder-data-set)
-   [Part 1: Factor management](#part-1-factor-management)
    -   [Drop Oceania and remove unused factor levels](#drop-oceania-and-remove-unused-factor-levels)
    -   [Reorder the levels of countries in the forcats package](#reorder-the-levels-of-countries-in-the-forcats-package)
    -   [Experiment with the arrange() function](#experiment-with-the-arrange-function)
-   [Part 2: File I/O](#part-2-file-io)
-   [Part 3: Visualization design](#part-3-visualization-design)
-   [Part 4: Writing figures to file](#part-4-writing-figures-to-file)
-   [But I want to do more!](#but-i-want-to-do-more)

Load required packages
======================

First, I will load the required packages along with the gapminder data set that I will be working with for my final assignment for STAT545A.

``` r
library(tidyverse)
library(knitr)
library(scales)
library(devtools) 
# devtools::install_github('thomasp85/gganimate') # install gganimate package this way because it is not installing when we use the install.packages() function with R 3.5.1. Also, gganimate is Github-based (not based on CRAN).
# webshot::install_phantomjs() was installed to include still of plot produced by plotly on Github
library(gganimate)
library(transformr) # load transformr package to draw line (part of gganimate plot later on)
library(plotly) 
library(gapminder)
```

The gapminder data set
======================

Since we are very familiarized with the gapminder data set because we have used it a lot in class and in previous assignments, I will only briefly introduce the data set.

Let's have a quick look at the head of the gapminder data set to remind ourselves what we are working with

``` r
head(gapminder) # view top six rows of gapminder data set
```

    ## # A tibble: 6 x 6
    ##   country     continent  year lifeExp      pop gdpPercap
    ##   <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
    ## 1 Afghanistan Asia       1952    28.8  8425333      779.
    ## 2 Afghanistan Asia       1957    30.3  9240934      821.
    ## 3 Afghanistan Asia       1962    32.0 10267083      853.
    ## 4 Afghanistan Asia       1967    34.0 11537966      836.
    ## 5 Afghanistan Asia       1972    36.1 13079460      740.
    ## 6 Afghanistan Asia       1977    38.4 14880372      786.

Let's also remind ourselves of how many rows there are in the gapminder data set.

``` r
nrow(gapminder)
```

    ## [1] 1704

We see that there are 1704 rows.

Ok, so we can clearly see from using the head function that we have the factors, country, continent, year, lifeExp, pop and gdpPercap to work with.

``` r
gapminder %>% 
  str() # look at the structure of the gapminder data set
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1704 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

In the str() function output for the gapminder data frame, focus in on the number of factor levels for continent. We can see there is currently 5 levels. Let's have a look at what exactly those levels are by using the levels() function

``` r
gapminder$continent %>% 
  levels() # see what the levels of gapminder$continent are.
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"   "Oceania"

We can see that the five levels are Africa, Americas, Asia, Europe, and Oceania.

Let's now have a quick look at the number of entries that correspond to the above continents.

``` r
gapminder$continent %>% 
fct_count() %>% 
  kable(col.names = c("Continent","Num")) 
```

| Continent |  Num|
|:----------|----:|
| Africa    |  624|
| Americas  |  300|
| Asia      |  396|
| Europe    |  360|
| Oceania   |   24|

This will change in Part 1 below, when we drop Oceania.

Part 1: Factor management
=========================

In this first part of the assignment, I will work with the gapminder data set. I have two major objectives, the first of which is to drop Oceania and remove unused factor levels. The second objective is to reorder the levels of the countries in the forcats package.

Drop Oceania and remove unused factor levels
--------------------------------------------

I will use the Base R: `droplevels()` which may operate on an entire data frame or a factor because all that I want to all entries related to Oceania in the gapminder data frame. To accomplish this, I must use the dplyr and forcats packages because I will be relying on various functions from those packages (as you will see below).

Next, I will filter all rows of the data frame for all continents but Oceania.

``` r
gap_no_Oceania <- gapminder %>% 
  filter(continent != "Oceania") # filters for rows for all continents but Oceania.
```

Let's check the number of rows of gap\_no\_Oceania. It should have 24 less than the 1704 rows from gapminder because we filtered out Oceania.

``` r
nrow(gap_no_Oceania)
```

    ## [1] 1680

Great. There are precisely 24 less rows than the 1704 rows from the gapminder data as we expected.

Now, what about the structure of the gap\_no\_Oceania data frame?

``` r
gap_no_Oceania %>% 
str()
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1680 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

Notice that there are still 5 levels for continent. Meaning, even though we have filtered out the rows corresponding to Oceania, we didn't remove Oceania. In support of this, we observe there are still the 142 levels for country, which is the same number of countries as in gapminder.

Now, what do we see when we check the levels of continent for gap\_no\_Oceania?

``` r
levels(gap_no_Oceania$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"   "Oceania"

The levels of continent still includes Oceania, even though we've filtered out the rows corresponding to that continent.

``` r
gap_no_Oceania$continent %>% 
fct_count() %>% 
  kable(col.names = c("Continent","Num")) 
```

| Continent |  Num|
|:----------|----:|
| Africa    |  624|
| Americas  |  300|
| Asia      |  396|
| Europe    |  360|
| Oceania   |    0|

This confirms our suspicions. There are 0 entries corresponding to Oceania, but Oceania is still listed under continent.

I can use the base R, droplevels() function to drop the unused factor levels for continent. So, we can use the droplevels() function to to remove unused factor level Oceania.

Note that we had to store the gap\_no\_Oceania into a variable (I called it gap\_no\_Oceania\_droplevels) to make the droplevels() function result last.

``` r
gap_no_Oceania_droplevels <- gap_no_Oceania %>% 
  droplevels() 
```

Now, we will view the structure of gap\_no\_Oceania\_droplevels.

``` r
gap_no_Oceania_droplevels %>%
  str() 
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1680 obs. of  6 variables:
    ##  $ country  : Factor w/ 140 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 4 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

By using the str() function, we can see that gap\_no\_Oceania\_droplevels now has four continent factor levels and not five as we saw before. Additionally, there are 140 levels for country and not 142 as we observed before we used droplevels().

We can confirm that Oceania was droppped in a visual way by using fct\_count().

``` r
gap_no_Oceania_droplevels$continent %>% 
fct_count() %>% 
  kable(col.names = c("Continent","Num")) 
```

| Continent |  Num|
|:----------|----:|
| Africa    |  624|
| Americas  |  300|
| Asia      |  396|
| Europe    |  360|

So, we do not have Oceania under continent now.

Let's check to see that Oceania was the continent factor level that was removed!

``` r
gap_no_Oceania_droplevels$continent %>% 
  levels() # see what the levels of gap_no_Oceania$continent are.
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"

Success! We can see that we only have the four continents, Africa, Americas, Asia and Europe as our factor levels for continent. We no longer have Oceania as a factor level for continent. So we succeeded in dropping the unused level from the continent factor in the gapminder data frame.

We can also check that the countries corresponding to Oceania were dropped in gap\_no\_Oceania\_droplevels.

First, let's view the countries of Oceania from the gapminder data frame.

``` r
gapminder %>% 
  filter(continent == "Oceania") %>% 
  select(country) %>% 
  unique()  # see what the unique countries are in Oceania
```

    ## # A tibble: 2 x 1
    ##   country    
    ##   <fct>      
    ## 1 Australia  
    ## 2 New Zealand

Notice that the only countries corresponding to Oceania in the gapminder data frame are Australia and New Zealand. So, we would expect Australia and New Zealand to be missing in gap\_no\_Oceania\_droplevels$country.

``` r
gap_no_Oceania_droplevels %>% 
  filter(country %in% c("Australia", "New Zealand")) # filters to Australia and New Zealand rows
```

    ## # A tibble: 0 x 6
    ## # ... with 6 variables: country <fct>, continent <fct>, year <int>,
    ## #   lifeExp <dbl>, pop <int>, gdpPercap <dbl>

We can see that there are no rows in the resulting tibble. Hence, we have confirmed that there are no Australia and New Zealand rows that were found in gap\_no\_Oceania\_droplevels$country.

Instead of using droplevels(), we could have used fct\_drop(). The difference is that fct\_drop() would drop only the levels corresponding to continent, whereas droplevels() impacted the data frame. For example, we saw that droplevels() impacted the country factor (we only had 140 countries in gap\_no\_Oceania\_droplevels instead of the original 142 as in gapminder).

Let's try out fct\_drop() to only drop the continent levels and see the results for ourselves!

``` r
gap_no_Oceania_fct <- gap_no_Oceania %>% 
  mutate(continent = fct_drop(continent))
```

Let's check the number of rows in gap\_no\_Oceania\_fct.

``` r
nrow(gap_no_Oceania_fct)
```

    ## [1] 1680

As was the case for droplevels(), we get 1680 rows (corresponding to just the Oceania rows being removed).

``` r
levels(gap_no_Oceania_fct$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"

The levels of gap\_no\_Oceania\_fct$continent are all the continents but Oceania, as was the case when we used droplevels().

Now, if we look at the structure of gap\_no\_Oceania\_fct, what do we see?

``` r
gap_no_Oceania_fct %>% 
  str()
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1680 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 4 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

Notice, we see 142 countries (like in the original gapminder data frame) and not 140 as we saw when we used droplevels(). This illustrates the difference between fct\_drop() and droplevels().

Even so, are Australia and New Zealand even used? They shouldn't be, but let's check this.

``` r
gap_no_Oceania_fct %>% 
  filter(country %in% c("Australia", "New Zealand")) # filters to Australia and New Zealand rows
```

    ## # A tibble: 0 x 6
    ## # ... with 6 variables: country <fct>, continent <fct>, year <int>,
    ## #   lifeExp <dbl>, pop <int>, gdpPercap <dbl>

We see that the countries Australia and New Zealand were not used, but they were not removed as in the case with droplevels(). So, we can reason that the droplevels() function is more useful than fct\_drop() in this case.

Reorder the levels of countries in the forcats package
------------------------------------------------------

Here, I will use the forcats package to change the order of the factor levels of a subset of countries in the gapminder data frame. I will base my reordering choice on the quantitative variable meanlifeExp. I will create meanlifeExp and use those means to analyze the life expectancy differences of the countries.

Now, we will use the ggplot2 package to do factor level reordering and display the results graphically. Specifically, we will filter for just the rows of Europe and display the life expectancy of the European countries in a scatterplot.

``` r
gap_europe <- gapminder %>% 
  filter(continent == "Europe") %>% # filter the rows corresponding to Europe data
  group_by(country) %>% 
  summarize(meanlifeExp = mean(lifeExp))

ggplot(gap_europe, aes(meanlifeExp, country)) + 
  geom_point() + # plot the plain old gap_europe
  ggtitle("Mean life exp. of the European countries") # add title
```

![](STAT545-HW05-factors-and-figures_files/figure-markdown_github/unnamed-chunk-23-1.png)

We can see the life expectancies are unordered. The above scatterplot looks rather chaotic and difficult to interpret.

We will first view the mean life expectancies of each country in a table to plainly see the meanlifeExp of each country.

``` r
gap_europe %>% 
  kable()
```

| country                |  meanlifeExp|
|:-----------------------|------------:|
| Albania                |     68.43292|
| Austria                |     73.10325|
| Belgium                |     73.64175|
| Bosnia and Herzegovina |     67.70783|
| Bulgaria               |     69.74375|
| Croatia                |     70.05592|
| Czech Republic         |     71.51050|
| Denmark                |     74.37017|
| Finland                |     72.99192|
| France                 |     74.34892|
| Germany                |     73.44442|
| Greece                 |     73.73317|
| Hungary                |     69.39317|
| Iceland                |     76.51142|
| Ireland                |     73.01725|
| Italy                  |     74.01383|
| Montenegro             |     70.29917|
| Netherlands            |     75.64850|
| Norway                 |     75.84300|
| Poland                 |     70.17692|
| Portugal               |     70.41983|
| Romania                |     68.29067|
| Serbia                 |     68.55100|
| Slovak Republic        |     70.69608|
| Slovenia               |     71.60075|
| Spain                  |     74.20342|
| Sweden                 |     76.17700|
| Switzerland            |     75.56508|
| Turkey                 |     59.69642|
| United Kingdom         |     73.92258|

We will now use the forcats package to manipulate the order of the factor levels of country specific to this subset of the gapminder data set.

Suppose that we wanted the mean to be in order in ascending or descending order. We will try that out below by employing the forcats package! We will use the `fct_reorder` function to do this because we want to reorder the factor levels by arranging according to another variable.

Note that `fct_reorder()` by default orders by the median weighted lifeExp. It computes a summary statistic (default: median) for each category in the factor.

Now, we will plot the mean life expectancy by country using ggplot2.

``` r
gap_europe %>% 
  mutate(country = fct_reorder(country, meanlifeExp)) %>% 
  ggplot(aes(meanlifeExp, country)) + 
  geom_point() + # plot a scatterplot of the above
  ggtitle("Mean life exp. of European countries (in ascending order)") # add title
```

![](STAT545-HW05-factors-and-figures_files/figure-markdown_github/unnamed-chunk-25-1.png)

The resulting scatterplot is much more organized and easy to interpret. For example, we can easily spot that Iceland has the highest mean life expetancy of the European countries, which is around 76.5 years old. In conclusion, we should probably all move to Iceland if we want to live long lives (just kidding).

We may set the parameter of the fct\_reorder() function to .desc = FALSE, but that is unnecessary because, by default fct\_reorder() orders from lowest to highest.

Now, we shall look use `fct_reorder` to view a graph of the contients ordered in descending order. Note that we did set .desc = TRUE to achieve that goal in a quick and easy way.

``` r
gap_europe %>% 
  mutate(country = fct_reorder(country, meanlifeExp, .desc = TRUE)) %>% # reorder the European countries mean life expectancy in descending order
  ggplot(aes(meanlifeExp, country)) +
  geom_point() + # plot a scatterplot
  ggtitle("Mean life exp. of European countries (in decreasing order)") # add title
```

![](STAT545-HW05-factors-and-figures_files/figure-markdown_github/unnamed-chunk-26-1.png)

The same information is conveyed as in the above scatterplot for the ascending order of the mean life expectancy of the European countries. So, we can still see that Iceland has the highest mean life expectancy at around 76.5 years old and Turkey has the lowest mean life expectancy at just under 60 years old. I should not that for this data, it probably makes more sense for the first scatterplot (in ascending order) for easy readability of the mean life expectancies of the European countries.

Now, suppose we wanted to hone in on the top five countries with high mean life expectancies (according to the results of our above scatterplots), which were Iceland, Sweden, Norway, Netherlands, and Switzerland. So we create a subset of the data from the original gapminder data so we can trace their lifeExp over time later on.

``` r
select_countries <- c("Iceland", "Sweden", "Norway", "Netherlands", "Switzerland")

gap_select <- gapminder %>%  # df of just the 5 countries
  filter(country %in% select_countries) %>% 
  droplevels() # drop unused levels

gap_select %>% 
  kable() # kable table it
```

| country     | continent |  year|  lifeExp|       pop|  gdpPercap|
|:------------|:----------|-----:|--------:|---------:|----------:|
| Iceland     | Europe    |  1952|   72.490|    147962|   7267.688|
| Iceland     | Europe    |  1957|   73.470|    165110|   9244.001|
| Iceland     | Europe    |  1962|   73.680|    182053|  10350.159|
| Iceland     | Europe    |  1967|   73.730|    198676|  13319.896|
| Iceland     | Europe    |  1972|   74.460|    209275|  15798.064|
| Iceland     | Europe    |  1977|   76.110|    221823|  19654.962|
| Iceland     | Europe    |  1982|   76.990|    233997|  23269.607|
| Iceland     | Europe    |  1987|   77.230|    244676|  26923.206|
| Iceland     | Europe    |  1992|   78.770|    259012|  25144.392|
| Iceland     | Europe    |  1997|   78.950|    271192|  28061.100|
| Iceland     | Europe    |  2002|   80.500|    288030|  31163.202|
| Iceland     | Europe    |  2007|   81.757|    301931|  36180.789|
| Netherlands | Europe    |  1952|   72.130|  10381988|   8941.572|
| Netherlands | Europe    |  1957|   72.990|  11026383|  11276.193|
| Netherlands | Europe    |  1962|   73.230|  11805689|  12790.850|
| Netherlands | Europe    |  1967|   73.820|  12596822|  15363.251|
| Netherlands | Europe    |  1972|   73.750|  13329874|  18794.746|
| Netherlands | Europe    |  1977|   75.240|  13852989|  21209.059|
| Netherlands | Europe    |  1982|   76.050|  14310401|  21399.460|
| Netherlands | Europe    |  1987|   76.830|  14665278|  23651.324|
| Netherlands | Europe    |  1992|   77.420|  15174244|  26790.950|
| Netherlands | Europe    |  1997|   78.030|  15604464|  30246.131|
| Netherlands | Europe    |  2002|   78.530|  16122830|  33724.758|
| Netherlands | Europe    |  2007|   79.762|  16570613|  36797.933|
| Norway      | Europe    |  1952|   72.670|   3327728|  10095.422|
| Norway      | Europe    |  1957|   73.440|   3491938|  11653.973|
| Norway      | Europe    |  1962|   73.470|   3638919|  13450.402|
| Norway      | Europe    |  1967|   74.080|   3786019|  16361.876|
| Norway      | Europe    |  1972|   74.340|   3933004|  18965.056|
| Norway      | Europe    |  1977|   75.370|   4043205|  23311.349|
| Norway      | Europe    |  1982|   75.970|   4114787|  26298.635|
| Norway      | Europe    |  1987|   75.890|   4186147|  31540.975|
| Norway      | Europe    |  1992|   77.320|   4286357|  33965.661|
| Norway      | Europe    |  1997|   78.320|   4405672|  41283.164|
| Norway      | Europe    |  2002|   79.050|   4535591|  44683.975|
| Norway      | Europe    |  2007|   80.196|   4627926|  49357.190|
| Sweden      | Europe    |  1952|   71.860|   7124673|   8527.845|
| Sweden      | Europe    |  1957|   72.490|   7363802|   9911.878|
| Sweden      | Europe    |  1962|   73.370|   7561588|  12329.442|
| Sweden      | Europe    |  1967|   74.160|   7867931|  15258.297|
| Sweden      | Europe    |  1972|   74.720|   8122293|  17832.025|
| Sweden      | Europe    |  1977|   75.440|   8251648|  18855.725|
| Sweden      | Europe    |  1982|   76.420|   8325260|  20667.381|
| Sweden      | Europe    |  1987|   77.190|   8421403|  23586.929|
| Sweden      | Europe    |  1992|   78.160|   8718867|  23880.017|
| Sweden      | Europe    |  1997|   79.390|   8897619|  25266.595|
| Sweden      | Europe    |  2002|   80.040|   8954175|  29341.631|
| Sweden      | Europe    |  2007|   80.884|   9031088|  33859.748|
| Switzerland | Europe    |  1952|   69.620|   4815000|  14734.233|
| Switzerland | Europe    |  1957|   70.560|   5126000|  17909.490|
| Switzerland | Europe    |  1962|   71.320|   5666000|  20431.093|
| Switzerland | Europe    |  1967|   72.770|   6063000|  22966.144|
| Switzerland | Europe    |  1972|   73.780|   6401400|  27195.113|
| Switzerland | Europe    |  1977|   75.390|   6316424|  26982.291|
| Switzerland | Europe    |  1982|   76.210|   6468126|  28397.715|
| Switzerland | Europe    |  1987|   77.410|   6649942|  30281.705|
| Switzerland | Europe    |  1992|   78.030|   6995447|  31871.530|
| Switzerland | Europe    |  1997|   79.370|   7193761|  32135.323|
| Switzerland | Europe    |  2002|   80.620|   7361757|  34480.958|
| Switzerland | Europe    |  2007|   81.701|   7554661|  37506.419|

``` r
gap_select %>% 
  ggplot(aes(year, lifeExp)) +
  geom_line(aes(group=country, colour=country)) + # plot of life expectancy over time for those 5 countries
  ggtitle("Mean life exp. over time for the top five European countries with highest mean life exp.") # add title
```

![](STAT545-HW05-factors-and-figures_files/figure-markdown_github/unnamed-chunk-28-1.png)

Now, we will use `fct_reorder2(f, x, y)` to reorder factor `f`, which for our example would be country. The function looks at max year (x) value and takes corresponding life expectancy (y) value. It will reorder the countries according to the life expectancy at the very end.

``` r
gap_select %>% 
  mutate(country = fct_reorder2(country, year, lifeExp)) %>% 
ggplot(aes(year, lifeExp)) +
  geom_line(aes(colour=country)) + # plot of life expectancy over time for those 5 countries
  ggtitle("Life exp. over time for the top five European countries with highest mean life exp.") # add title
```

![](STAT545-HW05-factors-and-figures_files/figure-markdown_github/unnamed-chunk-29-1.png)

Notice that when we used `fct_reorder2()`, Iceland went first, then Switzerland, then Sweden. This order corresponds to the reordering of the five countries by their last life expectancy (which would be in 2007). We can confirm that those countries have the highest mean life exp in 2007 by using the filter function to filter for the year 2007.

``` r
gap_select_2007 <- gap_select %>% 
   filter(year == 2007) 

 gap_select_2007 %>% 
  kable()
```

| country     | continent |  year|  lifeExp|       pop|  gdpPercap|
|:------------|:----------|-----:|--------:|---------:|----------:|
| Iceland     | Europe    |  2007|   81.757|    301931|   36180.79|
| Netherlands | Europe    |  2007|   79.762|  16570613|   36797.93|
| Norway      | Europe    |  2007|   80.196|   4627926|   49357.19|
| Sweden      | Europe    |  2007|   80.884|   9031088|   33859.75|
| Switzerland | Europe    |  2007|   81.701|   7554661|   37506.42|

Notice, that we could reorder the rows of that table by highest life expectancy to easily pinpoint the top three highest life expectancy. This is where the arrange() function comes into play.

Experiment with the arrange() function
--------------------------------------

I want to explore the arrange() function a little on the top five European countries in terms of life expectancy in 2007.

We can arrange that data set in terms of increasing life expectancy by reordering the factor levels. Let's do that below. Note that the following code is based on Jenny's from [here](https://www.stat.ubc.ca/~jenny/STAT545A/block08_bossYourFactors.html#specialty-functions-for-making-factors).

``` r
gap_select_2007_ascend <- within(gap_select_2007, country <- reorder(country, lifeExp))

levels(gap_select_2007_ascend$country) # View the order of the levels of the countries. We expect them to be arranged in ascending order of lifeExp.
```

    ## [1] "Netherlands" "Norway"      "Sweden"      "Switzerland" "Iceland"

However, if we look at gap\_select\_2007\_ascend, the row order has not changed to be arranged in order of ascending mean lifeExp.

Let's view it as a scatterplot to see see if the reordering of the levels impacts the scatterplot arrangement.

``` r
gap_select_2007 %>% 
  ggplot(aes(lifeExp, country)) + 
  geom_point() + # plot a scatterplot of life expectancy versus country for the five European countries with the highest life expectancies in 2007
  ggtitle("Life expectancy vs. country for the top five European countries in 2007") # add title
```

![](STAT545-HW05-factors-and-figures_files/figure-markdown_github/unnamed-chunk-32-1.png)

Interestingly, we got what we wanted. We wanted the order to be Netherlands, Norway, Sweden, Switzerland and then Iceland to be in order of ascending life expectancy.

As for the table issue, the arrange function comes to our rescue. This function from the plyr package lets us reorder the factor levels and make the new row order remain. As Jenny stated on her webpage (see the above link), the arrange function "is a nice wrapper around the built-in function order()".

Let's view a table of the results when we use the arrange function.

``` r
gap_select_2007_ascend_arr <- arrange(gap_select_2007_ascend, country)

gap_select_2007_ascend_arr %>% 
  kable()
```

| country     | continent |  year|  lifeExp|       pop|  gdpPercap|
|:------------|:----------|-----:|--------:|---------:|----------:|
| Netherlands | Europe    |  2007|   79.762|  16570613|   36797.93|
| Norway      | Europe    |  2007|   80.196|   4627926|   49357.19|
| Sweden      | Europe    |  2007|   80.884|   9031088|   33859.75|
| Switzerland | Europe    |  2007|   81.701|   7554661|   37506.42|
| Iceland     | Europe    |  2007|   81.757|    301931|   36180.79|

So, we did get that the rows were arranged in order of increasing lifeExp when we used arrange().

``` r
gap_select_2007_ascend_arr %>% # use the arrange function to display the countries in order of ascending life expectancy
  ggplot(aes(lifeExp, country)) + 
  geom_point() + # plot a scatterplot of life expectancy versus country for the five countries with the highest  life expectancies in 2007
  ggtitle("Life expectancy vs. country for the top five European countries in 2007") # add title
```

![](STAT545-HW05-factors-and-figures_files/figure-markdown_github/unnamed-chunk-34-1.png)

Additionally, the scatterplot is still exactly what we wanted. The European countries are arranged according in ascending order of life expectancy in 2007. So, the arrange() function doesn't appear to have an impact on figures.

Part 2: File I/O
================

I will experiment with write\_csv()/read\_csv(). Since we have already played with and filtered to get the five countries with the highest life expectancy in 2007 in the gapminder data frame, we will use what we have and see how it does when we write a csv of it. We will look at both versions, when the the rows are alphabetically ordered by country and when the rows are in ascending order of life expectancy in 2007.

First, we will look at the gap\_select\_2007, which was the output for the five countries with the highest life expectancy in 2007 and the rows are alphabetically ordered by country.

``` r
# Write gap_select_2007.csv 
write.csv(gap_select_2007, file = "gap_select_2007.csv") 

# Read in gap_select_2007.csv 
gap_select_2007_read <- read.csv("gap_select_2007.csv") 

gap_select_2007_read # view the read-in gap_select_2007.csv
```

    ##   X     country continent year lifeExp      pop gdpPercap
    ## 1 1     Iceland    Europe 2007  81.757   301931  36180.79
    ## 2 2 Netherlands    Europe 2007  79.762 16570613  36797.93
    ## 3 3      Norway    Europe 2007  80.196  4627926  49357.19
    ## 4 4      Sweden    Europe 2007  80.884  9031088  33859.75
    ## 5 5 Switzerland    Europe 2007  81.701  7554661  37506.42

We see that we have still retained the same alphabetical order by country. All six variables that were originally in gap\_select\_2007, which were country, continent, year, lifeExp, pop and gdpPercap are still there and their values have not changed. The only major difference that I can spot is that we now have a new column X which has five entries ranging from 1 to 5. Why is that happening? One explanation that I found from [here](https://stackoverflow.com/questions/10441437/why-am-i-getting-x-in-my-column-names-when-reading-a-data-frame) states that "read.csv() is a wrapper around the more general read.table() function". The behavior of adding a new column with an X header is consistent with the behaviour of the read.table() function. If we don't want this to happen, we can try to add `row.names = FALSE` to the write.csv() function to override that behaviour of adding an extra column for the rows with 1 to 5. Let's try that and see if we can get just the variables that we want.

``` r
# Write gap_select_2007.csv 
write.csv(gap_select_2007, file = "gap_select_2007_2.csv", row.names=FALSE) 

# Read in gap_select_2007.csv 
gap_select_2007_read_2 <- read.csv("gap_select_2007_2.csv") 

gap_select_2007_read_2 # let's see what we got when we read in the CSV
```

    ##       country continent year lifeExp      pop gdpPercap
    ## 1     Iceland    Europe 2007  81.757   301931  36180.79
    ## 2 Netherlands    Europe 2007  79.762 16570613  36797.93
    ## 3      Norway    Europe 2007  80.196  4627926  49357.19
    ## 4      Sweden    Europe 2007  80.884  9031088  33859.75
    ## 5 Switzerland    Europe 2007  81.701  7554661  37506.42

Excellent, that is precisely what we want. The gap\_select\_2007\_read\_2 looks exactly like gap\_select\_2007.

Next, I will look at the when the rows are in ascending order of life expectancy in 2007. In particular, we should look to see if the rows are still arranged in order of ascending life expectancy when we read in the CSV.

``` r
# Write gap_select_2007_ascend_arr.csv
write.csv(gap_select_2007_ascend_arr, file = "gap_select_2007_ascend_arr.csv", row.names=FALSE) 

# Read in gap_select_2007_ascend_arr.csv
gap_select_2007_ascend_arr1 <- read.csv("gap_select_2007_ascend_arr.csv") 

gap_select_2007_ascend_arr1 # let's see what we got when we read in the CSV
```

    ##       country continent year lifeExp      pop gdpPercap
    ## 1 Netherlands    Europe 2007  79.762 16570613  36797.93
    ## 2      Norway    Europe 2007  80.196  4627926  49357.19
    ## 3      Sweden    Europe 2007  80.884  9031088  33859.75
    ## 4 Switzerland    Europe 2007  81.701  7554661  37506.42
    ## 5     Iceland    Europe 2007  81.757   301931  36180.79

Interesting. We had gap\_select\_2007\_ascend\_arr in order of ascending life expectancy and when we import the CSV file that we wrote using that data, the row order was preserved.

What if we look at the levels of gap\_select\_2007\_ascend\_arr, which were in order of ascending life expectancy? Did that survive the CSV round trip to gap\_select\_2007\_ascend\_arr1?

``` r
levels(gap_select_2007_ascend_arr$country) # order of ascending life expectancy
```

    ## [1] "Netherlands" "Norway"      "Sweden"      "Switzerland" "Iceland"

``` r
levels(gap_select_2007_ascend_arr1$country) # after CSV round trip
```

    ## [1] "Iceland"     "Netherlands" "Norway"      "Sweden"      "Switzerland"

Nope. It looks like the levels after the CSV round trip were rearranged in alphabetical order.

Part 3: Visualization design
============================

In the previous HW03 for this course, I tried to use gganimate, which is one kind of package that can enhance ggplot2 by animating ggplot2 graphics. I will try to explore that again and see if, with a better understanding of ggplot2, I can create a plot that is both informative and that more effectively uses the gganimate package.

Now, I will show what I made in HW03 by using ggnimate. The following plot/animation illustrates a line chart over time of the mean gdpPercap of Singapore, Kuwait, and Hong Kong. I chose to follow the gdpPercap of those three places over time because they had a high mean GDP per capita of over 25,000 PPP dollars in 2007 (I explored that more in HW03). I thought that revealing the meaan gdpPercap over time would be interesting because then you don't see the whole picture immediately. You can focus more on how the mean gdpPercap got there.

``` r
lifeExp.bycontyr <- gapminder %>% 
  select(country, year, gdpPercap) %>% # only select country, year, and gdpPercap columns
  filter((country == "Kuwait" | country == "Singapore" | country == "Hong Kong, China") & year <= 1992) %>%
  group_by(country, year) %>% # group by country and year
  mutate(meangdp = mean(gdpPercap)) %>% # add mean gdpPercap columnr
  ggplot(aes(x =  year, y = meangdp, color = country, group = country)) +
  geom_path(size = 2, alpha = 0.4, arrow = arrow()) + # lines of the mean gdpPercap of the 3 countries over the years
  # gganimate code parts are below 
  transition_reveal(year, year) # transitions are the years

lifeExp.bycontyr # let's see the animation!
```

![](STAT545-HW05-factors-and-figures_files/figure-markdown_github/unnamed-chunk-40-1.gif)

One of the problems that I encountered with this animation was that it ran at a decent speed on my RStudio, but when I pushed it to Github, the animation ran at a snail's pace. I was bummed to learn that there wasn't an quick and easy fix to this becaue the transition\_reveal() is still being worked on. So, I left it at that.

Now, I am aiming to display different information from the gapminder data set and use the gganimate package. What I want to display is how the GDP per capita vs. populations of Americas and Europe changed over time. The size of the points corresponds to the mean gdpPercap at that time. For the base of the following code, see [here](https://github.com/thomasp85/gganimate).

``` r
Am_Euro_gap <- gapminder %>% 
filter(continent == "Americas" | continent == "Europe") %>% 
group_by(continent, year) %>% # group by continent and year
mutate(meangdp = mean(gdpPercap)) # add mean gdpPercap columnn

Am_Euro_gap_ggplot2 <- Am_Euro_gap %>% 
ggplot(aes(pop, gdpPercap, size = meangdp, colour = country)) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  scale_x_log10(labels = comma_format()) + # scale the gdpPercap by log10 and put in comman format
  scale_y_continuous(labels = dollar_format(), 
                     breaks = seq(10000, 50000, by=5000)) +
  facet_wrap(~ continent) # facet_wrap by continent

  Am_Euro_gap_ggplot2 + 
  # gganimate code parts are below 
  labs(title = 'Year: {frame_time}   GDP per cap vs. pop. of Americas and Europe over time', x = 'population', y = 'GDP per capita') +
  transition_time(year) +
  ease_aes('linear')
```

![](STAT545-HW05-factors-and-figures_files/figure-markdown_github/unnamed-chunk-41-1.gif)

This side-by-side animated plot gives us a feel for how the population vs GDP per capita changed over time for Americas and Europe. It is more general than precise. It allows us to see general patterns, and then we could construct more specific plots for the patterns that we wish to examine.

We can see a couple things right off the bat on the plot. Overall, we can see that as the populations increase, the European countries tyipically have a higher GDP per capita than the countries from the Americas as the years go by. Also note that the size of the points for Europe quickly get much larger than the Americas, meaning that the mean GDP per capita of the European countries gets larger than the Americas countries over time. Another thing to note is that it appears that the GDP per capita of the European countries rise relatively close together as as the population of the European countries stabilizes. On the other hand, for the GDP per capita of the Americas countries, there appears to be two countries which have a relatively stable high population and for which their GDP per capita rise above the rest. Another related observation about the Americas is that the population of the majority of the countries increases over time but their GDP per capita does not get nearly as large as the two countries whose GDP per capita rose above the rest.

We can try to use use the plotly package to accomplish something similar to what gganimate did. Let's examine how exactly the plot produced by plotly differs from the plot that we produced using gganimate.

Also, I installed phantomjs() by using `webshot::install_phantomjs()` to display a preview shot of the plot produced using plotly on Github.

First, we can try the easy option and just use ggplotly on Am\_Euro\_gap\_ggplot to see what we get.

``` r
ggplotly_Am_Euro <- ggplotly(Am_Euro_gap_ggplot2) # ggplotly Am_Euro_gap_ggplot2 to see what we get.

htmlwidgets::saveWidget(ggplotly_Am_Euro, file = "ggplotly_Am_Euro.html")
```

The actual plotly plot is may be interacted with [here](https://raw.githack.com/STAT545-UBC-students/hw05-rachlobay/master/ggplotly_Am_Euro.html).

The result looks like art, but doesn't show the transition over time like gganimate did. However, it is cool that if we hover over the plot produced by plotly, we can see the important information - what the population, GDP per capita, mean GDP per capita, and what country that the spot that we are hovering over pertains to. However, there are some drawbacks to using ggplotly. For example, when I tried to animate the above ggplotly, it became very difficult very quick because the following `animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")` line in particular threw an error that is difficult to fix.

On the other hand, if we manually attach am\_Euro\_gap and manually create a plot\_ly of that data, it becomes easier to animate it. For the base of the following code, see [here](https://plot.ly/r/animations/).

``` r
attach(Am_Euro_gap) # attach am_Euro_gap 

Am_Euro_gap_plotly <-   plot_ly(
    x = ~pop, 
    y = ~gdpPercap, 
    size = ~meangdp, 
    color = ~country, 
    frame = ~year, 
    text = ~country, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )
  )
# plotly version of how the GDP per capita and populations of Americas and Europe changed over time

plotly_Am_Euro_gap <- Am_Euro_gap_plotly %>% 
  animation_opts(1000, easing = "elastic", redraw = FALSE)  %>% 
  animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom") %>% # add an animation button
animation_slider(currentvalue = list(prefix = "YEAR ", font = list(color = "red"))) # add a slider

htmlwidgets::saveWidget(plotly_Am_Euro_gap, file = "plotly_Am_Euro_gap.html")
```

The interactive plot is found [here](https://raw.githack.com/STAT545-UBC-students/hw05-rachlobay/master/plotly_Am_Euro_gap.html).

This basically does what gganimate does - we can track how population vs GDP per capita changed over time for Americas and Europe. One awesome and informative aspect of this plot is that if we hover over the plot produced by plotly, we can see what particular country each dot corresponds to. Hence, plotly makes it easier to track particular countries over time than ggnimate does. The drawback was that I couldn't easily find an equivalent to `facet_wrap()` like we have in ggplot2 so that the plots are side-by-side for the Americas and Europe. So, the countries for Americas and Europe are on the same plot, which is not the separation we were looking for.

Part 4: Writing figures to file
===============================

I will use `ggsave()` to save a plot to file. Following that, I will use `![Alt text](/path/to/img.png)` to load and embed that figure in my report.

It is worth mentioning that if we have just produced the plot, we can simply type `ggsave("SPIFFY_PLOT_NAME.png")` to save our nifty plot. I will use a simple violin plot with jitter for life expectancy separated by continent to demonstrate this. When I use ggsave(), I will scale the plot to be 3/4 of its original size. We should see this size difference when we load and embed the figure after.

When we use the scale argument, the number that is put in to make the figure bigger or smaller is rather counterintuitive. For a scale number less than 1, the plot will be bigger and for scale number greater than 1 the figure will be smaller.Also, the scale argument changes the size of the text on your plot (meaning the title and axes labels).

``` r
gapminder %>% 
ggplot(aes(x = continent, y = lifeExp, fill = continent)) + 
  geom_violin() + # simple violin with jitter of life expectancy by continent
  geom_jitter(alpha = 0.2, aes(color = continent)) + # add some jitter colored by continent to the boxplot
  ggtitle("Life expectancy by each continent for the gapminder data")
```

![](STAT545-HW05-factors-and-figures_files/figure-markdown_github/unnamed-chunk-44-1.png)

``` r
ggsave("lifeExp-continent-violin-plot.png", scale = 1.75) # save the plot using ggsave() 
```

Now, I will load and embed the figure.

![lifeExp continent violin plot](lifeExp-continent-violin-plot.png)

We can more precisely change the width or height of the figure, using those arguments in ggsave(). You might ask, what are the units of the width and height? Well, the default is the current graphics device size. If we don't want that, we can specify the width and height using the `units` argument. Some units we could use are "cm", "mm", "in". For example, if we wanted the units of both width and height to be in cm, simply type `units = "cm"`.

``` r
ggsave("lifeExp-continent-violin-plot-cm.png", width = 40, height = 40, units = "cm") # show the  effect of changing the width and height to 20 cm each
```

![lifeExp continent violin plot in cm](lifeExp-continent-violin-plot-cm.png)

The dpi argument allows us to change the resolution of the plot. According to this [ggplot2 reference](https://ggplot2.tidyverse.org/reference/ggsave.html), it may take in "retina" (320), "print" (300), or "screen" (72) and applies to only raster output.

So what is a raster output and how does it differ from vector output? Well, this [source](https://designshack.net/articles/layouts/vector-vs-raster-what-do-i-use/) says:

> "There are two types of digital graphics files – vector and raster. Vector images are made of hundreds of thousands of tiny lines and curves (or paths) to create an image. Raster images are composed of pixels. But how do you know what format is best for your next project?""

The above source defines what a raster graphic is:

> "Raster images are often called bitmap images because they are made of millions of tiny squares, called pixels."

Some examples of raster images are graphics like, jpg, gif, png, and tif.

The source confirms that the majority of images that you see on websites are of the raster variety. Such raster images are useful for online publications, but not so much for printed projects because raster images are often saved in low resolution. This is why we would want to change the DPI when we save an image. If we knew we wanted to print the image sometime, go for high DPI so that the image will not "suffer during the printing process".

``` r
ggsave("lifeExp-continent-violin-plot-print-dpi.png", dpi = "print") # save the plot with dpi set to print (so we get high DPI and our image will hopefully look good when we go to print it).
```

    ## Saving 7 x 5 in image

And let's load and embed the image to see the high dpi output:

![lifeExp continent violin plot with print dpi](lifeExp-continent-violin-plot-print-dpi.png)

Since we've talked some about rasters, we can move on and talk a little about vectors. Again, I am referring to [here](https://designshack.net/articles/layouts/vector-vs-raster-what-do-i-use/)

> "Vector images, which are made of thin lines and curves known as paths, are rooted in mathematical theory."

So, vectors are in terms of lines and nodes. Vectors are such that they look smooth when you zoom in (which is a benefit over raster images).

Some examples of raster images are graphics like, PDF, SVG, and postscript.

> Text is one of the most common types of vector image. No matter how much you increase a font’s size, for example, its look never changes.

A vector's file size is often smaller than a raster graphic's file size because we are not using millions of pixels, but mathematical-based language in a vector.

One negative side to vectors is that people often just save vectors as a program-specific file. So, vectors sometimes have compatibility issues. This means that if other people don't have access to that particular program, they will have a heck of a time trying to access the program-specific vector file.

Note that ggsave() is advisable for saving ggplot() figures. However, when we are looking at saving non-ggplot figures and writing them to files, there are different methods available.

The process for writing a figure to a vector file in RStudio could be thought of as follows: 1) Open a graphics device before creating the plot or figure. For example, on Mac I could use quartz(), wheras on a Windows computer I could use windows(). 2) Create the graph. 3) Turn off (close) the graphics device.

Suppose we have finished typing up and displaying a plot that we want to write as a pdf.

``` r
# We created the below plot
x <- c(1,2,3,4,5) # define x vector 
y <- c(2,2,2,3,3) # define y vector
plot(x, y) # simple plot of x versus y 
```

![](STAT545-HW05-factors-and-figures_files/figure-markdown_github/unnamed-chunk-47-1.png)

When we have a figure up on our screen like that, we can quickly write it as a pdf by doing the following:

``` r
quartz() # open a graph window before creating graph
dev.print(pdf, "beautiful-plot.pdf") # the leftmost argument says to put a clone of the above figure as a pdf and the right argument indicates the name of our pdf
```

    ## quartz_off_screen 
    ##                 2

The longer way to write a pdf is by using the pdf() function. Even though this is longer, it does have some advantages over the quick method. For example, we Once we have everything we want in our pdf, using this method we should close the pdf file once we are done writing it by typing `dev.off()`

``` r
quartz() # open a graph window before creating graph
pdf("test.pdf") # write the PDF as a file
x <- c(1,2,3,4,5) # define x vector 
y <- c(2,4,6,8,10) # define y vector
plot(x, y) # simple plot of x versus y 
dev.off() # close file of PDF
```

    ## quartz_off_screen 
    ##                 2

We can test that we indeed got a pdf written in our working directory from the list.files() command:

``` r
list.files(pattern = "^test*") # see if our test.pdf file is in our working directory
```

    ## [1] "test.pdf"

So, there is a test.pdf in our working directory.

But I want to do more
=====================

I will pick a handful of the countries, each of which one may associate with a stereotypical desserts. My objective is to create a new factor by mapping the existing country factor levels to the new levels. The factor that I will create is stereotypical desserts for the countries Australia, France, Germany, Italy, Japan, Turkey, and United States.

So, first I must create an excerpt of the Gapminder data, filtered to just those countries. That is easy enough using our handy filter function and the droplevels() function.

``` r
dessert_countries <- c("Australia", "Turkey", "United States", "France", "Germany", "Italy", "Japan")

gap_country_desserts <- gapminder %>% 
  filter(country %in% dessert_countries) %>% 
  droplevels() # drop unused factor levels (because we'd still have all the countries from gapminder data, if we didn't).

gap_country_desserts$country %>% 
  levels() # see what the levels of gap_country_desserts$country are.
```

    ## [1] "Australia"     "France"        "Germany"       "Italy"        
    ## [5] "Japan"         "Turkey"        "United States"

Awesome. So gap\_country\_desserts is comprised of the countries that we will use for our stereotypical dessert factor addition.

Next, I will use a combination of the mutate function from the dplyr package and the fct\_recode() function from the forcats package, which will allow me to change the factor levels of the countries to have the desserts for the specified countries.

``` r
gap_country_desserts <- gap_country_desserts %>% 
mutate(stereotypicaldessert = fct_recode(country, "Lamingtons" = "Australia", "Turkish Delight" = "Turkey", "Apple Pie" = "United States", "Crème Brûlée" = "France", "Black Forest Cake" = "Germany", "Cannoli" = "Italy", "Matcha Ice Cream" = "Japan"))

head(gap_country_desserts) # have a look at the head of the dataset to see if we got a new column with the desserts corresponding to the country.
```

    ## # A tibble: 6 x 7
    ##   country   continent  year lifeExp      pop gdpPercap stereotypicaldessert
    ##   <fct>     <fct>     <int>   <dbl>    <int>     <dbl> <fct>               
    ## 1 Australia Oceania    1952    69.1  8691212    10040. Lamingtons          
    ## 2 Australia Oceania    1957    70.3  9712569    10950. Lamingtons          
    ## 3 Australia Oceania    1962    70.9 10794968    12217. Lamingtons          
    ## 4 Australia Oceania    1967    71.1 11872264    14526. Lamingtons          
    ## 5 Australia Oceania    1972    71.9 13177000    16789. Lamingtons          
    ## 6 Australia Oceania    1977    73.5 14074100    18334. Lamingtons

Let's also check the tail of gap\_country\_desserts to check that the country in the last six rows corresponds to the correct stereotypicaldessert.

``` r
tail(gap_country_desserts) 
```

    ## # A tibble: 6 x 7
    ##   country     continent  year lifeExp     pop gdpPercap stereotypicaldess…
    ##   <fct>       <fct>     <int>   <dbl>   <int>     <dbl> <fct>             
    ## 1 United Sta… Americas   1982    74.6  2.32e8    25010. Apple Pie         
    ## 2 United Sta… Americas   1987    75.0  2.43e8    29884. Apple Pie         
    ## 3 United Sta… Americas   1992    76.1  2.57e8    32004. Apple Pie         
    ## 4 United Sta… Americas   1997    76.8  2.73e8    35767. Apple Pie         
    ## 5 United Sta… Americas   2002    77.3  2.88e8    39097. Apple Pie         
    ## 6 United Sta… Americas   2007    78.2  3.01e8    42952. Apple Pie

We get what we wanted. To elaborate, we see that United States corresponds to the Apple Pie level of the stereotypicaldessert factor.

Finally, we will check to see what the levels of stereotypicaldessert are. We expect them to be the desserts that we specified above.

``` r
gap_country_desserts$stereotypicaldessert %>% 
  levels()
```

    ## [1] "Lamingtons"        "Crème Brûlée"      "Black Forest Cake"
    ## [4] "Cannoli"           "Matcha Ice Cream"  "Turkish Delight"  
    ## [7] "Apple Pie"

As was expected, we got that the levels of stereotypicaldessert are the desserts specified for each of the countries from the above subset.
