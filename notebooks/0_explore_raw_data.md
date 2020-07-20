Exploring Melbourne business establishments
================

Labelled data
-------------

This data associates a trading name with an ANZIC, and is collected as part of the City of Melbourne's Census of Land Use and Employment (CLUE) for the period 2002-2018.

Here is a sample of the data from the Melbourne business survey:

``` r
data_tr %>% as_tibble %>% head(5)
```

    ## # A tibble: 5 x 11
    ##   Census.year Block.ID Property.ID Base.property.ID CLUE.small.area Trading.name
    ##         <int>    <int>       <int>            <int> <fct>           <fct>       
    ## 1        2002      620      109652           109653 East Melbourne  Metro Parki…
    ## 2        2002      620      109652           109653 East Melbourne  Breast Clin…
    ## 3        2002      620      109652           109653 East Melbourne  Bone Densii…
    ## 4        2002      620      109652           109653 East Melbourne  Colposcopy …
    ## 5        2002      620      109652           109653 East Melbourne  Mens Health…
    ## # … with 5 more variables: Industry..ANZSIC4..code <int>,
    ## #   Industry..ANZSIC4..description <fct>, x.coordinate <dbl>,
    ## #   y.coordinate <dbl>, Location <fct>

Close to 20K businesses are surveyed a year.

``` r
data_tr %>% group_by(Census.year) %>% tally %>% arrange(-Census.year)
```

    ## # A tibble: 17 x 2
    ##    Census.year     n
    ##          <int> <int>
    ##  1        2018 19118
    ##  2        2017 18997
    ##  3        2016 18874
    ##  4        2015 18937
    ##  5        2014 18812
    ##  6        2013 18458
    ##  7        2012 18287
    ##  8        2011 18224
    ##  9        2010 17835
    ## 10        2009 17598
    ## 11        2008 16961
    ## 12        2007 15880
    ## 13        2006 14509
    ## 14        2005 13978
    ## 15        2004 13258
    ## 16        2003 13098
    ## 17        2002 13046

Inspecting a few businesses we can see a number of businesses were surveyed multiple times (over time or across many sites). There's also a large proportion of `Vacant` or `vacant` properties.

``` r
data_tr %>% group_by(Trading.name) %>% tally %>% arrange(-n) %>% head(5)
```

    ## # A tibble: 5 x 2
    ##   Trading.name                    n
    ##   <fct>                       <int>
    ## 1 Vacant                      37605
    ## 2 vacant                       1456
    ## 3 7-Eleven                      515
    ## 4 RMIT University               504
    ## 5 The University of Melbourne   399

Bigger organisations with with multiple sites can have different ANZSICs depending on what type of business they run on the site.

``` r
data_tr %>% filter(Trading.name == "The University of Melbourne") %>% group_by(Industry..ANZSIC4..code, Industry..ANZSIC4..description) %>% tally %>% arrange(-n)
```

    ## # A tibble: 4 x 3
    ## # Groups:   Industry..ANZSIC4..code [4]
    ##   Industry..ANZSIC4..code Industry..ANZSIC4..description     n
    ##                     <int> <fct>                          <int>
    ## 1                    8102 Higher Education                 387
    ## 2                    6910 Scientific Research Services       7
    ## 3                    8710 Child Care Services                3
    ## 4                       0 Vacant Space                       2

What is the count of unique trading names by geography:

``` r
data_tr %>% 
  select(Trading.name,CLUE.small.area) %>% 
  unique() %>% 
  group_by(CLUE.small.area) %>% 
  tally %>% 
  arrange(-n)
```

    ## # A tibble: 13 x 2
    ##    CLUE.small.area                  n
    ##    <fct>                        <int>
    ##  1 Melbourne (CBD)              27478
    ##  2 Carlton                       3118
    ##  3 North Melbourne               2971
    ##  4 Docklands                     2735
    ##  5 Southbank                     2388
    ##  6 East Melbourne                1660
    ##  7 West Melbourne (Residential)  1587
    ##  8 Port Melbourne                1246
    ##  9 Melbourne (Remainder)         1192
    ## 10 West Melbourne (Industrial)    911
    ## 11 Kensington                     851
    ## 12 Parkville                      743
    ## 13 South Yarra                    115

From inspecting the raw data, initial data cleaning actions need to:

-   Filter out `Vacant`, `Vacant Land` and `vacant` trading names (they are NULLs).
-   If a business name has more than one ANZSIC, assign the highest counted ANZSIC to that business name
