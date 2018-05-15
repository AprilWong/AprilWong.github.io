    top2017 <- read_csv("/home/wong9/Mscs 264 S18/Project/wongmeyer/OurRProject/top2017.csv")

    ## Parsed with column specification:
    ## cols(
    ##   id = col_character(),
    ##   name = col_character(),
    ##   artists = col_character(),
    ##   danceability = col_double(),
    ##   energy = col_double(),
    ##   key = col_double(),
    ##   loudness = col_double(),
    ##   mode = col_double(),
    ##   speechiness = col_double(),
    ##   acousticness = col_double(),
    ##   instrumentalness = col_double(),
    ##   liveness = col_double(),
    ##   valence = col_double(),
    ##   tempo = col_double(),
    ##   duration_ms = col_double(),
    ##   time_signature = col_double()
    ## )

    daily200 <- read_csv("/home/wong9/Mscs 264 S18/Project/wongmeyer/OurRProject/daily200.csv")

    ## Parsed with column specification:
    ## cols(
    ##   Position = col_integer(),
    ##   `Track Name` = col_character(),
    ##   Artist = col_character(),
    ##   Streams = col_integer(),
    ##   URL = col_character(),
    ##   Date = col_date(format = ""),
    ##   Region = col_character()
    ## )

    colnames(daily200) <- c("position",
                            "name",
                            "artists",
                            "streams",
                            "url",
                            "date",
                            "region")


    topsongs <- top2017 %>%
      inner_join(daily200, by = "name")

    topsongs <- topsongs %>% 
      mutate(region = fct_recode(region, 
                argentina = "ar",
                 austria = "at",
                 australia = "au",
                 belgium = "be",
                 bolivia = "bo",
                 brazil = "br",
                 canada = "ca",
                 switzerland = "ch",
                 chile = "cl",
                 colombia = "co",
                 costa_rica = "cr",
                 czechia = "cz",
                 germany = "de",
                 denmark = "dk",
                 dominican_republic = "do",
                 ecuador = "ec",
                 estonia = "ee",
                 spain = "es",
                 finland = "fi",
                 france = "fr",
                 great_britain = "gb",
                 greece = "gr",
                 guatemala = "gt",
                 hong_kong = "hk",
                 honduras = "hn",
                 hungary = "hu",
                 indonesia = "id",
                 ireland = "ie",
                 iceland = "is",
                 italy = "it",
                 japan = "jp",
                 lithuania = "lt",
                 luxembourg = "lu",
                 latvia = "lv",
                 mexico = "mx",
                 malaysia = "my",
                 netherlands = "nl",
                 norway = "no",
                 new_zealand = "nz",
                 panama = "pa",
                 peru = "pe",
                 philippines = "ph",
                 poland = "pl",
                 portugal = "pt",
                 paraguay = "py",
                 sweden = "se",
                 singapore = "sg",
                 slovakia = "sk",
                 el_salvador = "sv",
                 turkey = "tr",
                 taiwan = "tw",
                 united_states = "us",
                 uruguay = "uy"
                 ))

How many times songs on the top 100 show up on the daily 200 shows how
popular the songs are compared to each other. The most popular song
worldwide on Spotify in 2017 that was released that year is 'Shape of
You' by Ed Sheeron.

    timesondaily <- topsongs%>%
      group_by(name)%>%
      count()%>%
      arrange(desc(n))
    timesondaily

    ## # A tibble: 100 x 2
    ## # Groups:   name [100]
    ##    name                                                                  n
    ##    <chr>                                                             <int>
    ##  1 Shape of You                                                      19365
    ##  2 Believer                                                          16775
    ##  3 Despacito (Featuring Daddy Yankee)                                16284
    ##  4 Something Just Like This                                          16194
    ##  5 Rockabye (feat. Sean Paul & Anne-Marie)                           16177
    ##  6 "I Don’t Wanna Live Forever (Fifty Shades Darker) - From \"Fifty… 15447
    ##  7 Closer                                                            15136
    ##  8 Scared to Be Lonely                                               14836
    ##  9 It Ain't Me (with Selena Gomez)                                   14801
    ## 10 Let Me Love You                                                   14562
    ## # ... with 90 more rows

How many times songs from the top 100 showed up on each countries daily
200. We can see that at the top, New Zealand had 'Closer' on their daily
200 a total of 446 times, however 'Let Me Love You' shows up for
multiple countries in the top 10.

    regionaltimes <- topsongs%>%
      group_by(name, region)%>%
      count()%>%
      arrange(desc(n))
    regionaltimes

    ## # A tibble: 4,924 x 3
    ## # Groups:   name, region [4,924]
    ##    name            region            n
    ##    <chr>           <fct>         <int>
    ##  1 Closer          new_zealand     446
    ##  2 Unforgettable   united_states   431
    ##  3 Believer        netherlands     410
    ##  4 Let Me Love You australia       406
    ##  5 Let Me Love You netherlands     394
    ##  6 Unforgettable   canada          390
    ##  7 Let Me Love You canada          384
    ##  8 Closer          japan           374
    ##  9 Chantaje        uruguay         372
    ## 10 Let Me Love You costa_rica      372
    ## # ... with 4,914 more rows

We have found the top song for each country based on the number of days
that song spent in the top 200 song list for that country. Countries
whose songs spent an equal number of days on the list have all their
tying songs on the list, which is why there are 110 top songs for 54
countries. I have joined back in the data about the songs in order to
find trends about each countries preferences.

    top1_per_country <- topsongs %>%
      group_by(name, region) %>%
      count() %>%
      ungroup() %>%
      group_by(region) %>%
      top_n(1) %>%
      arrange(desc(n)) %>%
      ungroup()

    ## Selecting by n

    top1_per_country2 <- top1_per_country %>%
      inner_join(top2017, by = "name")

    top1_per_country3 <- top1_per_country2 %>%
      mutate(region = fct_collapse(region,
                                 "Latin_America" = c("argentina", "bolivia", "brazil", "chile", "colombia", "costa_rica", "dominican_republic", "ecuador", "guatemala", "mexico", "panama", "peru", "portugal", "paraguay", "el_salvador", "uruguay", "honduras"),
                                 "Western_Europe" = c("belgium", "switzerland", "germany", "denmark", "spain", "finland", "france", "great_britain", "ireland", "iceland", "luxembourg", "netherlands", "norway", "sweden"),
                                 "North_America" = c("canada", "united_states"),
                                 "Central/Eastern_Europe" = c("austria", "czechia", "estonia", "hungary", "lithuania", "latvia", "poland", "slovakia"), 
                                 "Southern_Europe" = c("italy", "greece", "turkey"),
                                 "East_Asia" = c("hong_kong", "japan", "taiwan"), 
                     "Oceania" = c("australia", "indonesia", "malaysia", "new_zealand", "philippines", "singapore")
                 ))

    top1_per_country3 %>%
    ggplot(mapping = aes(x = tempo, y = region)) +
      geom_point()

![](ProjectScript_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    top1_per_country3 %>%
      ggplot(mapping = aes(x = ))

![](ProjectScript_files/figure-markdown_strict/unnamed-chunk-4-2.png)

This plot shows that the amount of times that a song appears on the
daily 200 has some correlation to a song's rank on the top 100 songs of
the year.

    top20171 <- top2017%>% 
      mutate(rank = 1:n()) 

    timesdaily <- top20171%>%
      inner_join(timesondaily, by = "name")

    timesdaily%>%
      ggplot(aes(x = rank, y = n)) +
      geom_point()+
      geom_smooth()

    ## `geom_smooth()` using method = 'loess'

![](ProjectScript_files/figure-markdown_strict/unnamed-chunk-5-1.png)

This shows each country's mean qualities of music as well as each area's
mean qualities of music. Overall each area has the same mean qualties in
music taste. These are taken from only those in the daily 200 that cross
the top 2017 which will obviously effect it to some degree, but each
instance that the song from the top 2017 is on the daily is an
individual row.

    regional1 <- regionaltimes%>%
      inner_join(top2017, by = c("name"))

    regional2 <- topsongs%>%
      group_by(region)%>%
      summarise(meandance = mean(danceability), meanenergy = mean(energy), meanloud = mean(loudness), meanspeech = mean(speechiness), meanacoustic = mean(acousticness), meaninstrumental = mean(instrumentalness), meanliveness = mean(liveness), meanvalence = mean(valence), meantemp = mean(tempo))
    regional2

    ## # A tibble: 54 x 10
    ##    region      meandance meanenergy meanloud meanspeech meanacoustic
    ##    <fct>           <dbl>      <dbl>    <dbl>      <dbl>        <dbl>
    ##  1 argentina       0.691      0.696    -5.18     0.0836        0.166
    ##  2 austria         0.691      0.671    -5.50     0.0966        0.157
    ##  3 australia       0.695      0.651    -5.77     0.108         0.167
    ##  4 belgium         0.692      0.663    -5.58     0.0990        0.164
    ##  5 bolivia         0.698      0.694    -5.19     0.0791        0.168
    ##  6 brazil          0.689      0.679    -5.39     0.0916        0.161
    ##  7 canada          0.701      0.650    -5.77     0.111         0.168
    ##  8 switzerland     0.696      0.671    -5.51     0.0981        0.164
    ##  9 chile           0.693      0.696    -5.22     0.0860        0.161
    ## 10 colombia        0.696      0.685    -5.38     0.0891        0.164
    ## # ... with 44 more rows, and 4 more variables: meaninstrumental <dbl>,
    ## #   meanliveness <dbl>, meanvalence <dbl>, meantemp <dbl>

    byarea <- topsongs%>%
      mutate(region = fct_collapse(region,                              "Latin_America" = c("argentina", "bolivia", "brazil", "chile", "colombia", "costa_rica", "dominican_republic", "ecuador", "guatemala", "mexico", "panama", "peru", "portugal", "paraguay", "el_salvador", "uruguay", "honduras"),
                                 "Western_Europe" = c("belgium", "switzerland", "germany", "denmark", "spain", "finland", "france", "great_britain", "ireland", "iceland", "luxembourg", "netherlands", "norway", "sweden"),
                                 "North_America" = c("canada", "united_states"),
                                 "Central/Eastern_Europe" = c("austria", "czechia", "estonia", "hungary", "lithuania", "latvia", "poland", "slovakia"), 
                                 "Southern_Europe" = c("italy", "greece", "turkey"),
                                 "East_Asia" = c("hong_kong", "japan", "taiwan"), 
                     "Oceania" = c("australia", "indonesia", "malaysia", "new_zealand", "philippines", "singapore")
                 ))

    set.seed(666)
    byarea_small <- byarea %>%
      sample_n(10000)

    area1 <- byarea%>%
      group_by(region)%>%
      summarise(meandance = mean(danceability), meanenergy = mean(energy), meanloud = mean(loudness), meanspeech = mean(speechiness), meanacoustic = mean(acousticness), meaninstrumental = mean(instrumentalness), meanliveness = mean(liveness), meanvalence = mean(valence), meantemp = mean(tempo))
    area1

    ## # A tibble: 8 x 10
    ##   region             meandance meanenergy meanloud meanspeech meanacoustic
    ##   <fct>                  <dbl>      <dbl>    <dbl>      <dbl>        <dbl>
    ## 1 Latin_America          0.695      0.686    -5.34     0.0886        0.164
    ## 2 Central/Eastern_E…     0.692      0.665    -5.56     0.0979        0.164
    ## 3 Oceania                0.688      0.656    -5.69     0.103         0.169
    ## 4 Western_Europe         0.694      0.666    -5.58     0.101         0.164
    ## 5 North_America          0.704      0.645    -5.82     0.114         0.171
    ## 6 global                 0.702      0.662    -5.66     0.106         0.168
    ## 7 Southern_Europe        0.699      0.671    -5.48     0.100         0.162
    ## 8 East_Asia              0.685      0.664    -5.56     0.0957        0.169
    ## # ... with 4 more variables: meaninstrumental <dbl>, meanliveness <dbl>,
    ## #   meanvalence <dbl>, meantemp <dbl>

Latin Americans seem to enjoy most simple. As the next three plots show,
of all the regions studied, Latin America has the lowest average
wordcount as well as lowest electronic instrument prevalence, with the
lowest acoustic instrument prevalence with one exception in their
popular songs.

    area1 %>%
      ggplot(aes(y = region, x = meaninstrumental)) +
      geom_point()

![](ProjectScript_files/figure-markdown_strict/unnamed-chunk-7-1.png)

    area1 %>%
      ggplot(aes(y = region, x = meanspeech)) +
      geom_point()

![](ProjectScript_files/figure-markdown_strict/unnamed-chunk-7-2.png)

    area1 %>% 
      ggplot(aes(y = region, x = meanacoustic)) +
      geom_point()

![](ProjectScript_files/figure-markdown_strict/unnamed-chunk-7-3.png)

Somewhat suprising to us is the results of these two graphs which show
that North Americans enjoy the quietest and slowest music around the
world.

    area1 %>%
      ggplot(aes(y = region, x = meantemp)) +
      geom_point()

![](ProjectScript_files/figure-markdown_strict/unnamed-chunk-8-1.png)

    area1 %>%
      ggplot(aes(y = region, x = meanloud)) +
      geom_point()

![](ProjectScript_files/figure-markdown_strict/unnamed-chunk-8-2.png)

This plot shows the realtionship between a song's ranking on the charts
with it's danceability and speechiness. As we can see there is no
correlation between rank and either of the two, but we can see that most
songs do have a lower speechiness

    timesdaily%>%
      ggplot(aes(x = rank, y = danceability, color = speechiness)) +
      geom_point()

![](ProjectScript_files/figure-markdown_strict/unnamed-chunk-9-1.png)

As you can see from this plot the number of streams a song gets actually
doesn't contribute to the ranking of a song.

    streams1 <- topsongs%>%
      group_by(name)%>%
      summarise(meanstreams = mean(streams))

    streams2 <- streams1%>%
      left_join(top20171, by = "name")
      
    streams2%>%
      ggplot(aes(x = rank, y = meanstreams)) + 
      geom_point()+
      geom_smooth()

    ## `geom_smooth()` using method = 'loess'

![](ProjectScript_files/figure-markdown_strict/unnamed-chunk-10-1.png)

    streams2%>%
      ggplot(aes(x = tempo, y = meanstreams)) + 
      geom_point()+
      geom_smooth()

    ## `geom_smooth()` using method = 'loess'

![](ProjectScript_files/figure-markdown_strict/unnamed-chunk-10-2.png)

    streams2%>%
      ggplot(aes(x = acousticness, y = meanstreams)) + 
      geom_point()+
      geom_smooth()

    ## `geom_smooth()` using method = 'loess'

![](ProjectScript_files/figure-markdown_strict/unnamed-chunk-10-3.png)
