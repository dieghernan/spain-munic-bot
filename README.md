# spain-munic-bot :es::robot:

![tweet-maps](https://github.com/dieghernan/spain-munic-bot/workflows/last-tweet/badge.svg)
![progress](https://img.shields.io/badge/dynamic/json?label=progress&query=progress&url=https://dieghernan.github.io/spain-munic-bot/assets/lasttweet.json)
![last-visited](https://img.shields.io/badge/dynamic/json?label=last%20visited&query=lastseen&url=https://dieghernan.github.io/spain-munic-bot/assets/lasttweet.json&style=social)
![last-tweet](https://img.shields.io/badge/dynamic/json?label=last%20tweet&query=lasttweet&url=https://dieghernan.github.io/spain-munic-bot/assets/lasttweet.json&style=social&logo=twitter)
![Followers](https://img.shields.io/twitter/follow/spainmunic?label=Followers&style=social)
[![ko-fi](https://img.shields.io/badge/buy%20me%20a%20coffee-donate-yellow.svg)](https://ko-fi.com/dieghernan)

## A Twitter bot written in R

<p align="center">
<a href="https://twitter.com/spainmunic"><img src="https://img.shields.io/badge/Follow-%40spainmunic-blue?style=flat&labelColor=1DA1F2&color=1DA1F2&logo=twitter&logoColor=white" alt="Follow spainmunic on Twitter" height=25 ></a>
</p>

The bot generated a map of a Spanish town together with its name, province and
autonomous community (and an inset map of Spain showing the region and the
community). The bot was configured to publish every 20 minutes. Automated posting is
currently disabled. The last public output record is dated May 17, 2022.

## [Project website](https://dieghernan.github.io/spain-munic-bot/)

## Buildings

The project maps the urban growth of Spanish cities, based on the works of
**Dominic Royé:** [Visualize urban
growth](https://dominicroye.github.io/en/2019/visualize-urban-growth/)

![Buildings in Málaga colored by construction period, with a locator map of Spain.](assets/img/sample.png)

Browse the [generated-map records](https://github.com/dieghernan/spain-munic-bot/blob/main/assets/datalog.csv).

## Project journey

![Map of municipalities visited by the bot: 7,536 of 7,608, or 99.05%, in the saved project record.](assets/img/journey.png)

## R packages

Core packages used in the project are:

-   [**CatastRo**](https://ropenspain.github.io/CatastRo/) for the buildings.
-   [**mapSpain**](https://ropenspain.github.io/mapSpain/) for the location of the
    municipalities.
-   [**ggplot2**](https://ggplot2.tidyverse.org/) for plotting.
-   [**rtweet**](https://docs.ropensci.org/rtweet/) for posting.

Other packages include **sf**, **dplyr** and supporting tools.

The project uses **renv** to record its R package dependencies. External data
services and publishing APIs may have changed since the maps were generated.

## Credits

-   Based on

    -   [italiancomuni](https://twitter.com/italiancomuni), by \@espinielli.

-   Automation:

    -   GitHub Actions as per \@espinielli's [GitHub
        Repo](https://github.com/espinielli/italian-comuni-bot).

-   Sources:

    -   [mapSpain](https://ropenspain.github.io/mapSpain/) R package.
    -   [osmdata](https://docs.ropensci.org/osmdata/) for the streets.

## Original execution workflow

The original entry point is shown below. It invokes the map-generation and
Twitter publishing code. Review its configuration, dependencies and API
credentials before running it; it is not a preview-only command.

        $ Rscript R/trigger.R

This will trigger `R/01_create_map.R` and compose the tweet.
