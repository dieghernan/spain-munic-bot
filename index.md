---
layout: default
title: spain-munic-bot
subtitle: Maps of Spanish municipalities generated with R
excerpt: An archive of R-generated maps of buildings and urban growth in Spanish municipalities. Automated posting is disabled.
project_links:
    - url: https://github.com/dieghernan/spain-munic-bot/
      icon: fab fa-github my-2
      label: View on GitHub
---

## About the project

<div markdown="1" class="badges">
![tweet-maps](https://github.com/dieghernan/spain-munic-bot/workflows/last-tweet/badge.svg)
![progress](https://img.shields.io/badge/dynamic/json?label=progress&query=progress&url=https://dieghernan.github.io/spain-munic-bot/assets/lasttweet.json)
![last-visited](https://img.shields.io/badge/dynamic/json?label=last%20visited&query=lastseen&url=https://dieghernan.github.io/spain-munic-bot/assets/lasttweet.json&style=social)
![last-tweet](https://img.shields.io/badge/dynamic/json?label=last%20tweet&query=lasttweet&url=https://dieghernan.github.io/spain-munic-bot/assets/lasttweet.json&style=social&logo=twitter)
![Followers](https://img.shields.io/twitter/follow/spainmunic?label=Followers&style=social)
[![ko-fi](https://img.shields.io/badge/buy%20me%20a%20coffee-donate-yellow.svg)](https://ko-fi.com/dieghernan)
</div>

<div class="text-center my-3">
<a href="https://twitter.com/spainmunic?ref_src=twsrc%5Etfw" class="twitter-follow-button" data-size="large" data-show-count="false">Follow @spainmunic</a><script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
</div>

This project generated maps of buildings in Spanish municipalities, inspired
by Dominic Royé’s [urban-growth visualizations](https://dominicroye.github.io/en/2019/visualize-urban-growth/).
Automated posting is currently disabled. The last public output record is
dated May 17, 2022.

<a class="twitter-timeline" data-height="550" data-theme="dark" href="https://twitter.com/spainmunic?ref_src=twsrc%5Etfw">Tweets by spainmunic</a>

## Buildings

Example: buildings in Málaga grouped by construction period.

![Buildings in Málaga colored by construction period, with a locator map of Spain.](/assets/img/sample.png)

## Project journey

![Map of municipalities visited by the bot: 7,536 of 7,608, or 99.05%, in the saved project record.](/assets/img/journey.png)

Browse the [generated-map records](https://github.com/dieghernan/spain-munic-bot/blob/main/assets/datalog.csv).

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

    -   [italiancomuni](https://twitter.com/italiancomuni), by
        [\@espinielli](https://github.com/espinielli).

-   Automation:

    -   GitHub Actions as per [\@espinielli](https://github.com/espinielli)'s
        [GitHub Repo](https://github.com/espinielli/italian-comuni-bot).

-   Sources:

    -   [mapSpain](https://ropenspain.github.io/mapSpain/) R package.
    -   [**CatastRo**](https://ropenspain.github.io/CatastRo/) for the buildings.

## Original execution workflow

The original entry point is shown below. It invokes the map-generation and
Twitter publishing code. Review its configuration, dependencies and API
credentials before running it; it is not a preview-only command.

        $ Rscript R/trigger.R

This will trigger `R/01_create_map.R` and compose the tweet.
