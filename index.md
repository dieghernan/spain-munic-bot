---
layout: default
title: spain-munic-bot
subtitle: "Beep beep! I create maps"
project_links:
    - url: https://github.com/dieghernan/spain-munic-bot/
      icon: fab fa-github my-2
      label: See on Github
---

## 🤖 Twitter bot: random municipalities of Spain 🇪🇸 with {mapSpain} posted with {rtweet} via a GitHub Action

<div markdown="1" class="badges">
![tweet-maps](https://github.com/dieghernan/spain-munic-bot/workflows/last-tweet/badge.svg)
![](https://img.shields.io/badge/active-since%202020–01%E2%80%9329-brightgreen)
![progress](https://img.shields.io/badge/dynamic/json?color=blue&label=progress&query=%24.progress%5B%3A1%5D&url=https%3A%2F%2Fdieghernan.github.io%2Fspain-munic-bot%2Fassets%2Flasttweet.json)
![last-visited](https://img.shields.io/badge/dynamic/json?color=yellow&label=last%20visited&query=%24.lastseen%5B%3A1%5D&url=https%3A%2F%2Fdieghernan.github.io%2Fspain-munic-bot%2Fassets%2Flasttweet.json&style=social)
![last-tweet](https://img.shields.io/badge/dynamic/json?color=yellow&label=last%20tweet&query=%24.lasttweet%5B%3A1%5D&url=https%3A%2F%2Fdieghernan.github.io%2Fspain-munic-bot%2Fassets%2Flasttweet.json&style=social&logo=twitter)
![Followers](https://img.shields.io/twitter/follow/spainmunic?label=Followers&style=social)
[![ko-fi](https://img.shields.io/badge/buy%20me%20a%20coffee-donate-yellow.svg)](https://ko-fi.com/dieghernan)
</div>


<div class="text-center my-3">
<a href="https://twitter.com/spainmunic?ref_src=twsrc%5Etfw" class="twitter-follow-button" data-size="large" data-show-count="false">Follow @spainmunic</a><script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
</div>


Hi! I am a bot 🤖 that tweets a random map of a Spanish municipality with its name, province, and autonomous community (and a inset map of Spain showing the region and the community). I run 🏃‍♀️ every 20 minutes.


<a class="twitter-timeline" data-height="550" href="https://twitter.com/spainmunic?ref_src=twsrc%5Etfw">Tweets by spainmunic</a> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

## 🕒 Last maps 

![satellite](/assets/img/munic-satellite.png)

![streets](/assets/img/munic-streets.png)

## ✈️ The Journey

![journey](/assets/img/journey.png)

Places I have visited 🇪🇸
{: .caption}

### See [here](https://dieghernan.github.io/spain-munic-bot/journey) the maps I have created 🗺

## 📦 R packages

Core packages used in the project are:

- [{mapSpain}](https://ropenspain.github.io/mapSpain/) for the location of the municipalities, base polygons and coordinates and imagery,
- [{osmdata}](https://docs.ropensci.org/osmdata/) for the streets,
- [{tmap}](https://mtennekes.github.io/tmap/) for plotting,
- [{rtweet}](https://docs.ropensci.org/rtweet/) for posting,

Other packages used are {sf}, {dplyr} and another common supporting packages. 

This project uses {renv} for ensuring fully reproducibility across platforms.

## 🙌🏻 Credits

-   Based on

    -   [italiancomuni](https://twitter.com/italiancomuni), by \@espinielli.

-   Automation:

    -   GitHub Actions as per \@espinielli's [GitHub Repo](https://github.com/espinielli/italian-comuni-bot).

-   sources:

    - [mapSpain](https://ropenspain.github.io/mapSpain/) R package.
    - [osmdata](https://docs.ropensci.org/osmdata/) for the streets.

## ❔How to run it

From the command line it is enough to run the following command:

        $ Rscript R/trigger.R

This will trigger `R/01_create_map.R` and compose the tweet.
