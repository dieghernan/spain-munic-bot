# spain-munic-bot 🇪🇸🤖

![tweet-maps](https://github.com/dieghernan/spain-munic-bot/workflows/lasttweet/badge.svg)
[![Twitter Follow](https://img.shields.io/twitter/follow/spainmunic?style=social)](https://twitter.com/spainmunic)
[![ko-fi](https://img.shields.io/badge/buy%20me%20a%20coffee-donate-yellow.svg)](https://ko-fi.com/dieghernan)

A Twitter bot written in R, see <https://twitter.com/spainmunic> for actual tweets.

It tweets a map of a random Spanish town together with its name, province, and autonomous community (and a inset map of Spain showing the region and the community). The bot is set to run every 15 minutes.

### 🌍 Satellite maps

![last-map-satellite](/assets/img/munic-satellite.png)

### 🚶🏻‍♂️ Street maps

![last-map-streets](/assets/img/munic-streets.png)

Find [here](./assets/img/) all the maps generated by the bot.

## ✈️ The journey of the 🤖

![journey](/assets/img/journey.png)

Places I have visited 🇪🇸

## 🙌🏻 Credits

-   Based on

    -   [italiancomuni](https://twitter.com/italiancomuni), by \@espinielli.

-   Automation:

    -   GitHub Actions as per \@espinielli's [GitHub Repo](https://github.com/espinielli/italian-comuni-bot).

-   sources:

    -   [mapSpain](https://ropenspain.github.io/mapSpain/) R package.

## ❔ How to run it

From the command line it is enough to run the following command:

        $ Rscript R/trigger.R

This will trigger `R/01_create_map.R` and compose the tweet.
