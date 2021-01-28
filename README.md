# spain-munic-bot

![tweet-maps](https://github.com/dieghernan/spain-munic-bot/workflows/tweet-maps/badge.svg)
[![Twitter Follow](https://img.shields.io/twitter/follow/spainmunic?style=social)](https://twitter.com/spainmunic)

A Twitter bot written in R, see <https://twitter.com/spainmunic> for actual tweets.

It tweets a map of a random Spanish town together with its name, province, and autonomous community (and a inset map of Spain showing the region and the community). The bot is set to run every 15 minutes.

### Satellite map

![last-map-sat](data/munic-raster.png)

### Street maps

![last-map-streets](data/munic-streets.png)

Find [here](./data/archive) all the maps generated by the bot.

## The journey

![journey](data/journey.png)

## Credits

-   Based on

    -   [italiancomuni](https://twitter.com/italiancomuni), by \@espinielli.

-   Automation:

    -   GitHub Actions as per \@espinielli's [GitHub Repo](https://github.com/espinielli/italian-comuni-bot).

-   sources:

    -   [mapSpain](https://ropenspain.github.io/mapSpain/) R package.

## How to run it

From the command line it is enough to run the following command:

        $ Rscript R/trigger.R

This will trigger `R/01_create_map.R` and compose the tweet.
