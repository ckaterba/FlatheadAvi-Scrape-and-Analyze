The aim of this project is to develop a daily ML-generated avalanche danger forecast for Northwest Montana. The project should proceed in a few steps:

1. Scrape the [Flathead Avalanche Center](https://flatheadavalanche.org/)'s historic record of daily avalanche forecasts.
2. Scrape the [National Avalache Center(https://looper.avalanche.state.co.us/fac/)'s compilation of regional weather station data at some predetermined time, either 5 or 6AM. 
3. Clean and tidy the data for use in a ML algorthim. The most important factors are likely temperature, temperature change, wind speed, and snow accumulation. 
4. Train and test various ML algorthims to predict a daily avalanch danger rating along with possible avalanche types (eg. wind slab, storm slab, etc.)
5. Develop an application that checks the weather station daily and sends out an email with the day's forecasted avalanche danger.

This project has currently stalled out in the *clean and tidy* phase. I'm hoping to work on it again this winter.
