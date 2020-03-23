library(dplyr)
library(reshape2)
library(lubridate)
library(ggplot2)
library(plotly)
library(magrittr)
library(stringr)
library(RColorBrewer)
library(htmlwidgets)
library(htmltools)


# Prepare data ------------------------------------------------------------

try_get_data <- try({
data_rep <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

corona <- list(
  confirmed = read.csv(paste0(data_rep, "time_series_19-covid-Confirmed.csv"), check.names = FALSE),
  deaths = read.csv(paste0(data_rep, "time_series_19-covid-Deaths.csv"), check.names = FALSE),
  recovered = read.csv(paste0(data_rep, "time_series_19-covid-Recovered.csv"), check.names = FALSE)
)

# merge
corona <- bind_rows(corona, .id = "type")
# flatten in long format
corona <- corona %>% melt(id.vars = 1:5, variable.name = "date", value.name = "cases")
# convert date to proper date
corona <- corona %>% mutate(date = mdy(date))

# save for offline usage
save(corona, file = "corona.RData")
})

if(inherits(try_get_data, "try-error")) load("corona.RData")

## summarize countries
corona_country <- corona %>% 
  group_by(`Country/Region`, type, date) %>% summarise(cases = sum(cases)) %>%
  ungroup

## prepare ranking
corona_country_ranking <- corona_country %>% group_by(`Country/Region`) %>%
  filter(date == max(date), type == "confirmed") %>% ungroup %>% 
  arrange(cases) %>%
  mutate( `Country/Region` = ordered(`Country/Region`, levels = rev(`Country/Region`))) %>%
  .$`Country/Region`

# add other aspects
corona_country %<>% mutate(
  `daily cases` = c(NA, diff(cases)),
  `daily cases/total cases` = `daily cases`/c(NA, cases[-1])
)

# apply ranking
corona_country <- corona_country %>% 
  mutate(
    `Country/Region` = ordered(`Country/Region`, 
                               levels = levels(corona_country_ranking)),
    ranked_country = paste(
      str_pad(as.numeric(`Country/Region`), 3, pad = 0), 
      `Country/Region`))
selected_countries <- c("Germany", "France", "Spain", "US", "Italy", "United Kingdom")
subcorona <- corona_country %>% filter(`Country/Region` %in% selected_countries)
othercorona <- corona_country %>% filter(!(`Country/Region` %in% selected_countries))


# Prepare plots -----------------------------------------------------------

# make repeating colors
cols <- brewer.pal(12, "Set3")[-2] %>% 
  rep(length(corona_country_ranking) %/% 11 + 1) %>% 
  head(length(corona_country_ranking))

# generate figures
make_subfig <- function(.type, .showlegend, y = ~cases) {
  fig <- subcorona %>% 
    filter(type == .type) %>%  
    plot_ly(x = ~date, y = y, legendgroup = ~as.character(`Country/Region`),
            name = ~ranked_country,
            color = ~ranked_country,
            colors = cols,
            type = "scatter", mode = "lines", showlegend = .showlegend)
  fig <- fig %>% add_trace(
    data = othercorona %>% filter(type == .type), showlegend = .showlegend,
    visible = "legendonly")
}
figs <- list()
for(i in 1:3) {
  .type <- unique(subcorona$type)[i]
  figs[[i]] <- make_subfig( .type, c(TRUE,FALSE,FALSE)[i] )
  figs[[i]] %<>% layout(yaxis = list(title = .type))
}

caption <- paste0("based on data provided by https://github.com/CSSEGISandData/COVID-19 ", 
                  "hosted by the Johns Hopkins Corona Resource Center. ",
                  "Last update: ", Sys.time(), ". ",
                  "Visualization created by Almond Stoecker. ",
                  "No warranties of any kind.")


# plot confirmed only -----------------------------------------------------

title_confirmed <- "Worldwide overview of confirmed COVID-19 cases"

# save as individual html
figs[[1]] %>% layout(title = title_confirmed) %>% 
  layout(xaxis = list(rangeslider = list(type = "date"))) %>% 
  htmlwidgets::appendContent(htmltools::tags$p(caption)) %>% 
  saveWidget("covid-19-time-series-confirmed.html")


# plot all: confirmed, deaths, and recovered ------------------------------

fig_all <- do.call(subplot, c(figs, nrows = 3, shareX = TRUE, titleY = TRUE))
title_all <- "Worldwide overview of COVID-19 time series (counts)"

# save
fig_all %>% layout(title = title_all) %>% 
  # layout(xaxis = list(rangeslider = list(type = "date"))) %>% 
  htmlwidgets::appendContent(htmltools::tags$p(caption)) %>% 
  saveWidget("covid-19-time-series-all.html")


# plot all on log-scale ---------------------------------------------------

title_all_log <- "Worldwide overview of COVID-19 time series (log-counts)"

#save
fig_all %>% layout(title = title_all_log) %>% 
  layout(xaxis = list(rangeslider = list(type = "date"))) %>% 
  saveWidget("covid-19-time-series-all-log.html")


# plot daily cases --------------------------------------------------------

title_daily <- "Worldwide overview of confirmed COVID-19 cases (daily)"

fig_daily <- make_subfig("confirmed", TRUE, y = ~`daily cases`) %>%
  layout(yaxis = list(title = "daily cases"))
fig_daily_rel <- make_subfig("confirmed", FALSE, y = ~`daily cases/total cases`) %>%
  layout(yaxis = list(title = "relative daily increase"))

fig_daily %<>% subplot(fig_daily_rel, nrows = 2, shareX = TRUE, titleY = TRUE) %>% 
  layout(xaxis = list(rangeslider = list(type = "date")))

fig_daily  %>% layout(title = title_daily) %>% 
  htmlwidgets::appendContent(htmltools::tags$p(caption)) %>% 
  saveWidget("covid-19-time-series-daily.html")
