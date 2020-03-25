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
library(orca)

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
  mutate(cases_today = max(cases, na.rm = TRUE)) %>%
  filter(date == max(date), type == "confirmed") %>% ungroup %>% 
  arrange(cases_today) %>%
  mutate( `Country/Region` = ordered(`Country/Region`, levels = rev(`Country/Region`))) %>%
  .$`Country/Region`

# add other aspects
corona_country %<>% mutate(
  `daily cases` = c(NA, diff(cases)),
  `daily cases/total cases` = `daily cases`/c(NA, cases[-1])
)
corona_country[na.omit(corona_country$`daily cases/total cases`==-Inf),
              "daily cases/total cases"] <- NA

# apply ranking
corona_country <- corona_country %>% 
  mutate(
    `Country/Region` = ordered(`Country/Region`,
                               levels = levels(corona_country_ranking)),
    ranked_country = paste(
      str_pad(as.numeric(`Country/Region`), 3, pad = 0), 
      `Country/Region`))

    # cases_today = max(cases, na.rm = TRUE),
    # cases_country = paste(
    #   str_pad(cases_today, 6, pad = 0), 
    #   `Country/Region`)) %>% ungroup()



selected_countries <- c("Germany", "France", "Spain", "US", "Italy", "United Kingdom")
subcorona <- corona_country %>% filter(`Country/Region` %in% selected_countries)
othercorona <- corona_country %>% filter(!(`Country/Region` %in% selected_countries))


# Prepare plots -----------------------------------------------------------

# make repeating colors
cols <- brewer.pal(12, "Set3")[-2] %>% 
  rep(length(corona_country_ranking) %/% 11 + 1) %>% 
  head(length(corona_country_ranking))

# generate figures
make_subfig <- function(.type, .showlegend, y = ~cases, mode = "lines+markers") {
  fig <- subcorona %>% 
    filter(type == .type) %>%  
    plot_ly(x = ~date, y = y, legendgroup = ~as.character(`Country/Region`),
            name = ~ranked_country,
            color = ~ranked_country,
            colors = cols,
            type = "scatter", mode = mode, 
            showlegend = .showlegend)
  fig <- fig %>% add_trace(
    data = othercorona %>% filter(type == .type), showlegend = .showlegend,
    visible = "legendonly")
}
figs <- list()
for(i in 1:3) {
  .type <- unique(subcorona$type)[i]
  figs[[i]] <- make_subfig( .type, c(TRUE,FALSE,FALSE)[i] )
  figs[[i]] %<>% layout(
    yaxis = list(title = .type))
}

caption <- paste0("based on data provided by https://github.com/CSSEGISandData/COVID-19 ", 
                  "hosted by the Johns Hopkins Corona Resource Center. ",
                  "Last update: ", Sys.time(), ". ",
                  "Visualization created by Almond Stoecker. ",
                  "No warranties of any kind.")


rangeslider_config <- list(type = "date", range = list(as.Date("2020-02-22"), as.Date(Sys.time())))

# thumb_plotter

make_thumb <- function(p, name) {
  p %>% layout(showlegend = FALSE) %>% 
    orca( file = paste0("thumb/", name, ".png"), width = 2, height = 1)
}

# plot confirmed only -----------------------------------------------------

title_confirmed <- "Worldwide overview of confirmed COVID-19 cases"
file_confirmed <- "covid-19-time-series-confirmed"

# save as individual html
figs[[1]] %>% layout(title = title_confirmed) %>% 
  layout(xaxis = list(rangeslider = rangeslider_config)) %>% 
  htmlwidgets::appendContent(htmltools::tags$p(caption)) %>% 
  saveWidget(paste0(file_confirmed, ".html"))
# save thumb png
figs[[1]] %>% make_thumb(file_confirmed)

# plot all: confirmed, deaths, and recovered ------------------------------

fig_all <- do.call(subplot, c(figs, nrows = 3, shareX = TRUE, titleY = TRUE))
title_all <- "Worldwide overview of COVID-19 time series (counts)"
file_all <- "covid-19-time-series-all"

# save
fig_all %>% layout(title = title_all) %>% 
  htmlwidgets::appendContent(htmltools::tags$p(caption)) %>% 
  saveWidget(paste0(file_all, ".html"))
fig_all %>% make_thumb(file_all)


# plot all on log-scale ---------------------------------------------------

title_all_log <- "Worldwide overview of COVID-19 time series (log-counts)"
file_all_log <- "covid-19-time-series-all-log"

for(i in 1:3) 
  figs[[i]] %<>% layout(yaxis = list(type = "log"))
fig_all <- do.call(subplot, c(figs, nrows = 3, shareX = TRUE, titleY = TRUE))

#save
fig_all %>% layout(title = title_all_log) %>% 
  saveWidget(paste0(file_all_log, ".html"))
fig_all %>%  
  make_thumb(file_all_log)

# plot daily cases --------------------------------------------------------

title_daily <- "Worldwide overview of confirmed COVID-19 cases (daily)"
file_daily <- "covid-19-time-series-daily"

fig_daily <- make_subfig("confirmed", TRUE, y = ~`daily cases`, mode = "lines+markers") %>%
  layout(yaxis = list(title = "daily cases"))
fig_daily_rel <- make_subfig("confirmed", FALSE, y = ~`daily cases/total cases`, mode = "lines+markers") %>%
  layout(yaxis = list(title = "relative daily increase"))

fig_daily %<>% subplot(fig_daily_rel, nrows = 2, shareX = TRUE, titleY = TRUE)

fig_daily  %>% layout(title = title_daily) %>% 
  layout(xaxis = list(rangeslider = rangeslider_config)) %>% 
  htmlwidgets::appendContent(htmltools::tags$p(caption)) %>% 
  saveWidget(paste0(file_daily, ".html"))
fig_daily %>% make_thumb(file_daily)


