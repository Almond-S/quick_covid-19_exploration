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

# Prepare Johns Hopkins data ---------------------------------------------------

try_get_data <- try({
data_rep <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

corona <- list(
  confirmed = read.csv(paste0(data_rep, "time_series_covid19_confirmed_global.csv"), check.names = FALSE),
  deaths = read.csv(paste0(data_rep, "time_series_covid19_deaths_global.csv"), check.names = FALSE),
  recovered = read.csv(paste0(data_rep, "time_series_covid19_recovered_global.csv"), check.names = FALSE)
)
# fix name issue in recovered
names(corona$recovered)[1] <- "Province/State"

for(i in 1:3) {
  # flatten in long format
  corona[[i]] %<>% melt(id.vars = 1:4, variable.name = "date", value.name = "cases")
  # convert date to proper date
  corona[[i]] %<>% mutate(date = mdy(date))
}
  
# merge
corona <- bind_rows(corona, .id = "type")

# set negative cases ?? to NA
corona[corona$cases < 0, "cases"] <- NA

# save for offline usage
save(corona, file = "corona.RData")
})

if(inherits(try_get_data, "try-error")) load("corona.RData")

## remove longitude and latitude
corona %<>% select(-Lat, -Long)

## summarize countries
corona_country <- corona %>% 
  group_by(`Country/Region`, type, date) %>% summarise(cases = sum(cases)) %>%
  ungroup

# add other aspects
corona_country %<>% group_by(`Country/Region`, type) %>% mutate(
  `daily cases` = c(NA, diff(cases)),
  `daily cases/total cases` = `daily cases`/c(NA, cases[-length(cases)])
) %>% ungroup()
# corona_country[na.omit(which(corona_country$`daily cases/total cases`==-Inf)),
              # "daily cases/total cases"] <- NA


    # cases_today = max(cases, na.rm = TRUE),
    # cases_country = paste(
    #   str_pad(cases_today, 6, pad = 0), 
    #   `Country/Region`)) %>% ungroup()


# merge UN population data -------------------------------------------------

try_pop <- try(load("population.RData"))
if(inherits(try_pop, "try-error")) {
  population <- read.csv("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv", 
                         stringsAsFactors = FALSE, encoding = "UTF-8")
  save(population, file = "population.RData")
}
# use 2019 data
population <- filter(population, Time == 2019) %>% select(Location, PopTotal)
# check country names
othernames <- setdiff(corona_country$`Country/Region`, population$Location)
names(othernames) <- othernames

# adjust corona_country names
detect_othernames <- sapply(othernames, function(pattern) 
  str_detect(population$Location, pattern))
othernames <- sapply(othernames, function(x) NA)
othernames[colSums(detect_othernames)==1] <- as.character(population$Location[
  apply(detect_othernames[,colSums(detect_othernames)==1], 2, which)])
othernames[c("US", "Burma", "Congo (Brazzaville)", "Congo (Kinshasa)")] <- 
  c("United States of America", "Myanmar", "Congo", "Democratic Republic of the Congo")
othernames[c("Cote d'Ivoire", "Korea, South", "Vietnam", "Laos")] <- 
  c("Côte d'Ivoire", "Republic of Korea", "Viet Nam", "Lao People's Democratic Republic")
# => no data yet for 'Diamond Princess' :-), Kosovo and 'West Bank and Gaza'

# switch name/value
othernames <- names(na.omit(othernames)) %>% structure(names = na.omit(othernames))

# change UN names as others are shorter
population %<>% mutate(Location = plyr::revalue(Location, othernames))

# merge
corona_country %<>% left_join(population, by = c("Country/Region" = "Location"))

# calc cases per 100.000 people
corona_country %<>% mutate(
  `cases in 100k` = cases/PopTotal * 100,
  `daily cases in 100k` = `daily cases`/PopTotal * 100
)

# Prepare plots -----------------------------------------------------------

## prepare ranking
corona_country <- corona_country %>% group_by(`Country/Region`) %>%
  mutate(
    cases_today = max(cases, na.rm = TRUE),
    cases_today_in100k = max(`cases in 100k`, na.rm = TRUE)) %>%
  ungroup() %>% mutate(
    rank_today = as.numeric(
      ordered(cases_today, levels = sort(unique(cases_today), TRUE))),
    rank_today_in100k = as.numeric(
      ordered(cases_today_in100k, levels = sort(unique(cases_today_in100k), TRUE))),
    ranked_country = paste(
      str_pad(rank_today, 3, pad = 0), 
      `Country/Region`),
    ranked_country_in100k = paste(
      str_pad(rank_today_in100k, 3, pad = 0), 
      `Country/Region`))
save(corona_country, file = "conrona_country.RData")

selected_countries <- c("Germany", "France", "Spain", "US", "Italy", "United Kingdom")
subcorona <- corona_country %>% filter(`Country/Region` %in% selected_countries)
othercorona <- corona_country %>% filter(!(`Country/Region` %in% selected_countries))

# make repeating colors
n_country <- length(unique(corona_country$`Country/Region`))
cols <- brewer.pal(12, "Set3")[-2] %>% 
  rep(n_country %/% 11 + 1) %>% 
  head(n_country)

# generate figures
make_subfig <- function(.type, .showlegend, y = ~cases, 
                        mode = "lines+markers", name = ~ranked_country) {
  fig <- subcorona %>% 
    filter(type %in% .type) %>%  
    plot_ly(x = ~date, y = y, legendgroup = ~as.character(`Country/Region`),
            name = name,
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

cpt_parts <- list()
cpt_parts$JohnsHopkins <- paste("based on data provided by https://github.com/CSSEGISandData/COVID-19", 
                               "hosted by the Johns Hopkins Corona Resource Center.")
cpt_parts$UNpopulation <- paste("Population estimates obtained from UN World Population Prospects 2019:",
                              "https://population.un.org/wpp/.")
cpt_parts$update <- paste0("Last update: ", Sys.time(), ".")
cpt_parts$create <- "Visualization created by Almond Stoecker. Not warranties of any kind."

caption <- paste(cpt_parts[c("JohnsHopkins", "update", "create")], collapse = " ")
caption_with_population <- paste(cpt_parts, collapse = " ")

rangeslider_config <- list(type = "date", range = list(as.Date("2020-02-22"), as.Date(Sys.time())))

# thumb_plotter

make_thumb <- function(p, name) {
  p %>% layout(showlegend = FALSE) %>% 
    orca( file = paste0("thumb/", name, ".png"), width = 2, height = 1)
}

# plot confirmed only -----------------------------------------------------

title_confirmed <- "Worldwide overview of positively tested COVID-19 cases"
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
title_all <- "Worldwide overview of COVID-19 positively tested cases and deaths and recovered amongst them"
file_all <- "covid-19-time-series-all"

# save
fig_all %>% layout(title = title_all) %>% 
  htmlwidgets::appendContent(htmltools::tags$p(caption)) %>% 
  saveWidget(paste0(file_all, ".html"))
fig_all %>% make_thumb(file_all)


# plot all on log-scale ---------------------------------------------------

title_all_log <- "Worldwide overview of COVID-19 positively tested cases and deaths and recovered amongst them (log-counts)"
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

title_daily <- "Worldwide overview of positively tested COVID-19 cases (daily)"
file_daily <- "covid-19-time-series-daily"

fig_daily <- make_subfig("confirmed", TRUE, y = ~`daily cases`, mode = "lines+markers") %>%
  layout(yaxis = list(title = "daily cases"))
fig_daily_rel <- make_subfig("confirmed", FALSE, y = ~`daily cases/total cases`, mode = "lines+markers") %>%
  layout(yaxis = list(title = "relative daily increase\n (values<1.5)", range = c(0,1.5)))

fig_daily %<>% subplot(fig_daily_rel, nrows = 2, shareX = TRUE, titleY = TRUE)

fig_daily  %>% layout(title = title_daily) %>% 
  layout(xaxis = list(rangeslider = rangeslider_config)) %>% 
  htmlwidgets::appendContent(htmltools::tags$p(caption)) %>% 
  saveWidget(paste0(file_daily, ".html"))
fig_daily %>% make_thumb(file_daily)


# plot daily cases in 100k --------------------------------------------------------

title_daily <- "Worldwide overview of positively tested COVID-19 cases per 100k population (daily)"
file_daily <- "covid-19-time-series-daily-100k"

fig_daily <- make_subfig("confirmed", TRUE, y = ~`daily cases in 100k`, 
                         mode = "lines+markers", name = ~ranked_country_in100k) %>%
  layout(yaxis = list(title = "daily confirmed\n in 100k"))
fig_daily_rel <- make_subfig("deaths", FALSE, y = ~`daily cases in 100k`, 
                             mode = "lines+markers", name = ~ranked_country_in100k) %>%
  layout(yaxis = list(title = "daily deaths\n in 100k"))

fig_daily %<>% subplot(fig_daily_rel, nrows = 2, shareX = TRUE, titleY = TRUE)

fig_daily  %>% layout(title = title_daily) %>% 
  layout(xaxis = list(rangeslider = rangeslider_config)) %>% 
  htmlwidgets::appendContent(htmltools::tags$p(caption_with_population)) %>% 
  saveWidget(paste0(file_daily, ".html"))
fig_daily %>% make_thumb(file_daily)


# plot cases in 100k --------------------------------------------------------

title_in100k <- "Worldwide overview of positively tested COVID-19 cases per 100k population"
file_in100k <- "covid-19-time-series-cases-in-100k"

fig_in100k <- make_subfig("confirmed", TRUE, y = ~`cases in 100k`, 
                          mode = "lines+markers", name = ~ranked_country_in100k) %>%
  layout(yaxis = list(title = "positively tested in 100k"))
fig_in100k_rel <- make_subfig("deaths", FALSE, y = ~`cases in 100k`, 
                              mode = "lines+markers", name = ~ranked_country_in100k) %>%
  layout(yaxis = list(title = "deaths in 100k"))

fig_in100k %<>% subplot(fig_in100k_rel, nrows = 2, shareX = TRUE, titleY = TRUE)

fig_in100k  %>% layout(title = title_in100k) %>% 
  layout(xaxis = list(rangeslider = rangeslider_config)) %>% 
  htmlwidgets::appendContent(htmltools::tags$p(caption_with_population)) %>% 
  saveWidget(paste0(file_in100k, ".html"))
fig_in100k %>% make_thumb(file_in100k)


