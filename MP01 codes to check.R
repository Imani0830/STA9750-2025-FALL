if(!dir.exists(file.path("data", "mp01"))){
  dir.create(file.path("data", "mp01"), showWarnings=FALSE, recursive=TRUE)
}

GLOBAL_TOP_10_FILENAME <- file.path("data", "mp01", "global_top10_alltime.csv")

if(!file.exists(GLOBAL_TOP_10_FILENAME)){
  download.file("https://www.netflix.com/tudum/top10/data/all-weeks-global.tsv", 
                destfile=GLOBAL_TOP_10_FILENAME)
}

COUNTRY_TOP_10_FILENAME <- file.path("data", "mp01", "country_top10_alltime.csv")

if(!file.exists(COUNTRY_TOP_10_FILENAME)){
  download.file("https://www.netflix.com/tudum/top10/data/all-weeks-countries.tsv", 
                destfile=COUNTRY_TOP_10_FILENAME)
  }

if(!require("tidyverse")) install.packages("tidyverse")
library(readr)
library(dplyr)
library(lubridate)

GLOBAL_TOP_10 <- read_tsv(GLOBAL_TOP_10_FILENAME)

str(GLOBAL_TOP_10)

glimpse(GLOBAL_TOP_10)

GLOBAL_TOP_10 <- GLOBAL_TOP_10 |>
  mutate(season_title = if_else(season_title=="N/A", NA, season_title))

if(!require("tidyverse")) install.packages("tidyverse")
library(readr)
library(dplyr)

COUNTRY_TOP_10 <- read_tsv(COUNTRY_TOP_10_FILENAME)

str(COUNTRY_TOP_10)

glimpse(COUNTRY_TOP_10)

COUNTRY_TOP_10 <- COUNTRY_TOP_10 |>
  mutate(season_title = if_else(season_title=="N/A", NA, season_title))

library(DT)
GLOBAL_TOP_10 |> 
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE))

install.packages("DT") 

library(stringr)
format_titles <- function(df){
  colnames(df) <- str_replace_all(colnames(df), "_", " ") |> str_to_title()
  df
}

GLOBAL_TOP_10 |> 
  format_titles() |>
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE)) |>
  formatRound(c('Weekly Hours Viewed', 'Weekly Views'))

GLOBAL_TOP_10 |> 
  select(-season_title) |>
  format_titles() |>
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE)) |>
  formatRound(c('Weekly Hours Viewed', 'Weekly Views'))


GLOBAL_TOP_10 |> 
  mutate(`runtime_(minutes)` = round(60 * runtime)) |>
  select(-season_title, 
         -runtime) |>
  format_titles() |>
  head(n=20) |>
  datatable(options=list(searching=FALSE, info=FALSE)) |>
  formatRound(c('Weekly Hours Viewed', 'Weekly Views'))


## Warm-Up Questions

### Question 1

Question: "How many different countries does Netflix operate in"
  
COUNTRY_TOP_10 |>
  distinct(country_name)

Answer: "94"

### Question 2 

Question: "Which non-English-language film has spent the most cumulative weeks in the global top 10? How many weeks did it spend?"

GLOBAL_TOP_10 |>
  select(show_title, weekly_rank, cumulative_weeks_in_top_10, category) |>
  filter(category == "Films (Non-English)") |>
  slice_max(cumulative_weeks_in_top_10)
 
Answer: "All Quiet on the Western Front, it spent 23 cumulative weeks in the top 10"

## Question 3

Question: "What is the longest film (English or non-English) to have ever appeared in the Netflix global Top 10? How long is it in minutes?"

GLOBAL_TOP_10 |>
  select(show_title, weekly_rank, runtime) |>
  slice_max(runtime)

Answer: Unitl You Burn is the longest film to have appeared in the global to 10. Its runtime is 47.6.

## Question 4 

Question: "For each of the four categories, what program has the most total hours of global viewership?"

GLOBAL_TOP_10 |>
  distinct(category, weekly_hours_viewed) |>
  select(category, weekly_hours_viewed) |>
  slice_max(weekly_hours_viewed)
  
Answer: The program: TV Non-English has the most total hours of global viewership.

## Question 5

Question: "Which TV show had the longest run in a country’s Top 10? How long was this run and in what country did it occur?"

COUNTRY_TOP_10 |>
  select(show_title, category, cumulative_weeks_in_top_10, country_name) |>
  filter(category == "TV") |>
  slice_max(cumulative_weeks_in_top_10)

COUNTRY_TOP_10|>
  distinct(category)

Answer: Money Heist tv show had the longest run. It was 127 weeks in the top 10, in Pakistan country

## Question 6

Question:"Netflix provides over 200 weeks of service history for all but one country in our data set. Which country is this and when did Netflix cease operations in that country?"

country_weeks 
COUNTRY_TOP_10 |>
  group_by(country_name) |>
  summarise(weeks_of_data = n_distinct(week), latest_date = max(week, na.rm = TRUE), .groups = 'drop') |>
  arrange(weeks_of_data)

Answer: Netflix ceased operations with Russia. The last date that it servuced in Russia was 2-27-22 with only 35 weeks of the latest data reported. 

## Question 7 

Question: What is the total viewership of the TV show Squid Game? Note that there are three seasons total and we are looking for the total number of hours watched across all seasons.

GLOBAL_TOP_10 |>
  filter(show_title == "Squid Game") |>
  group_by(show_title) |>
  summarise(sum(weekly_views, na.rm = TRUE))

Answer: 396900000 ... 

## Question 8 

Question: "The movie Red Notice has a runtime of 1 hour and 58 minutes. Approximately how many views did it receive in 2021? Note that Netflix does not provide the weekly_views values that far back in the past, but you can compute it yourself using the total view time and the runtime."

red_notice_runtime_hours <- 118 / 60 

GLOBAL_TOP_10 |>
  filter(show_title == "Red Notice",
         year(week) == 2021) |>
  summarise(
    total_hours = sum(weekly_hours_viewed, na.rm = TRUE),
    estimated_views = total_hours / red_notice_runtime_hours
  )
  
  Answer: The total number of views are 201732203
  
  ## Question 9
  
Question: "How many Films reached Number 1 in the US but did not originally debut there? That is, find films that first appeared on the Top 10 chart at, e.g., Number 4 but then became more popular and eventually hit Number 1? What is the most recent film to pull this off?"

us_films <- COUNTRY_TOP_10 |>
  filter(country_name == "United States", category == "Films")

us_films |>
  group_by(show_title) |>
  summarise(
    debut_week = min(week),
    debut_rank = weekly_rank[which.min(week)][1],
    hit_number_1 = any(weekly_rank == 1),
    first_week_at_1 = if (any(weekly_rank == 1)) min(week[weekly_rank == 1]) else as.Date(NA),
    .groups = "drop"
  ) |>
  filter(debut_rank > 1, hit_number_1 == TRUE) |>
  arrange(desc(first_week_at_1)) |>
  slice(1) 

nrow(films_that_climbed)

Answer: 45 films reached number 1 in the U.S. but did not origunally debut there. Unknown Number: The high is the most recent film to have pulled this off. It debuted number 4 week of 8-31-25 and first week at number 1 is 9-7-2025.

## Question 10

Question: Which TV show/season hit the top 10 in the most countries in its debut week? In how many countries did it chart?

library(dplyr)

COUNTRY_TOP_10 |>
  filter(category == "TV") |>
  
  group_by(show_title, season_title) |>
  mutate(debut_week = min(week)) |>
  ungroup() |>
    filter(week == debut_week) |>
    group_by(show_title, season_title, debut_week) |>
    summarise(
      countries_in_debut_week = n_distinct(country_name),
      .groups = "drop"
    ) |>
    arrange(desc(countries_in_debut_week)) |>
    slice(1)
  
Answer:In its debut week of 12-26-2021, Emily in Paris, Season 2 hit top 10 in the most counties (94 countries). 
  
## Press ReLease 1- Upcoming Season of Stranger Things (Jul 2016 - current)

## Stranger Things Hasn’t Left the Upside Down — Netflix’s Global Sensation Returns for One Final Season

GLOBAL_TOP_10 |>
select(show_title, cumulative_weeks_in_top_10, weekly_hours_viewed, season_title, runtime) |>
  filter(show_title == "Stranger Things")

Total Hours Viewed

GLOBAL_TOP_10 |>
  filter(str_detect(show_title, "Stranger Things")) |>
  summarise(total_hours = sum(weekly_hours_viewed, na.rm = TRUE)) |>
  pull(total_hours)

Total weeks in Top 10 (globallly)

GLOBAL_TOP_10 |>
  filter(str_detect(show_title, "Stranger Things")) |>
  summarise(max_weeks = max(cumulative_weeks_in_top_10)) |>
  pull(max_weeks)

Number of countries streamed in

COUNTRY_TOP_10 |>
  filter(str_detect(show_title, "Stranger Things")) |>
  summarise(countries = n_distinct(country_name)) |>
  pull(countries)
  
# Compare to other Popular English TV shows: "Wednesday" (Nov 2022-Sep 2025)

GLOBAL_TOP_10 |>
  select(category, show_title, weekly_rank, weekly_hours_viewed, cumulative_weeks_in_top_10, season_title) |>
  filter(category == "TV (English)")
 

GLOBAL_TOP_10 |>
  select(show_title, cumulative_weeks_in_top_10, weekly_hours_viewed, season_title, runtime) |>
  filter(show_title == "Wednesday")


GLOBAL_TOP_10 |>
  filter(str_detect(show_title, "Wednesday")) |>
  summarise(total_hours = sum(weekly_hours_viewed, na.rm = TRUE)) |>
  pull(total_hours)

GLOBAL_TOP_10 |>
  filter(str_detect(show_title, "Wednesday")) |>
  summarise(max_weeks = max(cumulative_weeks_in_top_10)) |>
  pull(max_weeks)

COUNTRY_TOP_10 |>
  filter(str_detect(show_title, "Wednesday")) |>
  summarise(countries = n_distinct(country_name)) |>
  pull(countries)

## Press Release 2: Commercial Success in India

## Streaming the Subcontinent: Netflix Surges Ahead in India’s Entertainment Market

us_titles <- COUNTRY_TOP_10 |>
  filter(country_name == "United States") |>
  distinct(show_title)

india_titles <- COUNTRY_TOP_10 |>
  filter(country_name == "India") |>
  distinct(show_title)

india_only_titles <- india_titles |>
  filter(!(show_title %in% us_titles$show_title))

india_only_titles

# Weeks in the Top 10 Not charted in the U.S. but, other countries

COUNTRY_TOP_10 |>
  filter(country_name == "India") |>
  group_by(show_title) |>
  summarise(
    weeks_in_top_10 = n_distinct(week),
    best_rank = min(weekly_rank, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(!(show_title %in% us_titles$show_title)) |>
  arrange(desc(weeks_in_top_10), best_rank)

# To verify was not charted in the U.S.

COUNTRY_TOP_10 |>
  select(country_name, show_title, weekly_rank, cumulative_weeks_in_top_10) |>
  filter(show_title == "The Railway Men - The Untold Story Of Bhopal 1984") |>
  filter(country_name == "United States")

## Press Release 3: Tyler Perry’s Netflix Partnership Sparks New Era of Subscriber Growth

## Beauty in Black, Binge in Red: Netflix and Tyler Perry’s Hit Series Drives New Viewership Surge

Total Viewership and Ranking - Globally 

GLOBAL_TOP_10 |>
  filter(show_title == "Beauty in Black") |>
  summarise(
    total_hours_viewed = sum(weekly_hours_viewed, na.rm = TRUE),
    total_weeks_in_top10 = n_distinct(week)
  )






