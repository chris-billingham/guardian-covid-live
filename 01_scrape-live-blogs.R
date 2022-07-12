library(tidyverse)
library(rvest)
library(lubridate)
library(polite)
library(furrr)
library(progressr)

# get the url
url <- "https://www.theguardian.com/world/series/coronavirus-live"

# lets bow to the guardian as i am a polite web scraper
guardian_bow <- bow(url)

# nod for the url of the date
session <- nod(guardian_bow, path = url)

# scrape the page, then get the total number of articles
total_articles <- scrape(session) %>%
  html_nodes("span.pagination__legend.hide-on-mobile-inline") %>%
  html_text() %>%
  str_replace_all(., "[^0-9]+", "") %>%
  as.numeric()

# there are 20 to a page so mod it plus 1
num_pages <- (total_articles %/% 20) + 1

# make all the pages
all_pages <- str_c(url, "?page=", seq(1, num_pages, 1))

# function to get all the urls from each page
get_urls <- function(url) {
  
  # bow for every url
  session <- bow(url)
  
  # scrape the page
  page <- scrape(session)
  
  # get the urls for all links on the page
  urls <- page %>%
    html_nodes("a.u-faux-block-link__overlay.js-headline-text") %>%
    html_attr("href")
  
  # send the urls back out
  return(urls)
}

# set up furrr
plan(multisession, workers = parallel::detectCores() - 1)

# create handler for progressr
handlers(list(
  handler_progress(
    format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
    width    = 120,
    complete = "+"
  )
))

# get all the articles by iterating from all the pages
with_progress({
  p <- progressor(steps = length(all_pages))
  
  all_articles <- map(all_pages, ~{
    p()
    get_urls(.x)
  })
})

all_articles <- all_articles %>% 
  unlist()

# function to read an article
get_article <- function(url) {
  
  # setup dummy df
  df <- tibble(date_time = character(),
               words = character())
  
  # while loop, there is no shame in a while loop!
  # we're using a while loop because, for any single article, i don't know
  # how many pages there are going to be for each article, so we use a while loop
  # which will stop when we get an na for the next url
  while (!is.na(url)) {
    
    # bow for the url
    session <- bow(url)
    
    # read the page
    page <- scrape(session)
    
    # extract the date_time
    date_time <- page %>%
      html_nodes("div.js-article__container") %>%
      html_nodes("a.block-time__link") %>%
      html_nodes("time") %>%
      html_attr("datetime")
    
    # get the words
    words <- page %>%
      html_nodes("div.js-article__container") %>%
      html_nodes("div.block-elements") %>%
      html_text()
    
    # clean them up
    words <- str_trim(words, side = "both") %>%
      str_replace_all(., "[\n]", " ")
    
    # create a df
    new_df <- tibble(date_time = date_time,
                     words = words)
    
    # bind them together
    df <- bind_rows(df, new_df)
    
    # find the next page
    new_page <- page %>%
      html_nodes("div.liveblog-navigation__older") %>%
      html_nodes("a.liveblog-navigation__link--primary") %>%
      html_attr("href") %>%
      unique()
    
    # create the new url
    url <- ifelse(!is.na(new_page), str_c("https://www.theguardian.com/", new_page), new_page)
  }
  
  # return the df
  return(df)
}

# with webscraping you can never trust the internet to work correctly so we're
# going to use a possibly so at least the map will complete
possible_article <- possibly(get_article, 
                             tibble(date_time = character(), 
                                    words = character()))

with_progress({
  p <- progressor(steps = length(all_articles[1:50]))
  
  all_live_blogs <- future_map_dfr(all_articles[1:50], ~{
    p()
    possible_article(.x)
  })
})

# quick tidy up
all_blogs_clean <- all_live_blogs %>% 
  mutate(date_time = ymd_hms(date_time)) %>% 
  rename(article = words)

# save off so we don't have to do that again
saveRDS(all_blogs_clean, "data/all_blogs_clean.rds")
