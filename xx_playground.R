library(tidyverse)
library(rvest)
library(lubridate)
library(pbapply)
library(tidytext)

# get the url
url <- "https://www.theguardian.com/world/series/coronavirus-live"

# read the page
page <- read_html(url)



# get the total number of articles
total_articles <- page %>%
  html_nodes("span.pagination__legend.hide-on-mobile-inline") %>%
  html_text() %>%
  gsub("[^0-9]+", "", .) %>%
  as.numeric()

# there are 20 to a page so mod it plus 1
num_pages <- (total_articles %/% 20) + 1

# make all the pages
all_pages2 <- paste0(url, "?page=", seq(1, num_pages, 1))

# function to get all the urls
get_urls <- function(url) {
  page <- read_html(url)
  
  urls <- page %>%
    html_nodes("a.u-faux-block-link__overlay.js-headline-text") %>%
    html_attr("href")
  
  return(urls)
}

# get all the articles
all_articles <- map(all_pages, get_urls) %>% unlist()

# function to read an article
get_article <- function(url) {
  
  # setup dummy df
  df <- tibble(date_time = character(),
               words = character())
  
  # while loop
  while (!is.na(url)) {
    
    # read the page
    page <- read_html(url)
    
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
    words <- gsub("[\n]", " ",  trimws(words))
    
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
    url <- ifelse(!is.na(new_page), paste0("https://www.theguardian.com/", new_page), new_page)
  }
  
  # return the df
  return(df)
}

possible_article <- possibly(get_article, tibble(date_time = character(), words = character()))

all_live_blogs <- pblapply(all_articles, possible_article) %>% bind_rows()


all_blogs_clean <- all_live_blogs %>% mutate(date_time = ymd_hms(date_time)) %>% rename(article = words)

all_blogs_tokens <- all_blogs_clean %>% unnest_tokens("words", "article")
afinn <- get_sentiments("afinn")
all_sent <- all_blogs_tokens %>% inner_join(afinn, by = c("words" = "word")) %>% mutate(date = as.Date(date_time)) %>% group_by(date) %>% summarise(daily_av_sent = mean(value)) %>% ungroup()
all_sent %>% ggplot(aes(date, daily_av_sent)) + geom_line() + geom_smooth(span = 0.25) + scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") + labs(x = "Date of Liveblog", y = "Average Liveblog Sentiment (AFINN)", title = "Guardian Coronavirus Liveblog Sentiment") + hrbrthemes::theme_ipsum_ps()