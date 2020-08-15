library(tidyverse)
library(rvest)
library(lubridate)
library(pbapply)

url <- "https://www.gov.uk/government/collections/slides-and-datasets-to-accompany-coronavirus-press-conferences"

page <- read_html(url)

links <- page %>%
  html_nodes("a") %>%
  html_attr("href")

speech_links <- str_c("https://www.gov.uk", links[str_detect(links, "speeches")]) %>%
  unique()

speech_scrape <- function(url) {
  
  page <- read_html(url)
  
  text <- page %>%
    html_nodes("div.govspeak") %>%
    html_nodes("p") %>%
    html_text() %>%
    .[str_detect(., "Coronavirus press conference", negate = TRUE)] %>%
    str_c(collapse = " ")
  
  date <- page %>%
    html_nodes("dd.app-c-important-metadata__definition") %>%
    html_text() %>%
    dmy()
  
  whom <- page %>%
    html_nodes("a.govuk-link") %>%
    html_text() %>%
    .[str_detect(., "Rt Hon")]
  
  df <- tibble(date, whom, text)
  return(df)
}

all_speeches <- pblapply(speech_links, speech_scrape) %>%
  bind_rows()

library(tidytext)

all_blog_tokens <- all_speeches %>% unnest_tokens("words", "text")
afinn <- get_sentiments("afinn")
all_sent <- all_blog_tokens %>% inner_join(afinn, by = c("words" = "word")) %>% group_by(whom, date) %>% summarise(daily_av_sent = mean(value)) %>% ungroup()
all_sent %>% ggplot(aes(date, daily_av_sent)) + geom_col(aes(fill = as.factor(whom))) + geom_smooth(span = 0.25) + scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") + 
  labs(x = "Date of Liveblog", y = "Average Liveblog Sentiment (AFINN)", 
       title = "Guardian Coronavirus Liveblog Sentiment") + hrbrthemes::theme_ipsum_ps()
