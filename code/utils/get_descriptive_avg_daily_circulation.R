library(rvest)
library(magrittr)
library(dplyr)

scrape_upplaga <- function(newspaper = 'AFTONBLADET'){
  if(stringr::str_detect(newspaper,' ')) {
    n <- paste(stringr::str_split(newspaper, ' ')[[1]], collapse = '+')
  } else {
    n <- newspaper
  }
  url <- paste0("http://tidning.kb.se/nld/nld/main?tidnId=&katId=39&PFD=1945-01-01&PTD=2018-12-31&titel=",n,"&fritext=&fritext2=&sortering=titel&sok=S%C3%B6k")
  url %>%
    read_html() %>%
    html_nodes(., "table") %>%
    .[1] %>%
    html_table(fill = TRUE) -> tmp_tbl
  
  tmp_tbl[[1]] %>%
    mutate(Edition = stringr::str_extract(string = Edition, pattern = 'A|B')) %>%
    filter(!is.na(Edition)) %>%
    group_by(Period, Edition) %>%
    summarise(Upplaga = mean(Upplaga)) %>%
    ungroup() %>%
    group_by(Period) %>%
    summarise(upplaga = sum(Upplaga)) -> tmp_tbl2
  
  tmp_tbl[[1]] %>%
    mutate(Edition = stringr::str_extract(string = Edition, pattern = 'A|B')) %>%
    filter(is.na(Edition)) %>%
    group_by(Period) %>%
    summarise(upplaga = mean(Upplaga)) -> tmp_tbl3
  
  dplyr::bind_rows(tmp_tbl2, tmp_tbl3) %>%
    mutate(year = substr(Period, 1, 4),
           newspaper = newspaper) -> res
  
  
  return(res)
}



dts <- lapply(c('Aftonbladet','Dagens Nyheter','Expressen','Svenska Dagbladet'), function(x) scrape_upplaga(newspaper = x))
dt <- data.table::rbindlist(dts)

library(ggplot2)


dt %>%
  group_by(year) %>%
  summarise(upplaga = sum(upplaga)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = year, y = upplaga, group = 1)) +
  scale_x_discrete(breaks=seq(1945,2018,5)) +
  labs(title = 'Total number of national newspaper editions',
       y = 'Editions (Upplagor)',
       x = 'Year') + 
  theme_bw()

ggplot(dt) +
  geom_line(aes(x = year, y = upplaga, group = newspaper, color = newspaper)) +
  labs(title = 'Total number of national newspaper editions, by newspaper',
       y = 'Editions (Upplagor)',
       x = 'Year') + 
  scale_x_discrete(breaks=seq(1945,2018,5)) +
  theme_bw()

 dt[, mean(upplaga), by = 'newspaper']
 dt[, mean(upplaga), by = 'newspaper']
 