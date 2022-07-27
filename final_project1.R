library(rvest)
library(readr)
library(dplyr)
library(RCurl)
library(tidyr)
library(stringr)
library(feather)
library(memoise)
library(mice)

coalesce2 <- function(...) {
  Reduce(function(x, y) {
    i <- which(is.na(x))
    x[i] <- y[i]
    x},
    list(...))
}

my_read_html <- memoise::memoise(
  rvest::read_html, # function to memoise
  ~ memoise::timeout(86400), # in seconds
  cache = cachem::cache_disk("{cache_dir}")
)

read_html_cache <- function(url) {
  fn <- tail(strsplit(url, '/')[[1]], 1)
  fn.path <- paste(fn, sep = '/')
  if (!file.exists(fn.path)) {
    text <- getURL(url)
    write(text, fn.path)
  }
  read_html(fn.path)
}

draft.header <- c('round', 'pick', 'team', 'player', 'pos', 'age', 'to', 'ap1', 'pb', 'st', 'carav', 'drav', 'games', 'pass.cmp', 'pass.att', 'pass.yds', 'pass.tds', 'pass.ints', 'rush.att', 'rush.yds', 'rush.tds', 'receptions', 'rec.yds', 'rec.tds', 'tackles', 'ints', 'sacks', 'college', 'stats')

combine.header <- c('player', 'pos', 'college', 'stats', 'height', 'weight', 'forty', 'vertical', 'bench', 'broad', 'threecone', 'shuttle', 'drafted')

url.extract <- function(tds) {
  results <- c()
  for(td in tds) {
    children <- html_children(td)
    if (length(children) == 0) {
      results <- c(results, NA)
    } else{
      results <- c(results, (html_attr(html_children(td), 'href')))
    }
  }
  results
}

headers <- list()
headers[['defense']] <- c('year', 'school', 'conf', 'class', 'pos', 'games', 'solo.tackes', 'ast.tackles', 'tackles', 'loss.tackles', 'sacks', 'int', 'int.yards', 'int.yards.avg', 'int.td', 'pd', 'fum.rec', 'fum.yds', 'fum.tds', 'fum.forced')
headers[['scoring']] <- c('year', 'school', 'conf', 'class', 'pos', 'games', 'td.rush', 'td.rec', 'td.int', 'td.fr', 'td.pr', 'td.kr', 'td.oth', 'td.tot', 'kick.xpm', 'kick.fgm', 'twopm', 'safety', 'total.pts')
headers[['punt_ret']] <- c('year', 'school', 'conf', 'class', 'pos', 'games', 'punt.returns', 'punt.return.yards', 'punt.return.avg', 'punt.return.td', 'kick.returns', 'kick.return.yards', 'kick.return.avg', 'kick.return.td')
headers[['receiving']] <- c('year', 'school', 'conf', 'class', 'pos', 'games', 'receptions', 'rec.yards', 'rec.avg', 'rec.td', 'rush.att', 'rush.yds', 'rush.avg', 'rush.td', 'scrim.plays', 'scrim.yds', 'scrim.avg', 'scrim.tds')
headers[['rushing']] <- c('year', 'school', 'conf', 'class', 'pos', 'games', 'receptions', 'rec.yards', 'rec.avg', 'rec.td', 'rush.att', 'rush.yds', 'rush.avg', 'rush.td', 'scrim.plays', 'scrim.yds', 'scrim.avg', 'scrim.tds')
headers[['passing']] <- c('year', 'school', 'conf', 'class', 'pos', 'games', 'completions', 'attempts', 'comp.pct', 'pass.yards', 'yards.per.attempt', 'adj.yards.per.attempt', 'pass.tds', 'pass.ints', 'int.rate')

parse_pfr_tables <- function(tables) {
  results = list()
  for (tbl in tables) {
    id <- html_attr(tbl, 'id')
    if (id %in% names(headers)) {
      
      df <- html_table(tbl) %>%
        head(-1) %>% tail(-1)
      
      if(ncol(df) == length(headers[[id]])) {
        colnames(df) <- headers[[id]]
      } else {
        next;
      }
      
      melted <- df %>%
        select(-year, -school, -conf, -class, -pos) %>%
        mutate(seasons = 1) %>%
        gather(stat, value) %>%
        mutate(stat = as.character(stat)) %>%
        filter(value != '') %>%
        mutate(value = as.numeric(value),
               section = id)
      results[[id]] <- melted
    }
  }
  bind_rows(results)
}

if (!file.exists('fulldrafts2.feather')) {
  
  fulldraft <- data_frame(year = 2016:2021) %>%
    group_by(year) %>% do({
      url <- paste('http://www.pro-football-reference.com/years/', .$year, '/draft.htm', sep ='')
      doc <- read_html(url)
      html.table <- doc %>%
        html_nodes('table') %>%
        first
      urls <- html.table %>%
        html_nodes('tr td:nth-child(29)') %>%
        url.extract
      my.table <- html_table(html.table)
      colnames(my.table) <- draft.header
      my.table <- my.table %>%
        filter(pos != 'Pos') %>%
        mutate(url = urls)
      my.table
    }) %>%
    ungroup
  write_feather(fulldraft, 'fulldrafts2.feather')
  
}

fulldraft

if (!file.exists('fullcombine2.feather')) {
  
  fullcombine <- tibble(year = 2016:2022) %>%
    group_by(year) %>% do({
      url <- paste('http://www.pro-football-reference.com/draft/', .$year, '-combine.htm', sep ='')
      html.table <- read_html(url) %>%
        html_nodes('table') %>%
        first
      urls <- html.table %>%
        html_nodes('tr td:nth-child(4)') %>%
        url.extract
      my.table <- html_table(html.table)
      colnames(my.table) <- combine.header
      my.table <- my.table %>%
        filter(pos != 'Pos') %>%
        mutate(url = urls)
      my.table
    }) %>%
    ungroup
  
  write_feather(fullcombine, 'fullcombine2.feather')
}

fullcombine

fullcombine$url <- sub("https", "http", fullcombine$url)


player.urls <- fullcombine %>%
  select(url) %>%
  filter(!is.na(url))


player.urls1 <- fulldraft %>% select(url) %>%
  filter(!is.na(url))

url.final <- rbind(player.urls, player.urls1)

url.final$url <- sub("https", "http", url.final$url)

url.final

urls.final1 <- url.final %>% distinct()

memoise::forget(my_read_html)

player.urls

fullcollege <- urls.final1 %>%
  group_by(url) %>% do({
    #cat('URL = ', .$url, '\n')
    doc <- read_html_cache(.$url)
    stats <- doc %>%
      html_nodes('table') %>%
      parse_pfr_tables
  })

row1 <- function(x) {
  row <- x %>%
    filter(stat == 'games') %>%
    with_groups(.groups = url, summarize, count = n())
  {
    row
    }
}

fullcollege1 <- row1(fullcollege)

fullcollege2 <- left_join(fullcollege1, fullcollege, by=c('url' = 'url'))

fullcollege3 <- fullcollege2 %>% na.omit()

fullcollege4 <- fullcollege3 %>% group_by(url, stat) %>% dplyr::slice(n = 1:count)

fullcollege5 <- fullcollege4 %>% group_by(url, stat, section) %>% summarise(value = sum(value))

########### DONE FROM HERE ###########

write_feather(fullcollege5, 'fullcollege5.feather')

fullcombine <- read_feather('fullcombine2.feather')
fulldraft <- read_feather('fulldrafts2.feather')
fullcollege5 <- read_feather('fullcollege5.feather')

######## UNDERSTAND #########

fullleft <- fulldraft %>%
  select(year, round, pick, team,
         player,
         college,
         pos,
         age,
         carav,
         drav,
         url) %>%
  mutate(key = ifelse(is.na(url), paste(player, year, sep = '-'), url))

fullleft

fullright <- fullcombine %>%
  select(year_combine = year,
         player_combine = player,
         pos_combine = pos,
         college_combine = college,
         height,
         weight,
         forty,
         vertical,
         broad,
         bench,
         threecone,
         shuttle,
         url_combine = url) %>%
  mutate(key = ifelse(is.na(url_combine),
                      paste(player_combine, year_combine, sep = '-'),
                      url_combine)) %>%
  ## This next block filters out multiple rows with the same player
  group_by(key) %>%
  mutate(appearance = row_number()) %>%
  filter(appearance == 1) %>%
  select(-appearance) %>%
  ungroup

fullright

fullright$url_combine <- sub("https", "http", fullright$url_combine)
fullright$key <- sub("https", "http", fullright$key)

fullcollege5$url <- sub("https", "http", fullcollege5$url)

fullright
fullleft
fullcollege5
fullcombine

fullcombined <- full_join(fullleft, fullright, by = 'key') %>%
  mutate(player = coalesce2(player, player_combine),
         pos = coalesce2(pos, pos_combine),
         college = coalesce2(college, college_combine),
         year = coalesce2(year, year_combine),
         url = coalesce2(url, url_combine))

fullcombined

fullcombined$height <- sub("'", "-", fullcombined$height)
fullcombined$height <- sub("\"", "", fullcombined$height)


fulltraining <- fullcombined %>%
  select(key, carav,
         height, weight,
         forty, vertical,
         bench, age,
         threecone, shuttle,
         broad) %>%
  mutate(height = ifelse(is.na(height), 'NA-NA', height)) %>%
  separate(height, c('feet', 'inches'), sep = '-', convert = TRUE) %>%
  mutate(height = feet * 12 + inches) %>%
  select(-feet, -inches) %>%
  gather(metric, value, carav,
         height, weight,
         forty, vertical,
         bench, age,
         threecone, shuttle,
         broad) %>%
  filter(!is.na(value), value != '') %>%
  mutate(value = as.numeric(value))

fulltraining

fulltraining1a <- fulltraining %>%
  spread(metric, value, fill = NA)

fulltraining1a

fulltraining1b <- complete(mice(fulltraining1a %>% select(-key, -carav)))

fulltraining1b

fulltraining1b$key <- fulltraining1a$key
fulltraining1b$key

fulltraining1b

fulltraining1b$carav <- fulltraining1a$carav
fulltraining1b$carav

fulltraining1c <- fulltraining1b %>%
  gather(metric, value, -key)

fulltraining1c

fulltraining2 <- fullcollege5 %>%
  group_by(url, stat) %>%
  mutate(row = row_number()) %>%
  filter(row == 1) %>%
  select(-row) %>%
  ungroup %>%
  rename(key = url, metric = stat) %>%
  select(-section) %>%
  mutate(metric = str_replace_all(metric, '[.]', '_'))

fulltraining2

fulltraining1c$key <- sub("https", "http", fulltraining1c$key)


## Convert back into wide form
fulltraining3 <- bind_rows(fulltraining1c, fulltraining2) %>%
  spread(metric, value, fill = 0) ## note we fill zeros, not NAs

fulltraining3

## Join the pick/position/college/year/team back on
## Aggregate smaller schools into representative small school
fulltrainingfinal <- fullcombined %>%
  select(key, player, pick, pos, college, year, team) %>%
  group_by(college) %>%
  mutate(n_college_picks = n()) %>%
  ungroup %>%
  mutate(short_college = ifelse(n_college_picks < 50, 'SMALL SCHOOL', college),
         pick = ifelse(is.na(pick), 257, as.numeric(pick))) %>%
  inner_join(fulltraining3)

fulltrainingfinal

write_csv(fulltrainingfinal, 'fulltrainingfinaljic.csv')

fulltrainingfinal %>% filter(pos=='QB') %>% 
  ggplot(mapping = aes(adj_yards_per_attempt, completions)) +
  geom_point()


df %>% case_when(fullcombin)
