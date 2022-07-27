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

if (!file.exists('draftsold.feather')) {
  
  draft.table <- data_frame(year = 2010:2015) %>%
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
  write_feather(draft.table, 'draftsold.feather')
  
}

draft.table

if (!file.exists('combineold.feather')) {
  
  combine.table <- tibble(year = 2010:2015) %>%
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
  
  write_feather(combine.table, 'combineold.feather')
}

combine.table

all.urls <- combine.table %>%
  select(url) %>%
  full_join(draft.table %>% select(url)) %>%
  filter(!is.na(url))

player.urls <- combine.table %>%
  select(url) %>%
  filter(!is.na(url))

memoise::forget(my_read_html)

player.urls

college.stats <- player.urls %>%
  group_by(url) %>% do({
    #cat('URL = ', .$url, '\n')
    doc <- read_html_cache(.$url)
    stats <- doc %>%
      html_nodes('table') %>%
      parse_pfr_tables
    if (nrow(stats) > 0) {
      stats <- stats %>%
        group_by(section, stat) %>%
        summarise(value = sum(value))
    }
    stats
  })

########### DONE FROM HERE ###########


write_feather(college.stats, 'college.stats.feather')

combine.table <- read_feather('combineold.feather')
draft.table <- read_feather('draftsold.feather')
college.stats <- read_feather('college.stats.feather')

######## UNDERSTAND #########

left <- draft.table %>%
  select(year, round, pick, team,
         player,
         college,
         pos,
         age,
         carav,
         drav,
         url) %>%
  mutate(key = ifelse(is.na(url), paste(player, year, sep = '-'), url))

left

right <- combine.table %>%
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

right

write.csv(right, 'right_players.csv')

right_players <- read_csv('right_players.csv')

combined <- full_join(left, right_players, by = 'key') %>%
  mutate(player = coalesce2(player, player_combine),
         pos = coalesce2(pos, pos_combine),
         college = coalesce2(college, college_combine),
         year = coalesce2(year, year_combine),
         url = coalesce2(url, url_combine))

combined

combined$height <- sub("'", "-", combined$height)
combined$height <- sub("\"", "", combined$height)


training1 <- combined %>%
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

training1

training1a <- training1 %>%
  spread(metric, value, fill = NA)

training1a

training1b <- complete(mice(training1a %>% select(-key, -carav)))
training1b

training1b$key <- training1a$key
training1b$key

training1b

training1b$carav <- training1a$carav
training1b$carav

training1c <- training1b %>%
  gather(metric, value, -key)

training1c

college.stats$url <- sub("https", "http", college.stats$url)


training2 <- college.stats %>%
  group_by(url, stat) %>%
  mutate(row = row_number()) %>%
  filter(row == 1) %>%
  select(-row) %>%
  ungroup %>%
  rename(key = url, metric = stat) %>%
  select(-section) %>%
  mutate(metric = str_replace_all(metric, '[.]', '_'))

training2

## Convert back into wide form
training3 <- bind_rows(training1c, training2) %>%
  spread(metric, value, fill = 0) ## note we fill zeros, not NAs
training3

## Join the pick/position/college/year/team back on
## Aggregate smaller schools into representative small school
training <- combined %>%
  select(key, player, pick, pos, college, year, team) %>%
  group_by(college) %>%
  mutate(n_college_picks = n()) %>%
  ungroup %>%
  mutate(short_college = ifelse(n_college_picks < 50, 'SMALL SCHOOL', college),
         pick = ifelse(is.na(pick), 257, as.numeric(pick))) %>%
  inner_join(training3)

training

write_csv(training, 'final_nfl_draft1.csv')



N <- nrow(training)
N

train.set <- (rbinom(N, 1, prob = 0.9) == 1 & training$year < 2022)

train.set

test.set <- (!train.set & training$year < 2022)

test.set

holdout.set <- !(test.set | train.set)

holdout.set

# Outcome variables
pick <- training$pick
carav <- training$carav
first.round <- as.numeric(training$pick <= 32)


library(xgboost)

sparseX <- sparse.model.matrix(~  + (1 + factor(pos)) * (1 +
                                                           factor(short_college) +
                                                           age + height + weight +
                                                           forty + bench + vertical +
                                                           threecone + broad + shuttle +
                                                           games + seasons +
                                                           completions + attempts +
                                                           pass_yards + pass_ints + pass_tds + 
                                                           rec_yards + rec_td + receptions +
                                                           rush_att + rush_yds + rush_td +
                                                           solo_tackes + tackles + loss_tackles + ast_tackles +
                                                           fum_forced + fum_rec + fum_tds + fum_yds +
                                                           sacks + int + int_td + int_yards + pd +
                                                           punt_returns + punt_return_td + punt_return_yards +
                                                           kick_returns + kick_return_td + kick_return_yards)
                               ,training)

sparseX

fitX <- model.matrix(~ 0 +
                       factor(pos) + year +
                       # Ensemble the sparse model here.
                       sparse.pick.hat +
                       age + height + weight +
                       forty + bench + vertical +
                       threecone + broad + shuttle +
                       games + seasons +
                       completions + attempts +
                       pass_yards + pass_ints + pass_tds + 
                       rec_yards + rec_td + receptions +
                       rush_att + rush_yds + rush_td +
                       solo_tackes + tackles + loss_tackles + ast_tackles +
                       fum_forced + fum_rec + fum_tds + fum_yds +
                       sacks + int + int_td + int_yards + pd +
                       punt_returns + punt_return_td + punt_return_yards +
                       kick_returns + kick_return_td + kick_return_yards
                     ,training)


b1.tuning <- expand.grid(depth = c(3, 4, 5, 6),
                         rounds = c(50, 100, 150, 200, 250)) %>%
  group_by(depth, rounds) %>%
  do({
    m <- xgboost(data = fitX[train.set,],
                 label = as.numeric(training$pick[train.set] <= 32),
                 max.depth = .$depth,
                 nround =.$rounds,
                 print.every.n = 50,
                 objective = 'binary:logistic')
    yhat <- predict(m, newdata = fitX)
    data_frame(test.set = test.set, yhat = yhat,
               label = as.numeric(training$pick <= 32))
  })

library(glmnet)

m1 <- cv.glmnet(sparseX[train.set,],
                first.round[train.set],
                alpha = 0.5,
                family = 'binomial')

training$sparse.fr.hat <- predict(m1, newx = sparseX, type = 'response')[,1]

library(ROCR)
preds <- prediction(training$sparse.fr.hat[test.set], first.round[test.set])
perf <- performance(preds, 'tpr', 'fpr')
plot(perf)

pre2015 <- with(training, year < 2022)

aucs <- b1.tuning %>%
  ungroup %>%
  filter(test.set) %>%
  group_by(depth, rounds) %>%
  do({
    auc <- performance(prediction(.$yhat, .$label), "auc")@y.values[[1]]
    data_frame(auc = auc)
  }) %>%
  ungroup %>%
  arrange(-auc)

best <- aucs %>% head(1)

best

b1.train <- xgboost(data = fitX[pre2015,],
                    label = first.round[pre2015],
                    max.depth = best$depth,
                    nround = best$rounds,
                    verbose = FALSE,
                    objective = "binary:logistic")

training$fr.hat2015 <- predict(b1.train, newdata = fitX)
preds2015 <- training %>%
  filter(year == 2015) %>%
  arrange(-fr.hat2015) %>%
  mutate(predicted.pick = row_number()) %>%
  select(predicted.pick, pick, player, college, pos, fr.hat2015) %>%
  head(32)
kable(preds2015, digits = 2)
