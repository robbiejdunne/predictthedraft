row1 <- function(x) {
  row <- x %>%
    filter(stat == 'games') %>%
    with_groups(.groups = url, summarize, count = n())
  {
    row
  }
  }

row1(college.stats %>% filter(url=='https://www.sports-reference.com/cfb/players/tavarres-king-1.html'))

college1 <- row1(college.stats)

final_thing <- left_join(college.stats, college1, by=c('url' = 'url'))

row2 <- function(x) {
  df %>%
  with_groups(.groups = url, slice, n = 1:row)
}

row1(college.stats)

new_thing1 <- final_thing %>% with_groups(.groups = stat, slice, n = 1:count, na.rm=TRUE)

andrew <- final_thing %>% group_by(url, stat) %>% with_groups(.groups = stat, slice, n = 1:count) %>%
  summarise(value = sum(value))

new_1 <- function(x){
  x %>% with_groups(.groups = stat, slice, n = 1:count)}

new_1(final_thing)

russell <- final_thing %>% filter(url== 'https://www.sports-reference.com/cfb/players/russell-wilson-1.html') %>% with_groups(.groups = stat, slice, n = 1:count)

f

russell <- final_thing %>% filter(url== 'https://www.sports-reference.com/cfb/players/russell-wilson-1.html') %>% with_groups(.groups = stat, slice, n = 1:count)

final_thing1 <- final_thing %>% na.omit()

full_on <- final_thing1 %>% group_by(url, stat) %>% dplyr::slice(n = 1:count)

full_on <- final_thing %>% group_by(url, stat) %>% with_groups(.groups = stat, slice, n = 1:count)

full_on1 <- full_on %>% group_by(url, stat, section) %>% summarise(value = sum(value))

full_on2 <- full_on1 %>% select(url, stat, section, value)

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

combined <- full_join(left, right, by = 'key') %>%
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

full_on2$url <- sub("https", "http", full_on2$url)


training2 <- full_on2 %>%
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

write_csv(training, 'thefullshebang.csv')

training %>% filter(pos=='QB') %>% 
  ggplot(mapping = aes(adj_yards_per_attempt, completions)) +
  geom_point()
