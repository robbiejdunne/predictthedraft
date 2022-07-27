library(visdat)

visdat::vis_dat(combine_final)


combine_final <- combine.table %>% mutate(weight = as.numeric(weight),
                                          forty = as.numeric(forty),
                                          vertical = as.numeric(vertical),
                                          bench = as.numeric(bench),
                                          broad = as.numeric(broad),
                                          threecone = as.numeric(threecone),
                                          shuttle = as.numeric(shuttle))

install.packages("measurements")
library("measurements")

ft_inch <- function(str_ft_inch){
  elem <- as.integer(unlist(strsplit(str_ft_inch, "'")))
  inch <- elem[1]*12 + elem[2]
  return(conv_unit(inch, "inch", "cm"))
}

df["cm"] <- sapply(df[,"height"], ft_inch)

right$key <- gsub('https', 'http', right$key)



html.table <- read_html("http://www.pro-football-reference.com/draft/2016-combine.htm") %>%
  html_nodes('table') %>%
  first

html.table

urls <- html.table %>%
  html_nodes('tr td:nth-child(4)') %>%
  url.extract

urls

my.table <- html_table(html.table)


colnames(my.table) <- combine.header


my.table <- my.table %>%
  filter(pos != 'Pos') %>%
  mutate(url = urls)


my.table

qbs <- training %>% filter(pos == 'QB') %>% select('player', 'pick', 'year', 'forty', 'college', 'pos', 'games', 'completions', 'attempts', 'comp_pct', 'pass_yards', 'yards_per_attempt', 'adj_yards_per_attempt', 'pass_tds', 'pass_ints', 'int_rate')

wrs <- training %>% filter(pos == 'WR')

skimr::skim(qbs)

ggplot(qbs, aes(pick, pass_yards)) +
  geom_point()

skimr::skim(training)

training %>% group_by(pos) %>% filter(pick < 257) %>% summarize(mean_pick = mean(pick)) %>%
  ggplot(aes(reorder(pos, -mean_pick), mean_pick)) +
  geom_col() + coord_flip() +
  labs(x = 'Position',
       y = 'Average Pick',
       title = 'Average draft pick by position | NFL drafts from 2010 to 2021') + theme_bw(base_size = 16)

qbs <- training %>% filter(pos == 'QB')

training %>% group_by(pos) %>% summarize(mean_count = n()) %>%
  ggplot(aes(reorder(pos, mean_count), mean_count)) +
  geom_col() + coord_flip() +
  labs(x = 'Position',
       y = 'No. of players',
       title = 'Number of players in draft by position | NFL drafts from 2010 to 2021') + theme_bw(base_size = 16)

cross <- training %>% filter(pos == 'OT')

cross

ggplot(cross) + 
  geom_point(aes(height, forty), size=2.5) +
  geom_point(aes(77, 4.95), color='red', size =4) + theme_bw(base_size = 16) +
  labs(title='Charles Cross height and 40-yard dash time',
       subtitle = 'Cross is highlighted in red')

physical <- training %>% filter(pick < 257) %>% select(age, pick, height, weight,
                                forty, bench, vertical, threecone,
                                broad, shuttle)

visdat::vis_cor(physical)

library(corrplot)

corrplot(physical, method = 'number')

dim(physical)

M <- cor(physical)

corrplot(M, method = 'circle') # colorful number

infer_model <- glm(pick ~ age + height + weight +
                     forty + bench + vertical +
                     threecone + broad + shuttle +
                     games + seasons +
                     completions + attempts +
                     pass_yards + pass_ints + pass_tds + 
                     rec_yards + rec_td + receptions +
                     scrim_yds + adj_yards_per_attempt +
                     int_rate + n_college_picks +
                     comp_pct + rush_avg + scrim_avg + scrim_plays +
                     scrim_tds + scrim_yds + td_tot + total_pts +
                     rush_att + rush_yds + rush_td +
                     solo_tackes + tackles + loss_tackles + ast_tackles +
                     fum_forced + fum_rec + fum_tds + fum_yds +
                     sacks + int + int_td + int_yards + pd +
                     punt_returns + punt_return_td + punt_return_yards +
                     kick_returns + kick_return_td + kick_return_yards, data = train_data)

summary(infer_model)

summary(train_data$ast_tackles)

library(ResourceSelection)
library(pscl)
library(VIF)

### Hosmer-Lemeshow goodness of fit test for log regression
hoslem.test(infer_model$y, fitted(infer_model), g=10) 
anova(infer_model, test = 'Chisq')
pR2(infer_model) 
VIF::vif(infer_model)


x <- rnorm(50)
y <- runif(30)

x
y
# Do x and y come from the same distribution?
ks.test(x, y)


ks.test(train_data$rec_avg, train_data$pick)
