# 分析 netflix、amazon_prime、disney_plus 和 hulu
# netflix(nf)

# library ----
library(tidyverse) 
library(forcats)
library(janeaustenr)
library(tidytext)
library(reshape2)
library(lubridate)
library(ggplot2)
library(GGally)
library(tibble)
library(naniar)
library(dplyr)
library(tidygraph)
library(DT)
library(plotly)
library(quanteda)
library(wordcloud)

# Read data ----

list.files(path = "data/netflix_titles.csv")
netflix <- read.csv("data/netflix_titles.csv")


glimpse(netflix)
summary(netflix)
gg_miss_which(netflix)


nf_added_date = as.character.Date(netflix$date_added)
nf_added_date = mdy(nf_added_date)
nf_added_year = year(nf_added_date)


# Are Movies on Netflix more than TV shows? ----
# Netflix 上的電影不僅僅是電視節目嗎？
netflix %>% count(type, sort = T) %>%
  
  mutate(prop = paste0(round(n / sum(n) * 100, 0), "%")) %>%
  ggplot(aes(x = "", y = prop, fill = type)) +
  geom_bar(
    stat = "identity",
    width = 1,
    color = "steelblue",
    size = 1
  ) +
  coord_polar("y", start = 0) +
  geom_text(
    aes(y = prop, label = prop),
    position = position_stack(vjust = 0.5),
    size = 6,
    col = "white",
    fontface = "bold"
  ) +
  scale_fill_manual (values = c('#e41a1c', '#377eb8')) +
  theme_void() +
  labs(
    title = "Are Movies on Netflix more than TV shows?",
    subtitle = "Pie Plot, proportion of Movies to TV shows",
    caption = "Kaggle: Netflix Movies and TV Shows",
    fill = ""
  )


# Years Difference between release year and added year!-----
# 年份 發布年份和添加年份之間的差異！

netflix <-netflix %>% 
  mutate(nf_year_diff = nf_added_year-release_year) 

netflix %>% count(nf_year_diff, sort = F) 
#由此得知有些項目在發布前就於平台上架，發布同年就上架的項目最多 有3241筆


#列出在發布前就於平台上架的項目
datatable(
  netflix %>% select(-cast, -description) %>%
    filter(nf_year_diff < 0) %>%
    arrange(nf_year_diff),
  caption = NULL,
  options = list(dom = 't')
)


netflix %>% select(nf_year_diff) %>%
  filter(!is.na(nf_year_diff)) %>%
  plot_ly(x = ~ nf_year_diff,
          type = "histogram",
          marker = list(line = list(color = "darkgray",
                                    width = 1.2))) %>%
  layout(
    title = "項目上架年份與發佈年份間隔年數",
    yaxis = list(title = "Count",
                 zeroline = FALSE),
    xaxis = list(title = "difference (Years)",
                 zeroline = FALSE)
  )

#列出 項目上架年份與發佈年份間隔年數最多的 前10名
datatable(netflix %>% select(title, type, release_year, date_added, nf_year_diff) %>%
            filter(nf_year_diff > 10) %>% 
            arrange(desc(nf_year_diff)),
          caption = NULL,
          options = list(dom = 't')
)


# Rating by Type ----
netflix %>% select(rating, type) %>%
  filter(!is.na(rating)) %>%
  mutate(rating = fct_lump(rating, 5)) %>%
  group_by(rating, type) %>%
  summarise(Count = n()) %>%
  arrange(Count) %>%
  plot_ly(
    x = ~ type ,
    y = ~ Count,
    type = "bar",
    color = ~ rating,
    text = ~ Count,
    textposition = 'outside',
    textfont = list(color = '#000000', size = 12)
  ) %>%
  layout(yaxis = list(categoryorder = "array",
                      categoryarray = ~ Count)) %>%
  layout(
    title = "Rating by Type",
    yaxis = list(title = "Type"),
    xaxis = list(title = "Count"),
    legend = list(title = list(text = '<b> Rating </b>'))
  )

# Distribution by Countries Top 10 ----
netflix %>% select(country) %>%
  filter(!is.na(country)) %>%
  mutate(country = fct_lump(country, 20)) %>%
  group_by(country) %>%
  summarise(Count = n()) %>%
  arrange(Count) %>%
  plot_ly(
    x = ~ Count ,
    y = ~ country,
    type = "bar",
    orientation = 'h'
  ) %>%
  layout(yaxis = list(categoryorder = "array", categoryarray = ~ Count)) %>%
  layout(
    title = "Items distribution by Country",
    yaxis = list(title = "Country"),
    xaxis = list(title = "Count")
  )

netflix %>% select(country) %>%
  filter(!is.na(country)) %>%
  mutate(country = fct_lump(country, 45)) %>%
  group_by(country) %>%
  summarise(Count = n()) %>%
  arrange(Count) %>%
  plot_ly(
    x = ~ Count ,
    y = ~ country,
    type = "bar",
    orientation = 'h'
  ) %>%
  layout(yaxis = list(categoryorder = "array", categoryarray = ~ Count)) %>%
  layout(
    title = "Items distribution by Country",
    yaxis = list(title = "Country"),
    xaxis = list(title = "Count")
  )


datatable(netflix %>% 
            select(-cast, -description) %>% 
            filter(!is.na(country),
                   country == "Taiwan"),
          caption = NULL,
          options = list(dom = 't'))

# Dataset split to check the durations ----
movies <- netflix %>% select(country, type, duration, rating, title) %>%
  filter(type == "Movie") %>%
  drop_na() %>% 
  mutate(duration_min = parse_number(duration))

tv_show <- netflix %>% select(country, type, duration, rating, title) %>%
  filter(type == "TV Show") %>% 
  drop_na() %>% 
  mutate(duration_season = parse_number(duration))

# Movies Durations
movies %>%
  plot_ly(
    x = ~ duration_min,
    type = "histogram",
    nbinsx = 40,
    marker = list(
      color = "drakblue",
      line = list(color = "black",
                  width = 1)
    )
  ) %>%
  layout(
    title = "電影片長時間",
    yaxis = list(title = "Count",
                 zeroline = FALSE),
    xaxis = list(title = "Duration (min)",
                 zeroline = FALSE)
  ) 

datatable(movies %>% select(title, duration_min) %>% 
            filter(duration_min >200) %>% arrange(desc(duration_min)),
          caption = NULL,
          options = list(dom = 't'))

# TV-Show Durations
tv_show %>% select(duration_season) %>%
  count(duration_season, sort = TRUE) %>%
  ggplot(aes(
    x = as.factor(duration_season),
    y = n,
    label = n
  )) +
  geom_col(aes(fill = duration_season)) +
  geom_text(vjust = -0.5, size = 3, col = "darkgreen") +
  theme_light() +
  theme(legend.position = "none") +
  labs(x = "Season duration",
       y = "Count",
       title = "影集季數",
       subtitle = "Column Plot, Season distrbution",
       caption = "Kaggle: Netflix Movies and TV Shows",
       fill = ""
  )

datatable(tv_show %>% select(title, duration_season) %>% 
            filter(duration_season >15) %>% arrange(desc(duration_season)),
          caption = NULL,
          options = list(dom = 't'))


# Time series ----
ggplotly(
  netflix %>% select(date_added) %>%
    filter(!is.na(date_added)) %>%
    mutate(nf_added_year) %>%
    group_by(nf_added_year) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ggplot(aes(
      x = nf_added_year,
      y = Count,
      label = Count
    )) +
    geom_line(size = 1, col = "darkred", alpha = 0.5) +
    geom_col(alpha = 0.6, fill = "steelblue") +
    geom_text(vjust = -0.7, size = 3) +
    theme_light() +
    scale_y_continuous(label =  NULL) +
    labs(
      x = "Year Added",
      y = "Count",
      title = "Number of Items added per year",
      subtitle = "Column and line Plot, Nunber of Items added per year",
      caption = "Kaggle: Netflix Movies and TV Shows"
    )
)


# word cloud ----
# Most frequent words in description variable For Movies (word cloud) 
desc_words_m <- netflix %>% select(type, show_id, description) %>%
  filter(type == "Movie") %>% 
  unnest_tokens(word, description) %>%
  anti_join(stop_words)

count_word <- desc_words_m %>%
  count(word, sort = TRUE)


wordcloud(words = count_word$word,  
          freq = count_word$n, 
          min.freq = 50,  
          max.words = nrow(count_word), 
          random.order = FALSE,  
          rot.per = 0.1,  
          colors = brewer.pal(8, "Dark2")) 

# Most frequent words in description variable For TV-Shows (word cloud)
desc_words_tv <- netflix %>% select(type, show_id, description) %>%
  filter(type == "TV Show") %>% 
  unnest_tokens(word, description) %>%
  anti_join(stop_words)

count_word <- desc_words_tv %>%
  count(word, sort = TRUE)


wordcloud(words = count_word$word,  
          freq = count_word$n, 
          min.freq = 30,  
          max.words = nrow(count_word), 
          random.order = FALSE,  
          rot.per = 0.1,  
          colors = brewer.pal(8, "Dark2")) 

