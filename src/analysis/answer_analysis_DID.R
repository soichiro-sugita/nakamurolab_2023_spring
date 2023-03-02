# データセットの作成
library(tidyverse)
source("./src/data/answer_make_dataser_DID.R")

# (8) DIDのためのデータを準備
## カリフォルニア州とその他という2グループのデータ
Cigar_did_sum <- Cigar %>%
  mutate(post = if_else(year > 87, 1, 0),
         ca = if_else(state == 5, 1, 0),
         state = factor(state),
         year_dummy = paste("D", year, sep = "_")) %>%
  group_by(post, year, year_dummy, ca) %>%
  summarise(sales = sum(sales*pop16)/sum(pop16))

## カリフォルニア州とその他の州という州ごとでのデータ
Cigar_did_data <- Cigar %>%
  mutate(post = if_else(year > 87, 1, 0),
         ca = if_else(state == 5, 1, 0),
         state = factor(state),
         year_dummy = paste("D", year, sep = "_")) %>%
  group_by(post, ca, year, year_dummy, state) %>%
  summarise(sales = sum(sales*pop16)/sum(pop16))

# (9) カリフォルニア州とその他というグループでの分析
## 2グループでのデータでの分析
Cigar_did_sum_reg <- Cigar_did_sum %>%
  lm(data = ., sales ~ ca + post + ca:post + year_dummy) %>%
  broom::tidy() %>%
  filter(!str_detect(term, "state"),
         !str_detect(term, "year"))

## 2グループでのデータでの分析(log)
Cigar_did_sum_logreg <- Cigar_did_sum %>%
  lm(data = ., log(sales) ~ ca + post + ca:post + year_dummy) %>%
  broom::tidy() %>%
  filter(!str_detect(term, "state"),
         !str_detect(term, "year"))