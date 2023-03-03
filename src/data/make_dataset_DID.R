
# Cigarデータセットの読み込み --------------------------------------------
# install.packages("Ecdat") # 初回のみ
library(Ecdat)

data("Cigar") # import:Cigarというオブジェクトができる

# Common Trend Assumptionの為に分析から特定の州を外す --------------------------------------------
# タバコの税金が1988年以降50セント以上上がった州のリスト
# (=今回関心のある介入以降に一定水準以上のタバコ税増税があった州)
# Alaska, Hawaii, Maryland, Michigan, New Jersey, New York, Washington

skip_state <- c(3,9,10,22,21,23,31,33,48)



### skip_stateに含まれる州のデータを削除 --------------------------------------------
Cigar <- Cigar %>%
  filter(!state %in% skip_state,
         year >= 70) %>%
  mutate(area = if_else(state == 5, "CA", "Rest of US"))


# (8) DIDのためのデータを準備 --------------------------------------------
## カリフォルニア州とその他という2グループのデータ
Cigar_did_sum <- Cigar %>%
  mutate(post = if_else(year > 87, 1, 0),
         ca = if_else(state == 5, 1, 0),
         state = factor(state),
         year_dummy = paste("D", year, sep = "_")) %>%
  group_by(post, year, year_dummy, ca) %>%
  summarise(sales = sum(sales*pop16)/sum(pop16))

write.csv(Cigar_did_sum ,"./data/in/cigar_did_data.csv")

### 分析には Cigar_did_sum を用いる。###
